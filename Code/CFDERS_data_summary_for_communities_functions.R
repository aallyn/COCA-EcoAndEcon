library_check<- function(libraries) {
  ## Details
  # This function will check for and then either load or install any libraries needed to run subsequent functions
  
  # Args:
  # libraries = Vector of required library names
  
  # Returns: NA, just downloads/loads libraries into current work space.
  
  ## Start function
  lapply(libraries, FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  })
  ## End function
}

bind_cfders<- function(data.path, out.path){
  ## Details
  # This function reads in yearly CFDERS datasets and binds them together. The individual reading and saving is incredible time consuming. So, ideally shouldn't have to do this more than once. After it is done, we can use the full dataset
  
  # Args:
    # data.path = Path to the CFDERS data files
    # out.path = Path to save mean landed value, mean landed volume and unique dealer summary files
  
  # Returns: Data frame with all of the yearly CFDERS datasets. This dataframe is saved as a flat file in out.path directory
  
  ## Start function
  # Install libraries
  library_check(c("tidyverse"))
  
  # Set arguments for debugging -- this will NOT run when you call the function. Though, you can run each line inside the {} and then you will have everything you need to walk through the rest of the function.
  if(FALSE){
    data.path = "/Volumes/Shared/Research/COCA-conf/Landings/data"
    out.path = "/Volumes/Shared/Research/COCA-conf/Landings/data/"
  }
  
  # Read in yearly cfders datasets
  df<- list.files(path = data.path, pattern = "cfders_", full.names = TRUE) %>% 
    map_df(~read.csv(.))
  # Make all columns lower case 
  colnames(df)<- tolower(colnames(df))
  
  # Save it 
  write_csv(df, paste(out.path, "2011_to_2015_combined_cfders.csv", sep = ""))
}

cfders_community_name_match<- function(data.path, out.path){
  ## Details
  # This function reads in the cfders ports and the communities from vtr and does work to match the names for future processing and alignment of cfders data with species distribution model and vtr community data
  
  # Args:
  # data.path = Path to the CFDERS data file of unique names and to VTR raster data for community names
  # out.path = Path to save file that has matched names (AND REQUIRES ADDITIONAL PROCESSING!)
  
  # Returns: NA
  
  ## Start function
  library_check(c("raster", "tidyverse"))
  
  # Read in Footprint data 
  vtr.dat<- readRDS(paste(data.path, "VTR fishing footprints by community and gear type 2011-2015.rds", sep = ""))
  
  # Get
  comm.names<- vtr.dat$JGS.COMMUNITY
  #unique(comm.names) # 126 total communities
  
  ## Read in CFDERS ports
  cfders.dat<- read_csv(paste(cfders.path, "port_name_list.csv", sep = "")) %>%
    dplyr::select(., -X1)
  #unique(cfders.dat$PORT) # 748 unique port codes
  #unique(cfders.dat$PORT_NAME) # Only 684 unique port names
  #unique(cfders.dat$BRAD_PORT_NAME_STATE) # 708 unique brad port names
  
  # Preliminary inspection -- what are going to be some known issues? what column of CFDERRS is most like the VTR community names
  #head(comm.names)
  #head(cfderrs.dat)
  
  # Well, immediate issue is that the VTR communities have multiple "_". Let's split the names by the LAST instance of the "_". This should give us two columns, one for community, one for state. Then we can remove any special characters or spaces from the community column in the VTR data as well as the PORT_NAME column for the cfderrs data. Finally, combining the stripped commuinity column with the second state column for VTR and then matching up to a combined stripped PORT_NAME column and PORT_STATE from the CFDERRS should give us the best chance to match. 
  # VTR data first
  comm.names.split<- strsplit(comm.names, "_\\s*(?=[^_]+$)", perl=TRUE)
  
  # First item in each list element will have community, second has the state...
  comm.dat<- data.frame("JGS" = comm.names, "CommunityOnly" = unlist(lapply(comm.names.split, "[", 1)), "StateOnly" = unlist(lapply(comm.names.split, "[", 2)))
  
  # Did that make sense?
  #unique(comm.dat$StateOnly)
  #unique(comm.dat$CommunityOnly)
  
  # Seems good...lets clean up the Community only column to remove all spaces and special characters
  comm.dat$CommunityOnlyStripped<- str_replace_all(comm.dat$CommunityOnly, "[^[:alnum:]]", "")
  
  # Make the community merge column
  comm.dat$MatchColumn<- paste(comm.dat$CommunityOnlyStripped, comm.dat$StateOnly, sep = "")
  
  # Now CFDERS. In Brad's, there is some addition bizarreness as there are many Port's that have Name (County). So, first get rid of anything bizarre in parentheses?
  cfders.dat$NoParen<- str_replace(cfders.dat$BRAD_PORT_NAME_STATE, " \\(.*\\)", "")
  # Once more, without space before paren
  cfders.dat$NoParen<- str_replace(cfders.dat$NoParen, "\\(.*\\)", "")
  
  # Looks good, no strip characters
  cfders.dat$MatchColumn<- str_replace_all(cfders.dat$NoParen, "[^[:alnum:]]", "")
  
  ## Merge them together and save the result
  comm.dat.unique<- comm.dat[!duplicated(comm.dat$JGS),]
  comm.cfderrs.dat<- comm.dat.unique %>%
    left_join(., cfders.dat)
  names.missed<- comm.cfders.dat[is.na(comm.cfders.dat$BRAD_PORT_NAME_STATE),]
  write_csv(comm.cfders.dat, paste(out.path, "VTR_CFDERS_Comparison.csv", sep = ""))
  write_csv(names.missed, paste(out.path, "VTR_CFDERS_Missed.csv", sep = ""))
  
  ## What is going on with these misses?
  #names.missed$JGS
  
  # Some make a lot of sense -- a bunch of smaller ports combined into one community as in Stonington_Mystic_Pawcatuck_CT. Others though, seem weird. Like Provincetown_MA, Ocean City_MD, Beals Island_ME, Stueben_ME. 
  
  # Can we find those in the original CFDERRS data?
  port.search<- c("PROVINCETOWN", "OCEAN CITY", "BEALS", "STUEBEN")
  
  # All of the string detection stuff seems to be behaving oddly. Going back to the basics...
  for(i in seq_along(port.search)){
    port.search.use<- port.search[i]
    temp<- cfders.dat[grepl(port.search.use, cfders.dat$BRAD_PORT_NAME_STATE),]
    
    if(i == 1){
      cfders.searched<- temp
    } else {
      cfders.searched<- bind_rows(cfders.searched, temp)
    }
  }
  
  # Alright, so that helps a little bit. We could manually enter these. For the other ones, those are all going to be combinations of ports, I think?
  find<- "REEDVILLE"
  find.df<- data.frame(cfders.dat[grepl(find, cfders.dat$BRAD_PORT_NAME_STATE),])
  #find.df
  
  # To fill these in, I went through the VTR_CFDERS_Comparison file that is generated from the code above. For one's with NAs for Brad's stuff, I then searched the CFDERS data and added them in manually. I tried to cover all combinations of things. For some, this means we now have multiple rows for the CFDERS stuff for one JGS community (for example: Stonington, Mystic, Pawcatuck has two rows for Brad/CFDERS stuff as Stonington and Mystic are unique ports). This was pretty straight forward, with a few exceptions. Like "Prospect" -- there is a Prospect Township and a Prospect Harbor. I only included the Harbor as the Township was island and a long ways from Gouldsboro/Corea and other ports in that JGS community. Finally, I deleted all incidences of (State) or (County). The resulting file is "VTR_CFDERS_Comparison_Edited_NoCounties.csv"
  
  # End function
}

summarize_cfders<- function(data.path, out.path, focal.comms){
  ## Details
  # This function summarizes CFDERS landed value and volume data and dealer data. Summaries are provided for:
    # Community - Gear - Species: 2011-2015 Total Landed Value/Volume, Mean Annual Landed Value/Volume, number of distinct dealers for focal communities
    # Community - Species: 2011-2015 Total landed Value, Volume, Mean Annual Landed Value/Volume, number of distinct dealers for all communities
  
  # Args:
    # data.path = Path to the CFDERS datafile (created by bind_cfders) and file with VTR community names
    # out.path = Path to save summary files
    # focal.comms = VTR focal communities to filter full dataset before calculating gear-species summaries

  # Returns: List of tidy datasets, one for each of the summary combinations outlined above. Each of these is also saved individually to the output location specified by out.path
  
  ## Start function
  # Install libraries
  library_check(c("tidyverse"))
  
  # Set arguments for debugging -- this will NOT run when you call the function. Though, you can run each line inside the {} and then you will have everything you need to walk through the rest of the function.
  if(FALSE){
    data.path = "/Volumes/Shared/Research/COCA-conf/Landings/data/"
    out.path = "/Volumes/Shared/Research/COCA-conf/Landings/summaries/"
    focal.comms<- c("STONINGTON_ME", "PORTLAND_ME", "NEW BEDFORD_MA", "POINT JUDITH_RI")
  }
  
  # Bring in cfders and vtr community data
  cfders<- read_csv(paste(data.path, "2011_to_2015_combined_cfders.csv", sep = ""))
  vtr.comm<- read_csv(paste(data.path, "VTR_CFDERS_Comparison_Edited_NoCounties.csv", sep = ""))
  colnames(vtr.comm)<- tolower(colnames(vtr.comm))
  
  df<- cfders %>%
    left_join(vtr.comm)
  
  # Data summaries 
  # First some house keeping, get rid of NA values, NA port names and County or State port names
  df.sub<- df %>%
    drop_na(., spplndlb, sppvalue, brad_port_name_state, port) %>%
    filter(., !grepl("(COUNTY)|(STATE)", brad_port_name_state))
  
  # Now summaries -- every kind of summary that doesn't include community
  # Empty list to store the results
  out.list<- vector("list", length = 2)
  
  # Focal communities by gear first
  focalcomm.spp.gear.temp<- df.sub %>%
    filter(., jgs %in% focal.comms) %>%
    mutate(., gear.type = case_when(negear_name %in% c('BEAM TRAWL, OTHER/NK SPECIES', 'BEAM TRAWL,FISH',
                                                       'OTTER TRAWL, BEAM','OTTER TRAWL, BOTTOM,FISH',
                                                       'OTTER TRAWL, BOTTOM,OTHER', 'OTTER TRAWL, BOTTOM,SCALLOP',
                                                       'OTTER TRAWL, BOTTOM,SHRIMP','OTTER TRAWL, HADDOCK SEPARATOR',
                                                       'OTTER TRAWL, MIDWATER','OTTER TRAWL, RUHLE',
                                                       'OTTER TRAWL,BOTTOM,TWIN','PAIR TRAWL, MIDWATER',
                                                       'TRAWL,OTTER,BOTTOM PAIRED','TRAWL,OTTER,BOTTOM,FISH',
                                                       'TRAWL,OTTER,BOTTOM,OTHER/NK SPECIES',
                                                       'TRAWL,OTTER,BOTTOM,SCALLOP','TRAWL,OTTER,BOTTOM,SHRIMP',
                                                       'TRAWL,OTTER,MIDWATER', 'TRAWL,OTTER,MIDWATER PAIRED')  ~ 'Trawl',
                                    
                                    negear_name %in% c('PURSE SEINE, OTHER/NK SPECIES','SEINE, PURSE')  ~ 'Purse-Seine',
                                    
                                    negear_name %in% c('POT, CONCH/WHELK',    'POT, CRAB',
                                                       'POT, EEL', 'POT, FISH', 'POT, HAG',    'POT, LOBSTER',
                                                       'POT, OTHER','POT/TRAP, LOBSTER INSH NK',
                                                       'POT/TRAP, LOBSTER OFFSH NK', 'POTS + TRAPS, HAGFISH',
                                                       'POTS + TRAPS,EEL', 'POTS + TRAPS,FISH',
                                                       'POTS + TRAPS,OTHER/NK SPECIES', 'TRAP') ~ 'Pots / Traps',
                                    
                                    negear_name %in% c('LONGLINE, BOTTOM', 'LONGLINE, PELAGIC') ~ 'Longline',
                                    
                                    negear_name %in% c('GILL NET, ANCHORED-FLOATING, FISH', 'GILL NET, DRIFT,LARGE MESH',
                                                       'GILL NET, DRIFT,SMALL MESH','GILL NET, DRIFT-SINK, FISH',
                                                       'GILL NET, FIXED OR ANCHORED,SINK, OTHER/NK SPECIES',
                                                       'GILL NET, OTHER','GILL NET, RUNAROUND', 'GILL NET, SINK') ~ 'Gillnet',
                                    
                                    negear_name %in% c('DREDGE, CLAM','DREDGE, OCEAN QUAHOG/SURF CLAM',
                                                       'DREDGE, OTHER','DREDGE, OTHER/NK SPECIES',
                                                       'DREDGE, SCALLOP,SEA','DREDGE, SCALLOP-CHAIN MAT',
                                                       'DREDGE, SURF CLAM + OCEAN QUAHO','DREDGE, URCHIN',
                                                       'DREDGE,SCALLOP,CHAIN MAT,MOD') ~ 'Dredge'))
  
  # First get yearly totals of landed value and volume
  yr.focalcomm.spp.gear<- focalcomm.spp.gear.temp %>%
    group_by(year, jgs, spp_common_name, gear.type) %>% 
    summarize(.,
              "sppvalue.sum" = sum(sppvalue, na.rm = TRUE),
              "spplndlb.sum" = sum(spplndlb, na.rm = TRUE)) %>%
    ungroup()
  
  # Now, get sum across years and average annual landed value and volume
  focalcomm.spp.gear<- yr.focalcomm.spp.gear %>%
    group_by(jgs, spp_common_name, gear.type) %>%
    summarize(., 
              "Totalsppvalue" = sum(sppvalue.sum, na.rm = TRUE),
              "Totalspplndlb" = sum(spplndlb.sum, na.rm = TRUE),
              "Meansppvalue" = mean(sppvalue.sum, na.rm = TRUE),
              "Meanspplndlb" = mean(spplndlb.sum, na.rm = TRUE)) %>%
    ungroup()
  
  # Also want distinct dealers across years
  focalcomm.dealers<- focalcomm.spp.gear.temp %>%
    group_by(jgs, spp_common_name, gear.type) %>%
    summarize(., 
              "DistinctDealers" = n_distinct(dealnum)) %>%
    ungroup() 
  
  # Join dealers with focalcomm.spp.gear and save the file
  focalcomm.spp.gear<- focalcomm.spp.gear %>%
    left_join(., focalcomm.dealers)
  
  out.list[[1]]<- focalcomm.spp.gear
  names(out.list)[1]<- "FocalComm.Spp.Gear"

  # Next, all communities by species (no gear) 
  # First get yearly totals of landed value and volume
  yr.comm.spp<- df.sub %>%
    group_by(year, jgs, spp_common_name) %>% 
    summarize(.,
              "sppvalue.sum" = sum(sppvalue, na.rm = TRUE),
              "spplndlb.sum" = sum(spplndlb, na.rm = TRUE)) %>%
    ungroup()
  
  # Now, get sum across years and average annual landed value and volume
  comm.spp<- yr.comm.spp %>%
    group_by(jgs, spp_common_name) %>%
    summarize(., 
              "Totalsppvalue" = sum(sppvalue.sum, na.rm = TRUE),
              "Totalspplndlb" = sum(spplndlb.sum, na.rm = TRUE),
              "Meansppvalue" = mean(sppvalue.sum, na.rm = TRUE),
              "Meanspplndlb" = mean(spplndlb.sum, na.rm = TRUE)) %>%
    ungroup()
  
  # Also want distinct dealers across years
  comm.dealers<- df.sub %>%
    group_by(jgs, spp_common_name) %>%
    summarize(., 
              "DistinctDealers" = n_distinct(dealnum)) %>%
    ungroup() 
  
  # Join dealers with focalcomm.spp.gear and save the file
  comm.spp<- comm.spp %>%
    left_join(., comm.dealers)
  
  out.list[[2]]<- comm.spp
  names(out.list)[2]<- "Comm.Spp"
  
  
  # Save each independently and then return the list
  for(i in seq_along(out.list)){
    write_csv(data.frame(out.list[[i]]), path = paste(out.path, names(out.list[i]), "summary.csv", sep = ""))
  }
  return(out.list)
  # End function
} 

sdm_fisheries_weightedchange<- function(cfders.path, sdm.path, out.path, focal.comms){
  ## Details
  # This function uses the CFDERS landed value and volume focal port summaries calculated in the "summarize_cfders" function as weights that are applied to species projected changes in relative biomass. 
  
  # Args:
  # data.path = Path to the CFDERS "FocalComm.Spp.Gear" summary file or "Comm.Spp" summary file
  # sdm.path = Path to the SDM "EcoToEconPortData03032019.csv" file
  # out.path = Path to save new file that adds landed value and volume importance weights to the SDM EcoToEcon results
  # focal.comms = VTR focal communities to filter full dataset before calculating gear-species summaries. If null, then the function works with the community-species summaries (no gear)
  
  # Returns: 
  

  ## Start function
  # Install libraries
  library_check(c("tidyverse", "ggmap"))
  
  # Debugging
  if(FALSE){
    cfders.path<-  "/Volumes/Shared/Research/COCA-conf/Landings/summaries/"
    sdm.path<- "~/GitHub/COCA/Results/"
    out.path<- "/Volumes/Shared/Research/COCA-conf/Landings/summaries/"
    focal.ports<- c("STONINGTON_ME", "PORTLAND_ME", "NEW BEDFORD_MA", "POINT JUDITH_RI")
    focal.ports<- NULL
  }
  
  if(!is.null(focal.ports)){
    # Read in species changes
    comm.diffs<- read_csv(paste(sdm.path, "EcoToEconPortData03032019.csv", sep = "")) %>%
      filter(., Community %in% focal.ports)
    colnames(comm.diffs)[11]<- "ProjectionValue"
    
    # LongLat Data
    google.api<- "AIzaSyDqGwT79r3odaMl0Hksx9GZhHmqe37KSEQ"
    register_google(key = google.api)
    geos.longlats<- geocode(location = unique(comm.diffs$CFDERSPortName), output = "latlon")
    comm.geo<- data.frame("CFDERSPortName" = unique(comm.diffs$CFDERSPortName), "Long" = geos.longlats$lon, "Lat" = geos.longlats$lat)
    
    # Merge in long lat data
    comm.diffs<- comm.diffs %>%
      left_join(., comm.geo)
    
    # Fisheries landings data
    fish.dat<- read_csv(paste(cfders.path, "FocalComm.Spp.Gearsummary.csv", sep = ""))
    colnames(fish.dat)<- c("Community", "CFDERSCommonName", "Gear", "TotalValue", "TotalVolume", "MeanValue", "MeanVolume", "DistinctDealers")
    
    # Now, get total community value and volume
    fish.dat.agg<- fish.dat %>%
      group_by(., Community) %>%
      summarize(., "CommTotalValue" = sum(TotalValue, na.rm = TRUE),
                "CommTotalVolume" = sum(TotalVolume, na.rm = TRUE))
    
    # Join community totals to community-species-gear dataset and then calculate proportion
    fish.dat<- fish.dat %>%
      left_join(., fish.dat.agg) %>%
      mutate(., "ProportionValue" = TotalValue/CommTotalValue,
             "ProportionVolume" = TotalVolume/CommTotalVolume)
    
    # Adjust gears to match the port.diffs
    fish.dat$Gear<- ifelse(fish.dat$Gear == "Pots / Traps", "Pot_Trap",
                           ifelse(fish.dat$Gear == "Purse-Seine", "Purse_Seine",
                                  ifelse(is.na(fish.dat$Gear), "Other", fish.dat$Gear)))
    
    # Merge in fisheries landings data
    comm.diffs<- comm.diffs %>%
      left_join(., fish.dat)
    comm.diffs
    summary(comm.diffs)
    
    # Write this out
    write.csv(comm.diffs, file = paste(out.path, "SpeciesFocalCommunityGearTypeChangesLandings_03032019.csv", sep= ""))
    
    # Calculate weights and then weight changes
    comm.diffs$ProjectionValue[is.infinite(comm.diffs$ProjectionValue)]<- 500
    comm.diffs<- comm.diffs %>%
      mutate(., "ChangeWeightedValue" = ProjectionValue*ProportionValue,
             "ChangeWeightedVolume" = ProjectionValue*ProportionVolume)
    
    # Write this out
    write.csv(comm.diffs, file = paste(out.path, "SpeciesFocalCommunityGearTypeWeightedChangesLandings_03032019.csv", sep= ""))
    
    # Port aggregated
    comm.aggregated.weighted.difference<- comm.diffs %>%
      dplyr::group_by(Community, Long, Lat, Footprint, ProjectionScenario) %>%
      dplyr::summarize(., "TotalChangeValue" = sum(ChangeWeightedValue, na.rm = TRUE),
                       "TotalChangeVolume" = sum(ChangeWeightedVolume, na.rm = TRUE))
    
    # Write this out and return it
    write.csv(comm.aggregated.weighted.difference, file = paste(out.path, "FocalCommunityWeightedChangesLandings_03032019.csv", sep= ""))
  } else {
    # No focal communities, so summaries only at ALL gear levels
    # Read in species changes
    comm.diffs<- read_csv(paste(sdm.path, "EcoToEconPortData03032019.csv", sep = "")) %>%
      filter(., Gear == "All")
    colnames(comm.diffs)[11]<- "ProjectionValue"
    
    # LongLat Data
    google.api<- "AIzaSyDqGwT79r3odaMl0Hksx9GZhHmqe37KSEQ"
    register_google(key = google.api)
    geos.longlats<- geocode(location = unique(comm.diffs$CFDERSPortName), output = "latlon")
    comm.geo<- data.frame("CFDERSPortName" = unique(comm.diffs$CFDERSPortName), "Long" = geos.longlats$lon, "Lat" = geos.longlats$lat)
    
    # Merge in long lat data
    comm.diffs<- comm.diffs %>%
      left_join(., comm.geo)
    
    # Fisheries landings data
    fish.dat<- read_csv(paste(cfders.path, "Comm.Sppsummary.csv", sep = ""))
    colnames(fish.dat)<- c("Community", "CFDERSCommonName", "TotalValue", "TotalVolume", "MeanValue", "MeanVolume", "DistinctDealers")
    
    # Now, get total community value and volume
    fish.dat.agg<- fish.dat %>%
      group_by(., Community) %>%
      summarize(., "CommTotalValue" = sum(TotalValue, na.rm = TRUE),
                "CommTotalVolume" = sum(TotalVolume, na.rm = TRUE))
    
    # Join community totals to community-species-gear dataset and then calculate proportion
    fish.dat<- fish.dat %>%
      left_join(., fish.dat.agg) %>%
      mutate(., "ProportionValue" = TotalValue/CommTotalValue,
             "ProportionVolume" = TotalVolume/CommTotalVolume)
    
    # Merge in fisheries landings data
    comm.diffs<- comm.diffs %>%
      left_join(., fish.dat)
    comm.diffs
    summary(comm.diffs)
    
    # Write this out
    write.csv(comm.diffs, file = paste(out.path, "SpeciesCommunityChangesLandings_03032019.csv", sep= ""))
    
    # Calculate weights and then weight changes
    comm.diffs$ProjectionValue[is.infinite(comm.diffs$ProjectionValue)]<- 500
    comm.diffs<- comm.diffs %>%
      mutate(., "ChangeWeightedValue" = ProjectionValue*ProportionValue,
             "ChangeWeightedVolume" = ProjectionValue*ProportionVolume)
    
    # Write this out
    write.csv(comm.diffs, file = paste(out.path, "SpeciesCommunityWeightedChangesLandings_03032019.csv", sep= ""))
    
    # Port aggregated
    comm.aggregated.weighted.difference<- comm.diffs %>%
      dplyr::group_by(Community, Long, Lat, Footprint, ProjectionScenario) %>%
      dplyr::summarize(., "TotalChangeValue" = sum(ChangeWeightedValue, na.rm = TRUE),
                       "TotalChangeVolume" = sum(ChangeWeightedVolume, na.rm = TRUE))
    
    # Write this out and return it
    write.csv(comm.aggregated.weighted.difference, file = paste(out.path, "CommunityWeightedChangesLandings_03032019.csv", sep= ""))
  }
  # End function
}

community_weightedchange_plot<- function(data.path, ){
  # This function plots a shelfwide map of the total projected SDM changes, weighted by landed value and volume importance, calculated in the sdm_fisheries_weightedchange function. 
  
  # Args:
  # data.path = Path to the file that has community - total projected SDM weighted changes "CommunityWeightedChangesLandings_03032019.csv"
  # out.path = Path to save the map
 
  # Returns: 
  
  
  ## Start function
  # Libraries
  library_check(c("tidyverse", "raster", "rgeos", "ggplot2", "viridis", "cowplot"))
  
  # Debugging
  if(FALSE){
    data.path<- "/Volumes/Shared/Research/COCA-conf/Landings/summaries/"
    out.path<- "/Volumes/Shared/Research/COCA-conf/Landings/summaries/"
  }
  
  # Load in data, only need to look at results from climate ensemble mean and regular footprint
  dat<- read_csv(paste(data.path, "CommunityWeightedChangesLandings_03032019.csv", sep= "")) %>%
    filter(., ProjectionScenario == "Future_mean_diff.combo.b" & Footprint == "Regular")
  
  # Spatial stuff -- gets us the states and shoreline
  # Spatial projections
  proj.wgs84<- CRS("+init=epsg:4326") #WGS84
  proj.utm<- CRS("+init=epsg:2960") #UTM 19
  
  #Bounds
  xlim.use<- c(-77, -65)
  ylim.use<- c(35.05, 45.2)
  states <- c("Maine", "New Hampshire", "Massachusetts", "Vermont", "New York", "Rhode Island", "Connecticut", "Delaware", "New Jersey", "Maryland", "Pennsylvania", "Virginia", "North Carolina", "South Carolina", "Georgia", "Florida", "District of Columbia", "West Virgina")
  provinces <- c("Ontario", "Québec", "Nova Scotia", "New Brunswick")
  us <- raster::getData("GADM",country="USA",level=1)
  us.states <- us[us$NAME_1 %in% states,]
  canada <- raster::getData("GADM",country="CAN",level=1)
  ca.provinces <- canada[canada$NAME_1 %in% provinces,]
  us.states.f<- fortify(us.states, NAME_1)
  ca.provinces.f<- fortify(ca.provinces, NAME_1)
  
  # Alright, plot time
  plot.out.value<- ggplot() +
    # Update "fill" and "color" to change map color
    geom_map(data = us.states.f, map = us.states.f,
             aes(map_id = id, group = group),
             fill = "#d9d9d9", color = "gray45", size = 0.15) +
    geom_map(data = ca.provinces.f, map = ca.provinces.f,
             aes(map_id = id, group = group),
             fill = "#d9d9d9", color = "gray45", size = 0.15) +
    geom_point(data = dat, aes(x = Long, y = Lat, fill = TotalChangeValue), shape = 21, size = 4.5, alpha = 0.65) +
    # Here you'd make adjustments to the point colors...
    scale_fill_gradient2(name = "Value", low = "blue", mid = "white", high = "red") +
    ylim(ylim.use) + ylab("Lat") +
    scale_x_continuous("Long", breaks = c(-75.0, -70.0, -65.0), labels = c("-75.0", "-70.0", "-65.0"), limits = xlim.use) +
    coord_fixed(1.3) +
    theme(panel.background = element_rect(fill = "white", color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(fill="white", color = "black"))
  
  plot.out.volume<- ggplot() +
    # Update "fill" and "color" to change map color
    geom_map(data = us.states.f, map = us.states.f,
             aes(map_id = id, group = group),
             fill = "#d9d9d9", color = "gray45", size = 0.15) +
    geom_map(data = ca.provinces.f, map = ca.provinces.f,
             aes(map_id = id, group = group),
             fill = "#d9d9d9", color = "gray45", size = 0.15) +
    geom_point(data = dat, aes(x = Long, y = Lat, fill = TotalChangeVolume), shape = 21, size = 4.5, alpha = 0.65) +
    # Here you'd make adjustments to the point colors...
    scale_fill_gradient2(name = "Volume", low = "blue", mid = "white", high = "red") +
    ylim(ylim.use) + ylab("Lat") +
    scale_x_continuous("Long", breaks = c(-75.0, -70.0, -65.0), labels = c("-75.0", "-70.0", "-65.0"), limits = xlim.use) +
    coord_fixed(1.3) +
    theme(panel.background = element_rect(fill = "white", color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(fill="white", color = "black"))
  
  plot.out<- plot_grid(plot.out.value, plot.out.volume, nrow = 1, labels = c("Value", "Volume"))
  ggsave(paste(out.path, "CommunityAggregatedWeightedChanges.jpg", sep = ""), plot.out, width = 11, height = 8, units = "in")
}
