#Description:
#getting average landings value and volume + unique dlr count for all ports in CFDERS 
#filtering out state and county spillover bins 
#taking annual averages for landings and volume metrics
#joining with Justin's and Lisa Colburn's naming conventions 

#OUTPUTs
#1
# aggregate landings by justin-names 'ports'
# dealer counts for justin-names 'ports' 
#2
# aggregate landings by justin-names * spp
#3
# aggregate landings by justin-names * spp * gear

### Preliminaries!!!
# Helper function to install any needed missing packages
library_check<- function(libraries) {
  lapply(libraries, FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  })
}

# Run it to install needed libraries
library_check(c("tidyverse"))

# Data pathways
cfders.path<- "/Volumes/Shared/Research/2015_2018_COCA/2_Data/CFDERS/csv_files/"
### End preliminaries

## Reading in cfderrs files...
files.list<- list.files(path = cfders.path, pattern  = "cfders")



cfders_2011 <- read.csv("J:/Research/2015_2018_COCA/2_Data/CFDERS/csv_files/cfders_2011.csv")
cfders_2012 <- read.csv("J:/Research/2015_2018_COCA/2_Data/CFDERS/csv_files/cfders_2012.csv")
cfders_2013 <- read.csv("J:/Research/2015_2018_COCA/2_Data/CFDERS/csv_files/cfders_2013.csv")
cfders_2014 <- read.csv("J:/Research/2015_2018_COCA/2_Data/CFDERS/csv_files/cfders_2014.csv")
cfders_2015 <- read.csv("J:/Research/2015_2018_COCA/2_Data/CFDERS/csv_files/cfders_2015.csv")

# Query function for cfders data 
cfders_process <- function(df) {
  df %>% 
    mutate(BRAD_PORT_NAME_STATE = paste(PORT_NAME, PORT_STATE, sep = "_")) %>% #creating port name as described in .csv 
    group_by(YEAR, BRAD_PORT_NAME_STATE, PORT) %>% 
    drop_na(SPPVALUE) %>%                                                 #filtering out NA's in value vocolumn
    filter(!grepl("(COUNTY)", BRAD_PORT_NAME_STATE, fixed = TRUE)) %>% #filtering out county spillover bins
    filter(!grepl("(STATE)", BRAD_PORT_NAME_STATE, fixed = TRUE)) %>%  #filtering out state spillover bins
    summarize(
      value = sum(SPPVALUE),
      volume = sum(SPPLNDLB)) %>% 
    ungroup() %>% 
    mutate(
      yr_cnt = if_else(
        YEAR > 0, 1, 0))
}


#function for counting the total unique dealers across the time period

dealer_cnt <- function(df) {
  
  df %>% 
    mutate(BRAD_PORT_NAME_STATE = paste(PORT_NAME, PORT_STATE, sep = "_")) %>% #creating port name as described in Andrew's thing
    group_by(YEAR, BRAD_PORT_NAME_STATE, PORT) %>%                                              #filtering out NA's in value vocolumn
    filter(!grepl("(COUNTY)", BRAD_PORT_NAME_STATE, fixed = TRUE)) %>% #filtering out county spillover bins
    filter(!grepl("(STATE)", BRAD_PORT_NAME_STATE, fixed = TRUE)) %>%  #filtering out state spillover bins
    distinct(YEAR, BRAD_PORT_NAME_STATE, PORT, DEALNUM)
}


#making the 2011 - 2015 cfders data frames into a list 

cfders.list <- list(cfders_2011,cfders_2012, cfders_2013, cfders_2014, cfders_2015)


#aplying landings function (i could do this better) and appending summed values

cfders_result <- lapply(cfders.list, cfders_process)

total_cfders_20112015 <- rbind(cfders_result[[1]], cfders_result[[2]],cfders_result[[3]],cfders_result[[4]],cfders_result[[5]] )
#writing to file so tedious cfders import is not repeated often
write.csv(total_cfders_20112015, 
          "//LAZ/Shared/Research/COCA-conf/Landings/port names_baseline query/final_workflow/total_cfders_20112015.csv")


#apply unique DEALER function and appending resultant dataframes

dlr_result <- lapply(cfders.list, dealer_cnt)

total_dlr_result <- rbind(dlr_result[[1]], dlr_result[[2]], dlr_result[[3]], dlr_result[[4]], dlr_result[[5]])

#Calculating Unique dealers across the 2011 - 2015 baseline for each port 

unique_dlrs <- total_dlr_result %>% 
  group_by(BRAD_PORT_NAME_STATE, PORT) %>% 
  summarise(
    total_unique_dlr = n_distinct(DEALNUM))


total_cfders_20112015 %>% 
  group_by(BRAD_PORT_NAME_STATE) %>% 
  summarise(
    avg_value = mean(value),
    avg_volume = mean(value)) %>% 
  filter(BRAD_PORT_NAME_STATE %in% c("WESTPORT_MA", "WESTPORT ISLAND_ME", "CUSHING_ME", "CUSHING ISLAND_ME"))


#reading in andrew's table of lisa's + justins naming conventions

justin_names <- read.csv("//LAZ/Shared/Research/COCA-conf/Landings/port names_baseline query/JGS_vs_CFDERRS_PortNamesMatchTable.csv")

###LANDINGS

#taking average of value / volume / and dealer numbers. Also, seeing what x/5 yrs each port has in the data. 

justin_landings_agg <- total_cfders_20112015 %>% 
  left_join(., justin_names, by = c("BRAD_PORT_NAME_STATE" = "BRAD.PORT_NAME_STATE")) %>% 
  group_by(YEAR, JGS.FULL.NAME) %>% 
  summarise(
    value = sum(value),
    volume = sum(volume))


justin_landings_avg <- justin_landings_agg %>% 
  group_by(JGS.FULL.NAME) %>% 
  summarise(
    avg_value = mean(value),
    avg_volume = mean(volume),
    yr_check = sum(YEAR))

###DEALERS 


#summing unique dealers (when necessary) accroding to aggregate columns 

justin_dealer_agg <- unique_dlrs %>% 
  left_join(., justin_names, by = c("BRAD_PORT_NAME_STATE" = "BRAD.PORT_NAME_STATE")) %>% 
  group_by(JGS.FULL.NAME) %>% 
  summarise(
    total_unique_dlr = sum(total_unique_dlr))

###JOINING AND EXPORT

#joining with 2011 2015 baseline 
justin_joined <- justin_landings_avg %>% 
  left_join(., justin_dealer_agg, by = "JGS.FULL.NAME") %>% 
  drop_na(JGS.FULL.NAME)


write.csv(justin_joined, "//LAZ/Shared/Research/COCA-conf/Landings/port names_baseline query/final_workflow/justin_joined.csv")


######
#Port * spp 
##### Outputting landings by 'justin-names' by spp
##identical function as above but adding an additional grouping variable 

cfders_ssp <- function(df) {
  df %>% 
    mutate(BRAD_PORT_NAME_STATE = paste(PORT_NAME, PORT_STATE, sep = "_")) %>% #creating port name as described in .csv 
    group_by(YEAR, BRAD_PORT_NAME_STATE, PORT, SPP_COMMON_NAME) %>% 
    drop_na(SPPVALUE) %>%                                                 #filtering out NA's in value vocolumn
    filter(!grepl("(COUNTY)", BRAD_PORT_NAME_STATE, fixed = TRUE)) %>% #filtering out county spillover bins
    filter(!grepl("(STATE)", BRAD_PORT_NAME_STATE, fixed = TRUE)) %>%  #filtering out state spillover bins
    summarize(
      value = sum(SPPVALUE),
      volume = sum(SPPLNDLB)) %>% 
    ungroup() %>% 
    mutate(
      yr_cnt = if_else(
        YEAR > 0, 1, 0))
}


cfders_spp_result <- lapply(cfders.list, cfders_ssp)

cfders_spp_20112015 <- rbind(cfders_spp_result[[1]], cfders_spp_result[[2]],cfders_spp_result[[3]],
                             cfders_spp_result[[4]],cfders_spp_result[[5]] )
#writing to file so tedious cfders import is not repeated often
write.csv(cfders_spp_20112015, 
          "//LAZ/Shared/Research/COCA-conf/Landings/port names_baseline query/final_workflow/cfders_spp_20112015.csv")



######
#Port * gear * spp
##### Outputting landings by 'justin-names' by gear then  spp
##adding in spp grouping variable and gears via the gear function defined below


##gear function 
require(sqldf)

gear_agg <- function(input_data){sqldf("select *,  
                                       case
                                       when NEGEAR_NAME in('BEAM TRAWL, OTHER/NK SPECIES', 'BEAM TRAWL,FISH',
                                       'OTTER TRAWL, BEAM','OTTER TRAWL, BOTTOM,FISH',
                                       'OTTER TRAWL, BOTTOM,OTHER', 'OTTER TRAWL, BOTTOM,SCALLOP',
                                       'OTTER TRAWL, BOTTOM,SHRIMP','OTTER TRAWL, HADDOCK SEPARATOR',
                                       'OTTER TRAWL, MIDWATER','OTTER TRAWL, RUHLE',
                                       'OTTER TRAWL,BOTTOM,TWIN','PAIR TRAWL, MIDWATER',
                                       'TRAWL,OTTER,BOTTOM PAIRED','TRAWL,OTTER,BOTTOM,FISH',
                                       'TRAWL,OTTER,BOTTOM,OTHER/NK SPECIES',
                                       'TRAWL,OTTER,BOTTOM,SCALLOP','TRAWL,OTTER,BOTTOM,SHRIMP',
                                       'TRAWL,OTTER,MIDWATER', 'TRAWL,OTTER,MIDWATER PAIRED')  then 'Trawl'
                                       
                                       when NEGEAR_NAME in('PURSE SEINE, OTHER/NK SPECIES','SEINE, PURSE')    then 'Purse-Seine'
                                       
                                       when NEGEAR_NAME in('POT, CONCH/WHELK',    'POT, CRAB',
                                       'POT, EEL', 'POT, FISH', 'POT, HAG',    'POT, LOBSTER',
                                       'POT, OTHER','POT/TRAP, LOBSTER INSH NK',
                                       'POT/TRAP, LOBSTER OFFSH NK', 'POTS + TRAPS, HAGFISH',
                                       'POTS + TRAPS,EEL', 'POTS + TRAPS,FISH',
                                       'POTS + TRAPS,OTHER/NK SPECIES', 'TRAP')          then 'Pots / Traps'
                                       
                                       when NEGEAR_NAME in('LONGLINE, BOTTOM', 'LONGLINE, PELAGIC')      then 'Longline'
                                       
                                       when NEGEAR_NAME in('GILL NET, ANCHORED-FLOATING, FISH', 'GILL NET, DRIFT,LARGE MESH',
                                       'GILL NET, DRIFT,SMALL MESH','GILL NET, DRIFT-SINK, FISH',
                                       'GILL NET, FIXED OR ANCHORED,SINK, OTHER/NK SPECIES',
                                       'GILL NET, OTHER','GILL NET, RUNAROUND', 'GILL NET, SINK')     then 'Gillnet'
                                       
                                       when NEGEAR_NAME in('DREDGE, CLAM','DREDGE, OCEAN QUAHOG/SURF CLAM',
                                       'DREDGE, OTHER','DREDGE, OTHER/NK SPECIES',
                                       'DREDGE, SCALLOP,SEA','DREDGE, SCALLOP-CHAIN MAT',
                                       'DREDGE, SURF CLAM + OCEAN QUAHO','DREDGE, URCHIN',
                                       'DREDGE,SCALLOP,CHAIN MAT,MOD')                             
                                       then 'Dredge'
                                       
                                       else 'Other' end as gear_type
                                       from input_data")}


cfders_gear_ssp <- function(df) {
  df_1 <- df %>% filter(PORT %in% c(220101, 223503, 420209, 240403)) %>% 
    mutate(BRAD_PORT_NAME_STATE = paste(PORT_NAME, PORT_STATE, sep = "_"),
           gear_type = case_when(
             NEGEAR_NAME  %in% c('BEAM TRAWL, OTHER/NK SPECIES', 'BEAM TRAWL,FISH',
                                 'OTTER TRAWL, BEAM','OTTER TRAWL, BOTTOM,FISH',
                                 'OTTER TRAWL, BOTTOM,OTHER', 'OTTER TRAWL, BOTTOM,SCALLOP',
                                 'OTTER TRAWL, BOTTOM,SHRIMP','OTTER TRAWL, HADDOCK SEPARATOR',
                                 'OTTER TRAWL, MIDWATER','OTTER TRAWL, RUHLE',
                                 'OTTER TRAWL,BOTTOM,TWIN','PAIR TRAWL, MIDWATER',
                                 'TRAWL,OTTER,BOTTOM PAIRED','TRAWL,OTTER,BOTTOM,FISH',
                                 'TRAWL,OTTER,BOTTOM,OTHER/NK SPECIES',
                                 'TRAWL,OTTER,BOTTOM,SCALLOP','TRAWL,OTTER,BOTTOM,SHRIMP',
                                 'TRAWL,OTTER,MIDWATER', 'TRAWL,OTTER,MIDWATER PAIRED')  ~ 'Trawl',
             
             NEGEAR_NAME %in% c('PURSE SEINE, OTHER/NK SPECIES','SEINE, PURSE')  ~ 'Purse-Seine',
             
             NEGEAR_NAME %in% c('POT, CONCH/WHELK',    'POT, CRAB',
                                'POT, EEL', 'POT, FISH', 'POT, HAG',    'POT, LOBSTER',
                                'POT, OTHER','POT/TRAP, LOBSTER INSH NK',
                                'POT/TRAP, LOBSTER OFFSH NK', 'POTS + TRAPS, HAGFISH',
                                'POTS + TRAPS,EEL', 'POTS + TRAPS,FISH',
                                'POTS + TRAPS,OTHER/NK SPECIES', 'TRAP') ~ 'Pots / Traps',
             
             NEGEAR_NAME %in% c('LONGLINE, BOTTOM', 'LONGLINE, PELAGIC') ~ 'Longline',
             
             NEGEAR_NAME %in% c('GILL NET, ANCHORED-FLOATING, FISH', 'GILL NET, DRIFT,LARGE MESH',
                                'GILL NET, DRIFT,SMALL MESH','GILL NET, DRIFT-SINK, FISH',
                                'GILL NET, FIXED OR ANCHORED,SINK, OTHER/NK SPECIES',
                                'GILL NET, OTHER','GILL NET, RUNAROUND', 'GILL NET, SINK') ~ 'Gillnet',
             
             NEGEAR_NAME %in% c('DREDGE, CLAM','DREDGE, OCEAN QUAHOG/SURF CLAM',
                                'DREDGE, OTHER','DREDGE, OTHER/NK SPECIES',
                                'DREDGE, SCALLOP,SEA','DREDGE, SCALLOP-CHAIN MAT',
                                'DREDGE, SURF CLAM + OCEAN QUAHO','DREDGE, URCHIN',
                                'DREDGE,SCALLOP,CHAIN MAT,MOD') ~ 'Dredge'))
  
  df_1  %>% 
    group_by(YEAR, BRAD_PORT_NAME_STATE, PORT, SPP_COMMON_NAME, gear_type) %>% 
    drop_na(SPPVALUE) %>%                                                 #filtering out NA's in value vocolumn
    filter(!grepl("(COUNTY)", BRAD_PORT_NAME_STATE, fixed = TRUE)) %>% #filtering out county spillover bins
    filter(!grepl("(STATE)", BRAD_PORT_NAME_STATE, fixed = TRUE)) %>%  #filtering out state spillover bins
    summarize(
      value = sum(SPPVALUE),
      volume = sum(SPPLNDLB)) %>% 
    ungroup() %>% 
    mutate(
      yr_cnt = if_else(
        YEAR > 0, 1, 0)) 
}


cfders_sppgear_result <- lapply(cfders.list, cfders_gear_ssp)

cfders_sppgear_20112015 <- rbind(cfders_sppgear_result[[1]], cfders_sppgear_result[[2]],cfders_sppgear_result[[3]],
                                 cfders_sppgear_result[[4]],cfders_sppgear_result[[5]] )

#writing to file so tedious cfders import is not repeated often
write.csv(cfders_sppgear_20112015, 
          "//LAZ/Shared/Research/COCA-conf/Landings/port names_baseline query/final_workflow/cfders_sppgear_20112015.csv")
