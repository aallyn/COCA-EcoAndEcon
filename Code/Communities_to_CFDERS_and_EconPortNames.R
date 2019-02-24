##### 
## A short bit of code to look at matches and mismatches between communities identified from VTR data and the ports listed in the CFDERRS data. 
#####

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
library_check(c("raster", "tidyverse"))

# Data pathways
vtr.path<- "/Volumes/Shared/Research/COCA-conf/"
cfderrs.path<- "/Volumes/Shared/Research/COCA-conf/"
out.path<- "./Results/"
### End preliminaries

### Start code
## Read in Footprint data 
vtr.dat<- readRDS(paste(vtr.path, "VTR fishing footprints by community and gear type 2011-2015.rds", sep = ""))

# get the community names
comm.names<- vtr.dat$JGS.COMMUNITY
unique(comm.names) # 126 total communities

## Read in CFDERRS ports
cfderrs.dat<- read_csv(paste(cfderrs.path, "port_name_list.csv", sep = "")) %>%
  dplyr::select(., -X1)
unique(cfderrs.dat$PORT) # 748 unique port codes
unique(cfderrs.dat$PORT_NAME) # Only 684 unique port names
unique(cfderrs.dat$BRAD_PORT_NAME_STATE) # 708 unique brad port names

## Preliminary inspection -- what are going to be some known issues? what column of CFDERRS is most like the VTR community names
head(comm.names)
head(cfderrs.dat)

# Well, immediate issue is that the VTR communities have multiple "_". Let's split the names by the LAST instance of the "_". This should give us two columns, one for community, one for state. Then we can remove any special characters or spaces from the community column in the VTR data as well as the PORT_NAME column for the cfderrs data. Finally, combining the stripped commuinity column with the second state column for VTR and then matching up to a combined stripped PORT_NAME column and PORT_STATE from the CFDERRS should give us the best chance to match. 
# VTR data first
comm.names.split<- strsplit(comm.names, "_\\s*(?=[^_]+$)", perl=TRUE)

# First item in each list element will have community, second has the state...
comm.dat<- data.frame("JGS" = comm.names, "CommunityOnly" = unlist(lapply(comm.names.split, "[", 1)), "StateOnly" = unlist(lapply(comm.names.split, "[", 2)))

# Did that make sense?
unique(comm.dat$StateOnly)
unique(comm.dat$CommunityOnly)

# Seems good...lets clean up the Community only column to remove all spaces and special characters
comm.dat$CommunityOnlyStripped<- str_replace_all(comm.dat$CommunityOnly, "[^[:alnum:]]", "")

# Make the community merge column
comm.dat$MatchColumn<- paste(comm.dat$CommunityOnlyStripped, comm.dat$StateOnly, sep = "")

# Now CFDERRS. In Brad's, there is some addition bizarreness as there are many Port's that have Name (County). So, first get rid of anything bizarre in parentheses?
cfderrs.dat$NoParen<- str_replace(cfderrs.dat$BRAD_PORT_NAME_STATE, " \\(.*\\)", "")
# Once more, without space before paren
cfderrs.dat$NoParen<- str_replace(cfderrs.dat$NoParen, "\\(.*\\)", "")

# Looks good, no strip characters
cfderrs.dat$MatchColumn<- str_replace_all(cfderrs.dat$NoParen, "[^[:alnum:]]", "")

## Merge them together and save the result
comm.dat.unique<- comm.dat[!duplicated(comm.dat$JGS),]
comm.cfderrs.dat<- comm.dat.unique %>%
  left_join(., cfderrs.dat)
names.missed<- comm.cfderrs.dat[is.na(comm.cfderrs.dat$BRAD_PORT_NAME_STATE),]
write_csv(comm.cfderrs.dat, paste(out.path, "VTR_CFDERRS_Comparison.csv", sep = ""))
write_csv(names.missed, paste(out.path, "VTR_CFDERRS_Missed.csv", sep = ""))

## What is going on with these misses?
names.missed$JGS

# Some make a lot of sense -- a bunch of smaller ports combined into one community as in Stonington_Mystic_Pawcatuck_CT. Others though, seem weird. Like Provincetown_MA, Ocean City_MD, Beals Island_ME, Stueben_ME. 

# Can we find those in the original CFDERRS data?
port.search<- c("PROVINCETOWN", "OCEAN CITY", "BEALS", "STUEBEN")

# All of the string detection stuff seems to be behaving oddly. Going back to the basics...
for(i in seq_along(port.search)){
  port.search.use<- port.search[i]
  temp<- cfderrs.dat[grepl(port.search.use, cfderrs.dat$BRAD_PORT_NAME_STATE),]
  
  if(i == 1){
    cfderrs.searched<- temp
  } else {
    cfderrs.searched<- bind_rows(cfderrs.searched, temp)
  }
}

# Alright, so that helps a little bit. We could manually enter these. For the other ones, those are all going to be combinations of ports, I think?
find<- "REEDVILLE"
find.df<- data.frame(cfderrs.dat[grepl(find, cfderrs.dat$BRAD_PORT_NAME_STATE),])
find.df

# To fill these in, I went through the VTR_CFDERRS_Comparison file that is generated from the code above. For one's with NAs for Brad's stuff, I then searched the CFDERRS data and added them in manually. I tried to cover all combinations of things. For some, this means we now have multiple rows for the CFDERRS stuff for one JGS community (for example: Stonington, Mystic, Pawcatuck has two rows for Brad/CFDERRS stuff as Stonington and Mystic are unique ports). This was pretty straight forward, with a few exceptions. Like "Prospect" -- there is a Prospect Township and a Prospect Harbor. I only included the Harbor as the Township was island and a long ways from Gouldsboro/Corea and other ports in that JGS community. Finally, I deleted all incidences of (State) or (County). The resulting file is "VTR_CFDERRS_Comparison_Edited_NoCounties.csv"
