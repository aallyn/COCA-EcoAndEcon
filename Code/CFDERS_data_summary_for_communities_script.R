######
## CFDERS data summary code
######

# Background --------------------------------------------------------------
## Here is where you would actually source the functions in "CFDERS_data_summary_for_communities_functions.R" Importantly, all of these functions will require connection to the shared drive.

# Set path to functions code and load them all into R
code.path<- "./Code/" # You may need to update this
source(paste(code.path, "CFDERS_data_summary_for_communities_functions.R", sep = ""))

# This should have loaded nine different functions -- see the README.md file for more information on each function, or feel free to open the "CFDERS_data_summary_for_communities_functions.R" code and inspect each of the functions. 

# Setting paths -------------------------------------
# First, set stem directory to the shared drive, which will be different on a Mac than PC. After doing this, we can then generate all the folder paths to execute subsequent functions. 
# Change stem directory
stem.dir<-  "/Volumes/Shared/Research/"

# Path to the CFDERS yearly data file:
landings.path<- paste(stem.dir, "COCA-conf/SDM and CFDERS Integration/Data/Landings/", sep = "")

# Path to the references tables folder, which includes things like unique VTR communities, unique CFDERS ports, and eventually the VTR_CFDERS name matches with long/lats.
ref.tables.path<- paste(stem.dir, "COCA-conf/SDM and CFDERS Integration/Data/Reference Tables/", sep = "")

# Path to save processed CFDERRS files (summaries of trip data to different levels)
proc.summ.path<- paste(stem.dir, "COCA-conf/SDM and CFDERS Integration/Processed Summaries/", sep = "")

# Path to SDM results
sdm.path<- paste(stem.dir, "COCA-conf/SDM and CFDERS Integration/SDM Projections/", sep = "")

# Executing functions -----------------------------------------------------
# First, bind_cfders to get cfders data together. This function takes a long time, so it is currently set within a run statement. If you want to run it to see if it works, set run = TRUE.
run<- FALSE
if(run){
  bind_cfders(landings.path = landings.path, out.path = landings.path)
}

# Next, we would use the cfders_community_name_match and cfders_communitylonglat functions. Theses are also set within a run TRUE/FALSE block since it requires a google API key and some post processing to get the full list of matching vtr communities and cfders ports. However, also worth project team looking through things to make sure the function is behaving correctly.
run<- FALSE
if(run){
  cfders.comm.match<- cfders_community_name_match(data.path = ref.tables.path, out.path = ref.tables.path)
  cfders.comm.longlat<- cfders_community_longlat(data.path = ref.tables.path, google.api, out.path = ref.tables.path)
}

# After successfully creating reference tables to communicate between CFDERS fisheries data and VTR community fishing data, we can now proceed with calculating fisheries data summaries, joining these with species distribution model projections, and calculating community level importance weighted change vulnerability metrics. 
# Summarizing CFDERS data
cfders.summ<- summarize_cfders(landings.path = landings.path, ref.tables.path = ref.tables.path, out.path = proc.summ.path, focal.comms = c("STONINGTON_ME", "PORTLAND_ME", "NEW BEFORD_MA", "POINT JUDITH_RI"))

# In addition to writing these to a folder, each is saved in the cfders.summ object, which is a list:
names(cfders.summ)

# Summarizing GAR data
gar.summ<- summarize_gar(landings.path = landings.path, ref.tables.path = ref.tables.path, out.path = proc.summ.path)

# In addition to writing the sumamry to folder, summary is also returned as a data frame
str(gar.summ)

# Joining CFDERS and GAR fisheries data to species distribution model projections. Although a bit less efficient, to keep things simpler, this function needs to run with three different landings files: (1) the CFDERS "FocalComm.Spp.Gearsummary" summary file; (2) the "Comm.Sppsummary" summary file and (3) the "GARsummary" file. Instead of doing this one at a time, we can write a quick loop to get them all done.
# Vector of all three of the different fisheries data sets
landings.files<- paste(landings.path, c("FocalComm.Spp.Gearsummary.csv", "Comm.Sppsummary.csv", "GARsummary.csv"), sep = "")

# Empty list to store results
sdm.land.merge<- vector("list", length(landings.files))
names(sdm.land.merge)<- c("FocalComm.Spp.Gearsummary.csv", "Comm.Sppsummary.csv", "GARsummary.csv")

# Quick loop to run each through the "sdm_landings_merged" function, and save each one as part of an overall list
for(i in seq_along(landings.files)){
  file.use<- landings.files[i]
  sdm.land.merge[[i]]<- sdm_landings_merged(sdm.path = sdm.path, landings.file = file.use, focal.comms = c("STONINGTON_ME", "PORTLAND_ME", "NEW BEDFORD_MA", "POINT JUDITH_RI"), out.path = proc.summ.path)
  print(paste(file.use, " is done", sep = ""))
}

# Final step, summarize improtance weighted changes across communities and make a map of these vulnerability values across the Northeast Shelf Large Marine Ecosystem
# Again, we can execute a loop given that each of these could be done for the three different files created by the "sdm_landings_merged" function. 
# Vector of all three of the different fisheries data sets
sdm.land.files<- paste(proc.summ.path, c("SpeciesFocalCommunityGearTypeCFDERSWeightedChanges.csv", "SpeciesCommunityCFDERSWeightedChanges.csv", "SpeciesCommunityGARWeightedChanges.csv"), sep = "")

# Empty list to store results
comm.vuln<- vector("list", length(sdm.land.files))
names(comm.vuln)<- c("FocalCommunityGearAggregatedCFDERSWeightedChanges.csv", "CommunityAggregatedCFDERSWeightedChanges.csv", "CommunityAggregatedGARWeightedChanges.csv")

# Quick loop to run each through the "sdm_landings_merged" function, and save each one as part of an overall list
for(i in seq_along(sdm.land.files)){
  file.use<- sdm.land.files[i]
  comm.vuln[[i]]<- community_weighted_changes(sdm.landings.file = file.use, projection.scenario = c("Raw"), out.path = proc.summ.path, plot = TRUE)
  print(paste(file.use, " is done", sep = ""))
}

