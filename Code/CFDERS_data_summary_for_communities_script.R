######
## CFDERS data summary code
######

## Here is where you would actually source the functions in "CFDERS_data_summary_for_communities_functions.R" Importantly, all of these functions will require connection to the shared drive as we can't host a lot of the data anywhere else given its confidentiality restraints.

## Testing things out
code.path<- "./Code/" # You may need to update this
source(paste(code.path, "CFDERS_data_summary_for_communities_functions.R", sep = ""))

# This should have loaded three functions: library_check, bind_cfders, cfders_community_name_match, summarize_cfders. The library check is just a helper function the rest use to make sure required libraries are installed. bind_cfders is a function that reads in each of the eyarly data sets and saves the full data file. cfders_community_name_match works on matching cfders ports with vtr community names. summarize_cfders provides data summaries on landed value, volume, and unique dealers for different hierarchical groupings (e.g., year*community, year*community*species*gear)

# Running them
# First, bind_cfders to get cfders data together -- should only need to do this once, but worth checking to see if the function works.
bind_cfders(data.path = "/Volumes/Shared/Research/COCA-conf/Landings/data", 
            out.path = "/Volumes/Shared/Research/COCA-conf/Landings/data/")

# Next, we would do the cfders_community_name_match function -- no need to run this right now as it requires post processing and that's done already.

# Next, the workhorse summarize_cfders function. It does still take a while to read in the full dataset (minutes). We can also add in more summaries, right now it computes:
  # Year - Community (sum and n_distinct)
  # Community across years (mean, sum and n_distinct)
  # Community - Species across years (mean, sum and n_distinct)
  # Community - Gear - Species across years (mean, sum and n_distinct) only for the ports listed in focal.ports vector
cfders.summ<- summarize_cfders(data.path = "/Volumes/Shared/Research/COCA-conf/Landings/data/",
                               out.path = "/Volumes/Shared/Research/COCA-conf/Landings/summaries/",
                               focal.comms<- c("STONINGTON_ME", "PORTLAND_ME", "NEW BEDFORD_MA", "POINT JUDITH_RI"))

# In addition to writing these to a folder, each is saved in the cfders.summ object, which is a list:
names(cfders.summ)
