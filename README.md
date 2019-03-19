# COCA-EcoAndEcon
This is a collection of functions that integrates ecological and economic analyses from the Gulf of Maine Research Institute's Coastal and Ocean Climate Applications Project: Climate vulnerability and adaptation in fishing communities. The project goal is to assess the social-ecological vulnerability of fishing communities to climate impacts, particularly those associated with changes in the availability of target fish species. 

The functions within this repository, located in "CFDERS_data_summary_for_communities_functions.R" are designed to accomplish the following objectives:

1 - Establish a communication system to link vessel trip report communities with fisheries data ports
  Functions: 
  + bind_cfders = Reads in each year of CFDERS fisheries data (2011-2015) and binds them together in one file. This function takes a while given the size of each CFDERS file and likely does not need to be run. Exports combined CFDERS fisheries data csv file.
  + cfders_community_name_match = Establishes a reference table between VTR communities and CFDERS fuseries ports. Exports reference table. 
  + cfders_community_longlat = Geocodes VTR communities/CFDERS fishing ports. Exports reference table.

2 - Summarize fisheries data and then use summary values (total and annual average 2011-2015) to calculate an importance metric for each fish species in every community
  Functions:
  + summarize_cfders = Summarizes CFDERS fisheries data (total landed value and volume, annual average landed value and volume and unique dealers) by community, gear and species (for focal ports) or just by community. Exports summary csv files. 
  + summarize_gar = Summarizes GAR fisheries data (total landed value and volume, annual average landed value and volume) by community and species. Exports summary csv file.
  + sdm_landings_merged = Joins summarized CFDERS fisheries data and GAR fisheries data to species distribution model projections. Exports csv file with distribution model projections, fit statistics, and fisheries data (landed value/volume, importance). 

3 - Weight projected species distribution changes by fisheries importance and then plot these importance weighted distribution changes for all of the communities evaluated within the Northeast U.S. Shelf Large Marine Ecosystem. 
  + community_weighted_changes = Claculates a community scale vulnerability value of projected species distribution changes weighted by their importance to the community (either in value or volume). Exports csv file with community aggregated weighted importance changes and a shelf wide plot of community aggregated weighted importance change values. 
  
To carry out the functions, researchers can use the script "CFDERS_data_summary_for_communities_script.R" For questions, email Andrew Allyn (aallyn@gmri.org) and Brian Kennedy (bkennedy@gmri.org). 
