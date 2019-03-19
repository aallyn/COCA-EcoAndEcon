# COCA-EcoAndEcon
This is a collection of functions that helps integrate ecological and economic analyses from the Gulf of Maine Research Institute's Coastal and Ocean Climate Applications Project: Climate vulnerability and adaptation in fishing communities. The project goal is to assess the social-ecological vulnerability of fishing communities to climate impacts, particularly those associated with changes in the availability of target fish species. 

The functions within this repository have three goals:

1 - Establish a communication system to link vessel trip report communities with fisheries data ports
  Functions: 
  + bind_cfders = Reads in each year of CFDERS fisheries data (2011-2015) and binds them together in one file.
  + cfders_community_name_match = Establishes a reference table between VTR communities and CFDERS fuseries ports
  + cfders_community_longlat = Geocodes VTR communities/CFDERS fishing ports 

2 - Summarize fisheries data and then use summary values (total and annual average 2011-2015) to calculate an importance metric for each fish species in every community
  Functions:
  + summarize_cfders = Summarizes CFDERS fisheries data (total landed value and volume, annual average landed value and volume and unique dealers) by community, gear and species (for focal ports) or just by community.
  + summarize_valport = Summarizes GAR fisheries data (total landed value and volume, annual average landed value and volume) by community and species.
  + sdm_landings_merged = 

3 - Weight projected species distribution changes by fisheries importance and then plot these importance weighted distribution changes for all of the communities evaluated within the Northeast U.S. Shelf Large Marine Ecosystem 
