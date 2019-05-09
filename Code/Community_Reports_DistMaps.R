
# Preliminaries -----------------------------------------------------------
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

# Load libraries
library_check(c("tidyverse", "ggmap", "akima", "sf", "cowplot"))

# Stem directory to shared files
stem.dir<- "/Volumes/Shared/Research/COCA-conf/SDM and CFDERS Integration/"

# Directory to save outputs
dropbox.path<- "~/Desktop/"
out.paths.df<- data.frame("Community" = c("STONINGTON_ME", "PORTLAND_ME", "NEW.BEDFORD_MA", "POINT.JUDITH_RI"), "Path" = paste(dropbox.path, c("stonington/", "portland/", "new bedford/", "point judith/"), sep = ""))
out.paths.df<- data.frame("Community" = c("STONINGTON_ME", "PORTLAND_ME", "NEW.BEDFORD_MA", "POINT.JUDITH_RI"), "Path" = paste(dropbox.path))

# Source aesthetics
source("/Volumes/Shared/Research/COCA-conf/Landings/code/comm_reports_aesthetics.R")

# Read in prediction and footprints
preds.file<- paste(stem.dir, "Data/SDM Projections/SDMPredictions.rds", sep = "")
preds<- read_rds(preds.file)

foots.file<- paste(stem.dir, "Data/VTR/All VTR safe fishing footprints by community and gear type 2011-2015.grd", sep = "")
foots<- raster::stack(foots.file)

# Plot function -----------------------------------------------------------
comm_report_plot_func<- function(preds = preds, foots = foots, comm = comm.use, spp = spp.use, season = "both", xlim = c(-75, -66.5), ylim = c(38, 47.5)){
  ## Details
  # This function produces sample figures to accompany the community report vulnerability projections methods text. The function reads in the model predictions, as well as the VTR footprint data and then makes a few potential maps for the community and species defined: baseline and projected, baseline and projected with gear footprints, baseline and projected with gear extents. 
  
  # Args:
  # pred.path = SDM projection results
  # foot.path = VTR fishing footprints
  # comm = Focal community to produce maps 
  # spp = Vector of species names to produce maps for
  # season = Either "SPRING", "FALL" or "both"
  # xlim = Mapping dimensions
  # ylim = Mapping dimensions
  
  ## Start function
  # Install libraries
  library_check(c("tidyverse"))
  
  # Set arguments for debugging -- this will NOT run when you call the function. Though, you can run each line inside the {} and then you will have everything you need to walk through the rest of the function.
  if(FALSE){
    preds = preds
    foots = foots
    comm<- "POINT.JUDITH_RI"
    spp<- c("LONGFIN SQUID")
    season = "both"
    xlim = c(-75.75, -65.4) 
    ylim = c(36, 47.5)
  }
  
  # Spatial stuff
  proj.wgs84<- "+init=epsg:4326" #WGS84
  proj.utm<- "+init=epsg:2960" #UTM 19
  
  # Figure plotting over each of the species
  for(i in seq_along(spp)){
    
    # Housekeeping
    out.path.use<- as.character(out.paths.df$Path[which(out.paths.df$Community == comm.use)])
    
    # Extract projections
    spp.use<- spp[i]
    proj.vals.base<- preds %>%
      filter(., COMNAME == spp.use, Proj.Class == "Baseline.combo.b") %>%
      unnest() %>%
      group_by(., COMNAME, Proj.Class, x, y) %>%
      summarize(., "Projection" = mean(Projection, na.rm = T))
    
    proj.vals.future<- preds %>%
      filter(., COMNAME == spp.use, Proj.Class == "Future_mean.combo.b") %>%
      unnest() %>%
      group_by(., COMNAME, Proj.Class, x, y) %>%
      summarize(., "Projection" = mean(Projection, na.rm = T))
    
    proj.diff<- preds %>%
      filter(., COMNAME == spp.use, Proj.Class == "Future_mean_diff.combo.b") %>%
      unnest() %>%
      group_by(., COMNAME, Proj.Class, x, y) %>%
      summarize(., "Projection" = mean(Projection, na.rm = T))
    
    # Rescaling for nicer maps...
    # NELME domaine
    nelme<- st_read("~/GitHub/COCA/Data/NELME_clipped.shp")
    st_crs(nelme)<- proj.wgs84
    
    # Interpolation grid
    coords.df<- data.frame("x" = proj.vals.base$x, "y" = proj.vals.base$y)
    pred.df<- na.omit(data.frame("x" = coords.df$x, "y" = coords.df$y, "layer" = rep(0, length(coords.df$x))))
    pred.df.interp<- interp(pred.df[,1], pred.df[,2], pred.df[,3], duplicate = "mean", extrap = TRUE,
                            xo=seq(-87.99457, -57.4307, length = 115),
                            yo=seq(22.27352, 48.11657, length = 133))
    pred.df.interp.final<- data.frame(expand.grid(x = pred.df.interp$x, y = pred.df.interp$y), z = c(pred.df.interp$z))
    pred.sp<- st_as_sf(pred.df.interp.final, coords = c("x", "y"), crs = proj.wgs84)
    
    # Baseline
    data.use<- proj.vals.base
    pred.df.base<- na.omit(data.frame("x" = data.use$x, "y" = data.use$y, "layer" = data.use$Projection))
    pred.df.interp<- interp(pred.df.base[,1], pred.df.base[,2], pred.df.base[,3], duplicate = "mean", extrap = TRUE,
                            xo=seq(-87.99457, -57.4307, length = 115),
                            yo=seq(22.27352, 48.11657, length = 133))
    pred.df.interp.final<- data.frame(expand.grid(x = pred.df.interp$x, y = pred.df.interp$y), z = c(round(pred.df.interp$z, 2)))
    pred.sp<- st_as_sf(pred.df.interp.final, coords = c("x", "y"), crs = proj.wgs84)
    
    # Clip to nelme
    pred.df.temp.base<- pred.sp[which(st_intersects(pred.sp, nelme, sparse = FALSE) == TRUE),]
    coords.keep<- as.data.frame(st_coordinates(pred.df.temp.base))
    row.names(coords.keep)<- NULL
    pred.df.base<- data.frame(cbind(coords.keep, "z" = as.numeric(pred.df.temp.base$z)))
    names(pred.df.base)<- c("long", "lat", "z")
    
    # Difference
    data.use<- proj.vals.future
    pred.df.fut<- na.omit(data.frame("x" = data.use$x, "y" = data.use$y, "layer" = data.use$Projection))
    pred.df.interp<- interp(pred.df.fut[,1], pred.df.fut[,2], pred.df.fut[,3], duplicate = "mean", extrap = TRUE,
                            xo=seq(-87.99457, -57.4307, length = 115),
                            yo=seq(22.27352, 48.11657, length = 133))
    pred.df.interp.final<- data.frame(expand.grid(x = pred.df.interp$x, y = pred.df.interp$y), z = c(round(pred.df.interp$z, 2)))
    pred.sp<- st_as_sf(pred.df.interp.final, coords = c("x", "y"), crs = proj.wgs84)
    
    # Clip to nelme
    pred.df.temp.fut<- pred.sp[which(st_intersects(pred.sp, nelme, sparse = FALSE) == TRUE),]
    coords.keep<- as.data.frame(st_coordinates(pred.df.temp.fut))
    row.names(coords.keep)<- NULL
    pred.df.diff<- data.frame(cbind(coords.keep, "z" = as.numeric(pred.df.temp.fut$z)))
    names(pred.df.fut)<- c("long", "lat", "z")
    
    # Difference
    data.use<- proj.diff
    pred.df.diff<- na.omit(data.frame("x" = data.use$x, "y" = data.use$y, "layer" = data.use$Projection))
    pred.df.interp<- interp(pred.df.diff[,1], pred.df.diff[,2], pred.df.diff[,3], duplicate = "mean", extrap = TRUE,
                            xo=seq(-87.99457, -57.4307, length = 115),
                            yo=seq(22.27352, 48.11657, length = 133))
    pred.df.interp.final<- data.frame(expand.grid(x = pred.df.interp$x, y = pred.df.interp$y), z = c(round(pred.df.interp$z, 2)))
    pred.sp<- st_as_sf(pred.df.interp.final, coords = c("x", "y"), crs = proj.wgs84)
    
    # Clip to nelme
    pred.df.temp.diff<- pred.sp[which(st_intersects(pred.sp, nelme, sparse = FALSE) == TRUE),]
    coords.keep<- as.data.frame(st_coordinates(pred.df.temp.diff))
    row.names(coords.keep)<- NULL
    pred.df.diff<- data.frame(cbind(coords.keep, "z" = as.numeric(pred.df.temp.diff$z)))
    names(pred.df.diff)<- c("long", "lat", "z")
    
    # Now, looping over the different footprints
    gears<- c("Pot.Trap", "Trawl", "Gillnet", "Other", "Dredge", "Gillnet", "Longline", "Purse.Seine", "All")
    foot.names<- paste(comm.use, ".", gears, ".", "JGS.SAFE.PROPORTION", sep = "")
    
    for(j in seq_along(gears)){
      gear.use<- foot.names[j]
      foot.use<- foots[[which(names(foots) == gear.use)]]
      
      # Break out if no data
      if(nlayers(foot.use) == 0) {
        next()
      }
      
      # Reclassify
      m <- c(0, Inf, 1,  -Inf, 0, 0)
      rclmat <- matrix(m, ncol=3, byrow=TRUE)
      lay.bin<- raster::reclassify(foot.use, rclmat)
      
      # Get coordinates of footprint
      foot.pts <- as.data.frame(lay.bin, xy = TRUE)
      foot.pts$layer<- ifelse(foot.pts$layer == 1, "1", NA)
      
      # Make the plots -- which points are within the footprint
      keep.preds<- raster::extract(lay.bin, pred.df.temp.diff)
      pred.df.base.tile<- pred.df.base[which(keep.preds == 1),]
      pred.df.diff.tile<- pred.df.diff[which(keep.preds == 1),]

      # Spatial stuff -- gets us the states and shoreline
      state.data<- as_tibble(map_data("state")) %>% 
        rename(state_group = group, state_order = order) %>% 
        filter(region %in% c("maine", "new hampshire", "massachusetts", 
                             "vermont", "rhode island", "connecticut", "new york", "new jersey",
                             "pennsylvania", "delaware", "maryland", "district of columbia", "virginia", 
                             "north carolina"))
      
      # Arbitrary labeling
      pred.df.base<- pred.df.base %>%
        drop_na(z)
      base.breaks.use<- c(0, as.numeric(quantile(pred.df.base$z)))
      base.breaks.use<- base.breaks.use[-c(2,4)]
      base.labels.use<- c("Absent", "Low", "Average", "High")
      
      pred.df.fut<- pred.df.fut %>%
        drop_na(z)
      fut.breaks.use<- c(0, as.numeric(quantile(pred.df.fut$z)))
      fut.labels.use<- c("Absent", "Low", "Fair", "Average", "Moderate", "High")
      
      pred.df.diff<- pred.df.diff %>%
        drop_na(z)
      diff.breaks.use<- c(min(pred.df.diff$z), -0.001, 0.001, max(pred.df.diff$z))
      diff.labels.use<- c("Decreasing", "", "Neutral", "Increasing")
      
      plot.out.base<- ggplot() +
        # Update "fill" and "color" to change map color
        geom_polygon(data = state.data, aes(x=long, y=lat, group=state_group),
                     alpha = .15, colour = "white") + 
        # Here you'd make adjustments to the point colors...
        geom_tile(data = pred.df.base, aes(x = long, y = lat, fill = z)) +
        scale_fill_gradient(name = "2011-2015\nRelative biomass", low = gmri.light.gray, high = gmri.orange, breaks = base.breaks.use, labels = base.labels.use, limits = c(0, max(pred.df.base$z))) +
        geom_tile(data = pred.df.diff.tile, aes(x = long, y = lat), fill = NA, color = gmri.gray, size = 0.5) +
        xlim(xlim) + 
        ylim(ylim) +
        theme_map() + 
        coord_fixed(1.3) +
        theme(legend.position = "right", text = element_text(family = font.family, size = font.size)) 
      
      plot.out.fut<- ggplot() +
        # Update "fill" and "color" to change map color
        geom_polygon(data = state.data, aes(x=long, y=lat, group=state_group),
                     alpha = .15, colour = "white") + 
        # Here you'd make adjustments to the point colors...
        geom_tile(data = pred.df.fut, aes(x = long, y = lat, fill = z)) +
        scale_fill_gradient(name = "Future 2055\nRelative biomass", low = gmri.light.gray, high = gmri.orange, breaks = fut.breaks.use, labels = fut.labels.use, limits = c(0, max(pred.df.fut$z))) +
        xlim(xlim) + 
        ylim(ylim) +
        theme_map() + 
        coord_fixed(1.3) +
        theme(legend.position = "right", text = element_text(family = font.family, size = font.size)) 
      
      plot.out.diff.nofoot<- ggplot() +
        # Update "fill" and "color" to change map color
        geom_polygon(data = state.data, aes(x=long, y=lat, group=state_group),
                     alpha = .15, colour = "white") + 
        # Here you'd make adjustments to the point colors...
        geom_tile(data = pred.df.diff, aes(x = long, y = lat, fill = z)) +
        scale_fill_gradient2(name = "Projected\nRelative biomass changes", low = gmri.blue, mid = gmri.light.gray, high = gmri.orange, midpoint = 0, breaks = diff.breaks.use, labels = diff.labels.use, limits = c(min(pred.df.diff$z), max(pred.df.diff$z))) +
        xlim(xlim) + 
        ylim(ylim) +
        theme_map() + 
        coord_fixed(1.3) +
        theme(legend.position = "right", text = element_text(family = font.family, size = font.size)) 
      
      plot.out.diff<- ggplot() +
        # Update "fill" and "color" to change map color
        geom_polygon(data = state.data, aes(x=long, y=lat, group=state_group),
                     alpha = .15, colour = "white") + 
        # Here you'd make adjustments to the point colors...
        geom_tile(data = pred.df.diff, aes(x = long, y = lat, fill = z)) +
        scale_fill_gradient2(name = "Projected\nRelative biomass changes", low = gmri.blue, mid = gmri.light.gray, high = gmri.orange, midpoint = 0, breaks = diff.breaks.use, labels = diff.labels.use, limits = c(min(pred.df.diff$z), max(pred.df.diff$z))) +
        geom_tile(data = pred.df.diff.tile, aes(x = long, y = lat), fill = NA, color = gmri.gray, size = 0.5) +
        xlim(xlim) + 
        ylim(ylim) +
        theme_map() + 
        coord_fixed(1.3) +
        theme(legend.position = "right", text = element_text(family = font.family, size = font.size)) 
      
      plot.out<- plot_grid(plot.out.base, plot.out.diff, nrow = 1, align = "hv")
      ggsave(paste(out.path.use, spp.use, gear.use, "maps.jpg", sep = ""), plot.out, width = 11, height = 11, units = "in")
    }

  }
  
  # End function
}

# Executing plot function... ----------------------------------------------
# Community and species vector -- these would be pulled from looking at the ecology treemap figure. Also, could work to automate this by having the tree map figure save a dataframe. 
comm.use<- "STONINGTON_ME"
spp.use<- c("AMERICAN LOBSTER")
comm_report_plot_func(preds = preds, foots = foots, comm = comm.use, spp = spp.use, season = "both", xlim = c(-75, -66.5), ylim = c(38, 47.5))

comm.use<- "PORTLAND_ME"
spp.use<- c("ATLANTIC HERRING", "AMERICAN LOBSTER", "HAGFISH")
spp.use<- c("LONGFIN SQUID")

comm_report_plot_func(preds = preds, foots = foots, comm = comm.use, spp = spp.use, season = "both", xlim = c(-75, -66.5), ylim = c(38, 47.5))

comm.use<- "NEW.BEDFORD_MA"
spp.use<- c("SEA SCALLOP", "AMERICAN LOBSTER")
comm_report_plot_func(preds = preds, foots = foots, comm = comm.use, spp = spp.use, season = "both", xlim = c(-74, -66.5), ylim = c(40, 47.5))

comm.use<- "POINT.JUDITH_RI"
spp.use<- c("AMERICAN LOBSTER", "LONGFIN SQUID", "SCUP", "SEA SCALLOP", "ATLANTIC HERRING", "SILVER HAKE", "SUMMER FLOUNDER")
comm_report_plot_func(preds = preds, foots = foots, comm = comm.use, spp = spp.use, season = "both", xlim = c(-75, -66.5), ylim = c(38, 47.5))


