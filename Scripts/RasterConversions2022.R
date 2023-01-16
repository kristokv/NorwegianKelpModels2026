#####################################################
## The Norwegian Kelp Models # take two
## Putting together prediction layers 
## Guri Sogn Andersen, 2022
#####################################################

# This work is largely based on the models made in the Nordic Blue Carbon project (2020): http://pub.norden.org/temanord2020-541/#47359


# Packages ----------------------------------------------------------------

require(tidyverse)
require(rgdal)
require(raster)


# Raster options ----------------------------------------------------------

memory.limit()
rasterOptions(maxmemory = 3e+10)
rasterOptions(tmpdir = "./tempdir")
rasterOptions()

# Raster files ------------------------------------------------------------

# Check layers needed for predictive model
explvar_selection
lamhyp.manual.simp$pred.list$preds.3

# slope, light and grazing is dropped

# Templayers (Bio-ORACLE) are available from 2020-modelleing, but might be worth updating?
# Should stop to find the optimal salinity layer.... # Seemed pretty much the same ...

# Previously resampled, reprojected and stacked - from external drive
predstack_2020 <- "D:\\GIS\\PredstackWGS84_33N_25m_Norge_2020"
dem <- raster(paste(predstack_2020, "Predstack_dem_resgis", sep = "/"))
swm <- raster(paste(predstack_2020, "Predstack_swm_resgis", sep = "/"))
meantemp <- raster(paste(predstack_2020, "Predstack_BO2_tempmean_bdmin", sep = "/"))
maxtemp <- raster(paste(predstack_2020, "Predstack_BO2_tempmax_bdmin", sep = "/"))
curv500 <- raster(paste(predstack_2020, "Predstack_curv500_resgi", sep = "/"))
wspd90 <- raster(paste(predstack_2020, "Predstack_wspd90_resgis", sep = "/"))
salinitymax <- raster(paste(predstack_2020, "Predstack_BO2_salinitymax_bdmean", sep = "/"))

# New BioOracle layers - Present
BOlayers <- ".\\GIS-layers\\BioOracle-NutrOxLight"
BOlist <- list.files(BOlayers, full.names = TRUE)
BOrasters <- lapply(BOlist, raster) 

BOrasters_res <- lapply(BOrasters, function(x) {projectRaster(x, to = dem,
                                                              filename = paste0(BOlayers, "/Resampled/", names(x), "_GSAresampl"),
                                                              overwrite = TRUE, progress = "text")})
BOtempsal2022 <- ".\\GIS-layers\\BioOracle-TEMP-SAL-2022"
BOlist2 <- list.files(BOtempsal2022, full.names = TRUE)
BOtempsalrasters <- lapply(BOlist2, raster)

BOtempsalrasters_res <- lapply(BOtempsalrasters, function(x) {projectRaster(x, to = dem,
                                                              filename = paste0(BOtempsal2022, "/Resampled/", names(x), "_GSAresampl"),
                                                              overwrite = TRUE, progress = "text")})

# New BioOracle-layers - Future
BOlayers_future <- ".\\GIS-layers\\BioOracle-Future"
BOfuturelist <- list.files(BOlayers_future, pattern = "tif", full.names = TRUE)
futurerasters <- lapply(BOfuturelist, raster)
  
futurerasters_res <- lapply(futurerasters, function(x) {projectRaster(x, to = dem,
                                                                    filename = paste0(BOlayers_future, "/Resampled/", names(x), "_GSAresampl"),
                                                                    overwrite = TRUE, progress = "text")})

# For the biomass models we need a layer with latitudes - need to transform to lat lon
# Avoid timeconsuming transformation of huge raster
# from PaperFigures.R:
# lamhyp_downsample <- raster("./Predictions/predLAMHYdens2022_downsample500")
lamhyp_plot <- lamhyp_downsample %>% projectRaster(., crs = "+proj=longlat +datum=WGS84")
# extract lat values and backtransform
lat.500 <- init(lamhyp_plot, 'y') %>% projectRaster(., crs = "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs")
# # changig resolution to original
# lat_new <- raster::resample(lat.500, dem, file = "./GIS-layers/Predstack_add/lat25crop", overwrite = TRUE,
#            progress = "text")
# was really a pain in the ass to get to this!

lat_new
# For the biomass models we need swm-layer divided by 1e6
swm_1e6 <- raster::calc(swm, fun = function(x){x/1e6}, filename = "./GIS-layers/Predstack_add/swm_1e6", progress = "text")
