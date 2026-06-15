# Videre bruk av modeller fra HGU
# Hentet fra Blått karbon, brukes videre her
# 2026 kristina - skiftet fra raster-pakke til terra for beregninger for å teste om det gjør forskjell, men gjør ikke biomassemodell på nytt

# Pakker
require(readxl)
require(tidyverse)
#require(raster)
require(terra)
#require(rgeos)
#require(rgdal)
#require(dismo)
#library(mgcv)

maindir <- "D:\\Taremodeller\\NorwegianKelpModels_bigfiles"

predr.LHbiomass_reclass <- rast(paste0(maindir,"\\Predictions\\predLAMHYbiomass2022_reclass.grd")) # g/ind per grid cell

predLAMHYdens <- rast(paste0(maindir,"predLAMHYdens2022_crop40.grd")) # ind per m2 per grid cell
# Calculate g per m2 per grid cell
biomassLAMHYm2 <- predLAMHYdens * predr.LHbiomass_reclass
# Set negative values to 0
biomassLAMHYm2_reclass <- ifel(biomassLAMHYm2 <= 0, 0, biomassLAMHYm2)
# Converting to total biomass per grid cell (kg)
tot_kg_LAMHY <- biomassLAMHYm2_reclass * 0.625 # (* 625 / 1000) cells of 25 m2, expressed in kg rather than grams

writeRaster(tot_kg_LAMHY,  filename = paste0(maindir, "\\Predictions\\tot_kg_LAMHY.grd"), overwrite = TRUE)


# Get average ind. biomass to scale for Saccharina 
pathtofile<-"C:/Users/KVI/NIVA/240142 - The impact of climate change on Arctic blue carbon - WP2/01_Data/02_Biomass_data//02_Svalbard/Saccharina_Düsedau et al 2024.xlsx"

sacla_g_mean <- read_xlsx(pathtofile, skip = 46) %>%
  rename(ww_g = "Biom wm [g]", age= "Stipe age [a]") %>% 
  filter(!is.na(age) & age>1) %>% 
  summarize(mean = mean(ww_g, na.rm=T))

predfinal_sac <- rast(paste0(maindir,"\\Predictions\\predSACLATdens2022_crop40.grd"))
cell_area_m2 <- prod(res(predfinal_sac))  # m² per cell

tot_kg_SACLA <- predfinal_sac * as.numeric(sacla_g_mean/1000) * cell_area_m2
# Set negative values to 0
tot_kg_SACLA <- ifel(tot_kg_SACLA <= 0, 0, tot_kg_SACLA)

terraOptions(memfrac = 0.8, progress = 1, numThreads = parallel::detectCores())
writeRaster(tot_kg_SACLA,filename = file.path(maindir, "Predictions", "tot_kg_SACLA.tif"), overwrite = TRUE,  gdal = c("COMPRESS=NONE", "NUM_THREADS=ALL_CPUS"))



