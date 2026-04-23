# Videre bruk av modeller fra HGU
# Hentet fra Blått karbon, brukes videre her
# 2026 kristina - skiftet fra raster-pakke til terra for beregninger for å teste om det gjør forskjell, men gjør ikke biomassemodell på nytt

# Pakker
require(readxl)
require(tidyverse)
require(raster)
require(rgeos)
require(rgdal)
require(dismo)

library(mgcv)

maindir <- "D:\\Taremodeller\\NorwegianKelpModels_bigfiles\\Predictions\\"

predr.LHbiomass_reclass <- rast(paste0(maindir,"predLAMHYbiomass2022_reclass.grd")) # g/ind per grid cell
predLAMHYdens <- rast(paste0(maindir,"predLAMHYdens2022_crop40.grd")) # ind per m2 per grid cell
# Calculate g per m2 per grid cell
biomassLAMHYm2 <- predLAMHYdens * predr.LHbiomass_reclass
# Set negative values to 0
biomassLAMHYm2_reclass <- ifel(biomassLAMHYm2 <= 0, 0, biomassLAMHYm2)
# Converting to total biomass per grid cell (kg)
tot_kg_LAMHY <- biomassLAMHYm2_reclass * 0.625 # (* 625 / 1000)cells of 25 m2, expressed in kg rather than grams

writeRaster(tot_kg_LAMHY,  filename = paste0(maindir, "tot_kg_LAMHY.grd"), overwrite = TRUE)

# Total weight
sumLAMHY <- global(tot_kg_LAMHY, "sum", na.rm = TRUE)[1, 1]
sumLAMHY # kg
sumLAMHY / 1e9 # in million tonnes

# Splitting the estimate into forest and non-forest 
biomass_lowdens_kg <- global(tot_kg_LAMHY * (predLAMHYdens < 5), "sum",  na.rm = TRUE)[1, 1]

# biomass where density ≥ 5
biomass_forest_kg <- global(tot_kg_LAMHY * (predLAMHYdens >= 5), "sum",  na.rm = TRUE)[1, 1]


zonalLAMHYdens_kg <- data.frame(Category = c("Density < 5", "Density ≥ 5"),  Biomass_kg = c(biomass_lowdens_kg, biomass_forest_kg))
write.csv(file = "../Tables/zonalLAMHYdens_kg.csv", row.names = FALSE)


