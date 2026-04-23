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
zonalLAMHYdens_kg <-  tibble(Category = c("Density < 5 ind. m⁻²", "Density ≥ 5 ind. m⁻²"),
    Biomass_kg = c(global(tot_kg_LAMHY * (predLAMHYdens < 5),  "sum", na.rm = TRUE)[1, 1],
      global(tot_kg_LAMHY * (predLAMHYdens >= 5), "sum", na.rm = TRUE)[1, 1])) %>%
  mutate(Biomass_Mt = Biomass_kg / 1e9)  
  
write.csv(zonalLAMHYdens_kg, file = "../Tables/zonalLAMHYdens_kg.csv", row.names = FALSE)


