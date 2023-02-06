#####################################################
## The Norwegian Kelp Models # take two
## Map predictions
## Guri Sogn Andersen, 2022
#####################################################

# This work is largely based on the models made in the Nordic Blue Carbon project (2020): http://pub.norden.org/temanord2020-541/#47359

require(tidyverse)
require(raster)
require(rgeos)
require(rgdal)
require(dismo)
require(Hmisc)
require(gbm)

memory.limit()
rasterOptions(maxmemory = 3e+10)
rasterOptions(tmpdir = "./tempdir")
rasterOptions()


# Model data --------------------------------------------------------------

# Model
finalmodel_sac <- readRDS("./Models/saclat.simp.manual.gbm_v3.rds") #saclat.simp.manual.gbm 
finalmodel_sac$distribution

# Predvar
finalmodel_sac$var.names


# Map layers --------------------------------------------------------------

#predstack_2020 <- "D:\\GIS\\PredstackWGS84_33N_25m_Norge_2020"
predstack_2020<- ".\\GIS-layers\\Predstack_midl"
BOtempsal2022 <- ".\\GIS-layers\\BioOracle-TEMP-SAL-2022\\Resampled"
BOnutrlight2022 <- ".\\GIS-layers\\BioOracle-NutrOxLight\\Resampled"

Depth_mod <- raster(paste(predstack_2020, "Predstack_dem_resgis", sep = "/"))
swm <- raster(paste(predstack_2020, "Predstack_swm_resgis", sep = "/"))
Meantemp <- raster(paste(BOtempsal2022, "Present.Benthic.Min.Depth.Temperature.Mean_GSAresampl", sep = "/"))
Maxtemp <- raster(paste(BOtempsal2022, "Present.Benthic.Min.Depth.Temperature.Lt.max_GSAresampl", sep = "/"))
curv500 <- raster(paste(predstack_2020, "Predstack_curv500_resgi", sep = "/"))
wspd90 <- raster(paste(predstack_2020, "Predstack_wspd90_resgis", sep = "/"))
BO2_salinitymax_bdmean <- raster(paste(BOtempsal2022, "Present.Benthic.Mean.Depth.Salinity.Lt.max_GSAresampl", sep = "/"))
Dissolved.oxygen.Mean <- raster(paste(BOnutrlight2022, "Present.Benthic.Min.Depth.Dissolved.oxygen.Mean_GSAresampl", sep = "/"))
Nitrate.Mean <- raster(paste(BOnutrlight2022, "Present.Benthic.Min.Depth.Nitrate.Mean_GSAresampl", sep = "/"))
Phosphate.Mean <- raster(paste(BOnutrlight2022, "Present.Benthic.Min.Depth.Phosphate.Mean_GSAresampl", sep = "/"))
slope <- raster(paste(predstack_2020, "Predstack_slope_resgis", sep = "/"))

predstack_2022 <- stack(mget(finalmodel_sac$var.names))
predstack_2022

# masklayer from 2020, based on dem, removing "land" from predictions
masklayer <- raster("Predictions/masklayer")
image(masklayer)

gc()

# Map predictions ---------------------------------------------------------

tr <- blockSize(predstack_2022)
nr.round <- c(1:tr$n)

tr
nr.round

predr <- raster(predstack_2022, layer=0) # raster with no values
predr

# Start writing
predr <- writeStart(predr, 
                    filename = "./Predictions/predSACLATdens2022",
                    format = "raster", overwrite = TRUE)
# Writing stuff
for (i in 1:tr$n){ 
  # getting chunk of predictionlayer
  predstack_i <- getValuesBlock(predstack_2022, row = tr$row[i], nrows = tr$nrows[i]) %>% data.frame
  # predict
  pred_i <- predict(finalmodel_sac, n.trees = finalmodel_sac$gbm.call$best.trees, 
                    type = "response",
                    newdata = predstack_i)
  # backtransform from response = log + 1
  scale_i <- exp(pred_i) - 1
  # getting chunk of "masklayer"
  mask_i <- getValuesBlock(masklayer, row = tr$row[i], nrows = tr$nrows[i]) 
  # removing "land"
  newpred_i <- scale_i*mask_i
  # writing to new predictionlayer
  predr <- writeValues(predr, v = newpred_i, tr$row[i])
  # keeping track of where we are
  print(paste(nr.round[i], "of", length(nr.round), sep = " "))
}
# stop writing
predr <- writeStop(predr)

plot(predr)
predr

predfinal_sac1 <- raster("./Predictions/predSACLATdens2022")
predfinal_sac1
plot(predfinal_sac1)
# See PaperFigures for images intended for publication

# Perhaps we should mask this layer at depth -40?
plot(predfinal_sac1, predstack_2022$Depth_mod)

# -1 where depths are -40 to 0
# masklayer2 <- reclassify(dem, c(-Inf, -40, NA,  -40, 0, -1,  0, Inf, NA))
# writeRaster(masklayer2, file = "./GIS-layers/masklayer2_depth_0_40")
masklayer2 <- raster("./GIS-layers/masklayer2_depth_0_40")
plot(masklayer2)

# cropped prediction layer
# predfinal_sac <- raster::calc(predfinal_sac1, fun = function(x){x * -masklayer2}, 
#                               filename = "./Predictions/predSACLATdens2022_crop40",
#                               overwrite = TRUE)
# The laptop just repetedly started acting weird and shutting down - memory issues? Not sure...
# Trying this instead: (took forever - about 24 hrs - would use blocking next time - or GRASS)
# predfinal_sac <- predfinal_sac1 
# predfinal_sac[is.na(masklayer2)] <- NA
# predfinal_sac
# 
# raster::writeRaster(predfinal_sac, filename = "./Predictions/predSACLATdens2022_crop40", overwrite = TRUE)

predfinal_sac <- raster("./Predictions/predSACLATdens2022_crop40")
predfinal_sac

plot(predfinal_sac)
plot(predfinal_sac, predstack_2022$Depth_mod, maxpixels=1000000,
     ylim = c(-60, 0))

# Compared to previous model

pred2020_sac <- raster("./Predictions/predSACLATdens_crop.grd")
plot(pred2020_sac)
plot(pred2020_sac, predstack_2022$Depth_mod)


# dif_sac <- predfinal_sac1 - pred2020_sac
# writeRaster(dif_sac, "./Predictions/SACLAT_2022_2020_dif")
dif_sac <- raster("./Predictions/SACLAT_2022_2020_dif")

plot(dif_sac)
hist(dif_sac)

plot(predfinal_sac1, pred2020_sac, cex = 1)
plot(predfinal_sac, pred2020_sac, cex = 1)



