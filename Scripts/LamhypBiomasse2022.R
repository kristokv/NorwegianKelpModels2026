# Videre bruk av modeller fra HGU
# Hentet fra Blått karbon, brukes videre her

# Pakker
require(readxl)
require(tidyverse)
require(raster)
require(rgeos)
require(rgdal)
require(dismo)

library(mgcv)

# Rasteroptions

#rasterOptions(maxmemory = 16e+9)
rasterOptions(tmpdir = "./tempdir")
rasterOptions()

tmp <- read_xlsx("Data/Stortare.xlsx")

STORTAREdata=data.frame(
  nr=tmp$NR_HGU,
  sted=tmp$STED,
  sted_5=tmp$STED_5,
  sted_5nr=tmp$STED_5_NR,
  sted_6=tmp$STED_6,
  ecoregion=tmp$Ecoregion,
  sted_lat=tmp$STED_LAT,
  stasjon=tmp$STASJON,
  aar=tmp$AAR,
  mnd=tmp$MND,
  dag=tmp$DAG,
  lat=tmp$LATITUDE,
  long=tmp$LONGITUDE,
  lat_round=tmp$LAT_ROUND,
  long_round=tmp$LONG_ROUND,
  dyp=tmp$DYP,
  dyp_3=tmp$DYP_3,
  antall=tmp$ANTALL,
  tetthet=tmp$TETTHET,
  tetthet_integer=tmp$Tetthet_integer,
  bladlengde=tmp$BLADLENGDE,
  bladvekt=tmp$BLADVEKT,
  stilklengde=tmp$STILKLENGDE,
  stilkvekt=tmp$STILKVEKT,
  bladstilk=tmp$BLAD_STILK,
  bladtot=tmp$BLAD_TOT,
  haptervekt=tmp$HAPTERVEKT,
  totalvekt=tmp$TOTALVEKT,
  alder=tmp$ALDER,
  epifyttvekt=tmp$EPIFYTTVEKT,
  tarebiomasse=tmp$Tarebiomasse,
  dem=tmp$DEM,
  swm_kat=tmp$SWM_kat,
  swm=tmp$SWM_1000000)

# Ekskluderer cucullaria (som visst nok er obs nr 323-325):
STORTAREdata_sub <- subset(STORTAREdata, nr < 323 | nr > 325)
dim(STORTAREdata)
dim(STORTAREdata_sub)
STORTAREdata <- STORTAREdata_sub
summary(STORTAREdata)

# attach(STORTAREdata) # For ? kunne skrive variabelnavn uten $ foran


# Best models (from model selection, dAIC < 4) --------------------------------------------------
Best_bladlengde <- gamm(bladlengde ~ s(dyp, k = 4) + s(alder, k = 4), random=list(stasjon=~1), method = "ML", family = gaussian, data = STORTAREdata)
Best_bladvekt <- gamm(log(bladvekt+1) ~ s(dyp, k = 3) + s(alder, k = 4) + s(lat, k = 4), random=list(stasjon=~1), method = "ML", family = gaussian, data = STORTAREdata)
# Best_stilklengde <- gamm(stilklengde ~ s(dyp, k = 4) + s(swm, k = 4) + s(alder, k = 4) + s(lat, k = 4), random=list(stasjon=~1), method = "ML", family = gaussian, data = STORTAREdata_stilklengde)
Best_stilkvekt <- gamm(log(stilkvekt+1) ~ s(dyp, k = 4) + s(swm, k = 4) + s(alder, k = 4) + s(lat, k = 4), random=list(stasjon=~1), method = "ML", family = gaussian, data = STORTAREdata)
Best_bladstilk <- gamm(log(bladstilk+1) ~ s(swm, k = 4) + s(alder, k = 4) + s(lat, k = 4), random=list(stasjon=~1), method = "ML", family = gaussian, data = STORTAREdata)
Best_haptervekt <- gamm(log(haptervekt+1) ~ s(dyp, k = 4) + s(swm, k = 4) + s(alder, k = 4) + s(lat, k = 4), random=list(stasjon=~1), method = "ML", family = gaussian, data = STORTAREdata)
# Best_totalvekt <- gamm(log(totalvekt+1) ~ s(dyp, k = 4) + s(swm, k = 4) + s(alder, k = 4) + s(lat, k = 4), random=list(stasjon=~1), method = "ML", family = gaussian, data = STORTAREdata)
Best_epifyttvekt <- gamm(log(epifyttvekt+1) ~ s(dyp, k = 4) + s(alder, k = 4) + s(lat, k = 4), random=list(stasjon=~1), method = "ML", family = gaussian, data = STORTAREdata)
Best_tarebiomasse <- gamm(log(tarebiomasse+1) ~ s(dyp, k = 4) + s(swm, k = 4) + s(alder, k = 4), random=list(stasjon=~1), method = "ML", family = gaussian, data = STORTAREdata)
# Best_tetthet <- gamm(tetthet ~ s(dyp, k = 4) + s(swm, k = 4), random=list(stasjon=~1), method = "ML", family = poisson, data = STORTAREdata_tetthet)

# additional rasters
dyp_neg <- raster("./GIS-layers/Predstack_midl/Predstack_dem_resgis")
plot(dyp_neg)

lat_new <- raster("./GIS-layers/Predstack_add/lat25crop")
lat_new
plot(lat_new)

swm_1e6 <- raster("./GIS-layers/Predstack_add/swm_1e6")
swm_1e6

sp_tare <- SpatialPointsDataFrame(coords = cbind(STORTAREdata$long, STORTAREdata$lat), data = STORTAREdata, proj4string = CRS("+proj=longlat +datum=WGS84"))
sp_tare33N <- spTransform(sp_tare, CRS("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs"))


STORTAREdata$sp_tare_latnew <- raster::extract(lat_new, sp_tare33N)

# Vi skal jobbe med totalvekt
Best_totalvekt <- gamm(log(totalvekt+1) ~ s(dyp, k = 4) + s(swm, k = 4) + s(alder, k = 4) + s(sp_tare_latnew, k = 4), 
                          random=list(stasjon=~1), method = "ML", family = gaussian, data = STORTAREdata)

plot(Best_totalvekt$gam$fitted.values, Best_totalvekt$gam$residuals)

# prediksjoner til data
predictions <- predict(Best_totalvekt$gam, newdata = STORTAREdata) # OK - NA oppgis (kuttes ikke)

# prediksjoner til data der alder er satt til 7
predictions_age7 <- STORTAREdata %>% mutate(alder = 7) %>% 
  predict(Best_totalvekt$gam, newdata = .)

data.frame(obs = STORTAREdata$totalvekt, 
           pred = exp(predictions)-1, 
           pred_7 = exp(predictions_age7)-1) %>%
  pivot_longer(cols = c(pred:pred_7), names_to = "predtype", values_to = "prediction") %>% 
  ggplot(., aes(x = obs, y = prediction)) +
  geom_point(aes(colour = predtype))

plot(log((STORTAREdata$totalvekt)+1), predictions) # OK
plot(Best_totalvekt$gam$model$`log(totalvekt + 1)`, Best_totalvekt$gam$fitted.values)
cor(Best_totalvekt$gam$model$`log(totalvekt + 1)`, Best_totalvekt$gam$fitted.values)

# For å kjøre romlige prediksjoner trengs GIS-lag med følgende
Best_totalvekt$gam$var %>% names

# husk at swm her er delt på 1e06

# Alder foreslått satt til 7.8 år (litt høyt?)
hist(STORTAREdata$alder)
summary(STORTAREdata$alder)
# Synes det er bedre å bruke medianen, altså 7 år - denne kan legges inn i block under (i datarammen)
# For fremtiden kunne vi laget en modell for alder også kanskje...
# SWM, lat og dyp har vi kontroll på. 

biomass_predstack <- stack(dyp_neg, swm_1e6, lat_new)
names(biomass_predstack) <- c("dyp_neg", "swm_1e6", "lat_new")
biomass_predstack
plot(biomass_predstack)

# Legg kartlag med alder, SWM og dyp i mappe, hent inn og stack, navngi så det stemmer med modell, kjør prediksjonsskript
tr <- blockSize(biomass_predstack)
nr.round <- c(1:tr$n)

predr.LHbiomass <- raster(biomass_predstack, layer=0) # raster with no values
predr.LHbiomass

# Start writing
predr.LHbiomass <- writeStart(predr.LHbiomass, 
                              filename = "./Predictions/predLAMHYbiomass2022.grd",
                              format = "raster", overwrite = TRUE)
# Writing stuff
for (i in 1:tr$n){
  # getting chunk of predictionlayer
  v <- getValuesBlock(biomass_predstack, row = tr$row[i], nrows = tr$nrows[i])
  # creating dataframe
  v_data <- data.frame(v) %>% mutate(dyp = -dyp_neg, swm = swm_1e6, sp_tare_latnew = lat_new) 
  # dyp i modellen er positive!! 
  # adding age
  v_data$alder = 7
  # predictions
  v_predict <- predict(Best_totalvekt$gam, newdata = v_data)
  # # backtransform response
  v_response <- exp(v_predict)-1
  # writing to new predictionlayer
  predr.LHbiomass <- writeValues(predr.LHbiomass, v = v_response, tr$row[i])
  # keeping track of where we are
  print(paste(nr.round[i], "of", length(nr.round), sep = " "))
}
# stop writing
predr.LHbiomass <- writeStop(predr.LHbiomass)

predr.LHbiomass <- raster("./Predictions/predLAMHYbiomass2022")


# Sjekk verdier. Vær obs på om det er estimert veldig høye verdier i områder der tettheter sannsynligvis er veldig lave
# Mulig små verdier (nesten null) da vil få litt høye biomasse-estimater?
predr.LHbiomass
plot(predr.LHbiomass)

# sjekker fordelingen
hist(predr.LHbiomass) # noen veldig høye verdier? (responsen er i gram)


# Erstatter alle høye verdier over 5000 med 5000, fjerner negative verdier.
predr.LHbiomass_reclass <- writeRaster(reclassify(predr.LHbiomass, c(-Inf,0,0, 5000,Inf,5)), 
                                       file = "./Predictions/predLAMHYbiomass2022_reclass", overwrite = TRUE, progress = "text")

predr.LHbiomass_reclass <- raster("./Predictions/predLAMHYbiomass2022_reclass")
plot(predr.LHbiomass_reclass)

# Se om vi får tid til å regne på epifyttvekter i tillegg

# LAMHY
predLAMHYdens <- raster("./Predictions/predLAMHYdens2022_crop40.grd")
plot(predLAMHYdens, predr.LHbiomass_reclass)
plot(predLAMHYdens, predr.LHbiomass_reclass, xlim = c(0,2), ylim = c(0,500))

# biomassLAMHYm2 <- writeRaster(predLAMHYdens*predr.LHbiomass_reclass,
#                               file = "./Predictions/biomassLAMHY_g_m2", overwrite = TRUE, progress = "text")
plot(biomassLAMHYm2)
biomassLAMHYm2

# biomassLAMHYm2_reclass <- writeRaster(reclassify(biomassLAMHYm2, c(-Inf,0,0)),
#                                        file = "./Predictions/biomassLAMHY_g_m2_reclass", overwrite = TRUE, progress = "text")
biomassLAMHYm2_reclass <- raster("./Predictions/biomassLAMHY_g_m2_reclass")
plot(biomassLAMHYm2_reclass)

# Totoalt kg tare per gridcelle
tot_kg_LAMHY <- writeRaster(calc(biomassLAMHYm2_reclass, fun = function(x){x * 0.625}), # (* 625 / 1000)cells of 25 m2, expressed in kg rather than grams
                            file = "./Predictions/totLAMHY_kg_cell", overwrite = TRUE)

plot(tot_kg_LAMHY)

# Vekt totalt
sumLAMHY <- cellStats(tot_kg_LAMHY, stat = 'sum', na.rm = TRUE)
sumLAMHY # kg
sumLAMHY / (1000 * 1000000) # millioner tonn totalt
# ?? stemmer dette

# Hadde vært nyttig å dele dette på skog/ikke skog ...
# Kjør zonal på kategorier?
forestdensLAMHYcat <- writeRaster(reclassify(predLAMHYdens, c(-Inf,0.5,0, 0.5,4,1, 4,Inf,2)), 
                                  file = "./Predictions/forestdensLAMHYcat", overwrite = TRUE, progress = "text")
plot(forestdensLAMHYcat)

# Zonal
zonalLAMHYdens_kg <- zonal(tot_kg_LAMHY, forestdensLAMHYcat, fun = 'sum', na.rm = TRUE, progress = "text")
zonalLAMHYdens_kg %>% data.frame %>% dplyr::rename(Category = "zone", Biomass_kg = "sum") %>% 
  write.csv(file = "./Tables/zonalLAMHYdens_kg.csv", row.names = FALSE)

# Regne på karbontall?

