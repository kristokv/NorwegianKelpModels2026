#####################################################
## The Norwegian Kelp Models # take two
## Extracting additional variables 
## Guri Sogn Andersen, 2022
#####################################################

# This work is largely based on the models made in the Nordic Blue Carbon project (2020): http://pub.norden.org/temanord2020-541/#47359
# Here we extract variables describing urchin grazing, temp, and possibly light and sal

# Packages ----------------------------------------------------------------

require(tidyverse)
require(raster)
require(rgdal)


# Observation points ------------------------------------------------------

obs.points <- read.csv("./Data/obs.points.csv") %>% 
  rename("ID" = "X.1")

summary(obs.points)

# Creating spatial points df
obs_latlong <- obs.points %>% dplyr::select(X, Y) %>% 
  SpatialPointsDataFrame(., data = obs.points, proj4string = CRS("+proj=longlat +datum=WGS84"))
plot(obs_latlong)

# Salinity 2022
BO2_salmean_bdmin <- raster(".\\GIS-layers\\BioOracle-TEMP-SAL-2022\\Present.Benthic.Min.Depth.Salinity.Mean.tif")

# Historic data -----------------------------------------------------------

#### Copernicus data for temperature and salinity
cop_path <- ".\\GIS-layers\\Copernicus-TEMP-SAL\\RASTER-global-reanalysis-phy-001-030-yearly-stacked"

# Temperature
bottom_T_max  <- stack(paste(cop_path, "bottomT_max_1993-2018.tif", sep ="/"))
bottom_T_mean <- stack(paste(cop_path, "bottomT_mean_1993-2018.tif", sep ="/"))
bottom_T_min  <- stack(paste(cop_path, "bottomT_min_1993-2018.tif", sep ="/"))

# Surface Salinity
SS_max  <- stack(paste(cop_path, "so_max_1993-2018.tif", sep ="/"))
SS_mean <- stack(paste(cop_path, "so_mean_1993-2018.tif", sep ="/"))
SS_min  <- stack(paste(cop_path, "so_min_1993-2018.tif", sep ="/"))

#### GlobColour-PAR
glob_path <- ".\\GIS-layers\\GlobColour-PAR\\RASTER-PAR_Yearly_4km-stacked"

# PAR
PAR_max  <- stack(paste(glob_path, "PAR_max_1998-2019.tif", sep ="/"))
PAR_mean <- stack(paste(glob_path, "PAR_mean_1998-2019.tif", sep ="/"))
PAR_min  <- stack(paste(glob_path, "PAR_min_1998-2019.tif", sep ="/"))

#### Sea urchin front
Urchin <- stack(".\\GIS-layers\\Sea-urchin-front\\Stacked\\Barren_front_1980-2018.tif")

#### Bio-Oracle nutrients, oxygen and light
templist <- list.files(".\\GIS-layers\\BioOracle-NutrOxLight", full.names = TRUE)
templist

NutrOxLight <- stack(templist)
NutrOxLight

# Future predictions Climate Scenarios -----------------------------------

# Temperature predictions - BioOracle
BO2_RCP85_2100_meantemp_mindepth <- raster("./GIS-layers/BioOracle-Future/2100AOGCM.RCP85.Benthic.Min.Depth.Temperature.Mean.tif.BOv2_1.tif")
BO2_RCP85_2100_meantemp_mindepth

BO2_RCP85_2100_maxtemp_mindepth <- raster("./GIS-layers/BioOracle-Future/2100AOGCM.RCP85.Benthic.Min.Depth.Temperature.Lt.max.tif.BOv2_1.tif")
BO2_RCP85_2100_maxsal_meandepth <- raster("./GIS-layers/BioOracle-Future/2100AOGCM.RCP85.Benthic.Mean.Depth.Salinity.Lt.max.tif.BOv2_1.tif")

BO2_RCP85_2100 <- stack(BO2_RCP85_2100_meantemp_mindepth, BO2_RCP85_2100_maxtemp_mindepth, BO2_RCP85_2100_maxsal_meandepth)
names(BO2_RCP85_2100) <- c("RCP85meantemp_2100", "RCP85maxtemp_2100", "RCP85maxsal_2100")
BO2_RCP85_2100

# Data extraction # TEMPERATURE -------------------------------------------
# Hvilken rekkefølge kommer årene i? Hvilke år korresponderer til hvilke lag?

TSyears <- data.frame(TS_year = c(1993:2018), Yearnr = as.character(c(1:26)))
TSyears

image(bottom_T_max)
image(bottom_T_max$bottomT_max_1993.2018.26)

T_max_df <- raster::extract(bottom_T_max, obs_latlong, df = TRUE) %>% 
  pivot_longer(cols = bottomT_max_1993.2018.1:bottomT_max_1993.2018.26, names_to = "Layer", values_to = "Maxtemp") %>% 
  mutate(Yearnr = str_remove(Layer, "bottomT_max_1993.2018.")) %>% 
  left_join(TSyears)
summary(T_max_df) 

T_mean_df <- raster::extract(bottom_T_mean, obs_latlong, df = TRUE) %>% 
  pivot_longer(cols = bottomT_mean_1993.2018.1:bottomT_mean_1993.2018.26, names_to = "Layer", values_to = "Meantemp") %>% 
  mutate(Yearnr = str_remove(Layer, "bottomT_mean_1993.2018.")) %>% 
  left_join(TSyears)
summary(T_mean_df) 

# Which IDs have NA-values
missingT <- T_mean_df %>% filter(is.na(Meantemp)) %>% 
  dplyr::select(ID) %>% distinct

dim(missingT) # 4411 NA totalt.

obs_missing <- obs_latlong %>% subset(., ID %in% missingT$ID)

res(bottom_T_mean)

# T_mean_10km_df <- raster::extract(bottom_T_mean, obs_missing, 
#                                 buffer = 10000, fun = mean, na.rm = TRUE,
#                                 df = TRUE,
#                                 progress = "text") %>% 
#   pivot_longer(cols = bottomT_mean_1993.2018.1:bottomT_mean_1993.2018.26, names_to = "Layer", values_to = "Meantemp") %>% 
#   mutate(Yearnr = str_remove(Layer, "bottomT_mean_1993.2018.")) %>% 
#   left_join(TSyears)
# Tok veldig lang tid!!! Måtte stå mellom 11 og 16 timer et sted. Men jeg hadde ikke allokert mere minne til rasterpakka.

# T_max_10km_df <- raster::extract(bottom_T_max, obs_missing,
#                                 buffer = 10000, fun = mean, na.rm = TRUE, # mean of max
#                                 df = TRUE,
#                                 progress = "text") %>%
#   pivot_longer(cols = bottomT_max_1993.2018.1:bottomT_max_1993.2018.26, names_to = "Layer", values_to = "Maxtemp") %>%
#   mutate(Yearnr = str_remove(Layer, "bottomT_max_1993.2018.")) %>%
#   left_join(TSyears)

# ID is not the same as in missingT, refers to the point ID in the spDF
# IMPORTANT - Translate

# dummy1 <- T_mean_10km_df %>% dplyr::select(ID) %>% distinct(.) %>% c
# dummy2 <- missingT %>% c
# 
# IDtranslate <- data.frame(IDmissing = dummy2$ID, 
#                           IDspat = dummy1$ID)
# head(IDtranslate)
# rm(dummy1, dummy2)

# # Ensure correct ID with reference to initial dataframe
# T_mean_10km_df %>% left_join(IDtranslate, by = c("ID" = "IDspat")) %>% 
# write.csv(., file = "./Data/T_mean_10km_df.csv", row.names = FALSE)

# T_max_10km_df %>% left_join(IDtranslate, by = c("ID" = "IDspat")) %>%
# write.csv(., file = "./Data/T_max_10km_df.csv", row.names = FALSE)

# Reload
T_mean_10km_df <- read.csv("./Data/T_mean_10km_df.csv", stringsAsFactors = FALSE) %>% 
  dplyr::select(-ID) %>% 
  rename(ID = "IDmissing") # ID corresponding to observation point ID
summary(T_mean_10km_df)

T_max_10km_df <- read.csv("./Data/T_max_10km_df.csv", stringsAsFactors = FALSE) %>% 
  dplyr::select(-ID) %>% 
  rename(ID = "IDmissing") # ID corresponding to observation point ID
summary(T_max_10km_df)

T_mean_10km_df %>% filter(is.na(Meantemp)) %>% group_by(TS_year) %>% summarise(N = n()) # 1384 punkter med NA
T_max_10km_df %>% filter(is.na(Maxtemp)) %>% group_by(TS_year) %>% summarise(N = n())
# I datasettet fra Bio-Oracle er det bare 399

# Which IDs still have NA-values
missingT2 <- T_mean_10km_df %>% filter(is.na(Meantemp)) %>% 
  dplyr::select(ID) %>% distinct

dim(missingT2) # 1384 NA totalt.

obs_missing2 <- obs_latlong %>% subset(., ID %in% missingT2$ID)
obs_missing2

# Legger til 15 km buffer
# T_mean_15km_df <- raster::extract(bottom_T_mean, obs_missing2,
#                                 buffer = 15000, fun = mean, na.rm = TRUE,
#                                 df = TRUE,
#                                 progress = "text") %>%
#   dplyr::select(-ID) %>% 
#   cbind(., missingT2) %>% 
#   pivot_longer(cols = bottomT_mean_1993.2018.1:bottomT_mean_1993.2018.26, names_to = "Layer", values_to = "Meantemp") %>%
#   mutate(Yearnr = str_remove(Layer, "bottomT_mean_1993.2018.")) %>%
#   left_join(TSyears)
# 
# write.csv(T_mean_15km_df, file = "./Data/T_mean_15km_df.csv", row.names = FALSE)

T_mean_15km_df %>% filter(is.na(Meantemp)) %>% group_by(TS_year) %>% summarise(N = n()) # 649 NA igjen

T_max_15km_df <- raster::extract(bottom_T_max, obs_missing2,
                                  buffer = 15000, fun = mean, na.rm = TRUE, # mean of max
                                  df = TRUE,
                                  progress = "text") %>%
  dplyr::select(-ID) %>% 
  cbind(., missingT2) %>% 
  pivot_longer(cols = bottomT_max_1993.2018.1:bottomT_max_1993.2018.26, names_to = "Layer", values_to = "Maxtemp") %>%
  mutate(Yearnr = str_remove(Layer, "bottomT_max_1993.2018.")) %>%
  left_join(TSyears)

write.csv(T_max_15km_df, file = "./Data/T_max_15km_df.csv", row.names = FALSE)

T_max_15km_df %>% filter(is.na(Maxtemp)) %>% group_by(TS_year) %>% summarise(N = n()) # 649 NA igjen
summary(T_max_15km_df)

T_min_df <- raster::extract(bottom_T_min, obs_latlong, df = TRUE) %>% 
  pivot_longer(cols = bottomT_min_1993.2018.1:bottomT_min_1993.2018.26, names_to = "Layer", values_to = "Mintemp") %>% 
  mutate(Yearnr = str_remove(Layer, "bottomT_min_1993.2018."))%>% 
  left_join(TSyears)
summary(T_min_df) 

# Evaluate the difference between Bio-Oracle values and the buffered values, seen in comparison with the 
# difference between Bio-Oracle and unbuffered values... 
# A buffer of 10 and 15 km is quite large ...


#### Plot how these variables have changed at each observation point over time
# Interesting in it self, but also to evaluate whether this should be taken into account in the modeling process
# Temperatures have increased over time at the observation points!
# Seems like it might be a good idea to match actual temperatures, but have to compare to the values from the bioOracle-layer
# Resolution Copernicus: 0.83 x 0.83 degrees (resampled?)
# Resolution BioOracle: 5 arcmin - approx same ?


# Maxtemps
Tmax <- T_max_df %>% left_join(obs.points) %>% 
  mutate(Year = as.integer(Yearnr)) %>% 
  group_by(Year) %>% 
  summarise(Max = max(Maxtemp, na.rm = TRUE),
            Mean = mean(Maxtemp, na.rm = TRUE),
            Min = min(Maxtemp, na.rm = TRUE)) %>% 
  pivot_longer(cols = Max:Min, names_to = "Measure", values_to = "Temperature") %>% 
  ggplot(., aes(x = Year, y = Temperature, col = Measure)) +
  geom_point() +
  labs(title = "Maximum")#,
#       caption = "*Points not geographically evenly distributet across years, which means that matching to actual year may be important")
# Could group points according to latitude as well ...
Tmax

# Meantemps
Tmean <- T_mean_df %>% left_join(obs.points) %>% 
  mutate(Year = as.integer(Yearnr)) %>% 
  group_by(Year) %>% 
  summarise(Max = max(Meantemp, na.rm = TRUE),
            Mean = mean(Meantemp, na.rm = TRUE),
            Min = min(Meantemp, na.rm = TRUE)) %>% 
  pivot_longer(cols = Max:Min, names_to = "Measure", values_to = "Temperature") %>% 
  ggplot(., aes(x = Year, y = Temperature, col = Measure)) +
  geom_point() +
  labs(title = "Mean")#,
#       caption = "*Points not geographically evenly distributet across years, which means that matching to actual year may be important")
Tmean

# Mintemps
Tmin <- T_min_df %>% left_join(obs.points) %>% 
  mutate(Year = as.integer(Yearnr)) %>% 
  group_by(Year) %>% 
  summarise(Max = max(Mintemp, na.rm = TRUE),
            Mean = mean(Mintemp, na.rm = TRUE),
            Min = min(Mintemp, na.rm = TRUE)) %>% 
  pivot_longer(cols = Max:Min, names_to = "Measure", values_to = "Temperature") %>% 
  ggplot(., aes(x = Year, y = Temperature, col = Measure)) +
  geom_point() +
  labs(title = "Minimum")#,
#       caption = "*Points not geographically evenly distributet across years, which means that matching to actual year may be important")
Tmin

# Geographical distribution of observations across years
pdistr <- obs.points %>% ggplot(., aes(x = X, y = Y, col = Year_HGU)) +
  geom_point()
pdistr

# Plots
Tmin + Tmean + Tmax +
  plot_annotation(
    title = 'Annual temperatures across all observation points over time',
    caption = '*Points not geographically evenly distributet across years, which means that matching to actual year may be important') +
  plot_layout(guides = "collect") &
#  ylim(-2, 23) &
  labs(y = "")

# Compare values to bio-Oracle-layer
T_mean_df %>% left_join(obs.points) %>% 
  mutate(Diff = BO2_tempmean_bdmin - Meantemp) %>% 
  ggplot(aes(x = BO2_tempmean_bdmin, y = Meantemp, col = Diff)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, col = "red", lwd = 2) + 
  facet_wrap(~ as.integer(Yearnr))

# For observations
obs.points %>% left_join(T_mean_df, by = c("ID" = "ID", "Year_HGU" = "TS_year")) %>% 
  ggplot(aes(x = X, y = Y, col = Meantemp)) +
  geom_point()

obs.points %>% left_join(T_mean_df, by = c("ID" = "ID", "Year_HGU" = "TS_year")) %>% 
  ggplot(aes(x = X, y = Y, col = BO2_tempmean_bdmin)) +
  geom_point()

obs.points %>% left_join(T_mean_df, by = c("ID" = "ID", "Year_HGU" = "TS_year")) %>% 
  mutate(Diff = BO2_tempmean_bdmin - Meantemp) %>% 
#  filter(abs(Diff) < 1) %>% 
  ggplot(aes(x = X, y = Y, col = Diff)) +
  geom_point()

obs.points %>% left_join(T_mean_df, by = c("ID" = "ID", "Year_HGU" = "TS_year")) %>% 
  mutate(Diff = BO2_tempmean_bdmin - Meantemp) %>% 
  ggplot(aes(x = Year_HGU, y = Diff, col = Y)) +
  geom_point()

# For buffered values - 10 km radius
T_mean_10km_df %>% left_join(obs.points) %>%
  mutate(Diff = BO2_tempmean_bdmin - Meantemp) %>% 
  ggplot(aes(x = BO2_tempmean_bdmin, y = Meantemp, col = Diff)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, col = "red", lwd = 2) + 
  facet_wrap(~as.integer(TS_year))

# The Bio-Oracle layers are supposed to be the long-term averages between 2000 and 2014
# Compare 2000-2014 data to Bio-Oracle
obs.points %>% left_join(T_mean_df, by = c("ID" = "ID", "Year_HGU" = "TS_year")) %>% 
  filter(Year_HGU %in% c(2000:2014)) %>% 
  group_by(X, Y) %>% 
  summarise(BO2 = mean(BO2_tempmean_bdmin, na.rm = TRUE), Meant = mean(Meantemp, na.rm = TRUE)) %>% 
  mutate(Diff = BO2 - Meant) %>% 
  ggplot(aes(x = BO2, y = Meant, col = Diff)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, col = "red", lwd = 1.5) +
  geom_smooth(method = "lm")

linje <- obs.points %>% 
  left_join(
  T_mean_df, 
  by = c("ID" = "ID", "Year_HGU" = "TS_year")) %>% 
  filter(Year_HGU %in% c(2000:2014)) %>% 
  group_by(X, Y) %>% 
  summarise(BO2 = mean(BO2_tempmean_bdmin, na.rm = TRUE), Meant = mean(Meantemp, na.rm = TRUE)) %>% 
  lm(Meant~BO2, data =.)

linje
summary(linje)

# Compared to Bio-Oracle, the temperatures seems to generally be lower. And the difference can be quite large.
# Correlation is decent, but variance increase with mean, lm not appropriate. Probably better to use buffer values than
# attempting to fill out missing values in temperature layers using a model. 
# Could fill out using some sort of "grow" function i GRASS, but there are 26 years/layers to go through ...

#### Creating temperature dataset
# Because mean temperature was the most important variable in the first versions of the model, we spent some time extracting
# historic data. The aim was to couple observations to temperature estimates from the same year. However, even more
# appropriate would probably be to couple each observation to the mean temperature over a period (f.i 3 years)
# Could write a function to fix that

obs.points %>% 
  left_join(T_mean_df %>% mutate(Buffer = ifelse(is.na(Meantemp), NA, "0km")), by = c("ID" = "ID", "Year_HGU" = "TS_year")) %>% dplyr::select(-Yearnr) %>% # mean temperatures
  left_join(T_mean_10km_df %>% mutate(Buffer = ifelse(is.na(Meantemp), NA, "10km")), by = c("ID" = "ID", "Year_HGU" = "TS_year", "Layer" = "Layer")) %>% # Buffer of 10 km radius to fill out NAs
  left_join(T_mean_15km_df %>% mutate(Buffer = ifelse(is.na(Meantemp), NA, "15km")), by = c("ID" = "ID", "Year_HGU" = "TS_year", "Layer" = "Layer")) %>% # 15 km buffer
  rename(Meantemp.z = "Meantemp", Buffer.z = "Buffer") %>% 
  mutate(Meantemp = coalesce(Meantemp.x, Meantemp.y, Meantemp.z)) %>% 
  mutate(Buffer = coalesce(Buffer.x, Buffer.y, Buffer.z)) %>%
  #summary() # OK 
  ggplot(aes(x = BO2_tempmean_bdmin, y = Meantemp, col = Buffer)) +
  geom_abline(intercept = 0, slope = 1, col = "red", lwd = 1.5) +
  geom_point()
# Ser ikke urovekkende ut.

# Tempdata
Tempdata <- 
  obs.points %>% 
  left_join(T_mean_df, by = c("ID" = "ID", "Year_HGU" = "TS_year")) %>% dplyr::select(-Yearnr) %>% # mean temperatures
  left_join(T_mean_10km_df %>% dplyr::select(-Yearnr), by = c("ID" = "ID", "Year_HGU" = "TS_year", "Layer" = "Layer")) %>% # Buffer of 10 km radius to fill out NAs
  left_join(T_mean_15km_df %>% dplyr::select(-Yearnr), by = c("ID" = "ID", "Year_HGU" = "TS_year", "Layer" = "Layer")) %>% # 15 km buffer
    dplyr::select(-Layer) %>% 
  left_join(T_max_df, by = c("ID" = "ID", "Year_HGU" = "TS_year")) %>% 
  left_join(T_max_10km_df, by = c("ID" = "ID", "Year_HGU" = "TS_year", "Layer" = "Layer")) %>% # Buffer of 10 km radius to fill out NAs
  left_join(T_max_15km_df, by = c("ID" = "ID", "Year_HGU" = "TS_year", "Layer" = "Layer")) %>% # 15 km buffer
  rename(Meantemp.z = "Meantemp", Maxtemp.z = "Maxtemp") %>% 
  mutate(Meantemp = coalesce(Meantemp.x, Meantemp.y, Meantemp.z)) %>% 
  mutate(Maxtemp = coalesce(Maxtemp.x, Maxtemp.y, Maxtemp.z)) %>% 
  dplyr::select(ID, Year_HGU, Meantemp, Maxtemp) %>% 
  mutate(Buffer = "0km") %>% 
  mutate(Buffer = ifelse(ID %in% missingT$ID, "10km", Buffer)) %>% 
  mutate(Buffer = ifelse(ID %in% missingT2$ID, "15km", Buffer))

summary(Tempdata)

write.csv(Tempdata, file = "./Data/Tempdata.csv", row.names = FALSE)

# Future predictions BioOracle
Future_df <- raster::extract(BO2_RCP85_2100, obs_latlong, df = TRUE) 
head(Future_df)

write.csv(Future_df, file = "./Data/Future_df.csv", row.names = FALSE)

obs.points %>% left_join(Future_df) %>% 
  ggplot(aes(x = BO2_tempmean_bdmin, y = RCP85meantemp_2100)) +
  geom_point() + 
  geom_abline(intercept = 0, slope = 1)

obs.points %>% left_join(Future_df) %>% 
  mutate(Diff = RCP85meantemp_2100 - BO2_tempmean_bdmin) %>% 
  ggplot(aes(x = BO2_tempmean_bdmin, y = Diff)) +
  geom_point()

obs.points %>% left_join(Future_df) %>% 
  mutate(Diff = RCP85meantemp_2100 - BO2_tempmean_bdmin) %>% 
  ggplot(aes(x = X, y = Y, color = Diff)) +
  geom_point()

# Templag fra Kystonemodellering (KVI) ---------
ssp585 <- raster("./GIS-layers/bottomtemp_1993_ssp585.tif")
ssp585

testdekning <- raster::extract(ssp585, obs_latlong, df = TRUE)
summary(testdekning)
summary()
# Like dårlig dekning som copernicus


# A closer look at # SALINITY ---------------------------------------------

# Over time
SS_mean_df <- raster::extract(SS_mean, obs_latlong, df = TRUE) %>% 
  pivot_longer(cols = so_mean_1993.2018.1:so_mean_1993.2018.26, names_to = "Layer", values_to = "Meansal") %>% 
  mutate(Yearnr = str_remove(Layer, "so_mean_1993.2018.")) %>% 
  left_join(TSyears)
summary(SS_mean_df) 

# Mean sal over time
SSmean <- SS_mean_df %>% left_join(obs.points) %>% 
  mutate(Year = as.integer(Yearnr)) %>% 
  group_by(Year) %>% 
  summarise(Max = max(Meansal, na.rm = TRUE),
            Mean = mean(Meansal, na.rm = TRUE),
            Min = min(Meansal, na.rm = TRUE)) %>% 
  pivot_longer(cols = Max:Min, names_to = "Measure", values_to = "Salinity") %>% 
  ggplot(., aes(x = Year, y = Salinity, col = Measure)) +
  geom_point() +
  labs(title = "Mean")

SSmean

# Test if salmean explains more than salmax ... just to be sure
Salmean_df <- raster::extract(BO2_salmean_bdmin, obs_latlong, df = TRUE) %>% 
  rename(Salmean = Present.Benthic.Min.Depth.Salinity.Mean)

head(Salmean_df)
write.csv(Salmean_df, file = "./Data/Salmean_df.csv", row.names = FALSE)

# Data extraction # URCHIN FRONTS -----------------------------------------

Urchinyears <- data.frame(U_year = c(1980:2018), Yearnr = as.character(c(1:39)))
Urchinyears

Urchin_df <- raster::extract(Urchin, obs_latlong, df = TRUE) %>% 
  pivot_longer(cols = Barren_front_1980.2018.1:Barren_front_1980.2018.39, names_to = "Layer", values_to = "Grazing") %>% 
  mutate(Yearnr = str_remove(Layer, "Barren_front_1980.2018.")) %>% 
  left_join(Urchinyears)

Urchin_df %>% left_join(obs.points) %>% 
  ggplot(aes(x = ID, y = as.integer(Yearnr), col = as.factor(Grazing))) +
  geom_point()
# Shows that we do have some observation points that may have experienced changes in grazing pressure since the 80s

head(Urchin_df)
head(obs.points)

Urchindata <- obs.points %>% left_join(Urchin_df, by = c("ID" = "ID", "Year_HGU" = "U_year")) %>% 
  dplyr::select(ID:Year_HGU, Grazing)

Urchindata %>% ggplot(aes(x = X, y = Y, col = Grazing)) +
  geom_point()

write.csv(Urchindata, file = "./Data/Urchindata.csv", row.names = FALSE)

# Data extraction # NUTR OX LIGHT -----------------------------
NutrOxLightData <- raster::extract(NutrOxLight, obs_latlong, df = TRUE) 
names(NutrOxLightData) <- gsub("Present.Benthic.Min.Depth.", "", names(NutrOxLightData))
summary(NutrOxLightData)

write.csv(NutrOxLightData, file = "./Data/NutrOxLightData.csv", row.names = FALSE)
