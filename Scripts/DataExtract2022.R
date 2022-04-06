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

# Future predictions Climate Scenarios -----------------------------------

# Temperature predictions - BioOracle
BO2_RCP85_2100_meantemp_mindepth <- raster("./GIS-layers/BioOracle-Future/2100AOGCM.RCP85.Benthic.Min.Depth.Temperature.Mean.tif.BOv2_1.tif")
BO2_RCP85_2100_meantemp_mindepth

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

obs_missing <- obs_latlong %>% subset(., ID %in% missingT$ID)

res(bottom_T_mean)
T_mean_5km_df <- raster::extract(bottom_T_mean, obs_missing, 
                                buffer = 5000, fun = mean, na.rm = TRUE,
                                df = TRUE,
                                progress = "text") %>% 
  pivot_longer(cols = bottomT_mean_1993.2018.1:bottomT_mean_1993.2018.26, names_to = "Layer", values_to = "Meantemp") %>% 
  mutate(Yearnr = str_remove(Layer, "bottomT_mean_1993.2018.")) %>% 
  left_join(TSyears)
# Tok veldig lang tid!!! 
# write.csv(T_mean_5km_df, file = "./Data/T_mean_5km_df.csv", row.names = FALSE)
# summary(T_mean_5km_df)
# Ga kun NA med buffer = 0.5
# Bedre å bruke BioOracle nå midlertidig til vi finner en løsning?
# Ser på avstander mellom NA og ikke-NA i rasterlag

avstander <- raster::distance(bottom_T_mean$bottomT_mean_1993.2018.1)
summary(avstander[avstander!=0])
hist(avstander[avstander!=0])


T_min_df <- raster::extract(bottom_T_min, obs_latlong, df = TRUE) %>% 
  pivot_longer(cols = bottomT_min_1993.2018.1:bottomT_min_1993.2018.26, names_to = "Layer", values_to = "Mintemp") %>% 
  mutate(Yearnr = str_remove(Layer, "bottomT_min_1993.2018."))%>% 
  left_join(TSyears)
summary(T_min_df) 

# A lot of NAs. Consider replacing these values with nearest ... Much fewer NAs in the Bio-Oracle layers. 
# Compare values and consider using solely present day values or replacing NAs with present day values.

# T_max_df %>% 
#   ggplot(., aes(x = ID, y = as.integer(Yearnr), col = Maxtemp)) +
#   geom_point()
# 
# # S-N gradient?
# T_max_df %>% left_join(obs.points) %>% 
#   ggplot(., aes(x = as.integer(Yearnr), y = Y, col = Maxtemp)) +
#   geom_point()

#### Plot how these variables have changed at each observation points over time
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
  ggplot(., aes(x = BO2_tempmean_bdmin, y = Meantemp)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, col = "red", lwd = 2) + 
  facet_wrap(~ Yearnr)

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

# Compared to Bio-Oracle, the maximum temperatures seems to generally be higher (negative diff), 
# and the minimum temperatures generally lower (positive diff).
# Thus, the copernicus data may separate data more, but depends on actual resolution of both dem and temp model ...
# Moving on to urchin front at this point, since this is more important to handle right away

# Future predictions BioOracle
Future_df <- raster::extract(BO2_RCP85_2100_meantemp_mindepth, obs_latlong, df = TRUE) %>% 
  rename("RCP85meantemp_2100" = "X2100AOGCM.RCP85.Benthic.Min.Depth.Temperature.Mean.tif.BOv2_1")

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

Urchindata %>% head
# match these to obs.data

# Not sure what to do about temperature etc...

write.csv(Urchindata, file = "./Data/Urchindata.csv", row.names = FALSE)

