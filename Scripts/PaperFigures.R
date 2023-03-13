## Paper figures
require(tidyverse)
require(dismo)
require(Hmisc)
require(gbm)
require(tmap)
require(sf)
require(sp)
require(leaflet)
require(leafletCN)
require(mapview)
require(tmap)

maindir <- "C:\\Rprojects\\NorwegianKelpModels"

# Models
lamhyp.manual.cut.gbm <- readRDS(paste(maindir, "Models/lamhyp.manual.cut.gbm.rds", sep ="/"))
lamhyp.manual.cut.gbm$var.names

saclat.simp.manual.gbm <- readRDS(paste(maindir, "Models/saclat.simp.manual.gbm_v3.rds", sep ="/"))
saclat.simp.manual.gbm$var.names

# Data
lamhyp <- read.csv(paste(maindir, "./Data/lamhyp2022.csv", sep = "/"), stringsAsFactors = FALSE)
lamhyp %>% head

lamdens <- lamhyp %>% dplyr::select(X, Y, Tetthet, Coverage, Year_HGU,
                                    # adding some envvar to look at geo. distr.
                                    Maxtemp, Meantemp, 
                                    Dissolved.oxygen.Mean,
                                    curv500, BO2_salinitymax_bdmean,
                                    Nitrate.Mean, Phosphate.Mean)
head(lamdens)
summary(lamdens)

lamdens %>% dplyr::select(Tetthet, Coverage) %>% table

saclat <- read.csv("./Data/saclat2022.csv", stringsAsFactors = FALSE) %>% 
  filter(!is.na(Tetthet)) # Fjerner en NA
saclat %>% head

sacdens <- saclat %>% dplyr::select(X, Y, Tetthet, Coverage, Year_HGU,
                                   # adding some envvar to look at geo. distr.
                                   Maxtemp, Meantemp, 
                                   Dissolved.oxygen.Mean,
                                   curv500, BO2_salinitymax_bdmean,
                                   Nitrate.Mean, Phosphate.Mean)
summary(sacdens)

# Sampling ----------------------------------------------------------------

# Color gradients
grad <- c(seq(from = 0, to = 10, by = 0.5))
grad
gradpal <- colorNumeric("Greens", grad)
gradpal(4)

grad_sac <- c(seq(from = 0, to = 15, by = 0.5))
grad_sac
gradpal_sac <- colorNumeric("Reds", grad_sac)
gradpal_sac(4)

# SpatialPoints dataframes
lamgeo <- SpatialPointsDataFrame(coords = lamdens %>% dplyr::select(X,Y),
                                 data = lamdens %>% mutate(Farge = gradpal(Tetthet)), 
                                 proj4string = CRS("+proj=longlat +datum=WGS84"))

sacgeo <- SpatialPointsDataFrame(coords = sacdens %>% dplyr::select(X,Y),
                                 data = sacdens %>% mutate(Farge = gradpal_sac(Tetthet)), 
                                 proj4string = CRS("+proj=longlat +datum=WGS84"))


par(mfrow = c(1,1))
plot(lamgeo)
lamgeo

tmap_mode("view")
#tm_basemap("Stamen.TonerBackground") +
tm_shape(lamgeo) +
  tm_dots(col = "Year_HGU")

tmap_mode("view")
#tm_basemap("Stamen.TonerBackground") +
tm_shape(sacgeo) +
  tm_dots(col = "Year_HGU")

tmap_mode("view")
#tm_basemap("Stamen.TonerBackground") +
  tm_shape(lamgeo) +
  tm_dots(col = "Tetthet")

  tmap_mode("view")
  #tm_basemap("Stamen.TonerBackground") +
  tm_shape(sacgeo) +
    tm_dots(col = "Tetthet")
  
  
map1 <- leaflet(lamgeo,
                options = leafletOptions(
  attributionControl=FALSE,
  zoomControl = FALSE)) %>% # removed zoom control for better export to small image
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>% #CartoDB
  addCircles(color = ~Farge, opacity = 0.8, fillColor = ~Farge, fillOpacity = 0.8) %>% 
  addScaleBar("topleft") %>% 
  addLegend("bottomright", pal = gradpal, values = ~ Tetthet,
            title = "Observed density </br> of tangle kelp </br> (plants m<sup>-2</sup>)") %>% 
  setView(lng = 16, lat = 65.5, zoom = 5)

map1


mapshot(map1, file = paste(maindir, "Figures/LamhypSampleMap_v2.png", sep = "/"), 
        remove_controls = c("zoomControl", "layersControl"),
        vwidth = 700, vheight = 744)

map2 <- leaflet(sacgeo,
                options = leafletOptions(
                  attributionControl=FALSE,
                  zoomControl = FALSE)) %>% # removed zoom control for better export to small image 
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>% #CartoDB #Esri.WorldGrayCanvas
  addCircles(color = ~Farge, opacity = 0.8, fillColor = ~Farge, fillOpacity = 0.8) %>% 
  addScaleBar("topleft") %>% 
  addLegend("bottomright", pal = gradpal_sac, values = ~ Tetthet,
            title = "Observed density </br> of sugar kelp </br> (plants m<sup>-2</sup>)") %>% 
  setView(lng = 16, lat = 65.5, zoom = 5) 
map2

mapshot(map2, file = paste(maindir, "Figures/SaclatSampleMap_v2.png", sep = "/"), 
        remove_controls = c("zoomControl", "layersControl"),
        vwidth = 700, vheight = 744)


# Kan bruke "mapshot" for å fjerne kontroller og slikt
# Maybe better to make maps for the publication in QGIS?

# Sampling times and coverage
lamhyp %>% dplyr::select(Year_HGU, Coverage) %>% mutate(Coverage = round(Coverage)) %>% table
# Which is why we should use Tetthet. The scale probably changed from 0-3 to 0-4 pretty late in the series
lamhyp %>% dplyr::select(Coverage, Tetthet) %>% table
# Hege has accounted for this in the caclulations of Tetthet

# Sampling times and Tetthet/Density
lamhyp  %>% mutate(Density = round(Tetthet)) %>% 
  dplyr::select(Year_HGU, Density) %>% table 

lamhyp %>% group_by(Year_HGU) %>% summarise(N = n())

saclat %>% group_by(Year_HGU) %>% summarise(N = n()) %>% print(n = Inf)

# Environmental space -----------------------------------------------------

cfig1 <- lamhyp.manual.cut.gbm$contributions %>% 
  ggplot(aes(x = reorder(var, rel.inf), y = rel.inf)) +
  geom_bar(stat = "identity", fill = "aquamarine4") +
  labs(title = "Tangle kelp model", x = "", y = "Relative influence of variable (%)") +
  coord_flip() +
  ylim(0,28) +
  theme_minimal()
  
cfig2 <- saclat.simp.manual.gbm$contributions %>% 
  ggplot(aes(x = reorder(var, rel.inf), y = rel.inf)) +
  geom_bar(stat = "identity", fill = "coral3") +
  labs(title = "Sugar kelp model", x = "", y = "Relative influence of variable (%)") +
  coord_flip() +
  ylim(0,28) +
  theme_minimal()

cfig <- cfig1+cfig2
cfig
ggsave(cfig, path = "./Figures",
       filename = "BRTVarInf.png",
       device = "png")

tmap_mode("view")
tm_shape(lamgeo) +
  tm_dots(col = "Nitrate.Mean")

tmap_mode("view")
tm_shape(lamgeo) +
  tm_dots(col = "Phosphate.Mean")

tmap_mode("view")
tm_shape(lamgeo) +
  tm_dots(col = "Dissolved.oxygen.Mean")

tmap_mode("view")
tm_shape(lamgeo) +
  tm_dots(col = "curv500")

tmap_mode("view")
tm_shape(lamgeo) +
  tm_dots(col = "Meantemp")

tmap_mode("view")
tm_shape(lamgeo) +
  tm_dots(col = "Maxtemp")


# Density distribution and environmental space ----------------------------

lamdens %>% ggplot(aes(x = Tetthet)) +
  geom_histogram(fill = "rosybrown", bins = 5)

lamhyp.manual.cut.gbm$var.names

data.frame(lamhyp, Kelp = "Tangle") %>% 
  full_join(data.frame(saclat, Kelp = "Sugar")) %>% 
  dplyr::select(Kelp, Tetthet, Depth_mod, #curv500, slope, 
                         swm, #wspd90, 
                         #Dissolved.oxygen.Mean, Nitrate.Mean, Phosphate.Mean, Meantemp, 
                         Maxtemp, BO2_salinitymax_bdmean) %>% 
  mutate(Kelp = factor(Kelp, levels = c("Tangle", "Sugar"))) %>% 
  mutate(Tetthet = round(Tetthet)) %>% 
  filter(Tetthet > 0) %>% 
  pivot_longer(cols = Depth_mod:BO2_salinitymax_bdmean) %>% 
  ggplot(aes(x = as.factor(Tetthet), y = value, fill = Kelp)) +
  geom_boxplot() +
  coord_flip() +
  labs(x = "Density (count)", y = "") +
  scale_fill_manual(values=c("aquamarine4", "coral3")) +
  facet_grid(Kelp ~ name, scales = "free") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
# Might be a bit confusing?

# TABLE data
data.frame(lamhyp, Kelp = "Tangle") %>% 
  full_join(data.frame(saclat, Kelp = "Sugar")) %>% 
  mutate(kSWM = swm/1000) %>% 
  dplyr::select(Kelp, Tetthet, Depth_mod, curv500, slope,
                         kSWM, wspd90, 
                         Nitrate.Mean, Phosphate.Mean, Dissolved.oxygen.Mean,
                         Meantemp, Maxtemp, BO2_salinitymax_bdmean) %>% 
  # give more readable names
  rename(Depth = "Depth_mod",
         Curvature = "curv500",
         Slope = "slope",
         Current = "wspd90",
         Nitrate = "Nitrate.Mean",
         Phosphate = "Phosphate.Mean",
         Oxygen = "Dissolved.oxygen.Mean",
         `Mean temp.` = "Meantemp",
         `Max temp.` = "Maxtemp",
         Salinity = "BO2_salinitymax_bdmean") %>% 
  
  mutate(Tetthet = round(Tetthet)) %>% 
  filter(Tetthet > 0) %>% # NB!!!!!!!!!!!!!!!!!!
  pivot_longer(cols = Depth:Salinity) %>% 
  group_by(name) %>% summarise(Min_tangle = round(min(value[Kelp == "Tangle"], na.rm = TRUE), digits = 2), 
                               Mean_tangle = round(mean(value[Kelp == "Tangle"], na.rm = TRUE), digits = 2), 
                               Max_tangle = round(max(value[Kelp == "Tangle"], na.rm= TRUE), digits = 2),
                               Min_sugar = round(min(value[Kelp == "Sugar"], na.rm = TRUE), digits = 2), 
                               Mean_sugar = round(mean(value[Kelp == "Sugar"], na.rm = TRUE), digits = 2), 
                               Max_sugar = round(max(value[Kelp == "Sugar"], na.rm= TRUE), digits = 2)) %>% 
  print(n = Inf) %>% 
  write.csv2(file = "./Tables/EnvTable.csv", row.names = FALSE)

### NB!! these are the ranges within which kelp was recorded!!! ###


# Model statistics --------------------------------------------------------

lamhyp.manual.cut.gbm$n.trees
lamhyp.manual.cut.gbm$self.statistics
lamhyp.manual.cut.gbm$cv.statistics  


# Marginal effects ------------------------------------------------------

lamhyp.manual.cut.gbm$var.names

gbm.plot(lamhyp.manual.cut.gbm, write.title=TRUE, show.contrib=TRUE,
         common.scale = TRUE,
         smooth = TRUE,
         plot.layout = c(2,5),
         lty = "dotted") 

gbm.plot.fits(lamhyp.manual.cut.gbm)

# Ask if Kristina has script to plot these a bit neater
# Have created own script to source into manuscript

# Interactions ------------------------------------------------------------

find.int <- gbm.interactions(lamhyp.manual.cut.gbm)

find.int$rank.list
find.int$interactions

gbm.perspec(lamhyp.manual.cut.gbm, 2, 1,
            z.range = c(-1, 3))

gbm.perspec(lamhyp.manual.cut.gbm, 9, 1,
            z.range = c(-1, 3))

gbm.perspec(lamhyp.manual.cut.gbm, 6, 5,
            z.range = c(-1, 3))

gbm.perspec(lamhyp.manual.cut.gbm, 10, 5,
            z.range = c(-1, 3))

gbm.perspec(lamhyp.manual.cut.gbm, 5, 1,
            z.range = c(-1, 3))

find.int_sac <- gbm.interactions(saclat.simp.manual.gbm)

find.int_sac$rank.list
find.int_sac$interactions

gbm.perspec(saclat.simp.manual.gbm, 6, 5,
            z.range = c(-1, 3))

gbm.perspec(saclat.simp.manual.gbm, 11, 10,
            z.range = c(-1, 3))

gbm.perspec(saclat.simp.manual.gbm, 7, 3,
            z.range = c(-1, 3))

gbm.perspec(saclat.simp.manual.gbm, 4, 3,
            z.range = c(-1, 3))


# Map predictions ---------------------------------------------------------

# Lamhyp downsample:
predfinal <- raster("./Predictions/predLAMHYdens2022_full.grd")
plot(predstack_2022, predfinal)

hist(predfinal[predfinal > 0])

# lamhyp_downsample <- raster::aggregate(predfinal,
#                                fact = 20,
#                                fun = mean,
#                                na.rm = TRUE)
# writeRaster(lamhyp_downsample, "./Predictions/predLAMHYdens2022_downsample500")


# Saclat downsample:
predfinal_sac <- raster("./Predictions/predSACLATdens2022_crop40")
plot(predstack_2022, predfinal_sac)

# Model now truncated at 40 m depth

# saclat_downsample <- raster::aggregate(predfinal_sac,
#                                fact = 20,
#                                fun = mean,
#                                na.rm = TRUE,
#                                filename = "./Predictions/predSACLATdens2022_downsample500",
#                                overwrite = TRUE)

## PLOT Lamhyp

lamhyp_downsample <- raster("./Predictions/predLAMHYdens2022_downsample500")

plot(lamhyp_downsample)
lamhyp_plot <- lamhyp_downsample %>% projectRaster(., crs = "+proj=longlat +datum=WGS84")
lamhyp_plot[lamhyp_plot < 1] <- NA
extent(lamhyp_plot)

tmap_mode(mode = "view")
tm_shape(lamhyp_plot) +
  tm_raster(style = "cont", palette = "Greens") +
  tm_layout(title = "BRT model tangle kelp",
            legend.outside = TRUE)

# Color gradients
gradpal2 <- colorNumeric("Spectral", values(lamhyp_plot), na.color = "transparent", reverse = TRUE)

predmap1 <- leaflet(lamhyp_plot,
                  options = leafletOptions(
                  attributionControl=FALSE,
                  zoomControl = FALSE)) %>% # removed zoom control for better export to small image
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>% #CartoDB.DarkMatter #Esri.WorldGrayCanvas
  addRasterImage(lamhyp_plot, colors = gradpal2) %>% 
  addScaleBar("topleft")  %>%
  addLegend("bottomright", pal = gradpal2, values = values(lamhyp_plot),
            title = "Predicted density </br> of tangle kelp </br> (plants m<sup>-2</sup>)") %>% 
  setView(lng = 16, lat = 65.5, zoom = 5)

predmap1

mapshot(predmap1, file = paste(maindir, "Figures/LamhypBRTMap.png", sep = "/"), 
        remove_controls = c("zoomControl", "layersControl"),
        vwidth = 700, vheight = 744)

## PLOT Saclat

saclat_downsample <- raster("./Predictions/predSACLATdens2022_downsample500")
saclat_downsample

plot(saclat_downsample)
saclat_plot <- saclat_downsample %>% projectRaster(., crs = "+proj=longlat +datum=WGS84")
saclat_plot[saclat_plot < 1] <- NA
extent(saclat_plot)

tmap_mode(mode = "view")
tm_shape(saclat_plot) +
  tm_raster(style = "cont", palette = "Reds") +
  tm_layout(title = "BRT model sugar kelp",
            legend.outside = TRUE)

# Color gradients
gradpal2_sac <- colorNumeric("Spectral", values(saclat_plot), na.color = "transparent", reverse = TRUE)

predmap2 <- leaflet(saclat_plot,
                    options = leafletOptions(
                      attributionControl=FALSE,
                      zoomControl = FALSE)) %>% # removed zoom control for better export to small image
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>% #CartoDB.DarkMatter #Esri.WorldGrayCanvas
  addRasterImage(saclat_plot, colors = gradpal2_sac) %>% 
  addScaleBar("topleft")  %>%
  addLegend("bottomright", pal = gradpal2_sac, values = values(saclat_plot),
            title = "Predicted density </br> of sugar kelp </br> (plants m<sup>-2</sup>)") %>% 
  setView(lng = 16, lat = 65.5, zoom = 5)

predmap2

mapshot(predmap2, file = paste(maindir, "Figures/SaclatBRTMap.png", sep = "/"), 
        remove_controls = c("zoomControl", "layersControl"),
        vwidth = 700, vheight = 744)



# Area summaries ----------------------------------------------------------

lamhyp_rastertable <- freq(predfinal, progress = "text")
lamhyp_rastertable

zonalLAMHYdens_kg <- read.csv("./Tables/zonalLAMHYdens_kg.csv") %>% 
  filter(Category != 0) %>% 
  mutate(densities = ifelse(Category == 1, "1 - 4", "> 4")) %>% 
  mutate(Biomass_milltonn = Biomass_kg/1000000000)
str(zonalLAMHYdens_kg)

lamhyp_rastertable %>% data.frame %>% 
  filter(value > 0) %>% 
  mutate(area_m = count*25*25,
         area_km = area_m/1e+06,
         forest = ifelse(value > 4, 1L, 0L)) %>%
  group_by(forest) %>% 
  summarise(N = sum(count), Area_m = sum(area_m), Area_km = sum(area_km)) %>% 
  add_column(densities = 0L, .after = "forest") %>%
  mutate(densities = ifelse(forest == 1, "> 4", "1 - 4")) %>%
  dplyr::select(-forest) %>% data.frame() %>% 
  left_join(zonalLAMHYdens_kg) %>% 
  dplyr::select(-c(Area_m, Category, Biomass_kg)) %>% 
  print %>% 
  write.csv(., file = "./Tables/lamhyp_rastertable.csv", row.names = FALSE)


saclat_rastertable <- freq(predfinal_sac, progress = "text")
saclat_rastertable

saclat_rastertable %>% data.frame() %>% 
  filter(value > 0) %>% 
  filter(!is.na(value)) %>% 
  mutate(area_m = count*25*25,
         area_km = area_m/1e+06,
         forest = ifelse(value > 6, 1L, 0L)) %>%
  group_by(forest) %>% 
  summarise(N = sum(count), Area_m = sum(area_m), Area_km = sum(area_km)) %>% 
  add_column(densities = 0L, .after = "forest") %>%
  mutate(densities = ifelse(forest == 1, "> 6", "1 - 6")) %>%
  dplyr::select(-forest) %>%  
  print %>% 
  write.csv(., file = "./Tables/saclat_rastertable.csv", row.names = FALSE)

# Sjekk Frigstad et al ...
# Extreme differences in the predictions of S. latissima forest areas
# I do trust this model more than the previous (range of values etc. seems much more realistic)
# Har dobbeltsjekka med å regne ut i excel og kommer til samme arealer


