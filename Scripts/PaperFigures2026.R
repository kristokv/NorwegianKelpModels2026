## Paper figures
require(tidyverse)
require(dismo)
require(Hmisc)
require(gbm)
require(tmap)
require(sf)
require(sp)
require(leaflet)
require(leafletCN) # NOT AVAILABLE
require(mapview)
require(tmap)
require(webshot)
require(webshot2)
require(terra)
require(patchwork)
require(writexl)


maindir <- "D:\\Taremodeller\\NorwegianKelpModels"

# Models
lamhyp.manual.cut.gbm <- readRDS(paste(maindir, "Models/lamhyp.manual.cut.gbm.rds", sep ="/"))
lamhyp.manual.cut.gbm$var.names

saclat.simp.manual.gbm <- readRDS(paste(maindir, "Models/saclat.simp.manual.gbm_v3.rds", sep ="/"))
saclat.simp.manual.gbm$var.names

# Data
lamhyp <- read.csv("../Data/lamhyp2022.csv", stringsAsFactors = FALSE)
#lamhyp %>% head

lamdens <- lamhyp %>% dplyr::select(X, Y, Tetthet, Coverage, Year_HGU,
                                    # adding some envvar to look at geo. distr.
                                    Maxtemp, Meantemp, 
                                    Dissolved.oxygen.Mean,
                                    curv500, BO2_salinitymax_bdmean,
                                    Nitrate.Mean, Phosphate.Mean)
#head(lamdens)
#summary(lamdens)

#lamdens %>% dplyr::select(Tetthet, Coverage) %>% table

saclat <- read.csv(paste(maindir, "./Data/saclat2022.csv", sep = "/"), stringsAsFactors = FALSE) %>% 
  filter(!is.na(Tetthet)) # Fjerner en NA
#saclat %>% head

sacdens <- saclat %>% dplyr::select(X, Y, Tetthet, Coverage, Year_HGU,
                                   # adding some envvar to look at geo. distr.
                                   Maxtemp, Meantemp, 
                                   Dissolved.oxygen.Mean,
                                   curv500, BO2_salinitymax_bdmean,
                                   Nitrate.Mean, Phosphate.Mean)
#summary(sacdens)

# Observational data ----------------------------------------------------------------

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


map1 <- leaflet(lamgeo,
                options = leafletOptions(
  attributionControl=FALSE,
  zoomControl = FALSE)) %>% # removed zoom control for better export to small image
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>% #CartoDB
  addCircles(color = ~Farge, opacity = 0.8, fillColor = ~Farge, fillOpacity = 0.8) %>% 
  #addScaleBar("topleft") %>% 
  addLegend("bottomright", pal = gradpal, values = ~ Tetthet,
            title = "Observed density </br> of tangle kelp </br> (ind. m<sup>-2</sup>)") %>% 
  addControl(html = "<b style='font-size:20px;'>A</b>", position = "topleft") %>%
  setView(lng = 16, lat = 65.5, zoom = 5)

map1


mapshot(map1, file = paste(maindir, "Figures/LamhypSampleMap_v3.png", sep = "/"), 
        remove_controls = c("zoomControl", "layersControl"),
        vwidth = 700, vheight = 744)

map2 <- leaflet(sacgeo,
                options = leafletOptions(
                  attributionControl=FALSE,
                  zoomControl = FALSE)) %>% # removed zoom control for better export to small image 
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>% #CartoDB #Esri.WorldGrayCanvas
  addCircles(color = ~Farge, opacity = 0.8, fillColor = ~Farge, fillOpacity = 0.8) %>% 
  #addScaleBar("topleft") %>% 
  addLegend("bottomright", pal = gradpal_sac, values = ~ Tetthet,
            title = "Observed density </br> of sugar kelp </br> (ind. m<sup>-2</sup>)") %>% 
  addControl(html = "<b style='font-size:20px;'>B</b>", position = "topleft") %>%
  setView(lng = 16, lat = 65.5, zoom = 5) 
map2

mapshot(map2, file = paste(maindir, "Figures/SaclatSampleMap_v3.png", sep = "/"), 
        remove_controls = c("zoomControl", "layersControl"),
        vwidth = 700, vheight = 744)


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

# Environmental space ----------------------------

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


# Closer inspection of the relationship with oxygen
xlims <- range(c(lamdens$Dissolved.oxygen.Mean,sacdens$Dissolved.oxygen.Mean),na.rm=T)
ylims <- c(-10, 15)  
oxplot_lam <- lamdens %>% 
  ggplot(aes(Dissolved.oxygen.Mean, Tetthet)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "gam") +
  xlim(xlims) + ylim(ylims) +
  labs(x = "Oxygen", y = "Density of tangle kelp (ind.m²)", tag = "A") +
  theme_bw(base_size = 14) + 
  theme(panel.grid.minor = element_line(color = "grey90"),
        plot.tag = element_text(size = 18, face = "bold"),
        plot.tag.position = c(0.02, 0.98))
  
oxplot_sac <- sacdens %>% 
  ggplot(aes(Dissolved.oxygen.Mean, Tetthet)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "gam") +
  xlim(xlims) + ylim(ylims) +
  labs(x = "Oxygen", y = "Density of sugar kelp (ind.m²)", tag = "B") +
  theme_bw(base_size = 14) + 
  theme(panel.grid.minor = element_line(color = "grey90"),
        plot.tag = element_text(size = 18, face = "bold"),
        plot.tag.position = c(0.02, 0.98))

oxplots <- oxplot_lam + oxplot_sac


# Model statistics --------------------------------------------------------
# Funksjon for å skrive ut diverse summary statistics om BRT-modell
compare.brt <- function(x){
  tab.compare.brt <- data.frame(Model = character(),
                                DevExpTrain = numeric(),
                                DevExpCV = numeric(),
                                CorTrain = numeric(),
                                CorCV = numeric(),
                                #AUCtrain = numeric(), # AUC er bare for binære utfall
                                #AUCcv = numeric(),
                                stringsAsFactors = FALSE)
  
  
  for(i in 1:length(x)){
    y <- eval(parse(text = x[i]))
    tot.dev <- y$self.statistics$mean.null # mean null deviance
    res.devTrain <- y$self.statistics$mean.resid # mean residual deviance training
    res.devCV <- y$cv.statistics$deviance.mean  # mean residual deviance cv
    dev.expTrain <- round((tot.dev-res.devTrain)/tot.dev,2) # deviance explained
    dev.expCV <- round((tot.dev-res.devCV)/tot.dev,2) # deviance explained
    CorTrain <- round(y$self.statistics$correlation,2)
    CorCV <- round(y$cv.statistics$correlation.mean,2)
    #CorSE <- round(y$cv.statistics$correlation.se,2)
    # AUCtrain <- round(y$self.statistics$discrimination,2) # AUC training data
    # AUCcv    <- round(y$cv.statistics$discrimination.mean,2) # AUC cross validation
    #AUCcvSE    <- round(y$cv.statistics$discrimination.se,2) # AUC standard error
    tab.compare.brt[i, ] <- c(x[i], dev.expTrain,dev.expCV, CorTrain, CorCV)
  }
  return(tab.compare.brt)
}

compare.brt(c("lamhyp.manual.cut.gbm","saclat.simp.manual.gbm "))

# Marginal effects ------------------------------------------------------
source("gbm_plot_own_GSA.R")

varnames_match <- setNames(c("Depth, m",  "Wave exposure, m2/s", "Curvature, m", "Slope", "Nitrate, µmol m-3", "Oxygen, µmol m-3", 
                     "Phosphate, µmol m-3", "Mean temperature, °C", "Max temperature, °C", "Currents, m/s","Max salinity"),
                     c("Depth_mod", "swm", "curv500","slope", "Nitrate.Mean", "Dissolved.oxygen.Mean" ,"Phosphate.Mean",
                       "Meantemp", "Maxtemp", "wspd90", "BO2_salinitymax_bdmean"))



# gbm.plot(lamhyp.manual.cut.gbm, write.title=TRUE, show.contrib=TRUE,
#          common.scale = TRUE,
#          smooth = TRUE,
#          plot.layout = c(2,5),
#          lty = "dotted") 

varnames_lamhyp <- unname(varnames_match[lamhyp.manual.cut.gbm$contributions$var])

png("../Figures/MarginalEffectsLamhyp.png", width = 2700, height = 1500, res = 300)
par(mar=c(2.5,2,1.5,0), oma = c(1.5,2,0,1), mfrow = c(2,6))
gbm.plot.own(lamhyp.manual.cut.gbm, dat = lamhyp, varnames = varnames_lamhyp,
             with_yax=c(1,6),species="tangle kelp", tag = "A")
dev.off()

varnames_saclat <-  unname(varnames_match[saclat.simp.manual.gbm$contributions$var])

png("../Figures/MarginalEffectsSaclat.png", width = 2700, height = 1500, res = 300)
par(mar=c(2.5,2,1.5,0), oma = c(1.5,3,0,1), mfrow = c(2,6))
gbm.plot.own(saclat.simp.manual.gbm, dat = saclat, varnames = varnames_saclat, tag = "B",
             with_yax=c(1,6),species="sugar kelp")
dev.off()

# Plot only oxygen effect

gbm.plot(lamhyp.manual.cut.gbm, variable.no = 6, plot.layout = c(1,1), xlim = xlims)
gbm.plot( saclat.simp.manual.gbm, variable.no = 7, plot.layout = c(1,1), xlim = xlims)


# Interactions ------------------------------------------------------------
source("gbm_persp_plot_own_KVI.R")

find.int <- gbm.interactions(lamhyp.manual.cut.gbm)
#find.int_alt <- gbm::interact.gbm(lamhyp.manual.cut.gbm, data = lamhyp, i.var = c("swm", "Depth_mod"))

find.int$rank.list
find.int$interactions

find.int_sac <- gbm.interactions(saclat.simp.manual.gbm)
#find.int_alt <- gbm::interact.gbm(saclat.simp.manual.gbm, data = lamhyp, i.var = c("swm", "Depth_mod"))

find.int_sac$rank.list
find.int_sac$interactions

int.table <- find.int$rank.list  %>% 
  dplyr::select(var1.names, var2.names, int.size) %>% 
  mutate(Species = "Tangle kelp") %>%
  rbind(find.int_sac$rank.list %>% 
          dplyr::select(var1.names, var2.names, int.size) %>% 
          mutate(Species = "Sugar kelp"))  %>% 
  relocate(Species)  %>% 
  mutate(var1.names = unname(varnames_match[var1.names]),
         var2.names = unname(varnames_match[var2.names]))  %>% 
  rename("1st variable" = var1.names, "2nd variable" = var2.names, "Interaction strength" = int.size)
  
write_xlsx(int.table, path = "../Tables/int.table.xlsx")


png("../Figures/InteractionEffectsBoth.png", width = 2700, height = 2500, res = 300)
par(mar=c(4,4,2,1), oma = rep(0,4), mfrow = c(2,2))

gbm_contour_filled_labeled(
  gbm.object = lamhyp.manual.cut.gbm,  x = 2,  y = 1,  
  xlab = unname(varnames_match[find.int$rank.list$var1.names[1]]),
  ylab = unname(varnames_match[find.int$rank.list$var2.names[1]]),
  # optional styling:
  palette = grDevices::colorRampPalette(c("#FFFFD4", "#FED98E", "#FE9929", "#D95F0E", "#993404")),
  contour.col = "grey15",
  contour.lwd = 0.8,
  contour.labcex = 0.8,
  add_legend = FALSE)
mtext(side = 3, "A. Tangle kelp", adj = 0.05)

gbm_contour_filled_labeled(
  gbm.object = lamhyp.manual.cut.gbm,  x = 9,  y = 1,  
  xlab = unname(varnames_match[find.int$rank.list$var1.names[2]]),
  ylab = unname(varnames_match[find.int$rank.list$var2.names[2]]),
  # optional styling:
  palette = grDevices::colorRampPalette(c("#FFFFD4", "#FED98E", "#FE9929", "#D95F0E", "#993404")),
  contour.col = "grey15",
  contour.lwd = 0.8,
  contour.labcex = 0.8,
  add_legend = FALSE)


gbm_contour_filled_labeled(
  gbm.object = saclat.simp.manual.gbm,  x = 6,  y = 5,  
  xlab = unname(varnames_match[find.int_sac$rank.list$var1.names[1]]),
  ylab = unname(varnames_match[find.int_sac$rank.list$var2.names[1]]),
  # optional styling:
  palette = grDevices::colorRampPalette(c("#FFFFD4", "#FED98E", "#FE9929", "#D95F0E", "#993404")),
  contour.col = "grey15",
  contour.lwd = 0.8,
  contour.labcex = 0.8,
  add_legend = FALSE)
mtext(side = 3, "B. Sugar kelp", adj = 0.05)

gbm_contour_filled_labeled(
  gbm.object = saclat.simp.manual.gbm,  x =11,  y = 10,  
  xlab = unname(varnames_match[find.int_sac$rank.list$var1.names[2]]),
  ylab = unname(varnames_match[find.int_sac$rank.list$var2.names[2]]),
  # optional styling:
  palette = grDevices::colorRampPalette(c("#FFFFD4", "#FED98E", "#FE9929", "#D95F0E", "#993404")),
  contour.col = "grey15",
  contour.lwd = 0.8,
  contour.labcex = 0.8,
  add_legend = FALSE)


dev.off()



# 3d-plots, harder to interpret:

gbm.perspec(lamhyp.manual.cut.gbm, 2, 1,
            z.range = c(-1, 3))

gbm.perspec(lamhyp.manual.cut.gbm, 9, 1,
            z.range = c(-1, 3))


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
predfinal <- rast("../Predictions/predLAMHYdens2022_full.grd") 

#hist(predfinal[predfinal > 0])

# lamhyp_downsample <- terra::aggregate(predfinal,
#                                fact = 20,
#                                fun = mean,
#                                na.rm = TRUE)
# writeRaster(lamhyp_downsample, "../Predictions/predLAMHYdens2022_downsample500.grd", overwrite = TRUE)


# Saclat downsample:
predfinal_sac <- rast("../Predictions/predSACLATdens2022_crop40.grd")
#predfinal_sac <- rast("../Predictions/SACpred_Norway.tif")

# Model now truncated at 40 m depth

# saclat_downsample <- terra::aggregate(predfinal_sac,
#                                fact = 20,
#                                fun = mean,
#                                na.rm = TRUE)
# writeRaster(saclat_downsample, "../Predictions/predSACLATdens2022_downsample500.grd")

## PLOT Lamhyp

lamhyp_downsample <- rast("../Predictions/predLAMHYdens2022_downsample500.grd")


#plot(lamhyp_downsample)
lamhyp_plot <- project(lamhyp_downsample, "EPSG:4326", method = "bilinear")
# Covnert to raster for leaflet::addRasterimage
lamhyp_plot <- raster::raster(lamhyp_plot)
lamhyp_plot[lamhyp_plot < 1] <- NA
#ext(lamhyp_plot)
vals <- raster::values(lamhyp_plot)
rng <- range(vals, na.rm = TRUE)

tmap_mode(mode = "view")
tm_shape(lamhyp_plot) +
  tm_raster(style = "cont", palette = "Greens") +
  tm_layout(title = "BRT model tangle kelp",
            legend.outside = TRUE)

# Color gradients
gradpal2 <- colorNumeric(palette = "Spectral", domain = rng, na.color = "transparent", reverse = TRUE)

predmap1 <- leaflet(lamhyp_plot,
                  options = leafletOptions(
                  attributionControl=FALSE,
                  zoomControl = FALSE)) %>% # removed zoom control for better export to small image
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>% #CartoDB.DarkMatter #Esri.WorldGrayCanvas
  addRasterImage(lamhyp_plot, colors = gradpal2) %>% 
  addLegend("bottomright", pal = gradpal2, values = vals,
            title = "Predicted density </br> of tangle kelp </br> (ind.m<sup>-2</sup>)") %>% 
  addControl(html = "<b style='font-size:20px;'>A</b>", position = "topleft") %>%
  setView(lng = 16, lat = 65.5, zoom = 5)

predmap1

# Not working, too much data?
# mapview::mapshot(predmap1, file = paste(maindir, "/Figures/LamhypBRTMap_v3_PLACEHOLDER.png", sep = "/"), 
#         remove_controls = c("zoomControl", "layersControl"),
#         vwidth = 700, vheight = 744, delay = 3)

out_html <- file.path(paste(maindir, "/Figures/LamhypBRTMap_v3.html", sep = "/"))
out_png  <- file.path(paste(maindir, "/Figures/LamhypBRTMap_v3.png", sep = "/"))

htmlwidgets::saveWidget(predmap1, out_html, selfcontained = TRUE)
webshot2::webshot(out_html, file = out_png, vwidth = 700, vheight = 744, delay = 3)


## PLOT Saclat

saclat_downsample <- rast("../Predictions/predSACLATdens2022_downsample500.grd")

#plot(saclat_downsample)
saclat_plot <- project(saclat_downsample, "EPSG:4326", method = "bilinear")
# Covnert to raster for leaflet::addRasterimage
saclat_plot <- raster::raster(saclat_plot)
saclat_plot[saclat_plot < 1] <- NA
#extent(saclat_plot)
vals <- raster::values(saclat_plot)
rng <- range(vals, na.rm = TRUE)

tmap_mode(mode = "view")
tm_shape(saclat_plot) +
  tm_raster(style = "cont", palette = "Reds") +
  tm_layout(title = "BRT model sugar kelp",
            legend.outside = TRUE)

# Color gradients
gradpal2_sac <- colorNumeric(palette = "Spectral", domain = rng, na.color = "transparent", reverse = TRUE)

predmap2 <- leaflet(saclat_plot,
                    options = leafletOptions(
                      attributionControl=FALSE,
                      zoomControl = FALSE)) %>% # removed zoom control for better export to small image
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>% #CartoDB.DarkMatter #Esri.WorldGrayCanvas
  addRasterImage(saclat_plot, colors = gradpal2_sac) %>% 
  #addScaleBar("topleft")  %>%
  addLegend("bottomright", pal = gradpal2_sac, values = values(saclat_plot),
            title = "Predicted density </br> of sugar kelp </br> (ind. m<sup>-2</sup>)") %>% 
  addControl(html = "<b style='font-size:20px;'>B</b>", position = "topleft") %>%
  setView(lng = 16, lat = 65.5, zoom = 5)

predmap2

# Not working, too much data?
# mapshot(predmap2, file = paste(maindir, "Figures/SaclatBRTMap.png", sep = "/"), 
#         remove_controls = c("zoomControl", "layersControl"),
#         vwidth = 700, vheight = 744)

out_html <- file.path(paste(maindir, "/Figures/SaclatBRTMap_v3.html", sep = "/"))
out_png  <- file.path(paste(maindir, "/Figures/SaclatBRTMap_v3.png", sep = "/"))

htmlwidgets::saveWidget(predmap2, out_html, selfcontained = TRUE)
webshot2::webshot(out_html, file = out_png, vwidth = 700, vheight = 744, delay = 3)


# Zoomed in areas ----------------------------------------------------------
ext_ll <- ext(5.35, 5.95, 62.25, 62.43)
ext_poly  <- as.polygons(ext_ll, crs = "EPSG:4326")
ext_poly2 <- project(ext_poly, crs(predfinal))     
ext_new   <- ext(ext_poly2)

# Crop
lamhyp_plot_zoom <- crop(predfinal, ext_new)
lamhyp_plot_zoom <- project(lamhyp_plot_zoom, "EPSG:4326", method = "bilinear")
# Covnert to raster for leaflet::addRasterimage
lamhyp_plot_zoom <- raster::raster(lamhyp_plot_zoom)
lamhyp_plot_zoom[lamhyp_plot_zoom < 1] <- NA


#ext(lamhyp_plot_zoom_zoom)
vals <- raster::values(lamhyp_plot_zoom)
rng <- range(vals, na.rm = TRUE)

tmap_mode(mode = "view")
tm_shape(lamhyp_plot_zoom) +
  tm_raster(style = "cont", palette = "Greens") +
  tm_layout(title = "BRT model tangle kelp",
            legend.outside = TRUE)

# Color gradients
gradpal2 <- colorNumeric(palette = "Spectral", domain = rng, na.color = "transparent", reverse = TRUE)

lng_zoom <- 5.6
lat_zoom <- 62.35

predmap1 <- leaflet(lamhyp_plot_zoom,
                    options = leafletOptions(
                      attributionControl=FALSE,
                      zoomControl = FALSE)) %>% # removed zoom control for better export to small image
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>% #CartoDB.DarkMatter #Esri.WorldGrayCanvas
  addRasterImage(lamhyp_plot_zoom, colors = gradpal2) %>% 
  addLegend("bottomright", pal = gradpal2, values = vals,
            title = "Predicted density </br> of tangle kelp </br> (ind.m<sup>-2</sup>)") %>% 
  addControl(html = "<b style='font-size:20px;'>A</b>", position = "topleft") %>%
  setView(lng = lng_zoom, lat = lat_zoom, zoom = 11.5)

predmap1

out_html <- file.path(paste(maindir, "/Figures/LamhypBRTMapZoom_v3.html", sep = "/"))
out_png  <- file.path(paste(maindir, "/Figures/LamhypBRTMapZoom_v3.png", sep = "/"))

htmlwidgets::saveWidget(predmap1, out_html, selfcontained = TRUE)
webshot2::webshot(out_html, file = out_png, vwidth = 700, vheight = 744, delay = 3)



saclat_plot_zoom <- crop(predfinal_sac, ext_new)
saclat_plot_zoom <- project(saclat_plot_zoom, "EPSG:4326", method = "bilinear")
# Covnert to raster for leaflet::addRasterimage
saclat_plot_zoom <- raster::raster(saclat_plot_zoom)
saclat_plot_zoom[saclat_plot_zoom < 0] <- NA

#ext(saclat_plot_zoom_zoom)
vals <- raster::values(saclat_plot_zoom)
rng <- range(vals, na.rm = TRUE)

tmap_mode(mode = "view")
tm_shape(saclat_plot_zoom) +
  tm_raster(style = "cont", palette = "Greens") +
  tm_layout(title = "BRT model tangle kelp",
            legend.outside = TRUE)

gradpal2 <- colorNumeric(palette = "Spectral", domain = rng, na.color = "transparent", reverse = TRUE)

predmap2 <- leaflet(saclat_plot_zoom,
                    options = leafletOptions(
                      attributionControl=FALSE,
                      zoomControl = FALSE)) %>% # removed zoom control for better export to small image
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>% #CartoDB.DarkMatter #Esri.WorldGrayCanvas
  addRasterImage(saclat_plot_zoom, colors = gradpal2) %>% 
  addLegend("bottomright", pal = gradpal2, values = vals,
            title = "Predicted density </br> of sugar kelp </br> (ind.m<sup>-2</sup>)") %>% 
  addControl(html = "<b style='font-size:20px;'>B</b>", position = "topleft") %>%
  setView(lng = lng_zoom, lat = lat_zoom, zoom = 11.5)

predmap2

out_html <- file.path(paste(maindir, "/Figures/SaclatBRTMapZoom_v3.html", sep = "/"))
out_png  <- file.path(paste(maindir, "/Figures/SaclatBRTMapZoom_v3.png", sep = "/"))

htmlwidgets::saveWidget(predmap2, out_html, selfcontained = TRUE)
webshot2::webshot(out_html, file = out_png, vwidth = 700, vheight = 744, delay = 3)


# Area summaries ----------------------------------------------------------
# Estimate area depending on threshold
# lamhyp_rastertable <- freq(predfinal)
# 
# lamhyp_rastertable <- lamhyp_rastertable %>%  
#   filter(value > 0) %>% 
#   dplyr::select(!layer) %>% 
#   mutate(area_m = count*25*25,
#          area_km = area_m/1e+06) %>% 
#   mutate(area_km_sum = rev(cumsum(rev(area_km))))
# 
# write.csv(lamhyp_rastertable, file = "../Tables/lamhyp_rastertable_full.csv", row.names = FALSE)
# 
# saclat_rastertable <- freq(predfinal_sac)
# 
# saclat_rastertable <- saclat_rastertable %>%  
#   filter(!is.na(value)) %>% 
#   filter(value > 0) %>% 
#   mutate(area_m = count*25*25,
#          area_km = area_m/1e+06) %>% 
#   mutate(area_km_sum = rev(cumsum(rev(area_km))))
# 
# write.csv(saclat_rastertable, file = "../Tables/saclat_rastertable_full.csv", row.names = FALSE)

lamhyp_rastertable <- read.csv("../Tables/lamhyp_rastertable_full.csv")  
saclat_rastertable <- read.csv("../Tables/saclat_rastertable_full.csv")  

area_lim_lam_5 <- lamhyp_rastertable %>%  
  filter(value == 5) %>%
  pull(area_km_sum)

area_lim_lam_1 <- lamhyp_rastertable %>%  
  filter(value == 1) %>% pull(area_km_sum)

areaplot_lam <- lamhyp_rastertable %>% 
  ggplot(aes(x = value, y = area_km_sum)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue") +
  geom_vline(xintercept = 5, color = "grey40", linewidth = 0.9, linetype = "dashed") +
  geom_hline(yintercept = area_lim_lam_5, color = "grey40", linewidth = 0.9, linetype = "dashed") +
  geom_hline(yintercept = area_lim_lam_1, color = "grey40", linewidth = 0.9, linetype = "dashed") +
  annotate("text",
           x = max(lamhyp_rastertable$value) * 0.98,   
           y = area_lim_lam_5,
           label = paste0("Area = ", round(area_lim_lam_5), " km²"),
           vjust = -0.5, hjust = 1, size = 4) +
  annotate("text",
           x = max(lamhyp_rastertable$value) * 0.98,
           y = area_lim_lam_1,
           label = paste0("Area = ", round(area_lim_lam_1), " km²"),
           vjust = -0.5, hjust = 1, size = 4) +
  labs(x = "Density threshold (ind. m⁻²)", y = "Predicted extent of tangle kelp (km²)", tag = "A") +
  theme_bw(base_size = 14) + 
  theme(panel.grid.minor = element_line(color = "grey90"),
        plot.tag = element_text(size = 18, face = "bold"),
        plot.tag.position = c(0.02, 0.98))

area_lim_sac_7 <- saclat_rastertable %>%  
  filter(value == 7) %>%
  pull(area_km_sum)

area_lim_sac_1 <- saclat_rastertable %>%  
  filter(value == 1) %>%
  pull(area_km_sum)

areaplot_sac <- saclat_rastertable %>%
  ggplot(aes(x = value, y = area_km_sum)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue") +
  geom_vline(xintercept = 7, color = "grey40", linewidth = 0.9, linetype = "dashed") +
  geom_hline(yintercept = area_lim_sac_1, color = "grey40", linewidth = 0.9, linetype = "dashed") +
  geom_hline(yintercept = area_lim_sac_7, color = "grey40", linewidth = 0.9, linetype = "dashed") +
  annotate("text",
           x = max(saclat_rastertable$value) * 0.98,   
           y = area_lim_sac_7,
           label = paste0("Area = ", round(area_lim_sac_7), " km²"),
           vjust = -0.5, hjust = 1, size = 4) +
  annotate("text",
           x = max(saclat_rastertable$value) * 0.98,
           y = area_lim_sac_1,
           label = paste0("Area = ", round(area_lim_sac_1), " km²"),
           vjust = -0.5, hjust = 1, size = 4) +
  labs(x = "Density threshold (ind. m⁻²)", y = "Predicted extent of sugar kelp (km²)", tag = "B") +
  theme_bw(base_size = 14) + 
  theme(panel.grid.minor = element_line(color = "grey90"),
        plot.tag = element_text(size = 18, face = "bold"),
        plot.tag.position = c(0.02, 0.98))

areaplot <- areaplot_lam + areaplot_sac

ggsave(areaplot, path = "../Figures",
       filename = "arealestimates.png",
       device = "png", width = 30, height = 15, units = "cm")


# Histograms as in the Blue Carbon report for comparison:

lamhyp_rastertable %>% 
  ggplot(aes(x = value, y = area_km)) +
  geom_col(fill = "steelblue") +
  labs(x = "Density (ind. m⁻²)", y = "Predicted extent of tangle kelp (km²)", tag = "A") +
  scale_y_continuous(breaks = seq(0, 12000, by = 1000)) +
  theme_bw(base_size = 14) + 
  theme(panel.grid.minor = element_line(color = "grey90"),
        plot.tag = element_text(size = 18, face = "bold"),
        plot.tag.position = c(0.02, 0.98))

saclat_rastertable %>% 
  ggplot(aes(x = value, y = area_km)) +
  geom_col(fill = "steelblue") +
  labs(x = "Density (ind. m⁻²)", y = "Predicted extent of sugar kelp (km²)", tag = "A") +
  scale_y_continuous(breaks = seq(0, 12000, by = 1000)) +
  ylim(c(0,500)) +
  theme_bw(base_size = 14) + 
  theme(panel.grid.minor = element_line(color = "grey90"),
        plot.tag = element_text(size = 18, face = "bold"),
        plot.tag.position = c(0.02, 0.98))

# Biomass estimates ----------------------------------------------------------


# Add biomass for lamhyp

zonalLAMHYdens_kg <- read.csv("../Tables/zonalLAMHYdens_kg.csv") %>% 
  filter(Category != 0)  %>% 
  mutate(densities = ifelse(Category == 1, "1 - 4", ">= 5")) %>% 
  mutate(Biomass_milltonn = Biomass_kg/1000000000)
str(zonalLAMHYdens_kg)

lamhyp_rastertable %>% data.frame %>% 
  mutate(forest = ifelse(value >= 5, 1L, 0L)) %>%
  group_by(forest) %>% 
  summarise(N = sum(count), Area_km = sum(area_km)) %>% 
  add_column(densities = 0L, .after = "forest") %>%
  mutate(densities = ifelse(forest == 1, ">= 5", "1 - 4")) %>%
  dplyr::select(-forest) %>% data.frame() %>% 
  left_join(zonalLAMHYdens_kg) %>% 
  dplyr::select(-c(Biomass_kg)) %>% 
  write.csv(., file = "../Tables/lamhyp_rastertable_2026.csv", row.names = FALSE)

saclat_rastertable %>% 
  mutate(forest = ifelse(value >= 7, 1L, 0L)) %>%
  group_by(forest) %>% 
  summarise(Area_km = sum(area_km)) %>% 
  add_column(densities = 0L, .after = "forest") %>%
  mutate(densities = ifelse(forest == 1, ">= 7", "1 - 6")) %>%
  dplyr::select(-forest) %>%  
  print %>% 
  write.csv(., file = "../Tables/saclat_rastertable_2026.csv", row.names = FALSE)

# Sjekk Frigstad et al ...
# Extreme differences in the predictions of S. latissima forest areas
# I do trust this model more than the previous (range of values etc. seems much more realistic)
# Har dobbeltsjekka med å regne ut i excel og kommer til samme arealer


