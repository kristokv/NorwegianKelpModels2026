## Paper figures
require(tidyverse)
require(dismo)
#require(Hmisc)
require(gbm)
require(tmap)
require(sf)
require(sp)
require(scales)
#require(leaflet)
#require(mapview)
#require(tmap)
#require(webshot)
#require(webshot2)
require(RColorBrewer)
require(rnaturalearth)
require(rnaturalearthdata)
require(rnaturalearthhires)
require(osmdata)
require(readxl)

require(terra)
require(patchwork)
#require(writexl)


maindir <- "D:\\Taremodeller\\NorwegianKelpModels_bigfiles"

# Basemap for plotting
land <- ne_countries(scale = "large", returnclass = "sf")

labels_df <- data.frame(
  name = c("North Sea", "Norwegian Sea", "Barents Sea",
           "NORWAY", "SWEDEN", "FINLAND"),
  lon = c(5.2, 7, 30,
          9, 16, 26.5),
  lat = c(57.85, 66, 71.8,
          62, 63, 64))

basemap <- ggplot() +
  geom_sf(data = land,
          fill = "grey90",
          color = "grey70",
          size = 0.2) +
  geom_text(data = labels_df,
            aes(x = lon, y = lat, label = name),
            size = 5,
            fontface = "bold",
            color = "grey40") +
  theme_minimal(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.85, 0.2),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.background = element_rect(fill = "white", color = "grey50"))

# Find total available area (<40 m depth)
dem <- rast("D:/QGISprojects/EelgrassBlueConnect/input_GIS/BathyAndTerrain/DEM25Norge.tif")

dem_bin <- dem >= -40
n_cells <- global(dem_bin, "sum", na.rm=TRUE)[1,1]
cell_area <- prod(res(dem))  # 25 * 25 = 625 m²
area_m2 <- n_cells * cell_area
area_km2 <- area_m2 / 1e6


# Models
lamhyp.manual.cut.gbm <- readRDS("../Models/lamhyp.manual.cut.gbm.rds")
lamhyp.manual.cut.gbm$var.names

saclat.simp.manual.gbm <- readRDS("../Models/saclat.simp.manual.gbm_v3.rds")
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

saclat <- read.csv("../Data/saclat2022.csv") %>% 
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
pal <- brewer.pal(9, "YlOrRd")

lam_sf <- st_as_sf(lamdens, coords = c("X", "Y"), crs = 4326)

map1 <- basemap +
  geom_sf(data = lam_sf,
          aes(color = Tetthet),
          size = 1,
          alpha = 0.5) +
  scale_color_gradientn(colours = pal,
                        limits = c(1, 15),   
                        oob = scales::squish, 
                        name = "Tangle kelp density\n(ind. m⁻²)",
                        guide = guide_colorbar(reverse = FALSE) ) +
  labs(tag = "A") +
  coord_sf(xlim = c(3, 34),
           ylim = c(57.6, 72),
           expand = FALSE)

map1

ggsave("../Figures/LamhypSampleMap_v4.png", map1, width = 180, height = 200, dpi = 300, units = "mm") 

sac_sf <- st_as_sf(sacdens, coords = c("X", "Y"), crs = 4326)

map2 <- basemap +
  geom_sf(data = sac_sf,
          aes(color = Tetthet),
          size = 1,
          alpha = 0.5) +
  scale_color_gradientn(colours = pal,
                        limits = c(1, 15),   
                        oob = scales::squish, 
                        name = "Sugar kelp density\n(ind. m⁻²)",
                        guide = guide_colorbar(reverse = FALSE) ) +
  labs(tag = "B") +
  coord_sf(xlim = c(3, 34),
           ylim = c(57.6, 72),
           expand = FALSE)

map2

ggsave("../Figures/SaclatSampleMap_v4.png", map2, width = 180, height = 200, dpi = 300, units = "mm") 


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

# lamhyp_pres <- lamhyp %>%
#   filter(Tetthet >0)
# 
# saclat_pres <- saclat %>%
#   filter(Tetthet >0)
# 
# xranges <- tibble(variable = names(varnames_match),) %>%
#   mutate(xmin = map_dbl(variable, ~ min(lamhyp_pres[[.x]],
#                                         saclat_pres[[.x]], na.rm = TRUE
#     )),
#     xmax = map_dbl(variable, ~ max(
#       lamhyp_pres[[.x]], saclat_pres[[.x]], na.rm = TRUE)))
# 

# gbm.plot(lamhyp.manual.cut.gbm, write.title=TRUE, show.contrib=TRUE,
#          common.scale = TRUE,
#          smooth = TRUE,
#          plot.layout = c(2,5),
#          lty = "dotted") 

varnames_lamhyp <- unname(varnames_match[lamhyp.manual.cut.gbm$contributions$var])

png("../Figures/MarginalEffectsLamhyp.png", width = 2700, height = 1500, res = 300)
par(mar=c(2.5,1.5,1.5,0), oma = c(1.5,2,0,1), mfrow = c(2,6))
gbm.plot.own(lamhyp.manual.cut.gbm, dat = lamhyp, varnames = varnames_lamhyp,
             xrange_pres = TRUE, with_yax=c(1,7),species="tangle kelp", tag = "A")
dev.off()

varnames_saclat <-  unname(varnames_match[saclat.simp.manual.gbm$contributions$var])

png("../Figures/MarginalEffectsSaclat.png", width = 2700, height = 1500, res = 300)
par(mar=c(2.5,1.5,1.5,0), oma = c(1.5,2,0,1), mfrow = c(2,6))
gbm.plot.own(saclat.simp.manual.gbm, dat = saclat, varnames = varnames_saclat, tag = "B",
             xrange_pres = TRUE, with_yax=c(1,7),species="sugar kelp")
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
predfinal_full <- rast(paste0(maindir,"\\Predictions\\predLAMHYdens2022_full.grd")) 
predfinal <- rast(paste0(maindir,"\\Predictions\\predLAMHYdens2022_crop40.grd")) 

#hist(predfinal[predfinal > 0])

# lamhyp_downsample <- terra::aggregate(predfinal,
#                                fact = 20,
#                                fun = mean,
#                                na.rm = TRUE)
# writeRaster(lamhyp_downsample, "../Predictions/predLAMHYdens2022_downsample500.grd", overwrite = TRUE)


# Saclat downsample:
predfinal_sac <- rast(paste0(maindir,"\\Predictions\\predSACLATdens2022_crop40.grd"))
#predfinal_sac <- rast("../Predictions/SACpred_Norway.tif")

# Model now truncated at 40 m depth

# saclat_downsample <- terra::aggregate(predfinal_sac,
#                                fact = 20,
#                                fun = mean,
#                                na.rm = TRUE)
# writeRaster(saclat_downsample, "../Predictions/predSACLATdens2022_downsample500.grd")

## PLOT Lamhyp

lamhyp_downsample <- rast(paste0(maindir,"\\Predictions/predLAMHYdens2022_downsample500.grd"))

#plot(lamhyp_downsample)
lamhyp_plot <- project(lamhyp_downsample, "EPSG:4326", method = "bilinear")
#lamhyp_plot[lamhyp_plot < 1] <- NA
lamhyp_plot[lamhyp_plot < 1] <- 0

# Convert to dataframe for ggplot
lam_df <- as.data.frame(lamhyp_plot, xy = TRUE, na.rm = TRUE)
colnames(lam_df)[3] <- "Tetthet"

pal <- RColorBrewer::brewer.pal(9, "YlOrRd")
lamhyp_lims <- c(0, 12)
  
map1 <- basemap +
  geom_tile(data = lam_df,
            aes(x = x, y = y, fill = Tetthet), 
            width = 0.01, height = 0.01, alpha = 0.9) +
  # scale_fill_distiller(palette = "YlOrRd",
  #                      direction = 1,
  #                      limits = c(1, 12),   
  #                      oob = scales::squish, 
  #                      name = "Predicted density\nof tangle kelp\n(ind. m⁻²)") +
  scale_fill_gradientn(
    colours = c("white", pal[3:9]),  
    limits = lamhyp_lims,
    oob = scales::squish,
    name = "Predicted density\nof tangle kelp\n(ind. m⁻²)") +
    labs(tag = "A") +
  coord_sf(xlim = c(3, 34),
           ylim = c(57.6, 72),
           expand = FALSE) +
  theme(legend.position = c(0.1, 0.82))

map1

## PLOT Saclat
saclat_downsample <-  rast(paste0(maindir,"\\Predictions/predSACLATdens2022_downsample500.grd"))

saclat_plot <- project(saclat_downsample, "EPSG:4326", method = "bilinear")
saclat_plot[saclat_plot < 1] <- 0

sac_df <- as.data.frame(saclat_plot, xy = TRUE, na.rm = TRUE)
colnames(sac_df)[3] <- "Tetthet"

saclat_lims <- c(0, 7)
  
map2 <- basemap +
  geom_tile(data = sac_df,
            aes(x = x, y = y, fill = Tetthet), 
            width = 0.01, height = 0.01, alpha = 0.9) +
  # scale_fill_distiller(palette = "YlOrRd",
  #                      direction = 1,
  #                      limits = c(1, 6),   
  #                      oob = scales::squish, 
  #                      name = "Predicted density\nof sugar kelp\n(ind. m⁻²)") +
  scale_fill_gradientn(
    colours = c("white", pal[3:9]),  
    limits = saclat_lims,
    oob = scales::squish,
    name = "Predicted density\nof sugar kelp\n(ind. m⁻²)") +
    labs(tag = "B") +
  coord_sf(xlim = c(3, 34),
           ylim = c(57.6, 72),
           expand = FALSE) +
  theme(legend.position = c(0.1, 0.82))

map2


# Zoomed in areas ----------------------------------------------------------
bbox <- c(5.35, 5.95, 62.25, 62.43) # Runde area # xmin, ymin, xmax, ymax
bbox <- c(4.7, 5.3, 61.65, 62.2) # Sognefjorden area?
#bbox <- ext(12, 12.20, 65.8, 65.93) # Vega area
ext_ll <- ext(bbox) 
ext_poly  <- as.polygons(ext_ll, crs = "EPSG:4326")
ext_poly_utm <- project(ext_poly, crs(predfinal))     
ext_new   <- ext(ext_poly_utm)

coast_osm <- opq(bbox = c(bbox[c(2,4)],bbox[c(2,4)]), timeout = 200) %>%
  add_osm_feature(key = "natural", value = "coastline") %>%
  osmdata_sf()

coast <- coast_osm$osm_lines

# Crop
lamhyp_plot_zoom <- crop(predfinal_full, ext_new)

# Fill in empty areas with 0s
dem <- rast("D:/QGISprojects/EelgrassBlueConnect/input_GIS/BathyAndTerrain/DEM25Norge.tif")
dem_zoom <- crop(dem, ext_new)
dem_aligned <- resample(dem_zoom, lamhyp_plot_zoom, method = "near")

lamhyp_plot_zoom[is.na(lamhyp_plot_zoom) & !is.na(dem_aligned) & dem_aligned <= (-40)] <- 0

lamhyp_plot_zoom <- project(lamhyp_plot_zoom, "EPSG:4326", method = "bilinear")

#lamhyp_plot_zoom[lamhyp_plot_zoom < 1] <- NA
lam_zoom_df <- as.data.frame(lamhyp_plot_zoom, xy = TRUE, na.rm = TRUE)
colnames(lam_zoom_df)[3] <- "Tetthet"
lam_zoom_df$Tetthet[lam_zoom_df$Tetthet < 1] <- 0


map1_zoom <- ggplot() +
  theme_void() +  
  geom_tile(data = lam_zoom_df,
            aes(x = x, y = y, fill = Tetthet),
            alpha = 0.85) +
  # geom_sf(data = coast, color = "white", linewidth = 0.6) + 
  # geom_sf(data = coast, color = "black", linewidth = 1.0) +   
  scale_fill_gradientn(
    colours = c("white", pal[3:9]),
    limits = lamhyp_lims,
    oob = scales::squish) +
  coord_sf(xlim = bbox[c(1,2)],
           ylim = bbox[c(3,4)],
           expand = FALSE) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "grey", color = NA),  
        plot.background  = element_rect(fill = "grey", color = NA),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8))

map1_zoom

connect_line <- data.frame(x = c(5.5, 18),  y = c(61.9, 61))

map_combined <- map1 +
  annotate("rect",
    xmin = bbox[1],xmax = bbox[2], ymin = bbox[3], ymax = bbox[4], fill = NA,
    color = "black",
    linewidth = 1.2) +
  geom_path(data = connect_line,
            aes(x = x, y = y),
            linetype = "dashed",
            color = "grey40") +
 inset_element(map1_zoom,
                left = 0.31,
                bottom = 0.01,
                right = 0.98,
                top = 0.52)
  
#map_combined

ggsave("../Figures/LamhypBRTMap_TEST.png", map_combined, width = 180, height = 200, dpi = 300, units = "mm") 

saclat_plot_zoom <- crop(predfinal_sac, ext_new)
#dem_zoom <- crop(dem, ext_new)
#dem_aligned <- resample(dem_zoom, saclat_plot_zoom, method = "near")

saclat_plot_zoom[is.na(saclat_plot_zoom) & !is.na(dem_aligned) & dem_aligned <= (-40)] <- 0

#writeRaster(saclat_plot_zoom,  filename = paste0(maindir, "\\Predictions\\saclat_plot_zoom.grd"), overwrite = TRUE)

saclat_plot_zoom <- project(saclat_plot_zoom, "EPSG:4326", method = "bilinear")
sac_zoom_df <- as.data.frame(saclat_plot_zoom, xy = TRUE, na.rm = TRUE)
colnames(sac_zoom_df)[3] <- "Tetthet"
sac_zoom_df$Tetthet[sac_zoom_df$Tetthet < 1] <- 0

map2_zoom <- ggplot() +
  theme_void() +  
  geom_tile(data = sac_zoom_df,
            aes(x = x, y = y, fill = Tetthet),
            alpha = 0.85) +
  # geom_sf(data = coast, color = "white", linewidth = 0.6) + 
  # geom_sf(data = coast, color = "black", linewidth = 1.0) +   
  scale_fill_gradientn(
    colours = c("white", pal[3:9]),
    limits = saclat_lims,
    oob = scales::squish) +
  coord_sf(xlim = bbox[c(1,2)],
           ylim = bbox[c(3,4)],
           expand = FALSE) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "grey", color = NA),  
        plot.background  = element_rect(fill = "grey", color = NA),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8))

#map2_zoom

map_combined2 <- map2 +
  annotate("rect",
           xmin = bbox[1],xmax = bbox[2], ymin = bbox[3], ymax = bbox[4], fill = NA,
           color = "black",
           linewidth = 1.2) +
  geom_path(data = connect_line,
            aes(x = x, y = y),
            linetype = "dashed",
            color = "grey40") +
  inset_element(map2_zoom,
                left = 0.31,
                bottom = 0.01,
                right = 0.98,
                top = 0.52)
#map_combined2

ggsave("../Figures/SaclatBRTMap_v4.png", map_combined2, width = 180, height = 200, dpi = 300, units = "mm") 



# Area summaries ----------------------------------------------------------
# Estimate area depending on threshold
#predfinal_full <- rast(paste0(maindir,"\\Predictions\\predLAMHYdens2022_full.grd")) 
predfinal <- rast(paste0(maindir,"\\Predictions\\predLAMHYdens2022_crop40.grd")) 
predfinal_sac <- rast(paste0(maindir,"\\Predictions\\predSACLATdens2022_crop40.grd"))

# Old method, faster but somewhat less precise:
# lamhyp_rastertable_crop <- freq(predfinal_crop)
# lamhyp_rastertable_old <- lamhyp_rastertable_crop %>%
#   filter(value > 0) %>%
#   dplyr::select(!layer) %>%
#   mutate(area_m = count*25*25,
#          area_km = area_m/1e+06) %>%
#   mutate(area_km_sum = rev(cumsum(rev(area_km))))

cell_area_m2 <- prod(res(predfinal))  # m² per cell

# Laminaria:
r_max <- global(predfinal, "max", na.rm = TRUE)[1, 1]
thresholds <- seq(from = 1, to   = floor(r_max),  by   = 1)

lamhyp_rastertable <- tibble(value = thresholds) %>%
  mutate(count_eq = vapply(
      value,
      function(v) {
        global(
          predfinal >= v & predfinal < (v + 1),
          "sum",
          na.rm = TRUE
        )[1, 1]
      },
      numeric(1)
    ),
    count_ge = vapply(
      value,
      function(v) {
        global(predfinal >= v, "sum", na.rm = TRUE)[1, 1]
      },
      numeric(1)
    ),
    area_km = count_eq * cell_area_m2 / 1e6,
    area_km_cum = count_ge * cell_area_m2 / 1e6
  )


write.csv(lamhyp_rastertable, file = "../Tables/lamhyp_rastertable_full.csv", row.names = FALSE)


# Saccharina:
r_max <- global(predfinal_sac, "max", na.rm = TRUE)[1, 1]
thresholds <- seq(from = 1, to   = floor(r_max),  by   = 1)

saclat_rastertable <- tibble(value = thresholds) %>%
  mutate(count_eq = vapply(
    value,
    function(v) {
      global(
        predfinal_sac >= v & predfinal_sac < (v + 1),
        "sum",
        na.rm = TRUE
      )[1, 1]
    },
    numeric(1)
  ),
  count_ge = vapply(
    value,
    function(v) {
      global(predfinal_sac >= v, "sum", na.rm = TRUE)[1, 1]
    },
    numeric(1)
  ),
  area_km = count_eq * cell_area_m2 / 1e6,
  area_km_cum = count_ge * cell_area_m2 / 1e6
  )

write.csv(saclat_rastertable, file = "../Tables/saclat_rastertable_full.csv", row.names = FALSE)


comb_rastertable_ms <- lamhyp_rastertable %>%
  dplyr::select(c(value, area_km, area_km_cum)) %>%
  left_join(saclat_rastertable  %>%
              dplyr::select(c(value, area_km, area_km_cum)), by = "value") %>%
  mutate(across(where(is.numeric), round, 3)) %>%
  rename(Threshold = value,
         "Area at value" = area_km.x,
         "Area ≥ value" = area_km_cum.x,
         "Area at value.y" = area_km.y,
         "Area ≥ value.y" = area_km_cum.y)

write.csv(comb_rastertable_ms, file = "../Tables/comb_rastertable_ms.csv", row.names = FALSE)

lamhyp_rastertable <- read.csv("../Tables/lamhyp_rastertable_full.csv")
  
saclat_rastertable <- read.csv("../Tables/saclat_rastertable_full.csv")  

area_lam_1_to_4 <- lamhyp_rastertable %>%
  filter(value < 5) %>%
  mutate(area_km_sum = sum(area_km)) %>%
  pull(area_km_sum)

area_lim_lam_5 <- lamhyp_rastertable %>%
  filter(value == 5) %>%
  pull(area_km_cum)

area_lim_lam_1 <- lamhyp_rastertable %>%
  filter(value == 1) %>%
  pull(area_km_cum)

lamhyp_rastertable_plot <- lamhyp_rastertable %>% 
  filter(value < 14)

areaplot_lam <- lamhyp_rastertable_plot %>%
  ggplot(aes(x = value)) +
  geom_line(aes(y = area_km_cum), color = "steelblue", linewidth = 1) +
  geom_point(aes(y = area_km_cum), color = "steelblue") +
  geom_hline(yintercept = area_lim_lam_5, color = "grey20", linewidth = 0.9, linetype = "dashed") +
  geom_hline(yintercept = area_lim_lam_1, color = "grey80", linewidth = 0.9, linetype = "dashed") +
  geom_col(aes(x = value, y = area_km), fill = "steelblue", alpha = 0.75, width = 0.5) +
  annotate("text",
           x = max(lamhyp_rastertable_plot$value) * 0.98,   
           y = area_lim_lam_5,
           label = paste0(round(area_lim_lam_5), " km²"),
           vjust = -0.5, hjust = 1, size = 4) +
  annotate("text",
           x = max(lamhyp_rastertable_plot$value) * 0.98,
           y = area_lim_lam_1,
           label = paste0(round(area_lim_lam_1), " km²"),
           vjust = -0.5, hjust = 1, size = 4) +
  # dual y-axis
  scale_y_continuous(name = "Predicted spatial extent (km²)",
                     sec.axis = sec_axis(trans = ~ 100 * (. - area_lim_lam_5) / area_lim_lam_5)) +
  labs(x = "Density threshold (≥ ind. m⁻²)", title = "Tangle kelp",
       tag = "A.") +
  theme_bw(base_size = 14) +
  theme(panel.grid.minor = element_line(color = "grey90"),
    plot.tag = element_text(size = 18, face = "bold"),
    plot.tag.position = c(0.02, 0.98))

areaplot_lam

area_sac_1_to_7 <- saclat_rastertable %>%
  filter(value < 7) %>%
  mutate(area_km_sum = sum(area_km)) %>%
  pull(area_km_sum)

area_lim_sac_7 <- saclat_rastertable %>%  
  filter(value == 7) %>%
  pull(area_km_cum)

area_lim_sac_1 <- saclat_rastertable %>%  
  filter(value == 1) %>%
  pull(area_km_cum)

saclat_rastertable_plot <- saclat_rastertable %>% 
  filter(value < 14)

areaplot_sac <- saclat_rastertable_plot %>%
  ggplot(aes(x = value)) +
  geom_line(aes(y = area_km_cum), color = "steelblue", linewidth = 1) +
  geom_point(aes(y = area_km_cum), color = "steelblue") +
  geom_hline(yintercept = area_lim_sac_1, color = "grey80", linewidth = 0.9, linetype = "dashed") +
  geom_hline(yintercept = area_lim_sac_7, color = "grey20", linewidth = 0.9, linetype = "dashed") +
  geom_col(aes(x = value, y = area_km), fill = "steelblue", alpha = 0.75, width = 0.5) +
  annotate("text",
           x = max(saclat_rastertable_plot$value) * 0.98,   
           y = area_lim_sac_7,
           label = paste0(round(area_lim_sac_7), " km²"),
           vjust = -0.5, hjust = 1, size = 4) +
  annotate("text",
           x = max(saclat_rastertable_plot$value) * 0.98,
           y = area_lim_sac_1,
           label = paste0(round(area_lim_sac_1), " km²"),
           vjust = -0.5, hjust = 1, size = 4) +
  # dual y-axis
  scale_y_continuous(name = "",
                     sec.axis = sec_axis(trans = ~ 100 * (. - area_lim_sac_7) / area_lim_sac_7,
                                         name = "Change in predicted extent (%)")) +
  labs(x = "Density threshold (≥ ind. m⁻²)", tag = "B", title = "Sugar kelp") +
  theme_bw(base_size = 14) + 
  theme(panel.grid.minor = element_line(color = "grey90"),
        plot.tag = element_text(size = 18, face = "bold"),
        plot.tag.position = c(0.02, 0.98))

areaplot_sac

areaplot <- areaplot_lam + areaplot_sac

ggsave(areaplot, path = "../Figures",
       filename = "arealestimates.png",
       device = "png", width = 30, height = 15, units = "cm")


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

tot_kg_LAMHY <-rast(paste0(maindir, "\\Predictions\\tot_kg_LAMHY.grd"))

# Total weight - BUT INCLUDES AREAS WITH < IND/M2!
# sumLAMHY <- global(tot_kg_LAMHY, "sum", na.rm = TRUE)[1, 1]
# sumLAMHY # kg
# sumLAMHY / 1e6 # in Gg

# Sum biomass per density category (integer)
density_class <- floor(predfinal)
density_class[density_class < 1] <- NA
density_class[density_class > 35] <- 35

biom_eq <- zonal(tot_kg_LAMHY, density_class, fun = "sum", na.rm = TRUE)

biom_eq <- biom_eq %>%
  rename(Threshold = layer, Biomass_kg = layer.1)  %>%
  mutate(Biomass_Gg = Biomass_kg / 1e6) %>%
  arrange(desc(Threshold)) %>%
  mutate(Biomass_Gg_cum= cumsum(Biomass_Gg)) %>%
  arrange(Threshold) %>%
  mutate(C_Gg = Biomass_Gg*0.15*0.31, C_Gg_cum = Biomass_Gg_cum*0.15*0.31) 
  
# Join with areal estimates
lamhyp_appendixtable <- lamhyp_rastertable %>%    
  rename(Threshold = value)  %>%
  dplyr::select(-c(count_eq,count_ge)) %>%
  left_join(biom_eq) %>%
  dplyr::select(-c(Biomass_kg))%>%
  dplyr::select(Threshold,area_km, Biomass_Gg, C_Gg, area_km_cum, Biomass_Gg_cum, C_Gg_cum)

# Splitting the estimate into forest and non-forest
zonalLAMHYdens <- lamhyp_appendixtable %>%
  dplyr::select(Threshold, area_km, Biomass_Gg,C_Gg) %>%
  mutate(Density = case_when(
    Threshold %in% 1:4 ~ "Low density (1–4)",
    Threshold >= 5     ~ "High density (≥5)")) %>%
  group_by(Density) %>%
  summarise(Area_km = sum(area_km, na.rm = TRUE),
    Biomass_Gg = sum(Biomass_Gg, na.rm = TRUE),
    C_Gg = sum(C_Gg, na.rm = TRUE),.groups = "drop") %>%
  bind_rows(summarise(., across(where(is.numeric), sum), Density = "Total (≥1)") ) 

lamhyp_appendixtable <- lamhyp_appendixtable %>%    
  mutate(across(-Threshold, ~ ifelse(. < 1, number(., accuracy = 0.001, trim = TRUE), number(., accuracy = 1, trim = TRUE)))) 

write.csv(lamhyp_appendixtable, file = "../Tables/lamhyp_appendixtable.csv", row.names = FALSE)

zonalLAMHYdens <- zonalLAMHYdens %>%    
  mutate(across(-Density, ~ ifelse(. < 1, number(., accuracy = 0.001, trim = TRUE), number(., accuracy = 1, trim = TRUE)))) 

write.csv(zonalLAMHYdens, file = "../Tables/zonalLAMHYdens_Gg_2026.csv", row.names = FALSE)

tot_kg_SACLA <-rast(paste0(maindir, "\\Predictions\\tot_kg_SACLA.grd"))

# Sum biomass per density category (integer)
density_class <- floor(predfinal_sac)
density_class[density_class < 1] <- NA
density_class[density_class > 21] <- 21

biom_eq <- zonal(tot_kg_SACLA, density_class, fun = "sum", na.rm = TRUE)

biom_eq <- biom_eq %>%
  rename(Threshold = layer, Biomass_kg = layer.1)  %>%
  mutate(Biomass_Gg = Biomass_kg / 1e6) %>%
  arrange(desc(Threshold)) %>%
  mutate(Biomass_Gg_cum= cumsum(Biomass_Gg)) %>%
  arrange(Threshold) %>%
  mutate(C_Gg = Biomass_Gg*0.15*0.31, C_Gg_cum = Biomass_Gg_cum*0.15*0.31) 

# Join with areal estimates
saclat_appendixtable <- saclat_rastertable %>%    
  rename(Threshold = value)  %>%
  dplyr::select(-c(count_eq,count_ge)) %>%
  left_join(biom_eq) %>%
  dplyr::select(-c(Biomass_kg))%>%
  dplyr::select(Threshold,area_km, Biomass_Gg, C_Gg, area_km_cum, Biomass_Gg_cum, C_Gg_cum)

# Splitting the estimate into forest and non-forest
zonalSACLAdens <- saclat_appendixtable %>%
  dplyr::select(Threshold, area_km, Biomass_Gg,C_Gg) %>%
  mutate(Density = case_when(
    Threshold %in% 1:6 ~ "Low density (1–6)",
    Threshold >= 7     ~ "High density (≥7)")) %>%
  group_by(Density) %>%
  summarise(Area_km = sum(area_km, na.rm = TRUE),
            Biomass_Gg = sum(Biomass_Gg, na.rm = TRUE),
            C_Gg = sum(C_Gg, na.rm = TRUE),.groups = "drop") %>%
  bind_rows(summarise(., across(where(is.numeric), sum), Density = "Total (≥1)") ) 

saclat_appendixtable <- saclat_appendixtable %>%    
  mutate(across(-Threshold, ~ ifelse(. < 1, number(., accuracy = 0.001, trim = TRUE), number(., accuracy = 1, trim = TRUE)))) 

write.csv(saclat_appendixtable, file = "../Tables/saclat_appendixtable.csv", row.names = FALSE)

zonalSACLAdens <- zonalSACLAdens %>%    
  mutate(across(-Density, ~ ifelse(. < 1, number(., accuracy = 0.001, trim = TRUE), number(., accuracy = 1, trim = TRUE)))) 

write.csv(zonalSACLAdens, file = "../Tables/zonalSACLAdenss_Gg_2026.csv", row.names = FALSE)


# Carbon / production estimates ----------------------------------------------------------
# NPP: 309 g  C m-2 y-1 (147–581) 
# # Gg C y-1:
# (area_lim_lam_5 * 1e6 * 309)/ 1e+9
# #Tg C y-1
# (area_lim_lam_5 * 1e6 * 147) / 1e12
# (area_lim_lam_5 * 1e6 * 309) / 1e12
# (area_lim_lam_5 * 1e6 * 581) / 1e12

# CC: 87 (19-81) g C m-2 y-1
# Gg C y-1:
(area_lim_lam_5 * 1e6 * 19)/ 1e9 # Lower
(area_lim_lam_5 * 1e6 * 68)/ 1e9 # Mean
(area_lim_lam_5 * 1e6 * 81)/ 1e9




