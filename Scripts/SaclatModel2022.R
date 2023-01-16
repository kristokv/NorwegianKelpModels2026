#####################################################
## The Norwegian Kelp Models # take two
## Modeling sugar kelp
## Guri Sogn Andersen, 2022
#####################################################

# This work is largely based on the models made in the Nordic Blue Carbon project (2020): http://pub.norden.org/temanord2020-541/#47359
# Here we build new models incorporating additional variables (e.g. Urchindata)

# Packages ----------------------------------------------------------------

require(tidyverse)
require(patchwork)
require(gbm) # Generalized Boosted Regression Models
require(dismo) # Species Distribution Modeling
require(corrplot)

# Data --------------------------------------------------------------------
saclat <- read.csv("./Data/saclat2022.csv", stringsAsFactors = FALSE) %>% 
  filter(!is.na(Tetthet)) # Fjerner en NA
str(saclat)
names(saclat)
summary(saclat)

saclat %>% dplyr::select(bedpar:BO2_salinitymax_ss, Grazing:Maxtemp, Dissolved.oxygen.Mean:Salmean) %>% 
  cor(., use = "pairwise.complete.obs") %>% 
  corrplot.mixed(., 
                 order = "hclust",
                 tl.pos = "lt",
                 tl.cex = 0.7, tl.col = "black") 

### BRT modelling

# BRT models --------------------------------------------------------------
# Building on the work from the blue carbon project
# Compared GAM to BRT, and BRT performed much better 
# Tested BRT with both forest (presence/absence), and density (gaussian response and poisson response)
# The final model was poisson 

# Here, decided to test both 
# 1) log(Tetthet + 1) as response with gaussian distr AND
# 2) Tetthet*2 as response with poisson distr

# For reference, see: https://rspatial.org/raster/sdm/9_sdm_brt.html 

# explvar <- saclat %>% dplyr::select(Depth_mod, bedpar:curv500, slope:BO2_salinitymax_ss, 
#                                     Grazing:Maxtemp, 
#                                     Dissolved.oxygen.Mean:Salmean, Y) %>% names() 
explvar # same as for lamhyp

# Responses
saclat %>% ggplot(aes(x = Tetthet_kat, y = Tetthet)) +
  geom_jitter()

saclat %>% ggplot(aes(x = X, y = Y, col = Tetthet)) +
  geom_point()

responsvar_sac <- saclat %>% dplyr::select(X, Y, Tetthet) %>% 
  mutate(Dens2 = as.integer(Tetthet*2),
         Logdens = log(Tetthet+1))

summary(responsvar_sac)

# Models using density*2 as response (to get integers)
set.seed(123)
saclat.full.poisson.gbm <- gbm.step(data = cbind(saclat, responsvar_sac),   
                                    gbm.x = explvar,       
                                    gbm.y = "Dens2",                # Tetthet*2 as response
                                    learning.rate = 0.01,         # learning rate 
                                    tree.complexity = 5,          # tree splits
                                    family = "poisson",           # response is counts
                                    bag.fraction = 0.5)

par(mfrow = c(1,1), mar=c(4,10,2,2))
summary(saclat.full.poisson.gbm, las = 2) # importance of the different explvar
saclat.full.poisson.gbm$contributions

# Save BRT model to own object - just to be sure
saveRDS(saclat.full.poisson.gbm, file = "./Models/saclat.full.poisson.gbm_v3.rds")

# respons
gbm.plot(saclat.full.poisson.gbm, write.title=TRUE, show.contrib=TRUE, plot.layout = c(3,3)) 
gbm.plot.fits(saclat.full.poisson.gbm)

# prediksjoner
plot(saclat.full.poisson.gbm$data$y, saclat.full.poisson.gbm$fitted)
abline(a = 0, b = 1, col = "red")

cor(saclat.full.poisson.gbm$data$y, saclat.full.poisson.gbm$fitted) # pearson correlation

saclat.full.poisson.gbm$cv.statistics$correlation.mean
saclat.full.poisson.gbm$self.statistics$correlation # same as pearson correlation

# simplify - very slow, perhaps not necessary
set.seed(23)
saclat.poisson.simp <- gbm.simplify(saclat.full.poisson.gbm)
saveRDS(saclat.poisson.simp, file = "./Models/saclat.poisson.simp_v3.rds") 
# Suggest cutting 23 vars: 

saclat.poisson.simp

# Models using log(dens+1) as response 
set.seed(123)
saclat.full.gaus.gbm <- gbm.step(data = cbind(saclat, responsvar_sac),   
                                 gbm.x = explvar,       
                                 gbm.y = "Logdens",                # log(Tetthet+1) as response
                                 learning.rate = 0.01,         # learning rate 
                                 tree.complexity = 5,          # tree splits
                                 family = "gaussian",           # response is transformed
                                 bag.fraction = 0.5)

par(mfrow = c(1,1), mar=c(4,10,2,2))
summary(saclat.full.gaus.gbm, las = 2) # importance of the different explvar

saclat.full.gaus.gbm$contributions

# Save BRT model to own object - just to be sure
saveRDS(saclat.full.gaus.gbm, file = "./Models/saclat.full.gaus.gbm_v3.rds")

# respons
gbm.plot(saclat.full.gaus.gbm, write.title=TRUE, show.contrib=TRUE, plot.layout = c(3,3)) 
gbm.plot.fits(saclat.full.gaus.gbm)

# prediksjoner
par(mfrow = c(1,1))
plot(saclat.full.gaus.gbm$data$y, saclat.full.gaus.gbm$fitted)
abline(a = 0, b = 1, col = "red")

cor(saclat.full.gaus.gbm$data$y, saclat.full.gaus.gbm$fitted) # pearson correlation

saclat.full.gaus.gbm$cv.statistics$correlation.mean
saclat.full.gaus.gbm$self.statistics$correlation # same as pearson correlation

# The gaussian version seems better than the poisson based on cv-stats

# Trying to optimize parameters based on input from ERI on the Nordic paper:
source("./Scripts/trainBrt_AdamLillith.R")

opt.saclat.full.gaus <- trainBrt(data = cbind(saclat, responsvar_sac),  
                                    resp = "Logdens",             
                                    preds = explvar,
                                    family = "gaussian",
                                    learningRate = c(0.0001, 0.001, 0.01), #Elith et al. 2008 recommend 0.0001 to 0.1)
                                    treeComplexity = c(2, 4, 6),
                                    bagFraction=c(0.5, 0.6, 0.7), #Elith et al. 2008 recommend 0.5 to 0.7
                                    maxTrees = 10000,
                                    out=c('tuning', 'model'),
                                    verbose=TRUE)

# Best was complexity = 6, bagFraction = 0.7 and learning rate = 0.01
opt.saclat.full.gaus
saveRDS(opt.saclat.full.gaus, file = "./Models/optsac.gaus.rds")

# Define with new parameters? Check to see if very different. If very different - do the same with the poisson version
opt.saclat.full.gaus$model$gbm.call
opt.saclat.full.gaus$model$cv.statistics$correlation.mean
opt.saclat.full.gaus$model$self.statistics$correlation
# Differences, but no dramatic improvements on cv.statistics

opt.saclat.full.gaus$model$contributions
tot.dev <- opt.saclat.full.gaus$model$self.statistics$mean.null # mean total deviance
res.dev <- opt.saclat.full.gaus$model$self.statistics$mean.resid # mean residual deviance
dev.exp <- (tot.dev-res.dev)/tot.dev # deviance explained
dev.exp # 
rm(tot.dev, res.dev, dev.exp)

# simplify - very time consuming! 
set.seed(23)
saclat.gaus.simp <- gbm.simplify(opt.saclat.full.gaus$model)
saveRDS(saclat.gaus.simp, file = "./Models/saclat.gaus.simp.rds") 
# Suggest cutting 23 vars:

saclat.gaus.simp$pred.list
saclat.gaus.simp$pred.list$preds.23
saclat.gaus.simp$pred.list$preds.10

# A bit strange to include temprange perhaps?
saclat.simp.gaus.gbm <- gbm.step(data = cbind(saclat, responsvar_sac),   
                                   gbm.x = saclat.gaus.simp$pred.list$preds.23,       
                                   gbm.y = "Logdens",                # log(Tetthet+1) as response
                                   learning.rate = 0.01,         # learning rate 
                                   tree.complexity = 6,          # tree splits
                                   family = "gaussian",           # response is transformed
                                   bag.fraction = 0.7)
saveRDS(saclat.simp.gaus.gbm, file = "./Models/saclat.simp.gaus.gbm_v3.rds")

saclat.simp.gaus.gbm$cv.statistics$correlation.mean
saclat.simp.gaus.gbm$self.statistics$correlation # same as pearson correlation

tot.dev <- saclat.simp.gaus.gbm$self.statistics$mean.null # mean total deviance
res.dev <- saclat.simp.gaus.gbm$self.statistics$mean.resid # mean residual deviance
dev.exp <- (tot.dev-res.dev)/tot.dev # deviance explained
dev.exp
rm(tot.dev, res.dev, dev.exp)


# Do manual selection in accordance with lamhyp model and compare
explvar_selection

set.seed(123)
# Optimization showed that best results came with complexity = 6, bagFraction = 0.7 and learning rate = 0.01
saclat.manual.gaus.gbm <- gbm.step(data = cbind(saclat, responsvar_sac),   
                                 gbm.x = explvar_selection,       
                                 gbm.y = "Logdens",                # log(Tetthet+1) as response
                                 learning.rate = 0.01,         # learning rate 
                                 tree.complexity = 6,          # tree splits
                                 family = "gaussian",           # response is transformed
                                 bag.fraction = 0.7)

par(mfrow = c(1,1), mar=c(4,10,2,2))
summary(saclat.manual.gaus.gbm, las = 2) # importance of the different explvar

saclat.manual.gaus.gbm$contributions

# Save BRT model to own object - just to be sure
saveRDS(saclat.manual.gaus.gbm, file = "./Models/saclat.manual.gaus.gbm_v3.rds")

# respons
gbm.plot(saclat.manual.gaus.gbm, write.title=TRUE, show.contrib=TRUE, plot.layout = c(3,3)) 
gbm.plot.fits(saclat.manual.gaus.gbm)

# prediksjoner
par(mfrow = c(1,1))
plot(saclat.manual.gaus.gbm$data$y, saclat.manual.gaus.gbm$fitted)
abline(a = 0, b = 1, col = "red")

cor(saclat.manual.gaus.gbm$data$y, saclat.manual.gaus.gbm$fitted) # pearson correlation

saclat.manual.gaus.gbm$cv.statistics$correlation.mean
saclat.manual.gaus.gbm$self.statistics$correlation # same as pearson correlation

tot.dev <- saclat.manual.gaus.gbm$self.statistics$mean.null # mean total deviance
res.dev <- saclat.manual.gaus.gbm$self.statistics$mean.resid # mean residual deviance
dev.exp <- (tot.dev-res.dev)/tot.dev # deviance explained
dev.exp 
rm(tot.dev, res.dev, dev.exp)

# simplify
set.seed(23)
saclat.manual.gaus.simp <- gbm.simplify(saclat.manual.gaus.gbm)
saveRDS(saclat.manual.gaus.simp, file = "./Models/saclat.manual.gaus.simp.rds") 
# Suggest cutting X vars:

saclat.manual.gaus.simp$pred.list
saclat.manual.gaus.simp$pred.list$preds.2

saclat.simp.manual.gbm <- gbm.step(data = cbind(saclat, responsvar_sac),   
                                 gbm.x = saclat.manual.gaus.simp$pred.list$preds.2,       
                                 gbm.y = "Logdens",                # log(Tetthet+1) as response
                                 learning.rate = 0.01,         # learning rate 
                                 tree.complexity = 6,          # tree splits
                                 family = "gaussian",           # response is transformed
                                 bag.fraction = 0.7)
saveRDS(saclat.simp.manual.gbm, file = "./Models/saclat.simp.manual.gbm_v3.rds")

saclat.simp.manual.gbm$cv.statistics$correlation.mean
saclat.simp.manual.gbm$self.statistics$correlation # same as pearson correlation

tot.dev <- saclat.simp.manual.gbm$self.statistics$mean.null # mean total deviance
res.dev <- saclat.simp.manual.gbm$self.statistics$mean.resid # mean residual deviance
dev.exp <- (tot.dev-res.dev)/tot.dev # deviance explained
dev.exp
rm(tot.dev, res.dev, dev.exp)

# The optimised simplified model is PERHAPS the best. The difference is very slight ... 
# cv corr for manual simp is ~ 2 % lower, training data corr approx same
# deviance explained 69.1 % in manual simp vs. 69.5 % in opt simp ...
# simplified ...

# predictors
saclat.simp.manual.gbm$var.names
saclat.simp.gaus.gbm$var.names
# I like the manual version best since it is both more similar to the lamhyp-model, and because it is easier to use in future projections
# corresponding to different climate scenarios, etc ...

par(mfrow = c(1,1), mar=c(4,10,2,2))
summary(saclat.simp.manual.gbm, las = 2) # importance of the different explvar
saclat.simp.manual.gbm$contributions

# respons
gbm.plot(saclat.simp.manual.gbm, write.title=TRUE, show.contrib=TRUE, plot.layout = c(3,3)) 
gbm.plot.fits(saclat.simp.manual.gbm)

# prediksjoner
plot(saclat.simp.manual.gbm$data$y, saclat.simp.manual.gbm$fitted)
abline(a = 0, b = 1, col = "red")

finalmodel_sac <- saclat.simp.manual.gbm #"./Models/saclat.simp.manual.gbm_v3.rds"

# Future predictions ------------------------------------------------------

# use model with manual selection here:
# Built model with historic data, predicted with present from Bio-ORACLE
pred_hist_sac <-  saclat %>% left_join(responsvar_sac) %>% 
  predict(finalmodel_sac, newdata = .)

pred_now_sac <- saclat %>% left_join(responsvar_sac) %>% 
  mutate(Grazing = 0) %>% 
  dplyr::select(-Meantemp, Maxtemp) %>% 
  mutate(Meantemp = BO2_tempmean_bdmin,
         Maxtemp = BO2_tempmax_bdmin) %>% 
  predict(finalmodel_sac, newdata = .) # 

plot(pred_hist_sac, pred_now_sac)
abline(a = 0, b = 1, col = "red", lwd = 2)
cor(pred_hist_sac, pred_now_sac)

data.frame(saclat %>% left_join(responsvar_sac) %>% dplyr::select(names(saclat)), 
           Now = exp(pred_now_sac) -1 , 
           Fitted = exp(pred_hist_sac) -1) %>% 
  mutate(Diff = Now - Fitted) %>% 
  mutate(Flag = ifelse(Diff < 0, "Negative", ifelse(Diff > 0, "Positive", "Neutral"))) %>% 
  ggplot(aes(x = X, y = Y)) +
  geom_point(aes(color = Flag), alpha = 0.7) 

data.frame(saclat %>% left_join(responsvar_sac) %>% dplyr::select(names(saclat)), 
           Now = exp(pred_now_sac) -1 , 
           Fitted = exp(pred_hist_sac) -1) %>% 
  mutate(Diff = Now - Fitted) %>% 
  mutate(Flag = ifelse(Diff < 0, "Negative", ifelse(Diff > 0, "Positive", "Neutral"))) %>% 
  ggplot(aes(x = Diff, fill = Flag)) +
  geom_histogram()

# Ikke like store forskjeller her som for stortare, kanskje?

saclat %>% dplyr::select(Date) %>% mutate(Date2 = as.Date(Date, format = "%Y-%m-%d")) %>% summary
saclat %>% dplyr::select(Year_HGU) %>% summary
# 30-40 years worth of data, probably tempterature increase as well

# prediksjoner med fremtidscenario RCP 85 - temp og salinitet

pred_RCP85_2100_sac <- saclat %>% left_join(responsvar_sac) %>%
  mutate(Grazing = 0) %>% 
  left_join(obs.points) %>% 
  left_join(Future_df) %>% 
  dplyr::select(-Meantemp, Maxtemp, BO2_salinitymax_bdmean) %>%
  mutate(Meantemp = RCP85meantemp_2100,
         Maxtemp = RCP85maxtemp_2100,
         BO2_salmax_bdmean = RCP85maxsal_2100) %>% 
  predict(finalmodel_sac, newdata = .)

par(mfrow = c(1,1))
plot(pred_now_sac, pred_RCP85_2100_sac)

abline(a = 0, b = 1, col = "red")

data.frame(saclat %>% left_join(responsvar_sac) %>% dplyr::select(names(saclat)), 
           Now = exp(pred_now_sac) -1 , 
           RCP85 = exp(pred_RCP85_2100_sac) -1) %>% 
  mutate(Diff = RCP85 - Now) %>% 
  mutate(Flag = ifelse(Diff < 0, "Negative", ifelse(Diff > 0, "Positive", "Neutral"))) %>% 
  ggplot(aes(x = X, y = Y)) +
  geom_point(aes(color = Flag), alpha = 0.7) 


data.frame(saclat %>% left_join(responsvar_sac) %>% dplyr::select(names(saclat)), 
           Now = exp(pred_now_sac) -1 , 
           RCP85 = exp(pred_RCP85_2100_sac) -1) %>% 
  mutate(Diff = RCP85 - Now) %>% 
  mutate(Flag = ifelse(Diff < 0, "Negative", ifelse(Diff > 0, "Positive", "Neutral"))) %>% 
  ggplot(aes(x = X, y = Y)) +
  geom_point(aes(color = Diff), alpha = 0.7) 


data.frame(saclat %>% left_join(responsvar_sac) %>% dplyr::select(names(saclat)), 
           Now = exp(pred_now_sac) -1 , 
           RCP85 = exp(pred_RCP85_2100_sac) -1) %>%  
  mutate(Diff = RCP85 - Now) %>% 
  mutate(Flag = ifelse(Diff < 0, "Negative", ifelse(Diff > 0, "Positive", "Neutral"))) %>%
  ggplot(aes(x = Diff, fill = Flag)) +
  geom_histogram()
