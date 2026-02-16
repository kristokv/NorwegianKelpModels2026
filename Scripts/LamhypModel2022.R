#####################################################
## The Norwegian Kelp Models # take two
## Modeling tangle kelp
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
maindir <- "D:\\Taremodeller\\NorwegianKelpModels"

# Data --------------------------------------------------------------------
lamhyp <- read.csv(paste(maindir, "./Data/lamhyp2022.csv", sep="/"), stringsAsFactors = FALSE)
str(lamhyp)
names(lamhyp)
summary(lamhyp)

lamhyp %>% dplyr::select(bedpar:BO2_salinitymax_ss, Grazing:Maxtemp, Dissolved.oxygen.Mean:Salmean) %>% 
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

explvar <- lamhyp %>% dplyr::select(Depth_mod, bedpar:curv500, slope:BO2_salinitymax_ss, 
                                    Grazing:Maxtemp, 
                                    Dissolved.oxygen.Mean:Salmean, Y) %>% names() 
explvar

# Responses
lamhyp %>% ggplot(aes(x = Tetthet_kat, y = Tetthet)) +
  geom_jitter()

lamhyp %>% ggplot(aes(x = X, y = Y, col = Tetthet)) +
  geom_point()

responsvar <- lamhyp %>% dplyr::select(X, Y, Tetthet) %>% 
  mutate(Dens2 = as.integer(Tetthet*2),
         Logdens = log(Tetthet+1))

summary(responsvar)

# Models using density*2 as response (to get integers)
set.seed(123)
lamhyp.full.poisson.gbm <- gbm.step(data = cbind(lamhyp, responsvar),   
                           gbm.x = explvar,       
                           gbm.y = "Dens2",                # Tetthet*2 as response
                           learning.rate = 0.01,         # learning rate 
                           tree.complexity = 5,          # tree splits
                           family = "poisson",           # response is counts
                           bag.fraction = 0.5)

par(mfrow = c(1,1), mar=c(4,10,2,2))
summary(lamhyp.full.poisson.gbm, las = 2) # importance of the different explvar
lamhyp.full.poisson.gbm$contributions

# Save BRT model to own object - just to be sure
saveRDS(lamhyp.full.poisson.gbm, file = "./Models/lamhyp.full.poisson.gbm_v3.rds")

# respons
gbm.plot(lamhyp.full.poisson.gbm, write.title=TRUE, show.contrib=TRUE, plot.layout = c(3,3)) 
gbm.plot.fits(lamhyp.full.poisson.gbm)

# prediksjoner
plot(lamhyp.full.poisson.gbm$data$y, lamhyp.full.poisson.gbm$fitted)
abline(a = 0, b = 1, col = "red")

cor(lamhyp.full.poisson.gbm$data$y, lamhyp.full.poisson.gbm$fitted) # pearson correlation

# simplify - very slow, perhaps not necessary
set.seed(23)
lamhyp.poisson.simp <- gbm.simplify(lamhyp.full.poisson.gbm)
saveRDS(lamhyp.poisson.simp, file = "./Models/lamhyp.poisson.simp_v3.rds") 
# Suggest cutting 3 vars: Grazing, tempmax and tempmin

# Models using log(dens+1) as response 
set.seed(123)
lamhyp.full.gaus.gbm <- gbm.step(data = cbind(lamhyp, responsvar),   
                                    gbm.x = explvar,       
                                    gbm.y = "Logdens",                # log(Tetthet+1) as response
                                    learning.rate = 0.01,         # learning rate 
                                    tree.complexity = 5,          # tree splits
                                    family = "gaussian",           # response is transformed
                                    bag.fraction = 0.5)

par(mfrow = c(1,1), mar=c(4,10,2,2))
summary(lamhyp.full.gaus.gbm, las = 2) # importance of the different explvar
# Looks very similar to the poisson-model
lamhyp.full.gaus.gbm$contributions

# Save BRT model to own object - just to be sure
saveRDS(lamhyp.full.gaus.gbm, file = "./Models/lamhyp.full.gaus.gbm_v3.rds")

# respons
gbm.plot(lamhyp.full.gaus.gbm, write.title=TRUE, show.contrib=TRUE, plot.layout = c(3,3)) 
gbm.plot.fits(lamhyp.full.gaus.gbm)

# prediksjoner
par(mfrow = c(1,1))
plot(lamhyp.full.gaus.gbm$data$y, lamhyp.full.gaus.gbm$fitted)
abline(a = 0, b = 1, col = "red")

cor(lamhyp.full.gaus.gbm$data$y, lamhyp.full.gaus.gbm$fitted) # pearson correlation

# simplify - very slow, perhaps not necessary
set.seed(23)
lamhyp.gaus.simp <- gbm.simplify(lamhyp.full.gaus.gbm)
saveRDS(lamhyp.gaus.simp, file = "./Models/lamhyp.gaus.simp.gbm_v3.rds") 
# Suggest cutting 2 vars - Grazing and Light!!

lamhyp.gaus.simp
lamhyp.gaus.simp$pred.list$preds.2

# Trying to optimize parameters based on input from ERI on the Nordic paper:
source("./Scripts/trainBrt_AdamLillith.R")

out.lam <- trainBrt(data = cbind(lamhyp, responsvar),  
                    resp = "Logdens",             
                    preds = explvar,
                    family = "gaussian",
                    learningRate = c(0.0001, 0.001, 0.01), #Elith et al. 2008 recommend 0.0001 to 0.1)
                    treeComplexity = c(2, 4, 6),
                    bagFraction=c(0.5, 0.6, 0.7), #Elith et al. 2008 recommend 0.5 to 0.7
                    maxTrees = 10000,
                    out=c('tuning', 'model'),
                    verbose=TRUE)

# Best was complexity = 6, bagFraction = 0.6 and learning rate = 1e-02
out.lam
saveRDS(out.lam, file = "./Models/outlam.rds")

# It would perhaps be a good idea to test manual selection keeping in mind that
# we need to be able to predict for the future ...
# Salinity OK - doesn't seem to matter much which variable is used

explvar_selection <- c("Depth_mod", "swm", "Meantemp", "Maxtemp", "curv500", "slope", "Dissolved.oxygen.Mean", "wspd90",
                       "BO2_salinitymax_bdmean", "Nitrate.Mean", "Phosphate.Mean", "Light.bottom.Mean", "Grazing")

# Models using log(dens+1) as response 
set.seed(123)
lamhyp.manual.gaus.gbm <- gbm.step(data = cbind(lamhyp, responsvar),   
                                 gbm.x = explvar_selection,       
                                 gbm.y = "Logdens",                # log(Tetthet +1) as response
                                 learning.rate = 0.05,         # learning rate 
                                 tree.complexity = 5,          # tree splits
                                 family = "gaussian",           # response is counts
                                 bag.fraction = 0.5)

par(mfrow = c(1,1), mar=c(4,10,2,2))
summary(lamhyp.manual.gaus.gbm, las = 2) # importance of the different explvar

lamhyp.manual.gaus.gbm$contributions

# Save BRT model to own object - just to be sure
saveRDS(lamhyp.manual.gaus.gbm, file = "./Models/lamhyp.manual.gaus.gbm_v3.rds")

# respons
gbm.plot(lamhyp.manual.gaus.gbm, write.title=TRUE, show.contrib=TRUE, plot.layout = c(3,3)) 
gbm.plot.fits(lamhyp.manual.gaus.gbm)

# prediksjoner
par(mfrow = c(1,1))
plot(lamhyp.manual.gaus.gbm$data$y, lamhyp.manual.gaus.gbm$fitted)
abline(a = 0, b = 1, col = "red", lwd = 2)

cor(lamhyp.manual.gaus.gbm$data$y, lamhyp.manual.gaus.gbm$fitted) # pearson correlation

out.lam.simp <- trainBrt(data = cbind(lamhyp, responsvar),  
                    resp = "Logdens",             
                    preds = explvar_selection,
                    family = "gaussian",
                    learningRate = c(0.0001, 0.001, 0.01), #Elith et al. 2008 recommend 0.0001 to 0.1)
                    treeComplexity = c(2, 4, 6),
                    bagFraction=c(0.5, 0.6, 0.7), #Elith et al. 2008 recommend 0.5 to 0.7
                    maxTrees = 10000,
                    out=c('tuning', 'model'),
                    verbose=TRUE)
# Best was complexity = 6, bagFraction = 0.6 and learning rate = 1e-02
out.lam.simp
saveRDS(out.lam.simp, file = "./Models/outlamsimp.rds")

out.lam.simp <- readRDS("./Models/outlamsimp.rds")

lamhyp.manual.opt.gbm <- out.lam.simp$model
lamhyp.manual.opt.gbm$contributions

# simplify
set.seed(23)
lamhyp.manual.simp <- gbm.simplify(lamhyp.manual.opt.gbm)
saveRDS(lamhyp.manual.simp, file = "./Models/lamhyp.manual.simp.gbm_v3.rds") 
# Suggest cutting 3 vars
lamhyp.manual.simp <- readRDS("./Models/lamhyp.manual.simp.gbm_v3.rds")

lamhyp.manual.simp$pred.list$preds.3



set.seed(123)
lamhyp.manual.cut.gbm <- gbm.step(data = cbind(lamhyp, responsvar),   
                              gbm.x = lamhyp.manual.simp$pred.list$preds.3,       
                              gbm.y = "Logdens",                # log(Tetthet+1) as response
                              learning.rate = 0.01,         # learning rate 
                              tree.complexity = 6,          # tree splits
                              family = "gaussian",           
                              bag.fraction = 0.6)
saveRDS(lamhyp.manual.cut.gbm, file = "./Models/lamhyp.manual.cut.gbm.rds")

lamhyp.manual.cut.gbm$contributions


# Testing with inclusion of latitude --------------------------------------

set.seed(123)
lamhyp.manual.cut.withY.gbm <- gbm.step(data = cbind(lamhyp, responsvar),   
                                  gbm.x = c(lamhyp.manual.simp$pred.list$preds.3, "Y"),       
                                  gbm.y = "Logdens",                # log(Tetthet+1) as response
                                  learning.rate = 0.01,         # learning rate 
                                  tree.complexity = 6,          # tree splits
                                  family = "gaussian",           
                                  bag.fraction = 0.6)
saveRDS(lamhyp.manual.cut.withY.gbm, file = "./Models/lamhyp.manual.cut.withY.gbm.rds")

lamhyp.manual.cut.withY.gbm$contributions


finalmodel <- lamhyp.manual.cut.gbm # lamhyp.manual.cut.withY.gbm # 

# Marginal plots final model ----------------------------------------------
gbm.plot(finalmodel, write.title=TRUE, show.contrib=TRUE,
         common.scale = TRUE,
         smooth = TRUE,
         plot.layout = c(3,4),
         lty = "dotted",
         lty.smooth = "solid") 

plot(finalmodel, i.var = 1)
plot(finalmodel, i.var = 2)
plot(finalmodel, i.var = 3)
plot(finalmodel, i.var = 4)
plot(finalmodel, i.var = 5)
plot(finalmodel, i.var = 6)
plot(finalmodel, i.var = 7)

plot(finalmodel, i.var = 8)
plot(finalmodel, i.var = 9)
plot(finalmodel, i.var = 10)
plot(finalmodel, i.var = 11)

gbm.plot.fits(finalmodel)

# Comparing models --------------------------------------------------------

plot(responsvar$Tetthet, exp(lamhyp.full.gaus.gbm$fitted)-1)
plot(responsvar$Tetthet, lamhyp.full.poisson.gbm$fitted/2)
plot(responsvar$Tetthet, exp(lamhyp.manual.gaus.gbm$fitted)-1)

# Full gaus against manual selection
plot(exp(lamhyp.full.gaus.gbm$fitted)-1, exp(lamhyp.manual.gaus.gbm$fitted)-1)
abline(a = 0, b = 1, col = "red", lwd = 2)

cor(exp(lamhyp.full.gaus.gbm$fitted)-1, exp(lamhyp.manual.gaus.gbm$fitted)-1)

# Full model
lamhyp.full.gaus.gbm$cv.statistics$correlation.mean
lamhyp.full.gaus.gbm$self.statistics$correlation

# Manual selection of variables to be able to predict to future
lamhyp.manual.gaus.gbm$cv.statistics$correlation.mean
lamhyp.manual.gaus.gbm$self.statistics$correlation

# After optimizing model parameters
lamhyp.manual.opt.gbm$cv.statistics$correlation.mean
lamhyp.manual.opt.gbm$self.statistics$correlation

# After running simplification
lamhyp.manual.cut.gbm$cv.statistics$correlation.mean
lamhyp.manual.cut.gbm$self.statistics$correlation
tot.dev <- lamhyp.manual.cut.gbm$self.statistics$mean.null # mean total deviance
res.dev <- lamhyp.manual.cut.gbm$self.statistics$mean.resid # mean residual deviance
dev.exp <- (tot.dev-res.dev)/tot.dev # deviance explained
dev.exp # OK - stemmer med manus
rm(tot.dev, res.dev, dev.exp)

# I think this means that the cut and simplified model reduces the risk of overfitting, compared to the others.
# Would be interesting to compare predictions in small areas, e.g. north, mid and south Norway.

p1 <- data.frame(Measured = responsvar$Tetthet,
           Gausmod  = exp(lamhyp.full.gaus.gbm$fitted)-1,
           Poismod  = lamhyp.full.poisson.gbm$fitted/2,
           Manual   = exp(lamhyp.manual.gaus.gbm$fitted)-1,
           Optim    = exp(lamhyp.manual.opt.gbm$fitted)-1,
           Cut      = exp(lamhyp.manual.cut.gbm$fitted)-1) %>% 
  pivot_longer(cols = Gausmod:Cut, names_to = "Model", values_to = "Fitted") %>% 
  ggplot(aes(x = as.factor(Measured), y = Fitted, col = Model)) +
  labs(x = "Density categories") +
  geom_boxplot()

p1

p2 <- lamhyp.full.poisson.gbm$contributions %>% 
  rename("Poismod" = "rel.inf") %>% 
  left_join(lamhyp.full.gaus.gbm$contributions) %>% 
  rename("Gausmod" = "rel.inf") %>% 
  left_join(lamhyp.manual.gaus.gbm$contributions) %>% 
  rename("Manual" = "rel.inf") %>% 
  pivot_longer(cols = Poismod:Manual, names_to = "Model", values_to = "Importance") %>% 
  ggplot(aes(x = reorder(var, Importance), y = Importance, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge")+
  coord_flip()+
  labs(x = "")

p2
# When temperature is this important, using historic temperature data may be important 

p3 <- data.frame(Depth = lamhyp$Depth_mod,
           SWM_10K = lamhyp$swm/10000,
           Maxtemp = lamhyp$Maxtemp,
           Bedpar = lamhyp$bedpar,
           Measured = responsvar$Tetthet,
           Gausmod  = exp(lamhyp.full.gaus.gbm$fitted)-1,
           Gausman  = exp(lamhyp.manual.gaus.gbm$fitted)-1,
           Poismod  = lamhyp.full.poisson.gbm$fitted/2) %>% 
  pivot_longer(cols = Gausmod:Poismod, names_to = "Model", values_to = "Fitted") %>% 
  pivot_longer(cols = Depth:Bedpar, names_to = "Envvar") %>% 
  ggplot(aes(x = value, y = Fitted, col = Model)) +
  geom_point(alpha = 0.6) +
  labs(x = "") +
  facet_wrap(~Envvar, scales = "free")
p3

p4 <- lamhyp %>% ggplot(aes(y = BO2_tempmean_bdmin, x = as.factor(Grazing))) +
  geom_boxplot() +
  labs(x = "Grazing")
p4

lamhyp %>% ggplot(aes(y = Maxtemp, x = as.factor(Grazing))) +
  geom_boxplot()
lamhyp %>% ggplot(aes(y = Meantemp, x = as.factor(Grazing))) +
  geom_boxplot()

p2+p4+p3+p1 + plot_layout(guides = "collect")

lamhyp.full.poisson.gbm$cv.statistics$correlation.mean^2
lamhyp.full.gaus.gbm$cv.statistics$correlation.mean^2 # seems best
lamhyp.manual.gaus.gbm$cv.statistics$correlation.mean^2 # seems best
lamhyp.manual.cut.gbm$cv.statistics$correlation.mean^2 # seems best

# Grazing not important - perhaps other gradients (i.e. Y or temp) is better at 
# describing the variation in grazing pressure than the latitudinal placement of the front?

lamhyp %>% ggplot(aes(y = BO2_tempmean_bdmin, x = as.factor(Tetthet))) +
  geom_boxplot()
lamhyp %>% ggplot(aes(y = Y, x = as.factor(Tetthet))) +
  geom_boxplot()


# Future predictions ------------------------------------------------------

# use model with manual selection here:
# Buildt model with historic data, predicted with present from Bio-ORACLE
pred_now <- lamhyp %>% left_join(responsvar) %>% 
  mutate(Grazing = 0) %>% 
  dplyr::select(-Meantemp, Maxtemp) %>% 
  mutate(Meantemp = BO2_tempmean_bdmin,
         Maxtemp = BO2_tempmax_bdmin) %>% 
  predict(finalmodel, newdata = .) # 

plot(finalmodel$fitted, pred_now)
abline(a = 0, b = 1, col = "red", lwd = 2)
cor(finalmodel$fitted, pred_now)

data.frame(lamhyp, 
           Now = exp(pred_now) -1 , 
           Fitted = exp(finalmodel$fitted) -1) %>% 
  mutate(Diff = Now - Fitted) %>% 
  mutate(Flag = ifelse(Diff < 0, "Negative", ifelse(Diff > 0, "Positive", "Neutral"))) %>% 
  ggplot(aes(x = X, y = Y)) +
  geom_point(aes(color = Flag), alpha = 0.7) 

data.frame(lamhyp, 
           Now = exp(pred_now) -1 , 
           Fitted = exp(finalmodel$fitted) -1) %>% 
  mutate(Diff = Now - Fitted) %>% 
  mutate(Flag = ifelse(Diff < 0, "Negative", ifelse(Diff > 0, "Positive", "Neutral"))) %>% 
  ggplot(aes(x = Diff, fill = Flag)) +
  geom_histogram()

# Predikeres mindre tare med Bio-Oracle lag for nåtid enn med tempdata fra året observasjoner er gjort.

lamhyp %>% dplyr::select(Date) %>% mutate(Date2 = as.Date(Date, format = "%Y-%m-%d")) %>% summary
lamhyp %>% dplyr::select(Year_HGU) %>% summary
# 20 years worth of data, probably tempterature increase as well

# prediksjoner med fremtidscenario RCP 85 - temp og salinitet OG LYS!!

pred_RCP85_2100 <- lamhyp %>% left_join(responsvar) %>%
  mutate(Grazing = 0) %>% 
  left_join(obs.points) %>% 
  left_join(Future_df) %>% 
  dplyr::select(-Meantemp, Maxtemp, BO2_salinitymax_bdmean) %>%
  mutate(Meantemp = RCP85meantemp_2100,
         Maxtemp = RCP85maxtemp_2100,
         BO2_salmax_bdmean = RCP85maxsal_2100) %>% 
  mutate(Depth_mod = Depth_mod*2.698789) %>%  # Ref ThoughtExperimentsLight.R
  predict(finalmodel, newdata = .)

### NB !!! Ekstrem forskjell med og uten dybdejustering for å ta høyde for lys!!!
### Antagelig ikke riktig å bruke samme konstant over hele Norge. Mulig nordområder er
### bedre representert av K for dypere vannmasser (ref Opdal 2019)

par(mfrow = c(1,1))
plot(pred_now, pred_RCP85_2100)
abline(a = 0, b = 1, col = "red")

data.frame(lamhyp, 
           Now = exp(pred_now) -1 , 
           RCP85 = exp(pred_RCP85_2100) -1) %>% 
  mutate(Diff = RCP85 - Now) %>% 
  mutate(Flag = ifelse(Diff < 0, "Negative", ifelse(Diff > 0, "Positive", "Neutral"))) %>% 
  ggplot(aes(x = X, y = Y)) +
  geom_point(aes(color = Flag), alpha = 0.2) 


data.frame(lamhyp, 
           Now = exp(pred_now) -1 , 
           RCP85 = exp(pred_RCP85_2100) -1) %>% 
  mutate(Diff = RCP85 - Now) %>% 
  mutate(Flag = ifelse(Diff < 0, "Negative", ifelse(Diff > 0, "Positive", "Neutral"))) %>% 
  ggplot(aes(x = X, y = Y)) +
  geom_point(aes(color = Diff), alpha = 0.7) 
  

data.frame(lamhyp, 
           Now = exp(pred_now) -1 , 
           RCP85 = exp(pred_RCP85_2100) -1) %>% 
  mutate(Diff = RCP85 - Now) %>% 
  mutate(Flag = ifelse(Diff < 0, "Negative", ifelse(Diff > 0, "Positive", "Neutral"))) %>%
  ggplot(aes(x = Diff, fill = Flag)) +
  geom_histogram()

# Egentlig ingen store endringer når lys ikke tas med...

# interessant å se hva som skjer dersom vi legger på endringer i lys
# tenker vi da manipulerer dyp som en proxy til lysreduksjon

# Må vi legge til latitude? (y) Synes det er så rare effekter langs de ulike variablene
# Lurer på om noen av dem er mer koblet til kråkebolle-krabbe-tare-dynamikk eller lignende enn annet ...

# Ser ikke veldig annerledes ut virker det som. Synes det er mer safe å droppe Y ...
# Kanskje plotte miljøvariabler mot differanse?

data.frame(lamhyp, 
           Now = exp(pred_now) -1 , 
           RCP85 = exp(pred_RCP85_2100) -1) %>% 
  mutate(Diff = RCP85 - Now) %>% 
  ggplot(aes(x = Now, y = Diff)) +
  geom_point()

# How does the difference relate to environmental variables?
# Clear relationships with Maxtemp and Meantemp, less so with salinity - 
# Not so clear when light/depth adjustment is included
# Perhaps also with nutrients? Maybe interesting to look at the  
# interaction between temperature and nutrients?

data.frame(lamhyp %>% left_join(responsvar) %>%
             left_join(obs.points) %>% 
             left_join(Future_df) , 
           Now = exp(pred_now) -1 , 
           RCP85 = exp(pred_RCP85_2100) -1) %>% 
  mutate(Diff = RCP85 - Now) %>% 
  dplyr::select(Diff, Depth_mod, swm, Nitrate.Mean, Phosphate.Mean, Dissolved.oxygen.Mean, 
                Meantemp, Maxtemp, BO2_salinitymax_bdmean,
                RCP85meantemp_2100, RCP85maxtemp_2100, RCP85maxsal_2100) %>% 
  pivot_longer(cols = Depth_mod:RCP85maxsal_2100, names_to = "Variable", values_to = "Value") %>% 
  ggplot(aes(x = Value, y = Diff)) +
  geom_point() +
  facet_wrap(~ Variable, scales = "free_x")

# Temperature now vs. future predictions
lamhyp %>% left_join(responsvar) %>%
  left_join(obs.points) %>% 
  left_join(Future_df) %>% 
  dplyr::select(RCP85meantemp_2100, RCP85maxtemp_2100, Meantemp, Maxtemp) %>% 
  plot()

# Does the temperature increase everywhere? YES:
lamhyp %>% left_join(responsvar) %>%
  left_join(obs.points) %>% 
  left_join(Future_df) %>% 
  dplyr::select(X, Y, RCP85meantemp_2100, RCP85maxtemp_2100, Meantemp, Maxtemp) %>% 
  mutate(Diffmean = RCP85meantemp_2100 - Meantemp,
         Diffmax = RCP85maxtemp_2100 - Maxtemp) %>% 
  pivot_longer(cols = c(Diffmean, Diffmax), values_to = "Diff", names_to = "Variable") %>% 
  mutate(Flag = ifelse(Diff < 0, "Negative", ifelse(Diff > 0, "Positive", "Neutral"))) %>% 
  ggplot(aes(x = X, y = Y)) +
  geom_point(aes(color = Flag), alpha = 0.7) +
  facet_wrap(~ Variable)
  
# Interactions nutrients and temperature !!
final.int <- gbm.interactions(finalmodel)

final.int$rank.list
final.int$interactions

gbm.perspec(lamhyp.manual.cut.gbm, 4, 9,
            z.range = c(-1, 3))
gbm.perspec(lamhyp.manual.cut.gbm, 4, 10,
            z.range = c(-1, 3))

# Kan flekkvis negativ utvikling i framtidsprediksjoner skyldes høye kons av næringssalter?

# Interactions light/depth and temperature

gbm.perspec(lamhyp.manual.cut.gbm, 3, 1,
            z.range = c(-1, 3))

gbm.perspec(lamhyp.manual.cut.gbm, 4, 1,
            z.range = c(-1, 3))
# Interesting pattern for maxtemp. Obvious narrowing of vertical range with higher temperature!