#####################################################
## The Norwegian Kelp Models # take two
## Modeling 
## Guri Sogn Andersen, 2022
#####################################################

# This work is largely based on the models made in the Nordic Blue Carbon project (2020): http://pub.norden.org/temanord2020-541/#47359
# Here we build new models incorporating additional variables (e.g. Urchindata)

# Packages ----------------------------------------------------------------

require(tidyverse)
require(gbm) # Generalized Boosted Regression Models
require(dismo) # Species Distribution Modeling
require(corrplot)

# Data --------------------------------------------------------------------
lamhyp <- read.csv("./Data/lamhyp2022.csv", stringsAsFactors = FALSE)
str(lamhyp)
names(lamhyp)

lamhyp %>% dplyr::select(bedpar:BO2_salinitymax_ss, Grazing) %>% 
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
# ... just to be thorough ...

# For reference, see: https://rspatial.org/raster/sdm/9_sdm_brt.html 

explvar <- lamhyp %>% dplyr::select(Depth_mod, bedpar:curv500, slope:BO2_salinitymax_ss, Grazing, Y) %>% names() 
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
saveRDS(lamhyp.full.poisson.gbm, file = "./Models/lamhyp.full.poisson.gbm.rds")

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
saveRDS(lamhyp.poisson.simp, file = "./Models/lamhyp.poisson.simp.rds") 
# Suggest cutting 3 vars: Grazing, tempmax and tempmin

# Models using density*2 as response (to get integers)
set.seed(123)
lamhyp.full.gaus.gbm <- gbm.step(data = cbind(lamhyp, responsvar),   
                                    gbm.x = explvar,       
                                    gbm.y = "Logdens",                # Tetthet*2 as response
                                    learning.rate = 0.01,         # learning rate 
                                    tree.complexity = 5,          # tree splits
                                    family = "gaussian",           # response is counts
                                    bag.fraction = 0.5)

par(mfrow = c(1,1), mar=c(4,10,2,2))
summary(lamhyp.full.gaus.gbm, las = 2) # importance of the different explvar
# Looks very similar to the poisson-model
lamhyp.full.gaus.gbm$contributions

# Save BRT model to own object - just to be sure
saveRDS(lamhyp.full.gaus.gbm, file = "./Models/lamhyp.full.gaus.gbm.rds")

# respons
gbm.plot(lamhyp.full.gaus.gbm, write.title=TRUE, show.contrib=TRUE, plot.layout = c(3,3)) 
gbm.plot.fits(lamhyp.full.gaus.gbm)

# prediksjoner
par(mfrow = c(1,1))
plot(lamhyp.full.gaus.gbm$data$y, lamhyp.full.gaus.gbm$fitted)
abline(a = 0, b = 1, col = "red")

cor(lamhyp.full.gaus.gbm$data$y, lamhyp.full.gaus.gbm$fitted) # pearson correlation
