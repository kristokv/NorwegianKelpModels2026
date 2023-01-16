#### Sammenligning BRT
# Lager en funksjon som setter sammen tabell der brt-modellers forklaringsevner sammenlignes
# Vil ha med training AUC, cv AUC, Mean tot deviance, Residual deviance, Part explained and Contributions*Part explained
compare.brt <- function(x){
  tab.compare.var <- data.frame()
  tab.compare.brt <- data.frame(TotDev = numeric(),
                                ResDev = numeric(),
                                DevExp = numeric(),
                                Model = character(),
                                CorTrain = numeric(),
                                CorCV = numeric(),
                                CorSE = numeric(),
                                AUCtrain = numeric(),
                                AUCcv = numeric(),
                                AUCcvSE = numeric(), 
                                stringsAsFactors = FALSE)
  
  xframe <- eval(parse(text = x[1]))$contributions$rel.inf
  names(xframe) <- eval(parse(text = x[1]))$contributions$var
  tab.compare.var <- merge(tab.compare.var, t(xframe), all = TRUE) # creates only empty frame with names ... don't know why
  
  for(i in 1:length(x)){
    y <- eval(parse(text = x[i]))
    tot.dev <- y$self.statistics$mean.null # mean total deviance
    res.dev <- y$self.statistics$mean.resid # mean residual deviance
    dev.exp <- (tot.dev-res.dev)/tot.dev # deviance explained
    
    varcont <- y$contributions$rel.inf*dev.exp # deviance explained by that variable
    names(varcont) <- y$contributions$var
    varcont <- data.frame(t(varcont))
    varcont$Model <- x[i]
    CorTrain <- y$self.statistics$correlation
    CorCV <- y$cv.statistics$correlation.mean
    CorSE <- y$cv.statistics$correlation.se
    AUCtrain <- y$self.statistics$discrimination # AUC training data
    AUCcv    <- y$cv.statistics$discrimination.mean # AUC cross validation
    AUCcvSE    <- y$cv.statistics$discrimination.se # AUC standard error
    
    tab.compare.var <- merge(tab.compare.var, varcont, all = TRUE)
    tab.compare.brt[i, ] <- c(tot.dev, res.dev, dev.exp, x[i], CorTrain, CorCV, CorSE, AUCtrain, AUCcv, AUCcvSE)
  }
  
  tab.compare <- merge(tab.compare.var, tab.compare.brt, by = "Model", all = TRUE)
  return(tab.compare)
}

sammenligning <- compare.brt(x = c("forest.full.gbm",
                                   "forest.noloc.gbm", 
                                   "dens.full.gbm",
                                   "dens2.full.gbm",
                                   "dens3.full.gbm"))
tsammen <- data.frame(t(sammenligning))[-1, ]
names(tsammen) <- sammenligning$Model
tsammen
# Plot
# Define the number of colors you want
require(RColorBrewer)
nb.cols <- sammenligning %>% dplyr::select(-c(Model, TotDev:AUCcvSE)) %>% names(.) %>% length(.)
mycolors <- colorRampPalette(brewer.pal(11, "Spectral"))(nb.cols)
xylabels <- sammenligning %>% dplyr::select(Model, DevExp, CorTrain, CorCV, CorSE, AUCcv, AUCtrain, AUCcvSE) %>% 
  mutate(DevExp = as.numeric(DevExp)*100,
         Label1 = paste("Training: AUC =", 
                        round(as.numeric(AUCtrain), digits = 2), 
                        "Corr = ", round(as.numeric(CorTrain), digits = 2), 
                        sep = " "),
         Label2 = paste("CV: AUC =", 
                        round(as.numeric(AUCcv), digits = 2), "\u00B1", 
                        round(as.numeric(AUCcvSE), digits = 3), 
                        "Corr =",
                        round(as.numeric(CorCV), digits = 2), "\u00B1", 
                        round(as.numeric(CorSE), digits = 3),
                        sep = " "))
p1 <- sammenligning %>% 
  pivot_longer(cols = c(Depth_HGU:bedpar, Location), names_to = "Variable") %>% 
  ggplot(aes(x = reorder(Model, as.numeric(DevExp)), y = value)) +
  geom_col(aes(fill = Variable)) +
  labs(x = "BRT model", y = "Deviance explained (%)") +
  geom_text(data = xylabels, aes(x = Model, y = DevExp/2, label = Label1), nudge_x = 0.2, fontface = "bold") +
  geom_text(data = xylabels, aes(x = Model, y = DevExp/2, label = Label2), nudge_x = -0.2, fontface = "bold") +
  ylim(0,100) +
  coord_flip() +
  #scale_fill_brewer(palette = "Spectral")
  scale_fill_manual(values = mycolors) +
  theme(legend.position = "bottom")