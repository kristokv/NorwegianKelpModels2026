# Marginal effect plots

source("./Scripts/gbm_plot_own_GSA.R")


library(Cairo)

Cairo::Cairo(
  20, # width
  20, # height
  file = paste("./Figures/MarginalEffects", ".png", sep = ""),
  type = "png", #tiff
  bg = "white", #white or transparent depending on your requirement 
  dpi = 300,
  units = "cm" #you can change to pixels etc 
)

par(mar=c(3,1,1.5,0), oma = c(1,4,0,1))
layout(matrix(c(seq(1,12), rep(0,6), seq(13,24)), nrow=5, byrow = TRUE), heights = c(1,1,0.1,1,1))

varnames_lam <- c("Depth", "Wave exposure", "Nitrate", "Curvature", "Dissolved oxygen", "Phosphate", "Mean temperature", 
                  "Max temperature", "Currents", "Max salinity")

gbm.plot.own(lamhyp.manual.cut.gbm, lamhyp %>% left_join(responsvar), smooth = TRUE,
             varnames = varnames_lam,
             rug = TRUE, ylim = c(-1,1), y.label = "")
mtext(side=2, "Marginal effect on tangle kelp", outer = TRUE, cex = 1, line = 1.5, adj=0.84)
plot.new()
plot.new()

varnames_sac <- c("Depth", "Wave exposure", "Mean temperature", "Dissolved oxygen", "Slope", "Curvature", "Nitrate",
                  "Phosphate", "Max temperature", "Max salinity", "Currents")

gbm.plot.own(saclat.simp.manual.gbm, saclat %>% left_join(responsvar_sac), smooth = TRUE, s.col = "coral3",
             varnames = varnames_sac,
             rug = TRUE, ylim = c(-0.3,0.7), y.label = "")
mtext(side=2, "Marginal effect on sugar kelp", outer = TRUE, cex = 1, line = 1.5, adj=0.15)

dev.off()

