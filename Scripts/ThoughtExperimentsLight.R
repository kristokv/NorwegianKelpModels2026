## Thought experiments - Light
## GSA 2023

library(readxl)
library(tidyverse)
library(patchwork)
library(viridis)

# making a bunch of linear relationships

lfunc <- function(x, a, b){a*x + b}

X1 <- c(1:30)

Y1 <- lfunc(X1, 2, 16)
Y2 <- lfunc(X1, 1.1, 23)
Y3 <- lfunc(X1, 1, 1)

plot(X1, Y1, ylim = c(0,70))
abline(v = 20, col = "red")
abline(h = lfunc(20, 2, 16), col = "red")
abline(v = 20*0.2, col = "blue")
abline(h = lfunc(20, 2, 16)*0.2, col = "blue")

# Read data from Fig 1 in Opdal et al 2019

Kdata <- read_xlsx("./Data/Opdal2019Fig1_datasets.xlsx")
Kdata

Kdata %>% ggplot(aes(x = X, y = Y, colour = Series)) +
  geom_smooth()

Kdata %>% select(Series) %>% unique

Km2 <- lm(Y~poly(X, 2), data = Kdata %>% filter(Series == "Kest"))
Km2
summary(Km2)

Km3 <- lm(Y~poly(X, 3), data = Kdata %>% filter(Series == "Kest"))
Km3
summary(Km3)

Km4 <- lm(Y~poly(X, 4), data = Kdata %>% filter(Series == "Kest"))
Km4
summary(Km4)


Kest_pred2 <- predict(Km2)
Kest_pred2

Kest_pred3 <- predict(Km3)
Kest_pred3

Kest_pred4 <- predict(Km4)
Kest_pred4

plot(Kest_pred2, Kdata$Y[Kdata$Series == "Kest"])
points(Kest_pred3, Kdata$Y[Kdata$Series == "Kest"], add = TRUE, col = "blue")
points(Kest_pred4, Kdata$Y[Kdata$Series == "Kest"], add = TRUE, col = "red")

Yearpred <- data.frame(X = c(1900:2100))
Yearpred

Yearpred$Km2 <- predict(Km2, newdata = Yearpred)
Yearpred$Km3 <- predict(Km3, newdata = Yearpred)
Yearpred$Km4 <- predict(Km4, newdata = Yearpred)

Yearpred %>% 
  pivot_longer(cols = Km2:Km4) %>% 
  ggplot(aes(x = X, y = value, color = name)) +
  geom_point()

# Go for second order polynomial

Kupper2 <- lm(Y~poly(X, 2), data = Kdata %>% filter(Series == "UpperK"))
Kupper2
summary(Kupper2)

Klower2 <- lm(Y~poly(X, 2), data = Kdata %>% filter(Series == "LowerK"))
Klower2
summary(Klower2)

Yearpred$Ku <- predict(Kupper2, newdata = Yearpred)
Yearpred$Kl <- predict(Klower2, newdata = Yearpred)

K_time_plot <- Yearpred %>% 
#  pivot_longer(cols = c(Ku, Km2, Kl)) %>% 
  ggplot() + #aes(x = X, y = value, color = name)
  geom_line(aes(x = X, y = Kl), lwd = 0.5) +
  geom_line(aes(x = X, y = Km2), lwd = 0.7, lty = "dotted") +
  geom_line(aes(x = X, y = Ku), lwd = 0.5) +
  geom_ribbon(data = . %>% filter(X <= 2000), 
              aes(x = X, ymin = Kl, ymax = Ku), fill = "goldenrod1", alpha = 0.4) +
  geom_ribbon(data = . %>% filter(X >= 2000), 
              aes(x = X, ymin = Kl, ymax = Ku), fill = "goldenrod4", alpha = 0.4) +
  labs(x = "Year", y = expression(paste("Light attenuation coefficient (K m "^"-1",")")), color = "")
K_time_plot


# Choose years for predictions

Yearextract <- data.frame(X = c(1900, 1950, 2000, 2050, 2100)) 
Yearextract$Ku = predict(Kupper2, Yearextract)
Yearextract$Km2 = predict(Km2, Yearextract)
Yearextract$Kl = predict(Klower2, Yearextract)

# Depths

simdepths <- data.frame(Depth = c(1:40))
simdepths

Kpredict <- Yearextract %>% slice(rep(row_number(), 40)) %>% 
  mutate(Depth = rep(c(1:40), each = 5)) %>% 
  mutate(Ku_depth = Ku * Depth,
         Km_depth = Km2 * Depth,
         Kl_depth = Kl * Depth)

K_40_1950 <- Kpredict %>% filter(Depth == 40, X == 1950)  
K_40_1950$Km_depth

K_30_2000 <- Kpredict %>% filter(Depth == 30, X == 2000)  
K_30_2000$Km_depth

K_X_2000 <- K_40_1950$Km_depth / Yearextract$Km2[Yearextract$X == 2000]
K_X_2000

K_X2_2100 <- K_30_2000$Km_depth / Yearextract$Km2[Yearextract$X == 2100]
K_X2_2100

Kl_X_2000 <- K_40_1950$Kl_depth / Yearextract$Kl[Yearextract$X == 2000]
Kl_X_2000

Kl_X2_2100 <- K_30_2000$Kl_depth / Yearextract$Kl[Yearextract$X == 2100]
Kl_X2_2100

Ku_X_2000 <- K_40_1950$Ku_depth / Yearextract$Ku[Yearextract$X == 2000]
Ku_X_2000

Ku_X2_2100 <- K_30_2000$Ku_depth / Yearextract$Ku[Yearextract$X == 2100]
Ku_X2_2100

Km_plot <- Kpredict %>%  
  ggplot(aes(x = Depth, y = Km_depth, colour = as.factor(X))) + 
  geom_line(lwd = 1) +
  labs(y = "", colour = "Year", title = "prediction") +
  geom_segment(aes(x = 40, y = 0, xend = 40, yend = K_40_1950$Km_depth),
               colour = "firebrick1", lwd = 0.7,
               arrow = arrow(length = unit(.3, 'cm'))) +
  geom_segment(aes(x = 40, y = K_40_1950$Km_depth, xend = K_X_2000, yend = K_40_1950$Km_depth),
               colour = "firebrick2", lwd = 0.7, lty = "dashed") +
  geom_segment(aes(x = K_X_2000, y = K_40_1950$Km_depth, xend = K_X_2000, yend = 0),
               colour = "firebrick2", lwd = 0.7,
               arrow = arrow(length = unit(.3, 'cm'))) +
  geom_segment(aes(x = 30, y = 0, xend = 30, yend = K_30_2000$Km_depth),
               colour = "firebrick1", lwd = 0.7,
               arrow = arrow(length = unit(.3, 'cm'))) +
  geom_segment(aes(x = 30, y = K_30_2000$Km_depth, xend = K_X2_2100, yend = K_30_2000$Km_depth),
               colour = "firebrick2", lwd = 0.7, lty = "dashed") +
  geom_segment(aes(x = K_X2_2100, y = K_30_2000$Km_depth, xend = K_X2_2100, yend = 0),
               colour = "firebrick2", lwd = 0.7,
               arrow = arrow(length = unit(.3, 'cm'))) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  ylim(c(0,35))

Km_plot

Kl_plot <- Kpredict %>%  
  ggplot(aes(x = Depth, y = Kl_depth, colour = as.factor(X))) + 
  geom_line(lwd = 1) +
  labs(x = "", y = expression(paste("Light attenuation (K "["TOT"], ")")), colour = "Year", title = "lower") +
  geom_segment(aes(x = 40, y = 0, xend = 40, yend = K_40_1950$Kl_depth), 
               colour = "firebrick1", lwd = 0.7, 
               arrow = arrow(length = unit(.3, 'cm'))) +
  geom_segment(aes(x = 40, y = K_40_1950$Kl_depth, xend = Kl_X_2000, yend = K_40_1950$Kl_depth),
               colour = "firebrick2", lwd = 0.7, lty = "dashed") +
  geom_segment(aes(x = Kl_X_2000, y = K_40_1950$Kl_depth, xend = Kl_X_2000, yend = 0),
               colour = "firebrick2", lwd = 0.7,
               arrow = arrow(length = unit(.3, 'cm'))) +
  geom_segment(aes(x = 30, y = 0, xend = 30, yend = K_30_2000$Kl_depth), 
               colour = "firebrick1", lwd = 0.7, 
               arrow = arrow(length = unit(.3, 'cm'))) +
  geom_segment(aes(x = 30, y = K_30_2000$Kl_depth, xend = Kl_X2_2100, yend = K_30_2000$Kl_depth),
               colour = "firebrick2", lwd = 0.7, lty = "dashed") +
  geom_segment(aes(x = Kl_X2_2100, y = K_30_2000$Kl_depth, xend = Kl_X2_2100, yend = 0),
               colour = "firebrick2", lwd = 0.7,
               arrow = arrow(length = unit(.3, 'cm'))) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  ylim(c(0,35))

Kl_plot

Ku_plot <- Kpredict %>%  
  ggplot(aes(x = Depth, y = Ku_depth, colour = as.factor(X))) + 
  geom_line(lwd = 1) +
  labs(x = "", y = "", colour = "Year", title = "upper") +
  geom_segment(aes(x = 40, y = 0, xend = 40, yend = K_40_1950$Ku_depth), 
               colour = "firebrick1", lwd = 0.7, 
               arrow = arrow(length = unit(.3, 'cm'))) +
  geom_segment(aes(x = 40, y = K_40_1950$Ku_depth, xend = Ku_X_2000, yend = K_40_1950$Ku_depth),
               colour = "firebrick2", lwd = 0.7, lty = "dashed") +
  geom_segment(aes(x = Ku_X_2000, y = K_40_1950$Ku_depth, xend = Ku_X_2000, yend = 0),
               colour = "firebrick2", lwd = 0.7,
               arrow = arrow(length = unit(.3, 'cm'))) +
  geom_segment(aes(x = 30, y = 0, xend = 30, yend = K_30_2000$Ku_depth), 
               colour = "firebrick1", lwd = 0.7, 
               arrow = arrow(length = unit(.3, 'cm'))) +
  geom_segment(aes(x = 30, y = K_30_2000$Ku_depth, xend = Ku_X2_2100, yend = K_30_2000$Ku_depth),
               colour = "firebrick2", lwd = 0.7, lty = "dashed") +
  geom_segment(aes(x = Ku_X2_2100, y = K_30_2000$Ku_depth, xend = Ku_X2_2100, yend = 0),
               colour = "firebrick2", lwd = 0.7,
               arrow = arrow(length = unit(.3, 'cm'))) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  ylim(c(0,35))

Ku_plot

Kfig <- K_time_plot /
(Kl_plot | Km_plot | Ku_plot) + plot_layout(guides = "collect")


ggsave("./Figures/K_overview.png", Kfig)

X1 = 2100
X2 = 2000
# Adjusting depth -> Find "conversion factor" by dividing Kfuture ong Kpresent
# Depth equivalent for year 2100, assuming that the present day picture (model)
# is adequately represented by K in year 2000,
# would for instance be measuring depth multiplied by
Yearextract$Km2[Yearextract$X == X1]/Yearextract$Km2[Yearextract$X == X2]
# Like this
data.frame(Depth = c(1:40)) %>% 
  mutate(Depthekv = Depth*Yearextract$Km2[Yearextract$X == X1]/Yearextract$Km2[Yearextract$X == X2]) %>% 
  ggplot(aes(x = Depth, y = Depthekv)) +
  geom_line()


