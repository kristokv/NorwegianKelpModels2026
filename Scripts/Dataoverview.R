#####################################################
## The Norwegian Kelp Models # take two
## Data wrangling 
## Guri Sogn Andersen, 2022
#####################################################

# This work is largely based on the models made in the Nordic Blue Carbon project (2020): http://pub.norden.org/temanord2020-541/#47359

# Packages ----------------------------------------------------------------

require(tidyverse)
require(DataExplorer)

# Observational data and environment --------------------------------------

lamhyp_0 <- read.csv("./Data/lamhyp_moddf.csv")
saclat_0 <- read.csv("./Data/saclat_moddf.csv")

# Auto-generate data reports 
DataExplorer::create_report(lamhyp_0, output_file = "Datareport_Lamhyp.html")
DataExplorer::create_report(saclat_0, output_file = "Datareport_Saclat.html")

# Be aware of Depth_HGU in saclat:
saclat_0$Depth_HGU %>% summary() # -9999 skal være NA

# Which observations
saclat_0 %>% filter(Depth_HGU < -100 & Coverage > 0) 

# What does depth look like when comparing HGU to field-recordings
saclat_0 %>%
  filter(Depth_HGU != -9999) %>% 
  ggplot(aes(x = Depth, y = Depth_HGU)) +
  geom_point() # OK

lamhyp_0 %>%
  ggplot(aes(x = Depth, y = Depth_HGU)) +
  geom_point() # OK

# Replace with values from DEM? 
# Or maybe better to replace with NA since BRT handles missing values?

saclat <- saclat_0 %>% 
  add_column(Depth_mod = ifelse(saclat_0$Depth_HGU == -9999, NA, saclat_0$Depth_HGU), .after = "Depth_HGU") %>% 
  add_column(Year_HGU = saclat_0$Year, .after = "Year")

lamhyp <- lamhyp_0 %>% 
  add_column(Depth_mod = lamhyp_0$Depth_HGU, .after = "Depth_HGU")

# Unique coordinates, used to extract values for additinal historic layers

pos.lam <- lamhyp %>% dplyr::select(Y, X, Year_HGU, BO2_tempmean_bdmin) %>% distinct(.)
pos.sac <- saclat %>% dplyr::select(Y, X, Year_HGU,BO2_tempmean_bdmin) %>% distinct(.)

obs.points <- full_join(pos.lam, pos.sac) %>% distinct(.)
write.csv(obs.points, file = "./Data/obs.points.csv")

# Additional variables in 2022 --------------------------------------------

Urchindata <- read.csv("./Data/Urchindata.csv", stringsAsFactors = FALSE)

saclat2022 <- saclat %>% left_join(Urchindata)
lamhyp2022 <- lamhyp %>% left_join(Urchindata)

write.csv(saclat2022, file = "./Data/saclat2022.csv", row.names = FALSE)
write.csv(lamhyp2022, file = "./Data/lamhyp2022.csv", row.names = FALSE)
