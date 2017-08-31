####################################################################################################
###
### Hematocrit Project - Testerone: Bryant Dossman, Chris Tonra, Pete Marra
###
####################################################################################################

## Read in Data
hem <- read.csv("./data/Hematocrit_Tonra.csv")

## Libraries
library(tidyverse)

ggplot(data=hem %>% filter(species=="AMRE") %>% group_by(species) %>% filter(n() > 100, juldate > 50), aes(x=juldate, y=hematocrit, colour=species)) + 
  geom_point() + 
  stat_smooth()


