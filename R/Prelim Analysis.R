####################################################################################################
###
### Hematocrit Project - Testerone: Bryant Dossman, Chris Tonra, Pete Marra
###
####################################################################################################

## Read in Data
hem <- read.csv("./data/Hematocrit_Tonra.csv")

## Libraries
library(tidyverse)
library(lmodel2)

####################################################################################################
# Calculating Peig and Green (2009) condition indice for each bird
####################################################################################################
# creating column to store scaled mass index variables

## Step 1, visualizing the relationship between mass and body size (WING or Tarsus or PCA(BOTH))
# removing species with less than 20 observations...this will likely distort the relationship
# removing any outliers from this analysis... birds captured towards close to departure, birds with e
# exceptionally large tarsus or mass

amre <- hem %>% 
  group_by(species) %>% 
  filter (n() > 20, species=="AMRE", juldate < 100, tarsus < 18) %>% data.frame()

# calculating the slope of the sma regression with the log of both mass and tarsus

amre.exp <- lmodel2(log(mass)~log(tarsus), amre)$regression.results[3,3]

amre.tarsus.mean <- mean(amre$tarsus, na.rm=T)

# now using the slope along with mean tarsus size to estimate the body condiiton 
# (scaled mass) of each individual in the original analysis

final <- NULL
tmp <- hem %>% filter(species=="AMRE", !is.na(tarsus), !is.na(mass)) %>% 
  mutate(bc = mass*((amre.tarsus.mean/tarsus))^amre.exp) %>% data.frame()
final <- rbind(final,tmp)

## now repeating the above process for the top 4 species

# BAWW

baww <- hem %>% 
  group_by(species) %>% 
  filter (n() > 20, species=="BAWW") %>% 
  data.frame()

ggplot(data = baww,
       aes(x=tarsus, y=mass)) + 
  geom_point() + stat_smooth(method="lm", formula = y~exp(x)) + ylim(6,14) + xlim(15,19)


baww.exp <- lmodel2(log(mass)~log(tarsus), baww)$regression.results[3,3]

baww.tarsus.mean <- mean(baww$tarsus, na.rm=T)

tmp <- hem %>% filter(species=="BAWW", !is.na(tarsus), !is.na(mass)) %>% 
  mutate(bc = mass*((baww.tarsus.mean/tarsus))^baww.exp) %>% data.frame()

final <- rbind(final,tmp)

# OVEN
oven <- hem %>% 
  group_by(species) %>% 
  filter (species=="OVEN", juldate < 90, mass > 12) %>% 
  data.frame()

ggplot(data = oven,
       aes(x=tarsus, y=mass)) + 
  geom_point() + stat_smooth(method="lm", formula = y~exp(x))


oven.exp <- lmodel2(log(mass)~log(tarsus), oven)$regression.results[3,3]

oven.tarsus.mean <- mean(oven$tarsus, na.rm=T)

tmp <- hem %>% filter(species=="OVEN", !is.na(tarsus), !is.na(mass)) %>% 
  mutate(bc = mass*((oven.tarsus.mean/tarsus))^oven.exp) %>% data.frame()

final <- rbind(final,tmp)

# NOWA
nowa <- hem %>% 
  group_by(species) %>% 
  filter (species=="NOWA") %>% 
  data.frame()

ggplot(data = nowa,
       aes(x=tarsus, y=mass)) + 
  geom_point() + stat_smooth(method="lm", formula = y~exp(x))


nowa.exp <- lmodel2(log(mass)~log(tarsus), nowa)$regression.results[3,3]

nowa.tarsus.mean <- mean(nowa$tarsus, na.rm=T)

tmp <- hem %>% filter(species=="NOWA", !is.na(tarsus), !is.na(mass)) %>% 
  mutate(bc = mass*((nowa.tarsus.mean/tarsus))^nowa.exp) %>% data.frame()

final <- rbind(final,tmp)

final$forage.guild <- ifelse(final$species %in% c("AMRE","BAWW"), "non-ground", "ground")

### Plotting relationships between variables of

summary(lm(hematocrit~bs14+species, data=final))










