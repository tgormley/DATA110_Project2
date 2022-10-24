# DATA110_Project2

---
title: "Project2"
author: "Tycho Gormley"
date: "7/7/2021"
output: html_document
---

# Exoplanets
## In this visualization, I will attempt to correlate relationships between aspects of planets and stars.
## I am using the Open Exoplanet Catalogue from kaggle.com: https://www.kaggle.com/mrisdal/open-exoplanet-catalogue
## Primary Variables used: 
### PlanetIdentifier: name/code of the planet
### PlanetaryMassJpt: Mass of the planet (measured in Jupiter masses)
### Eccentricity: measure of which the planet's orbit deviates from a circle
### SurfaceTempK: Surface Temperature of the Planet (in degrees Kelvin)
### DiscoveryMethod: Method by which planet was discovered
### HostStarMassSlrMass: Mass of the star (measured in solar masses)
### HostStarMetallicity: Measure of the abundance of heavy elements present inside the star (measured in Fe/H)
#### If Fe/H=0, the star has the same abundance of iron as the Sun. If Fe/H = -1, it has 1/10th of the sun's value and vice versa.
### HostStarTempK: Temperature of the Star (measured in degrees Kelvin)
### SemiMajorAxisAU: The major axis of an orbit at its longest diameter (measured in AU(distance from Earth to Sun))

```{r}
#install.packages("scatterplot3d")
#install.packages("rgl")
#install.packages("contourPlot")
```

```{r}
# Load necessary packages
library(tidyverse)
library(plotly)
library(readr)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(ggrepel)
library(contourPlot)
library(rgl)
library(plot3D)
library(knitr)
library(scatterplot3d)
library(gridExtra)
```

```{r}
# Link .csv file in directory to R Studio
setwd("C:/Users/tycho/Desktop/DATA110")
exoplanets <- read_csv("oec.csv")

head(exoplanets)
# View & load the data
view(exoplanets)
exoplanets
```

```{r}
# Examine datafram structure
str(exoplanets)
```

```{r}
# Box plot for Star temperature
  ggp1 <- ggplot(exoplanets) +
  geom_boxplot(aes(x = HostStarTempK)) +
  xlab("Star Temperature (degrees K)") +
  ggtitle("Star Temperature Distribution")

# Box plot for Exoplanet Temperature
  ggp2 <- ggplot(exoplanets) +
  geom_boxplot(aes(x = SurfaceTempK)) +
  xlab("Exoplanet Temperature (in degrees K)") +
  ggtitle("Exoplanet Temperature Distribution")
  
  grid.arrange(ggp1, ggp2, ncol = 2)
```

```{r}
# Box plot for Star Mass
  ggp3 <- ggplot(exoplanets) +
  geom_boxplot(aes(x = HostStarMassSlrMass)) +
  xlab("Star Mass (in Solar Masses)") +
  ggtitle("Star Mass Distribution")

# Box plot for Exoplanet Mass
 ggp4 <-  ggplot(exoplanets) +
  geom_boxplot(aes(x = PlanetaryMassJpt)) +
  xlab("Exoplanet Mass (in Jupiter Masses)") +
  ggtitle("Exoplanet Mass Distribution")

grid.arrange(ggp3, ggp4, ncol = 2)
```



```{r}
# Scatter plot comparing Star Mass with Exoplanet Mass
exoplanets_scatter <- ggplot(exoplanets, aes(x = PlanetaryMassJpt, y = HostStarMassSlrMass,)) + 
  ggtitle("Planet Mass vs Star Mass") + 
  xlab("Planet Mass (in Jupiter Masses)") + 
  ylab("Star Mass (in Solar Masses") + 
  geom_point(aes(color=DiscoveryMethod), size = 0.5, alpha = 0.5)
ggplotly(exoplanets_scatter)
```


## Now without significant outliers:
```{r}
e <- exoplanets %>% 
  filter(PlanetaryMassJpt<5)
  
e %>%  
  
  ggplot(aes(x = PlanetaryMassJpt, y = HostStarMassSlrMass)) + 
  ggtitle("Planet Mass vs Star Mass") + 
  xlab("Planet Mass (in Jupiter Masses)") + 
  ylab("Star Mass (in Solar Masses") + 
  geom_point(aes(color=DiscoveryMethod), size = 0.5, alpha = 0.5)
```


## Now all planets with lower than 0.25 Jupiter Masses
```{r}
e <- exoplanets %>% 
  filter(PlanetaryMassJpt<0.25)
  
e %>%  
  
  ggplot(aes(x = PlanetaryMassJpt, y = HostStarMassSlrMass)) + 
  ggtitle("Planet Mass vs Star Mass") + 
  xlab("Planet Mass (in Jupiter Masses)") + 
  ylab("Star Mass (in Solar Masses") + 
  geom_point(aes(color=DiscoveryMethod), size = 1, alpha = 0.5) + 
  geom_smooth(method = lm, se = FALSE)
```


```{r}
# Scatter plot comparing Star Metallicity with Planet Mass
e_metal <- exoplanets %>% 
  filter(PlanetaryMassJpt<10)
  
e_metal %>%  
  
  ggplot(aes(x = PlanetaryMassJpt, y = HostStarMetallicity)) + 
  ggtitle("Planet Mass vs Star Metallicity") + 
  xlab("Planet Mass (in Jupiter Masses)") + 
  ylab("Star Metallicity (in [Fe/H]") + 
  geom_point(aes(color=DiscoveryMethod), size = 1, alpha = 0.5) + 
  geom_smooth(method = lm, se = FALSE)
```

## And again for planets with lower than 0.25 Jupiter Masses
```{r}
e_metal <- exoplanets %>% 
  filter(PlanetaryMassJpt<0.25)
  
e_metal %>%  
  
  ggplot(aes(x = PlanetaryMassJpt, y = HostStarMetallicity)) + 
  ggtitle("Planet Mass vs Star Metallicity") + 
  xlab("Planet Mass (in Jupiter Masses)") + 
  ylab("Star Metallicity (in [Fe/H]") + 
  geom_point(aes(color=DiscoveryMethod), size = 1, alpha = 0.5) + 
  geom_smooth(method = lm, se = FALSE)
```







```{r}
#3d Scatter plot comparing planet and star temperature
library(rgl)
exoplanets %>%
  filter(SemiMajorAxisAU<10)
  
mycolors <- c('royalblue1', 'darkcyan', 'oldlace', 'deeppink')
exoplanets$color <- mycolors[ as.numeric(exoplanets$"DiscoveryMethod") ]

plot3d(
  x=exoplanets$"SurfaceTempK", y=exoplanets$"HostStarTempK",
  col = exoplanets$color,
  type = "s",
  radius = 100,
  xlab="Planet Temperature (in degrees K)",  
  ylab="Star Temperature (in degrees K)"
)
rglwidget()

options(rgl.printRglwidget = TRUE)
```









```{r}
#3d Scatter plot comparing Planet Temperature, Star Temperature and Semi-major Axis
library(rgl)

mycolors <- c('royalblue1', 'darkcyan', 'oldlace', 'deeppink')
exoplanets$color <- mycolors[ as.numeric(exoplanets$"DiscoveryMethod") ]

plot3d(
  x=exoplanets$"SurfaceTempK", y=exoplanets$"HostStarTempK", z=exoplanets$"HostStarMetallicity",
  col = exoplanets$color,
  type = "s",
  radius = 100,
  xlab="Planet Temperature (in degrees K)",  
  ylab="Star Temperature (in degrees K)", 
  zlab="Star Metallicity (in Fe/H)"
)
rglwidget()

options(rgl.printRglwidget = TRUE)
```



#### For this visualization I chose the exoplanets dataset. This dataset lists all known exoplanets (as of June 8th, 2017) and their characteristics such as mass, surface temperature, eccentricity, as well as characteristics about their host stars such as temperature and metallicity. The data came from kaggle.com, a website where many such datasets are accessible. I chose this topic/dataset because I have always been interested in space and the many exoplanets which are still being discovered to this day. From this, I came to realize just how big the universe is and all the worlds out there that are just waiting to be discovered. One of the variables I used for my legends is Discovery Method (the method the exoplanets were discovered). This way I could observe if there are any trends in the data based off how the planets/stars were discovered.*

#### For the data, I first created boxplots for both temperature and mass of exoplanets and their host stars so I could compare the two as well as locate the general distribution and outliers. I found that stars and exoplanets are closely correlated in temperature but not as much so in terms of mass. Then I created a scatterplot comparing star mass and planet mass. The trend becomes easier to see when outliers are omitted, planet mass is positively correlated with star mass when looking at planet masses lower than 0.25 Jupiters. However the current data is biased in this regard, considering the fact that it is much easier to detect high mass planets orbiting low mass stars. In reality, we would see far more datapoints in the lower left section of the graph. Then I attempted to correlate planet mass with the star metallicity by the assumption that more metallic stars should produce planets that are more massive. Again, the positive trend was difficult to establish unless outliers were removed. Next I made a 3d scatterplots, one for Star Temperature & Planet Temperature and another for Star & Planet Temperature and Star Metallicity. One of the overarching issues that persisted throughout these visualizations is the bias that is present in the data: There are many higher mass planets being discovered orbiting around small, cooler stars since they are much easier to discover. If the data contained the real plot points for all the stars in the galaxy, it's likely there would be a massive abundance of low mass planets. This I cannot say for certain that any of the assumptions I made based off these visualizations would be proved.

#### *https://earthsky.org/space/how-do-astronomers-discover-exoplanets/













