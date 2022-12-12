# set wd

setwd("/Users/anacatarina/Desktop/catarina/TGI/Global gender gap/Data")

# load packages
library(gtools)
library(reshape)
library(ggplot2)
library(lattice)
library(boot)
library(agricolae)
library(Hmisc)
library(countrycode)
library(dplyr)
library(tidyverse)
library(rgdal)
library(gridExtra)
library(grid)
library(choroplethr)
library(choroplethrMaps)
library(maps)
library(mapdata)
library(rworldmap)
library(RColorBrewer)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgdal)
library(gridExtra)
library(grid)
library(ggridges)
library(lme4)
library(reshape2)


## read datasets 

female.le <- read.csv("female.le.csv")
male.le <- read.csv("male.le.csv")

life <- merge(female.le, male.le, by="code")

global <- read.csv("global.csv")
econ <- read.csv("econ.csv")
educ <- read.csv("educ.csv")
polit <- read.csv("polit.csv")

global <- merge(global, life, by="code")
econ <- merge(econ, life, by="code")
educ <- merge(educ, life, by="code")
polit <- merge(polit, life, by="code")

head(global)


## global gender gap 

global$sex.d2010 <- global$X2010.x-global$X2010.y


global$sex.d2020 <- global$X2020.x-global$X2020.y


global$sex.d2019 <- global$X2019.x-global$X2019.y


global$fle.d <- global$X2020.x-global$X2010.x
global$mle.d <- global$X2020.y-global$X2010.y


global$change.d <- global$sex.d2020 - global$sex.d2010
global$change.d19 <- global$sex.d2019 - global$sex.d2010

global$index <- global$X2020 - global$X2010



global$region.wb <- countrycode(
  sourcevar=global$code,
  origin = "iso3c",
  destination = "region",
  warn = TRUE,
  nomatch = NA,
  custom_dict = NULL,
  custom_match = NULL,
  origin_regex = NULL
)

global$region.un <- countrycode(
  sourcevar=global$code,
  origin = "iso3c",
  destination = "un.region.name",
  warn = TRUE,
  nomatch = NA,
  custom_dict = NULL,
  custom_match = NULL,
  origin_regex = NULL
)


model1 <- lm(change.d~index, data=global)
summary(model1)

model2 <- lm(change.d19~index, data=global)
summary(model2)

model1 <- lm(fle.d~index, data=global)
summary(model1)

model1 <- lm(mle.d~index, data=global)
summary(model1)



# analysis by region

model3 <- lm(change.d~index, data=global[global$region.wb=="Europe & Central Asia",])
summary(model3)

model3 <- lm(change.d~index, data=global[global$region.wb=="Sub-Saharan Africa",])
summary(model3)

model3 <- lm(change.d~index, data=global[global$region.wb=="South Asia",])
summary(model3)

model3 <- lm(change.d~index, data=global[global$region.wb=="Latin America & Caribbean",])
summary(model3)

model3 <- lm(change.d~index, data=global[global$region.wb=="Middle East & North Africa",])
summary(model3)

model3 <- lm(change.d~index, data=global[global$region.wb=="East Asia & Pacific",])
summary(model3)

model3 <- lm(change.d~index, data=global[global$region.un=="Europe",])
summary(model3)

model3 <- lm(change.d~index, data=global[global$region.un=="Africa",])
summary(model3)

model3 <- lm(change.d~index, data=global[global$region.un=="Asia",])
summary(model3)

model3 <- lm(change.d~index, data=global[global$region.un=="Americas",])
summary(model3)

model3 <- lm(change.d~index, data=global[global$region.un=="Oceania",])
summary(model3)


## Descriptive analysis

summary(global$X2010)
summary(global$X2021)

summary(econ$X2010)
summary(econ$X2021)

summary(educ$X2010)
summary(educ$X2021)

summary(polit$X2010)
summary(polit$X2021)

summary(global$X2010.x)
summary(global$X2020.x)

summary(global$X2010.y)
summary(global$X2020.y)





## trends over time

global.long <- melt(global, id.vars = c("code"), measure.vars = c(6:16), variable.name = "year", value.name="gggi")

econ.long <- melt(econ, id.vars = c("code"), measure.vars = c(6:16), variable.name = "year", value.name="econ")

educ.long <- melt(educ, id.vars = c("code"), measure.vars = c(6:16), variable.name = "year", value.name="educ")

polit.long <- melt(polit, id.vars = c("code"), measure.vars = c(6:16), variable.name = "year", value.name="polit")

female.long <- melt(global, id.vars = c("code"), measure.vars = c(68:78), variable.name = "year", value.name="female")

male.long <- melt(global, id.vars = c("code"), measure.vars = c(130:140), variable.name = "year", value.name="male")

global.long$year <- as.factor(global.long$year)

global.long$year<-gsub("X","",as.character(global.long$year))

global.long$year <- as.numeric(global.long$year)

data <- data.frame(global.long, female.long, male.long)

data <- data[,c(1:3,6,9)]

data1 <- aggregate(gggi~year, data=global.long, FUN=mean)

data4 <- aggregate(econ~year, data=econ.long, FUN=mean)

data5 <- aggregate(educ~year, data=educ.long, FUN=mean)

data6 <- aggregate(polit~year, data=polit.long, FUN=mean)

data2 <- aggregate(female~year, data=female.long, FUN=mean)

data3 <- aggregate(male~year, data=male.long, FUN=mean)

data <- data.frame(data1, data2[,2], data3[,2], data4[,2], data5[,2], data6[,2])

colnames(data) <- c("year", "gggi", "female", "male", "econ", "educ", "polit")

ggplot(data = data, aes(x = year))+
  geom_line(aes(y = gggi*100), colour = "cornflowerblue", size=1) +
  geom_line(aes(y = econ*100), colour = "cadetblue3", size=1) +
  geom_line(aes(y = educ*100), colour = "cadetblue3",  size=1) +
  geom_line(aes(y = polit*100), colour = "cadetblue3", size=1) +
  geom_line(aes(y = female), colour = "darkorange2", size=1, linetype="dashed") +
  geom_line(aes(y = male), colour="darkolivegreen3", size=1, linetype="dashed") +
  theme_classic() + 
  scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Life expectancy"), name="Global Gender Gap Index (modified)", 
                     limits = c(0, 100), breaks=c(0,25,50,75,100)) +
  xlab("Year") +
  scale_x_continuous(limits = c(2010,2021), breaks = c(2010,2012,2014,2016,2018,2020)) +
  geom_text(x=2020, y=93, label="Education subindex", color="cadetblue3") +
  geom_text(x=2020, y=58, label="mGGGI", color="cornflowerblue") +
  geom_text(x=2020, y=63, label="Economic subindex", color="cadetblue3") +
  geom_text(x=2020, y=18, label="Political subindex", color="cadetblue3") +
  geom_text(x=2020, y=73, label="Women's LE", color="darkorange2") +
  geom_text(x=2020, y=68, label="Men's LE", color="darkolivegreen3")
  


## Analysis by region

countries <- read.csv("countries.csv")

countries$code <- countrycode(
  sourcevar=countries$country,
  origin = "country.name",
  destination = "iso3c",
  warn = TRUE,
  nomatch = NA,
  custom_dict = NULL,
  custom_match = NULL,
  origin_regex = NULL
)


global <- merge(global, countries, by="code")
econ <- merge(econ, countries, by="code")
educ <- merge(educ, countries, by="code")
polit <- merge(polit, countries, by="code")


model3 <- lm(change.d~index, data=global[global$region=="South and Southeast Asia and Oceania",])
summary(model3)

model3 <- lm(change.d~index, data=global[global$region=="Sub-Saharan Africa",])
summary(model3)

model3 <- lm(change.d~index, data=global[global$region=="Central Asia and Central and Eastern Europe",])
summary(model3)

model3 <- lm(change.d~index, data=global[global$region=="North Africa and Middle East",])
summary(model3)

model3 <- lm(change.d~index, data=global[global$region=="Latin America and the Caribbean",])
summary(model3)

model3 <- lm(change.d~index, data=global[global$region=="High-income countries",])
summary(model3)


## Cross sectional analysis

model4 <- lm(X2020.x~X2021, data=global)
summary(model4)

model4 <- lm(X2020.y~X2021, data=global)
summary(model4)

model4 <- lm(sex.d2020~X2021, data=global)
summary(model4)


# analysis by regions

model3 <- lm(sex.d2020~X2021, data=global[global$region=="South and Southeast Asia and Oceania",])
summary(model3)

model3 <- lm(sex.d2020~X2021, data=global[global$region=="Sub-Saharan Africa",])
summary(model3)

model3 <- lm(sex.d2020~X2021, data=global[global$region=="Central Asia and Central and Eastern Europe",])
summary(model3)

model3 <- lm(sex.d2020~X2021, data=global[global$region=="North Africa and Middle East",])
summary(model3)

model3 <- lm(sex.d2020~X2021, data=global[global$region=="Latin America and the Caribbean",])
summary(model3)

model3 <- lm(sex.d2020~X2021, data=global[global$region=="High-income countries",])
summary(model3)


model3 <- lm(X2020.x~X2021, data=global[global$region=="South and Southeast Asia and Oceania",])
summary(model3)

model3 <- lm(X2020.x~X2021, data=global[global$region=="Sub-Saharan Africa",])
summary(model3)

model3 <- lm(X2020.x~X2021, data=global[global$region=="Central Asia and Central and Eastern Europe",])
summary(model3)

model3 <- lm(X2020.x~X2021, data=global[global$region=="North Africa and Middle East",])
summary(model3)

model3 <- lm(X2020.x~X2021, data=global[global$region=="Latin America and the Caribbean",])
summary(model3)

model3 <- lm(X2020.x~X2021, data=global[global$region=="High-income countries",])
summary(model3)


model3 <- lm(X2020.y~X2021, data=global[global$region=="South and Southeast Asia and Oceania",])
summary(model3)

model3 <- lm(X2020.y~X2021, data=global[global$region=="Sub-Saharan Africa",])
summary(model3)

model3 <- lm(X2020.y~X2021, data=global[global$region=="Central Asia and Central and Eastern Europe",])
summary(model3)

model3 <- lm(X2020.y~X2021, data=global[global$region=="North Africa and Middle East",])
summary(model3)

model3 <- lm(X2020.y~X2021, data=global[global$region=="Latin America and the Caribbean",])
summary(model3)

model3 <- lm(X2020.y~X2021, data=global[global$region=="High-income countries",])
summary(model3)


## Economic subindex

econ$sex.d2020 <- econ$X2020.x-econ$X2020.y

model3 <- lm(sex.d2020~X2021, data=econ[econ$region=="South and Southeast Asia and Oceania",])
summary(model3)

model3 <- lm(sex.d2020~X2021, data=econ[econ$region=="Sub-Saharan Africa",])
summary(model3)

model3 <- lm(sex.d2020~X2021, data=econ[econ$region=="Central Asia and Central and Eastern Europe",])
summary(model3)

model3 <- lm(sex.d2020~X2021, data=econ[econ$region=="North Africa and Middle East",])
summary(model3)

model3 <- lm(sex.d2020~X2021, data=econ[econ$region=="Latin America and the Caribbean",])
summary(model3)

model3 <- lm(sex.d2020~X2021, data=econ[econ$region=="High-income countries",])
summary(model3)


model3 <- lm(X2020.x~X2021, data=econ[econ$region=="South and Southeast Asia and Oceania",])
summary(model3)

model3 <- lm(X2020.x~X2021, data=econ[econ$region=="Sub-Saharan Africa",])
summary(model3)

model3 <- lm(X2020.x~X2021, data=econ[econ$region=="Central Asia and Central and Eastern Europe",])
summary(model3)

model3 <- lm(X2020.x~X2021, data=econ[econ$region=="North Africa and Middle East",])
summary(model3)

model3 <- lm(X2020.x~X2021, data=econ[econ$region=="Latin America and the Caribbean",])
summary(model3)

model3 <- lm(X2020.x~X2021, data=econ[econ$region=="High-income countries",])
summary(model3)


model3 <- lm(X2020.y~X2021, data=econ[econ$region=="South and Southeast Asia and Oceania",])
summary(model3)

model3 <- lm(X2020.y~X2021, data=econ[econ$region=="Sub-Saharan Africa",])
summary(model3)

model3 <- lm(X2020.y~X2021, data=econ[econ$region=="Central Asia and Central and Eastern Europe",])
summary(model3)

model3 <- lm(X2020.y~X2021, data=econ[econ$region=="North Africa and Middle East",])
summary(model3)

model3 <- lm(X2020.y~X2021, data=econ[econ$region=="Latin America and the Caribbean",])
summary(model3)

model3 <- lm(X2020.y~X2021, data=econ[econ$region=="High-income countries",])
summary(model3)


## Education subindex

educ$sex.d2020 <- educ$X2020.x-educ$X2020.y

model3 <- lm(sex.d2020~X2021, data=educ[educ$region=="South and Southeast Asia and Oceania",])
summary(model3)

model3 <- lm(sex.d2020~X2021, data=educ[educ$region=="Sub-Saharan Africa",])
summary(model3)

model3 <- lm(sex.d2020~X2021, data=educ[educ$region=="Central Asia and Central and Eastern Europe",])
summary(model3)

model3 <- lm(sex.d2020~X2021, data=educ[educ$region=="North Africa and Middle East",])
summary(model3)

model3 <- lm(sex.d2020~X2021, data=educ[educ$region=="Latin America and the Caribbean",])
summary(model3)

model3 <- lm(sex.d2020~X2021, data=educ[educ$region=="High-income countries",])
summary(model3)


model3 <- lm(X2020.x~X2021, data=educ[educ$region=="South and Southeast Asia and Oceania",])
summary(model3)

model3 <- lm(X2020.x~X2021, data=educ[educ$region=="Sub-Saharan Africa",])
summary(model3)

model3 <- lm(X2020.x~X2021, data=educ[educ$region=="Central Asia and Central and Eastern Europe",])
summary(model3)

model3 <- lm(X2020.x~X2021, data=educ[educ$region=="North Africa and Middle East",])
summary(model3)

model3 <- lm(X2020.x~X2021, data=educ[educ$region=="Latin America and the Caribbean",])
summary(model3)

model3 <- lm(X2020.x~X2021, data=educ[educ$region=="High-income countries",])
summary(model3)


model3 <- lm(X2020.y~X2021, data=educ[educ$region=="South and Southeast Asia and Oceania",])
summary(model3)

model3 <- lm(X2020.y~X2021, data=educ[educ$region=="Sub-Saharan Africa",])
summary(model3)

model3 <- lm(X2020.y~X2021, data=educ[educ$region=="Central Asia and Central and Eastern Europe",])
summary(model3)

model3 <- lm(X2020.y~X2021, data=educ[educ$region=="North Africa and Middle East",])
summary(model3)

model3 <- lm(X2020.y~X2021, data=educ[educ$region=="Latin America and the Caribbean",])
summary(model3)

model3 <- lm(X2020.y~X2021, data=educ[educ$region=="High-income countries",])
summary(model3)



## Political subindex

polit$sex.d2020 <- polit$X2020.x-polit$X2020.y

model3 <- lm(sex.d2020~X2021, data=polit[polit$region=="South and Southeast Asia and Oceania",])
summary(model3)

model3 <- lm(sex.d2020~X2021, data=polit[polit$region=="Sub-Saharan Africa",])
summary(model3)

model3 <- lm(sex.d2020~X2021, data=polit[polit$region=="Central Asia and Central and Eastern Europe",])
summary(model3)

model3 <- lm(sex.d2020~X2021, data=polit[polit$region=="North Africa and Middle East",])
summary(model3)

model3 <- lm(sex.d2020~X2021, data=polit[polit$region=="Latin America and the Caribbean",])
summary(model3)

model3 <- lm(sex.d2020~X2021, data=polit[polit$region=="High-income countries",])
summary(model3)


model3 <- lm(X2020.x~X2021, data=polit[polit$region=="South and Southeast Asia and Oceania",])
summary(model3)

model3 <- lm(X2020.x~X2021, data=polit[polit$region=="Sub-Saharan Africa",])
summary(model3)

model3 <- lm(X2020.x~X2021, data=polit[polit$region=="Central Asia and Central and Eastern Europe",])
summary(model3)

model3 <- lm(X2020.x~X2021, data=polit[polit$region=="North Africa and Middle East",])
summary(model3)

model3 <- lm(X2020.x~X2021, data=polit[polit$region=="Latin America and the Caribbean",])
summary(model3)

model3 <- lm(X2020.x~X2021, data=polit[polit$region=="High-income countries",])
summary(model3)


model3 <- lm(X2020.y~X2021, data=polit[polit$region=="South and Southeast Asia and Oceania",])
summary(model3)

model3 <- lm(X2020.y~X2021, data=polit[polit$region=="Sub-Saharan Africa",])
summary(model3)

model3 <- lm(X2020.y~X2021, data=polit[polit$region=="Central Asia and Central and Eastern Europe",])
summary(model3)

model3 <- lm(X2020.y~X2021, data=polit[polit$region=="North Africa and Middle East",])
summary(model3)

model3 <- lm(X2020.y~X2021, data=polit[polit$region=="Latin America and the Caribbean",])
summary(model3)

model3 <- lm(X2020.y~X2021, data=polit[polit$region=="High-income countries",])
summary(model3)



### Longitudinal 

## Economic domain

econ$sex.d2010 <- econ$X2010.x-econ$X2010.y

econ$sex.d2020 <- econ$X2020.x-econ$X2020.y

econ$fle.d <- econ$X2020.x-econ$X2010.x
econ$mle.d <- econ$X2020.y-econ$X2010.y

econ$change.d <- econ$sex.d2020 - econ$sex.d2010

econ$index <- econ$X2020 - econ$X2010

model4 <- lm(X2020.x~X2021, data=econ)
summary(model4)

model4 <- lm(X2020.y~X2021, data=econ)
summary(model4)

model4 <- lm(sex.d2020~X2021, data=econ)
summary(model4)


model1 <- lm(change.d~index, data=econ)
summary(model1)

model1 <- lm(fle.d~index, data=econ)
summary(model1)

model1 <- lm(mle.d~index, data=econ)
summary(model1)


## Education domain

educ$sex.d2010 <- educ$X2010.x-educ$X2010.y

educ$sex.d2020 <- educ$X2020.x-educ$X2020.y

educ$fle.d <- educ$X2020.x-educ$X2010.x
educ$mle.d <- educ$X2020.y-educ$X2010.y

educ$change.d <- educ$sex.d2020 - educ$sex.d2010

educ$index <- educ$X2020 - educ$X2010

model4 <- lm(X2020.x~X2021, data=educ)
summary(model4)

model4 <- lm(X2020.y~X2021, data=educ)
summary(model4)

model4 <- lm(sex.d2020~X2021, data=educ)
summary(model4)


model1 <- lm(change.d~index, data=educ)
summary(model1)

model1 <- lm(fle.d~index, data=educ)
summary(model1)

model1 <- lm(mle.d~index, data=educ)
summary(model1)




## Political domain

polit$sex.d2010 <- polit$X2010.x-polit$X2010.y

polit$sex.d2020 <- polit$X2020.x-polit$X2020.y

polit$fle.d <- polit$X2020.x-polit$X2010.x
polit$mle.d <- polit$X2020.y-polit$X2010.y

polit$change.d <- polit$sex.d2020 - polit$sex.d2010

polit$index <- polit$X2020 - polit$X2010

model4 <- lm(X2020.x~X2021, data=polit)
summary(model4)

model4 <- lm(X2020.y~X2021, data=polit)
summary(model4)

model4 <- lm(sex.d2020~X2021, data=polit)
summary(model4)


model1 <- lm(change.d~index, data=polit)
summary(model1)

model1 <- lm(fle.d~index, data=polit)
summary(model1)

model1 <- lm(mle.d~index, data=polit)
summary(model1)


## Figures

ggplot(global, aes(x=X2021*100, y=sex.d2020, colour=region)) + geom_point(size=2) + 
  geom_smooth(method="lm", colour="black") + theme_classic() + 
  ylab("Gender gap in  life expectancy") +
  xlab("Global gender gap index") +
  labs(colour="World region")

ggplot(global, aes(x=X2021*100, y=X2020.x, colour=region)) + geom_point(size=2) + 
  geom_smooth(method="lm", colour="black") + theme_classic() + 
  ylab("Women's life expectancy") +
  xlab("Global gender gap index") +
  labs(colour="World region")

ggplot(global, aes(x=X2021*100, y=X2020.y, colour=region)) + geom_point(size=2) + 
  geom_smooth(method="lm", colour="black") + theme_classic() + 
  ylab("Men's life expectancy") +
  xlab("Global gender gap index") +
  labs(colour="World region")


ggplot(econ, aes(x=index*100, y=change.d, colour=region)) + geom_point() + 
  geom_smooth(method="lm", colour="black") + theme_classic() + 
  ylab("Gender gap in  life expectancy") +
  xlab("Economic subindex of the global gender equity gap")

ggplot(educ, aes(x=index*100, y=change.d, colour=region)) + geom_point() + 
  geom_smooth(method="lm", colour="black") + theme_classic() + 
  ylab("Gender gap in  life expectancy") +
  xlab("Education subindex of the global gender equity gap")

ggplot(polit, aes(x=index*100, y=change.d, colour=region)) + geom_point() + 
  geom_smooth(method="lm", colour="black") + theme_classic() + 
  ylab("Gender gap in  life expectancy") +
  xlab("Political subindex of the global gender equity gap")


aggregate(X2021~region, data=educ, FUN=range)


## Figures longitudinal

ggplot(global, aes(x=index*100, y=change.d, colour=region)) + geom_point(size=2) + 
  geom_smooth(method="lm", colour="black") + theme_classic() + 
  ylab("Gender gap in  life expectancy") +
  xlab("Global gender gap index") +
  labs(colour="World region")

ggplot(global, aes(x=index*100, y=fle.d, colour=region)) + geom_point(size=2) + 
  geom_smooth(method="lm", colour="black") + theme_classic() + 
  ylab("Women's life expectancy") +
  xlab("Global gender gap index") +
  labs(colour="World region")

ggplot(global, aes(x=index*100, y=mle.d, colour=region)) + geom_point(size=2) + 
  geom_smooth(method="lm", colour="black") + theme_classic() + 
  ylab("Men's life expectancy") +
  xlab("Global gender gap index") +
  labs(colour="World region")


