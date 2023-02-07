# Copyright 2021 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

#fwi is a normal distribution
#area is negative exponential
# see below for exploritory code to determine.
# https://rpubs.com/eraldagjika/715261

#Load some helpful libraries for exploring correlation and distribution
library(ciTools)
library(fitdistrplus)
library(gamlss)

#Load Data
FireWeather<-readRDS(file='tmp/FireWeather')

#Some histograms and density plots
hist(FireWeather$area_Ha, freq = FALSE)
lines(density(FireWeather$area_Ha))

xlim=c(0,2000)
plot(FireWeather$area_Ha,FireWeather$IDW_fwi)

#Use ggplot to make nicer graphs
ggplot(data=FireWeather, aes(x=area_Ha, y=IDW_fwi)) +
  geom_point() +
  xlim(0, 20000) +
  ylim(0, 40) +
  geom_smooth(method=lm, se=FALSE)

#Explore a single fire
FireWeatherF<-FireWeather %>% dplyr::filter(Fire=='R11796')

#Group by Fire and Day, filter for time only during fires burning to reduce 0s in data
FireDataG<-FireWeather %>%
  group_by(Fire,wDay) %>%
  dplyr::summarise(n=n(), area=sum(area_Ha), wind=max(wind_speed), fwi=max(IDW_fwi),
        wind_direction=first(wind_direction)) %>%
  dplyr::filter(wDay>208 & wDay<245)

ggplot(data=FireDataG, aes(x=area, y=fwi)) +
  geom_point() +
  xlim(0,5500) +
  ylim(0, 65) +
  geom_smooth(method=lm, se=FALSE)

#quick and dirty regression model
attach(FireDataG)
model <- lm(area~fwi)
summary(model)

#Look at outliers - there are lots!
boxplot(FireDataG$area)

ggplot(FireDataG, aes(x=area)) +
  geom_histogram(binwidth = 5)  +
  theme_bw(base_size = 18) +
  labs(y = "Frequency",
       title = "Negative binomial",
       subtitle = "mean = 10, theta = 0.05" ) +
  annotate(geom = "text",
           label = paste("Proportion 0:", mean(FireDataG$area == 0), 
                         "\nMax Count:", max(FireDataG$area) ),
           x = 150, y = 100, size = 8)

######Data distribution exploration
#https://rpubs.com/eraldagjika/715261

#TestD<-FireDataG$area
TestD<-FireDataG$fwi
m=mean(TestD)
sig=sd(TestD)
#R package to help dial in data distribution
descdist(TestD)
#bootstrap data
descdist(TestD,boot=1000,discrete=TRUE)

#not all of these will work, depending on the attribute
nbinom.f<- fitdist(TestD, "nbinom") 
summary(nbinom.f)
pois.f<- fitdist(TestD, "pois")# Poisson
summary(pois.f)
exp.f <- fitdist(TestD, "exp") # Exponential
summary(exp.f)
norm.f <- fitdist(TestD, "norm") # Normal
summary(norm.f)

par(mfrow = c(2, 2))
plot.legend <- c("Exponential","Normal")
denscomp(list(exp.f,norm.f), legendtext = plot.legend)
qqcomp(list(exp.f,norm.f), legendtext = plot.legend)

cdfcomp(list(exp.f,norm.f), legendtext = plot.legend)
ppcomp(list(exp.f,norm.f), legendtext = plot.legend)

set.seed(512)
No=rnorm(length(TestD),1213.939,2687.426)# estimated rate= 0.03934638
plot(TestD,No)# scatterplot observed vs fitted
Po=rpois(length(TestD),1213.939)# estimated parameters
plot(TestD,Po)#  scatterplot observed vs fitted
neg.bin=rnbinom(length(TestD),size=0.04322351, mu=1212.66424860)# estimated parameters
plot(TestD,neg.bin)#  scatterplot observed vs fitted

par(mfrow = c(2, 2))
plot.legend <- c("NBionm","Pois")
denscomp(list(nbinom.f,pois.f), legendtext = plot.legend)
qqcomp(list(nbinom.f,pois.f), legendtext = plot.legend)

cdfcomp(list(nbinom.f,pois.f), legendtext = plot.legend)
ppcomp(list(nbinom.f,pois.f), legendtext = plot.legend)




######### Other random code
Weath<-colnames(FireDOBdata)
#"Fire"    "Day"     "fire_id" "bui"     "dc"      "dmc"     "fwi"     "isi"     "maxT"    "maxW"    "minRH"  "area_Ha"

FireData<-FireDOBdata %>%
  st_drop_geometry() %>%
  tidyr::complete(Day,Fire) %>%
  mutate_at(Weath[3:12], ~replace_na(.,0))


