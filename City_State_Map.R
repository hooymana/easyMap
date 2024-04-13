rm(list = ls())

#Be sure to set working directory prior to trying to import .csv files

library(usmap)
library(ggplot2)
library(maps)
library(zipcodeR)

#To convert zip code to longitude and latitude
reverse_zipcode(c("90210","10001"))

#To convert longitude and latitude to usmap coordinates
cities_with_xy = usmap_transform(us.cities,input_names = c("long", "lat"))

sample.data=read.csv("Sample.Map.Data.csv")
city_xy=read.csv("City.Data.csv")

#METHOD 1: RENDER CITIES BASED ON POPULATION SIZE
states.included=c("PA","NY","WV","NJ","DE","CT","MD") #Pick states you want to render
min.population.size=100000 #Select minimum population city size you want to render

#For only cities with a specific population size
plot_usmap(include = states.included) +#Renders just the states you want to include
  #Render points for participant location in orange
  geom_point(sample.data[sample.data$state %in% states.included, ], 
             mapping = aes(x = x, y = y), 
             color = "orange", 
             size = 2.5, 
             alpha = 0.5) +
  #Render points for cities you want to include based on population size in purple
  #For this particular area of the map there are a lot of cities with fairly high pop close to one another
  #If labels get too close then they won't be rendered.
  #Selecting just the cities you want to render may be the better option
  geom_point(data = city_xy[city_xy$country.etc %in% states.included &
                              city_xy$pop>min.population.size ,],
             aes(x = x, y = y),size=2,shape=15,
             alpha = 0.5,show.legend = F,color="purple")+
  geom_text_repel(data=city_xy[city_xy$country.etc %in% states.included &
                                 city_xy$pop>min.population.size,],
                  aes(x = x, y = y,label=name),size=2.2,
                  point.padding = .2)


#METHOD 2: RENDER SPECIFIC CITIES BY NAME
states.included=c("PA","NY","WV","NJ","DE","CT","MD") #Pick states you want to render
city.included=c("Scranton","New York","Philadelphia","Pittsburgh","Huntington","Buffalo") #Pick specific cities to render

plot_usmap(include = states.included) + #Renders just the states you want to include
  #Render points for participant location in orange
  geom_point(sample.data[sample.data$state %in% states.included, ], 
             mapping = aes(x = x, y = y), 
             color = "orange", 
             size = 2.5, 
             alpha = 0.5) +
  #Render points for cities you want to include in purple
  geom_point(data = city_xy[city_xy$country.etc %in% states.included &
                              city_xy$city %in% city.included ,],
             aes(x = x, y = y),size=2,shape=15,
             alpha = 0.5,color="purple")+
  #Render labels for cities you want to include
  geom_text_repel(data=city_xy[city_xy$country.etc %in% states.included &
                                 city_xy$city %in% city.included,],
                  aes(x = x, y = y,label=city),
                  size=3, #Change size of label here
                  point.padding = .2)


counties=map_data("county")


BH=reverse_zipcode(c("90210"))
BH[,c("state","lat","lng")]

counties.included="los angeles"
city.included2="Los Angeles"

#Fit polygons for county
ggplot(counties[counties$subregion %in% counties.included,],aes(x=long,y=lat,group=group))+
  geom_polygon(show.legend = F,fill="white",color="black")+
  #participant data
  geom_point(BH,mapping=aes(x=lng,lat),inherit.aes = F, color="orange")+
  #reference cities
  geom_point(city_xy[city_xy$city %in% city.included2,],mapping=aes(x=long,y=lat),
             inherit.aes = F,size=3,shape=15,color="purple",alpha=.5)+
  geom_text_repel(data=city_xy[city_xy$city %in% city.included2,],
                  aes(x = long, y = lat,label=city),
                  size=3, #Change size of label here
                  point.padding = 1,inherit.aes = F)
