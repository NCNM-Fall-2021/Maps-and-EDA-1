

library(tidyverse)
library(tnum)
library(dplyr)

tnum.authorize("mssp1.bu.edu")


tnum.setSpace("NCNM")

## how to explore number space

# sub111 <- tnum.getDBPathList(taxonomy = "subject")
# tnum.graphPathList(sub111)

## what subjects are here?
# sub1 <- tnum.getDBPathList(taxonomy="subject", levels=2)
# 
# tnum.graphPathList(sub1)




q1 <- tnum.query(query="#santa_fe# has #income# @year:2019", max=500)

df1 <- tnum.objectsToDf(q1)



ggplot(df1, aes(`numeric.value`)) + 
  geom_histogram(binwidth=10000, color = "white", fill = "lightgreen") +  
  labs(x = paste("2019 -", df1$property[1]), y = "Number of Census Tracts")






tract <- df1$subject[1:50] %>% str_split_fixed("t:",n=2)
tract <- tract[,2]


# Census Data API. Your API key is 1f59f7174f20c83db094f903d81498257d4f5176. 
#The key is active and ready to use.



library(tidycensus)
library(sf)
library(tigris)
library(janitor)

census_api_key("1f59f7174f20c83db094f903d81498257d4f5176")

## US map

variables_dec <- load_variables(year = 2010, dataset = "sf1", cache = TRUE)


variables_acs <- load_variables(year = 2017, dataset = "acs5", cache = TRUE)


variables_dec %>% 
  filter(str_detect(concept, "POPULATION")) 


states <- get_decennial(geography = "state",
                        variables = c(total_pop = "P001001"),
                        geometry = TRUE,
                        output = "wide") 

options(scipen=999)

states %>% 
  mutate(NAME = fct_reorder(NAME, total_pop)) %>% 
  ggplot(aes(NAME, total_pop, fill= GEOID==35)) +
  geom_col(color="white") +  
  scale_fill_manual(values=c("#008EFC","#FC2D00")) +
  theme(axis.text.x = element_text( color="#008EFC", size = 8), 
        axis.text.y = element_text(color="#008EFC", size = 6)) +
  theme(legend.position = "none") +
   coord_flip()


states %>% 
  filter(NAME != "Alaska",
         NAME != "Hawaii",
         !str_detect(NAME, "Puerto")) %>% 
  ggplot(aes(fill=GEOID==35)) +
  geom_sf(color="lightblue") +
  scale_fill_manual(values=c("#008EFC","#FC2D00")) +
  theme(legend.position = "none")




###################################################

new_mexico <- get_decennial(geography = "county",
                              variables = c(total_pop = "P001001"),
                              state = "NM",
                              geometry = TRUE,
                              output = "wide")
new_mexico %>% 
  ggplot(aes(fill = total_pop)) +
  geom_sf() +
  scale_fill_viridis_c()







#########################  stop here!!!  in progress...


santa_fe_tracts <- get_decennial(geography = "tract", state="NM", 
                                 county="Sante Fe County",
                                 geometry=TRUE)


###################################################################


library(tidycensus)
library(sf)
library(tigris)
library(ggmap)
library(janitor)



theme_set(theme_bw())

options(tigris_use_cache = TRUE,
        scipen = 4,
        digits = 3)





























# library(tidycensus)
# library(sf)
# library(tigris)
# library(ggmap)
# library(janitor)
# 
# 

# library(rgdal)    # for readOGR and others
# library(sp)       # for spatial objects
# library(leaflet)  # for interactive maps (NOT leafletR here)
# library(dplyr)    # for working with data frames
# library(ggplot2)  # for plotting
# 
# 
# 
# tract <- readOGR(dsn=".", layer = "cb_2018_36_tract_500k")
# 
# 
# tract@data$GEOID<-as.character(tract@data$GEOID)
# 
# 
# 
# data <- read.csv("x:/junk/claire/leaflet_plot/ACS_13_5YR_B19001.csv", stringsAsFactors = FALSE)
# 
# ############################
# 
# library(maptools)
# ggtract<-fortify(tract, region = "GEOID") 
# # join tabular data
# ggtract<-left_join(ggtract, data, by=c("id")) 
# 
