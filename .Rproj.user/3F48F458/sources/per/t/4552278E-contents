library(ggmap)
library(tidyverse)
library(osmdata)
library(sf)


range(sATM$atm_lat)
range(sATM$atm_lon)

# Specifikations ----------------------------------------------------------

coord <- as.matrix(data.frame(min = c(8, 55), 
                              max = c(13, 58), 
                              row.names = c("x","y")))

s <- sATM %>% filter(atm_lat > coord[2,1] & atm_lat < coord[2,2] & 
                     atm_lon < coord[1,2] & atm_lon > coord[1,1])



# First map ---------------------------------------------------------------

map8 <- get_stamenmap(coord, zoom = 9, maptype = "toner-lite", force = TRUE)

ggmap(map8) +
  geom_point(data = s, aes(atm_lon,atm_lat), alpha = 0.3, color = "#3690c0") + 
  labs(title="Map of stops in New Orleans", subtitle = "260.000 casesfrom 2011 - 2018")


sa <- s %>% filter(currency=="EUR") %>% 
  group_by(atm_id, atm_lon, atm_lat, atm_manufacturer) %>% summarize(N = n())

ggmap(map13) +
  geom_point(data = sa, aes(atm_lon, atm_lat, size=N, color=atm_manufacturer), alpha = 0.8) + 
  labs(title="Map of stops in New Orleans", subtitle = "260.000 casesfrom 2011 - 2018")
  