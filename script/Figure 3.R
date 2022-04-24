
library(sf)
library(raster)
library(foreach)
library(tidyverse)
library(exactextractr)
library(data.table)
library(units)
library(Metrics)
library(rasterVis)
library(RColorBrewer)


# subplot pop -------------------------------------------------------------

extent <- st_as_sf(data.frame(x = c(425000, 425000, 470000, 470000), 
                              y = c(4400000, 4435000, 4435000, 4400000)), 
                   coords = c("x", "y")) %>% 
  group_by() %>% 
  summarise() %>%
  st_cast("POLYGON") %>% 
  st_convex_hull()

files <- list.files('intermediate', full.names = T, pattern = 'predSVM_mod[0-9].tif')

plots <- vector('list', 4)

pop.stack <- stack(foreach(i = 1:4) %do% {
  
  rs <- raster(files[i])
  rs <- projectRaster(rs, crs = '+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs', method = 'ngb')
  rs <- mask(rs, extent)
  point <- rasterToPoints(rs, spatial = T)
  rs <- raster(SpatialPixelsDataFrame(point, data = as.data.frame(point[[1]])))
  rs[rs > 10000] <- 10000
  rs
  
})
  
legend.at = c(seq(0, 10, length.out = 4),
              seq(10, 50, length.out = 4)[-1],
              seq(50, 100, length.out = 4)[-1],
              seq(100, 500, length.out = 4)[-1],
              seq(500, 1000, length.out = 4)[-1],
              seq(1000, 10000, length.out = 4)[-1])

levelplot(pop.stack, 
          contour = F,
          margin = F,
          par.settings = rasterTheme(region = (brewer.pal(9, 'GnBu')), 
                                     axis.line = list(lwd = 0.5)),
          xlab = '',
          ylab = '',
          names.attr = rep('', 4), 
          xlim = c(425000, 470000), 
          ylim = c(4400000, 4435000), 
          scales = list(x = list(cex = 0.9), y = list(cex = 0.9), tck = 0.5),
          colorkey = T, 
          at = legend.at, 
          layout = c(1, 4))



# subplot ld --------------------------------------------------------------
rs <- raster('data/02_landuse/globaland_35_40_bj_100m.tif')
rs <- projectRaster(rs, crs = '+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs', method = 'ngb')
rs <- mask(rs, extent)
point <- rasterToPoints(rs, spatial = T)
rs <- raster(SpatialPixelsDataFrame(point, data = as.data.frame(point[[1]])))

rs <- as.factor(rs)

rat <- levels(rs)[[1]]
rat[["landcover"]] <- c("Cropland", "Woodland", "Grassland", "Waters", "Builtup")
levels(rs) <- rat

levelplot(rs, 
          contour = F,
          margin = F,
          par.settings = rasterTheme(region = c("#FFFFB3", "#8DD3C7", "#BEBADA", "#80B1D3", "#FB8072"), 
                                     axis.line = list(lwd = 0.5)),
          xlim = c(425000, 470000), 
          ylim = c(4400000, 4435000), 
          xlab = '',
          ylab = '',
          scales = list(draw = F, x = list(cex = 0.9), y = list(cex = 0.9), tck = 0.5))



# subplot mp --------------------------------------------------------------
mp <- raster('data/06_MP/02_resample/MP0713_21.tif')
mp <- projectRaster(mp, crs = '+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs', method = 'ngb')
mp <- mask(mp, extent)
point <- rasterToPoints(mp, spatial = T)
mp <- raster(SpatialPixelsDataFrame(point, data = as.data.frame(point[[1]])))
mp[mp > 10000] <- 10000
mp[mp == 0] <- NA

legend.at = c(seq(0, 10, length.out = 4),
              seq(10, 50, length.out = 4)[-1],
              seq(50, 100, length.out = 4)[-1],
              seq(100, 500, length.out = 4)[-1],
              seq(500, 1000, length.out = 4)[-1],
              seq(1000, 10000, length.out = 4)[-1])

levelplot(mp, 
          contour = F,
          margin = F,
          par.settings = rasterTheme(region = (brewer.pal(9, 'BuPu')), 
                                     axis.line = list(lwd = 0.5)),
          xlim = c(425000, 470000), 
          ylim = c(4400000, 4435000), 
          xlab = '',
          ylab = '',
          scales = list(draw = F, x = list(cex = 0.9), y = list(cex = 0.9), tck = 0.5), 
          colorkey = T, 
          at = legend.at)



# subplot road ------------------------------------------------------------
road <- st_read('data/03_road/beijing_road.shp') %>% 
  st_transform('+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=0 +y_0=0 +ellps=krass +units=m +no_defs') %>% 
  st_transform('+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs')

building <- st_read('data/07_building/building_beijing.shp') %>% 
  `st_crs<-`('+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=0 +y_0=0 +ellps=krass +units=m +no_defs') %>% 
  st_transform('+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs')

ggplot() + 
  geom_sf(aes(), road, lwd = 0.1, col = '#4D71DB', alpha = 0.6) + 
  geom_sf(aes(fill = Floor),  
          building %>%
            mutate(Floor = ifelse(Floor > 30, 5,
                                  ifelse(Floor > 10, 4,
                                         ifelse(Floor > 6, 3,
                                                ifelse(Floor > 3, 2,
                                                       1))))), col = NA, alpha = 0.7) +
  scale_fill_distiller(palette = 'RdYlGn', direction = -1,
                       breaks = c(1,2,3,4,5),
                       labels = c('1', '3', '6', '10', '>30')) +
  xlim(c(425000, 470000)) + 
  ylim(c(4400000, 4435000)) + 
  theme_bw() +
  theme(panel.grid = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.line = element_line(size = 0.005, colour = 'grey40'), 
        legend.key.size = unit(6, 'mm'), 
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 14), 
        legend.position = c(0.9, 0.25))



# subplot poi -------------------------------------------------------------
poi.residential <- poi.list[[5]] %>% 
  st_as_sf(coords = c('wgslon', 'wgslat'), crs = "+proj=longlat +datum=WGS84") %>% 
  st_transform('+proj=aea +lat_1=25 +lat_2=47 +lat_0=0 +lon_0=105 +x_0=0 +y_0=0 +ellps=krass +units=m +no_defs') %>% 
  st_transform('+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs') %>% 
  as.data.frame() %>% 
  mutate(lon = gsub('c[(](.*?),.*', '\\1', as.character(geometry)), 
         lat = gsub('.*,\\s(.*?)[)]$', '\\1', as.character(geometry)), 
         lon = as.numeric(lon), 
         lat = as.numeric(lat), 
         geometry = NULL)


ggplot() + 
  geom_sf(aes(), road[road$type %in% unique(road$type)[c(2,4)], ], lwd = 0.12, col = '#4D71DB', alpha = 0.7) + 
  geom_point(aes(lon, lat), data = poi.residential, pch = 20, col = '#df65b0', alpha = 0.4, size = 0.05) +
  xlab('') + 
  ylab('') + 
  xlim(c(425000, 470000)) + 
  ylim(c(4400000, 4435000)) + 
  theme_bw() +
  theme(panel.grid = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.line = element_line(size = 0.005, colour = 'grey40'), 
        legend.key.size = unit(3, 'mm'), 
        legend.position = c(0.09, 0.16))




