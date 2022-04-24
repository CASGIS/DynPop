
library(foreach)
library(tidyverse)
library(doParallel)
library(ggplot2)
library(ggalt)
library(sf)
library(units)
library(exactextractr)
library(raster)


beijing <- st_read('data/00_boundary/boundary_beijing.shp') %>% 
  st_transform("+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=0 +y_0=0 +ellps=krass +units=m +no_defs")

aoi <- st_read('data/05_aoi/beijing_AOI.shp') %>% 
  st_transform("+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=0 +y_0=0 +ellps=krass +units=m +no_defs")

aoi <- st_intersection(aoi, beijing) %>%
  mutate(area = drop_units(st_area(geometry))) %>% 
  mutate(Id = 1:nrow(.)) %>% 
  group_by(type) %>% 
  mutate(areaQuantile = as.numeric(cut_number(area, n = 10))) %>% 
  ungroup()

stopImplicitCluster()
registerDoParallel(cores = 20)

type.list <- list("地产小区", c("绿地", "休闲娱乐"), "购物商业", "交通设施", "医疗区域")
type.list.en <- c('Residential', 'Leisure place', 'Commercial', 'Transportation', 'Hospital')

pop.line <- bind_rows(foreach(i = c(1,2,3,4,7)) %do% {
  aoi.type <- aoi %>% 
    filter(type %in% type.list[[i]]) %>% 
    filter(area > 10000)
  
  dates <- c('0712', '0713', '0714', '0715', '0716', '0717', '0718')
  
  pop.type <- bind_rows(foreach(date = dates) %do% {
    
    files <- list.files('result/00_oneweek', full.names = T,
                        pattern = paste0('predSVM_mod5_', date, '.*'))
    
    bind_rows(foreach(j = 1:length(files)) %dopar% {
      
      data.frame(pop = exact_extract(raster(files[j]),
                                     aoi.type,
                                     function(values, coverage_fraction){
                                       sum(values * coverage_fraction, na.rm = T)
                                     },
                                     progress = T)) %>% 
        mutate(Id = as.character(aoi.type$Id)) %>%
        mutate(hour = as.integer(gsub('.*?_h(.*?).tif', '\\1', files[j])))
    }) %>%
      mutate(date = date)
  }) %>% 
    mutate(type = type.list[i])
  
  pop.type <- pop.type %>% 
    group_by(Id) %>% 
    mutate(popmean = mean(pop)) %>% 
    filter(popmean >= 100)
  
  pop.type %>% 
    group_by(Id) %>% 
    mutate(pop_std = as.numeric(scale(pop))) %>% 
    ungroup() %>% 
    mutate(hour = factor(hour), 
           date = factor(date)) %>% 
    mutate(x = as.integer(hour) + (as.integer(date) - 1) * 17)
  
  
  pop.type %>%
    group_by(x) %>%
    summarise(std_mean = mean(pop_std), 
              std_sd = sd(pop_std), 
              std_quan25 = quantile(pop_std, 0.25), 
              std_quan75 = quantile(pop_std, 0.75)) %>%
    ungroup() %>% 
    mutate(type = type.list.en[i])
}) %>% 
  mutate(type = factor(type, levels = type.list.en[c(1,2,3,4,7)]))


ggplot(pop.line) +
  geom_hline(yintercept = 0, col = 'grey50', lwd = 0.3) + 
  geom_vline(xintercept = seq(1, 119, 17), col = 'grey90', lwd = 0.1) + 
  geom_xspline(aes(x, std_mean, col = type), pop.line, 
               stat = "xspline", size = 0.6, alpha = 0.7, show.legend = F) + 
  geom_ribbon(aes(x, ymin = std_quan25, ymax = std_quan75, fill = type), pop.line, 
              alpha = 0.3, show.legend = F) + 
  facet_wrap(.~type, ncol = 1) + 
  scale_x_continuous('', expand = c(0, 0), breaks = seq(1, 119, 17), 
                     labels = c('Mon', 'Feb', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')) + 
  scale_y_continuous('Standardized population', expand = c(0, 0), limits = c(-2, 2), 
                     breaks = seq(-2, 2, 1), labels = seq(-2, 2, 1)) +
  theme_classic() + 
  theme(aspect.ratio = 0.25, 
        axis.title = element_text(size = 15), 
        axis.text = element_text(size = 14), 
        axis.line = element_line(size = 0.2), 
        axis.ticks = element_line(size = 0.2), 
        strip.background = element_rect(fill = 'grey90', colour = NA), 
        strip.text = element_text(size = 13))

  

