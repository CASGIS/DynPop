
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

# daytime ---------------------------------------------------------------

setwd('~/DynPop/')

files <- list.files('result/3hourBin', full.names = T, pattern = 'predSVM_[0-9]{4}.tif')

extent <- st_as_sf(data.frame(x = c(425000, 425000, 470000, 470000), 
                              y = c(4395000, 4440000, 4440000, 4395000)), 
                   coords = c("x", "y")) %>% 
  group_by() %>% 
  summarise() %>%
  st_cast("POLYGON") %>% 
  st_convex_hull()

pop.stack <- stack(foreach(i = c(2,6)) %do% {
  
  rs <- raster(files[i])
  rs <- projectRaster(rs, crs = '+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs', method = 'ngb')
  rs <- mask(rs, extent)
  point <- rasterToPoints(rs, spatial = T)
  rs <- raster(SpatialPixelsDataFrame(point, data = as.data.frame(point[[1]])))
  rs[rs > 100000] <- 100000
  rs
  
})

extent <- list(st_as_sf(data.frame(x = c(441500, 441500, 446500, 446500), 
                                   y = c(4434000, 4438000, 4438000, 4434000)), 
                        coords = c("x", "y")) %>% 
                 group_by() %>% 
                 summarise() %>%
                 st_cast("POLYGON") %>% 
                 st_convex_hull(), 
               st_as_sf(data.frame(x = c(439500, 439500, 444500, 444500), 
                                   y = c(4424000, 4428000, 4428000, 4424000)), 
                        coords = c("x", "y")) %>% 
                 group_by() %>% 
                 summarise() %>%
                 st_cast("POLYGON") %>% 
                 st_convex_hull(), 
               st_as_sf(data.frame(x = c(450500, 450500, 455500, 455500), 
                                   y = c(4416500, 4420500, 4420500, 4416500)), 
                        coords = c("x", "y")) %>% 
                 group_by() %>% 
                 summarise() %>%
                 st_cast("POLYGON") %>% 
                 st_convex_hull(), 
               st_as_sf(data.frame(x = c(457000, 457000, 462000, 462000), 
                                   y = c(4402000, 4406000, 4406000, 4402000)), 
                        coords = c("x", "y")) %>% 
                 group_by() %>% 
                 summarise() %>%
                 st_cast("POLYGON") %>% 
                 st_convex_hull())


legend.at = c(seq(0, 10, length.out = 4),
              seq(10, 50, length.out = 4)[-1],
              seq(50, 100, length.out = 4)[-1],
              seq(100, 500, length.out = 4)[-1],
              seq(500, 1000, length.out = 4)[-1],
              seq(1000, 10000, length.out = 4)[-1])


pdf('fig/pop_0911am.pdf', width = 5, height = 5)
levelplot(pop.stack[[1]], 
          contour = F,
          margin = F,
          par.settings = rasterTheme(region = (brewer.pal(9, 'GnBu')), 
                                     axis.line = list(lwd = 0.5)),
          xlab = '',
          ylab = '',
          xlim = c(425000, 470000), 
          ylim = c(4395000, 4440000), 
          scales = list(draw = F, x = list(cex = 0.9), y = list(cex = 0.9), tck = 0.5),
          colorkey = T, 
          at = legend.at) + 
  latticeExtra::layer(sp.polygons(as_Spatial(extent[[1]]), col = 'purple', lwd = 1)) + 
  latticeExtra::layer(sp.polygons(as_Spatial(extent[[2]]), col = 'purple', lwd = 1)) + 
  latticeExtra::layer(sp.polygons(as_Spatial(extent[[3]]), col = 'purple', lwd = 1)) + 
  latticeExtra::layer(sp.polygons(as_Spatial(extent[[4]]), col = 'purple', lwd = 1))
dev.off()


# nighttime - daytime ----------------------------------------------------

legend.at = c(seq(-4000, -1000, length.out = 3),
              seq(-1000, -100, length.out = 4)[-1],
              seq(-100, -10, length.out = 4)[-1],
              seq(-10, -2, length.out = 4)[-1],
              seq(-2, 2, length.out = 2)[-1],
              seq(2, 10, length.out = 4)[-1],
              seq(10, 100, length.out = 4)[-1],
              seq(100, 1000, length.out = 4)[-1],
              seq(1000, 4000, length.out = 3)[-1])

pdf('fig/pop_0911pm.pdf', width = 5, height = 5)
levelplot(pop.stack[[2]] - pop.stack[[1]],
          contour = F,
          margin = F,
          par.settings = rasterTheme(region = brewer.pal(9, 'RdBu'), 
                                     axis.line = list(lwd = 0.5), draw = F),
          xlab = '',
          ylab = '',
          xlim = c(425000, 470000), 
          ylim = c(4395000, 4440000), 
          scales = list(draw = F, x = list(cex = 0.9), y = list(cex = 0.9), tck = 0.5),
          colorkey = T, 
          at = legend.at) + 
  latticeExtra::layer(sp.polygons(as_Spatial(extent[[1]]), col = 'purple', lwd = 1)) + 
  latticeExtra::layer(sp.polygons(as_Spatial(extent[[2]]), col = 'purple', lwd = 1)) + 
  latticeExtra::layer(sp.polygons(as_Spatial(extent[[3]]), col = 'purple', lwd = 1)) + 
  latticeExtra::layer(sp.polygons(as_Spatial(extent[[4]]), col = 'purple', lwd = 1))
dev.off()




# Zoom in -----------------------------------------------------------------

files <- list.files('result/3hourBin', full.names = T, pattern = 'predSVM_[0-9]{4}.tif')
rs <- raster(files[1]) + raster(files[2]) + raster(files[3]) + raster(files[4]) + raster(files[5]) + raster(files[6])
rs.mean <- rs / 6

plots <- vector('list', 4)

for (k in 1:4) {
  
  files <- list.files('result/3hourBin', full.names = T, pattern = 'predSVM_[0-9]{4}.tif')
  
  pop.stack <- stack(foreach(i = 1:6) %do% {
    
    rs <- raster(files[i]) - rs.mean
    rs <- projectRaster(rs, crs = '+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs', method = 'ngb')
    rs <- mask(rs, extent[[k]])
    point <- rasterToPoints(rs, spatial = T)
    rs <- raster(SpatialPixelsDataFrame(point, data = as.data.frame(point[[1]])))
    rs[rs > 4000] <- 4000
    rs[rs < -4000] <- -4000
    rs
  })
  
  if (k == 1) {
    plots[[k]] <- levelplot(pop.stack,
                            contour = F,
                            margin = F,
                            par.settings = rasterTheme(region = brewer.pal(9, 'RdBu'), axis.line = list(lwd = 0.5)),
                            xlab = '',
                            ylab = '',
                            names.attr = c('6:00 - 8:00', '9:00 - 11:00', '12:00 - 14:00', 
                                           '15:00 - 17:00', '18:00 - 20:00', '21:00 - 23:00'), 
                            xlim = c(441500, 446500),
                            ylim = c(4434000, 4438000),  # 回龙观
                            scales = list(draw = F, x = list(cex = 0.9), y = list(cex = 0.9), tck = 0.5),
                            colorkey = F,
                            at = legend.at, 
                            layout = c(6, 1))
  } else if (k == 2) {
    plots[[k]] <- levelplot(pop.stack,
                            contour = F,
                            margin = F,
                            par.settings = rasterTheme(region = brewer.pal(9, 'RdBu'), axis.line = list(lwd = 0.5)),
                            xlab = '',
                            ylab = '',
                            names.attr = rep('', 6), 
                            xlim = c(439500, 444500),
                            ylim = c(4424000, 4428000),  # 中关村
                            scales = list(draw = F, x = list(cex = 0.9), y = list(cex = 0.9), tck = 0.5),
                            colorkey = F,
                            at = legend.at, 
                            layout = c(6, 1))
  } else if (k == 3) {
    plots[[k]] <- levelplot(pop.stack,
                            contour = F,
                            margin = F,
                            par.settings = rasterTheme(region = brewer.pal(9, 'RdBu'), axis.line = list(lwd = 0.5)),
                            xlab = '',
                            ylab = '',
                            names.attr = rep('', 6), 
                            xlim = c(450500, 455500),
                            ylim = c(4416500, 4420500),  # 国贸
                            scales = list(draw = F, x = list(cex = 0.9), y = list(cex = 0.9), tck = 0.5),
                            colorkey = F,
                            at = legend.at, 
                            layout = c(6, 1))
  } else if (k == 4) {
    plots[[k]] <- levelplot(pop.stack,
                            contour = F,
                            margin = F,
                            par.settings = rasterTheme(region = brewer.pal(9, 'RdBu'), axis.line = list(lwd = 0.5)),
                            xlab = '',
                            ylab = '',
                            names.attr = rep('', 6), 
                            xlim = c(457000, 462000),
                            ylim = c(4402000, 4406000),  # 产业园
                            scales = list(draw = F, x = list(cex = 0.9), y = list(cex = 0.9), tck = 0.5),
                            colorkey = F,
                            at = legend.at, 
                            layout = c(6, 1))
  }
}


pdf('fig/zoomin_s1.pdf', width = 10, height = 2)
plots[[1]]
dev.off()

pdf('fig/zoomin_s2.pdf', width = 10, height = 2)
plots[[2]]
dev.off()

pdf('fig/zoomin_s3.pdf', width = 10, height = 2)
plots[[3]]
dev.off()

pdf('fig/zoomin_s4.pdf', width = 10, height = 2)
plots[[4]]
dev.off()




# line plots for variation -----------------------------------------------

files <- list.files('result/1hourAvg', pattern = 'predSVM_h.*', full.names = T)

stack <- stack(foreach(i = 1:length(files)) %do% {
  raster(files[i]) %>% 
    projectRaster(crs = '+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs', method = 'ngb')
})


tb.plot <- bind_rows(foreach(i = 1:4) %do% {
  data.frame(aoi = i, 
             popest = unlist(foreach(j = 1:17) %do% {
               exact_extract(stack[[j]],
                             extent[[i]],
                             function(values, coverage_fraction){
                               sum(values * coverage_fraction, na.rm = T)
                             },
                             progress = T)
             }), 
             h = 7:23)
})


ggplot(tb.plot) + 
  geom_line(aes(h, popest / 1000), col = '#357EBD', lwd = 0.2) + 
  facet_wrap(.~aoi, ncol = 1, scales = 'free') + 
  scale_y_continuous(position = 'right', expand = c(0.1, 0.1)) + 
  xlab('Hour') + 
  ylab(expression(Population (10^3))) + 
  theme_classic() + 
  theme(text = element_text(size = 16), 
        aspect.ratio = 0.8, 
        axis.line = element_line(size = 0.2), 
        axis.ticks = element_line(size = 0.2), 
        strip.background = element_blank(), 
        strip.text = element_blank())

ggsave('fig/zoomin_popvar.pdf', width = 3, height = 7)








