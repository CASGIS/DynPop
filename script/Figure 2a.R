
library(sf)
library(raster)
library(foreach)
library(tidyverse)
library(doParallel)
library(exactextractr)
library(units)
library(Metrics)
library(ggplot2)
library(ggdist)

checktable <- 
  data.frame(cov = c('MP', 'dem', 'slope', 'ld_cropland', 'ld_woodland', 'ld_grassland', 'ld_waters', 'ld_builtup', 
                     'ld_barearea', 'poi_catering', 'poi_education', 'poi_leisure', 'poi_residential', 'poi_shopping',
                     'poi_transport', 'poi_working', 'roaddensity', 'builtarea'), 
             category = c('MP', rep('terrain', 2), rep('land use', 6), rep('urban function', 7), rep('urban morphology', 2)))


files <- list.files('result/STmodeling_model', full.names = T)
tb <- bind_rows(foreach(i = 2:length(files)) %do% {
  mod <- readRDS(files[i])
  bind_rows(foreach(j = c(1:nrow(mod$summary.fixed))) %do% {
    x = seq(as.numeric(sprintf('%.03f', mod$summary.fixed$mean[j] - 3 * mod$summary.fixed$sd[j])), 
            as.numeric(sprintf('%.03f', mod$summary.fixed$mean[j] + 3 * mod$summary.fixed$sd[j])), 
            by = 0.001)
    y = dnorm(x, mod$summary.fixed$mean[j], mod$summary.fixed$sd[j])
    data.frame(x = x, 
               y = y, 
               cov = row.names(mod$summary.fixed)[j], 
               model = i)
  })
}) %>% 
  mutate(yheight = round(y * 10))


tb.long <- bind_rows(
  foreach(i = 1:nrow(tb)) %do% 
    data.frame(cov = tb$cov[i], 
               model = tb$model[i], 
               x = rep(tb$x[i], tb$yheight[i]), 
               y = rep(tb$y[i], tb$yheight[i]), 
               yheight = tb$yheight[i]))

tb.long.rank <- tb.long %>% 
  group_by(cov, model) %>% 
  summarise(x = mean(x)) %>% 
  ungroup() %>% 
  group_by(cov) %>% 
  summarise(x = max(x)) %>% 
  ungroup() %>% 
  left_join(checktable, by = 'cov') %>% 
  mutate(category = factor(category, rev(c('MP', 'terrain', 'land use', 
                                           'urban function', 'urban morphology')))) %>% 
  group_by(category) %>% 
  arrange(x, .by_group = TRUE) %>% 
  mutate(facet = which(cov == unique(cov)) * 10) %>% 
  ungroup()
  

tb.long <- tb.long %>% 
  left_join(tb.long.rank[c('cov', 'category', 'facet')], by = 'cov') %>%
  mutate(modelname = ifelse(model == 2, '+ terrain',
                            ifelse(model == 3, '+ landuse',
                                   ifelse(model == 4, '+ urban morphology', '+ urban function'))),
         modelname = factor(modelname, c('+ terrain', '+ landuse', '+ urban morphology', '+ urban function')),
         model = factor(model, c(2,3,4,5))) %>%
  mutate(ymax = max(y)) %>%
  group_by(cov, model) %>%
  mutate(ymax_cov_model = max(y)) %>% 
  mutate(ymax_cov_model = ifelse(ymax_cov_model > 0.5 * ymax, 0.5 * ymax, ymax_cov_model))

tb.long.rank <- tb.long.rank %>% 
  mutate(cov = ifelse(cov == 'dem', 'altitude', cov), 
         cov = ifelse(cov == 'barearea', 'bare area', cov), 
         cov = ifelse(cov == 'roaddensity', 'road density', cov), 
         cov = ifelse(cov == 'builtarea', 'Built-up area', cov), 
         cov = gsub('^ld_', '', cov), 
         cov = gsub('^poi_', '', cov), 
         cov = ifelse(cov == 'builtup', 'Built-up', cov), 
         cov = ifelse(cov == 'shopping', 'commercial', cov), 
         cov = str_to_sentence(cov))
  
  

plots <- vector('list', 5)


plots[[1]] <- ggplot() + 
  geom_vline(xintercept = 0, lwd = 0.3, col = 'grey', lty = 'dashed') + 
  geom_hline(yintercept = seq(10, 10, 10), col = 'white', lwd = 0.5) + 
  ggdist::stat_halfeye(data = tb.long[tb.long$category == 'MP' & tb.long$facet == 10 & tb.long$model == 2, ], 
                       aes(x = x, y = facet, fill = modelname, col = modelname, height = 30 * ymax_cov_model / ymax),
                       shape = 18, point_size = 0.5, size = 0.1, .width = c(0.95), alpha = 0.8, show.legend = F) + 
  ggdist::stat_halfeye(data = tb.long[tb.long$category == 'MP' & tb.long$facet == 10 & tb.long$model == 3, ], 
                       aes(x = x, y = facet, fill = modelname, col = modelname, height = 30 * ymax_cov_model / ymax),
                       shape = 18, point_size = 0.5, size = 0.1, .width = c(0.95), alpha = 0.8, show.legend = F) + 
  ggdist::stat_halfeye(data = tb.long[tb.long$category == 'MP' & tb.long$facet == 10 & tb.long$model == 4, ], 
                       aes(x = x, y = facet, fill = modelname, col = modelname, height = 30 * ymax_cov_model / ymax),
                       shape = 18, point_size = 0.5, size = 0.1, .width = c(0.95), alpha = 0.8, show.legend = F) + 
  ggdist::stat_halfeye(data = tb.long[tb.long$category == 'MP' & tb.long$facet == 10 & tb.long$model == 5, ], 
                       aes(x = x, y = facet, fill = modelname, col = modelname, height = 30 * ymax_cov_model / ymax),
                       shape = 18, point_size = 0.5, size = 0.1, .width = c(0.95), alpha = 0.8, show.legend = F) + 
  scale_y_continuous('', expand = c(0, 0), limits = c(10, 25), 
                     breaks = seq(10, 10, 10) + 2, labels = 'Mobile phone') +
  scale_x_continuous('', breaks = c(-0.4, 0, 0.4, 0.8, 1.2), limits = c(-0.4, 1.2)) +
  coord_cartesian(clip = 'off') + 
  theme_classic() +
  theme(aspect.ratio = 1 / 12 * 1, 
        axis.line = element_line(size = 0.2),
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 13), 
        axis.ticks = element_line(size = 0.2), 
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), 
        panel.background = element_rect(fill = 'grey97'), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(), 
        plot.margin = ggplot2::margin(0, 4, 0, 4)) + 
  guides(fill = guide_legend(''))



plots[[2]] <- ggplot() + 
  geom_vline(xintercept = 0, lwd = 0.3, col = 'grey', lty = 'dashed') + 
  geom_hline(yintercept = seq(10, 20, 10), col = 'white', lwd = 0.5) + 
  ggdist::stat_halfeye(data = tb.long[tb.long$category == 'terrain' & tb.long$facet == 10 & tb.long$model == 2, ], 
                       aes(x = x, y = facet, fill = modelname, col = modelname, height = 30 * ymax_cov_model / ymax),
                       shape = 18, point_size = 0.5, size = 0.1, .width = c(0.95), alpha = 0.8, show.legend = F) + 
  ggdist::stat_halfeye(data = tb.long[tb.long$category == 'terrain' & tb.long$facet == 10 & tb.long$model == 3, ], 
                       aes(x = x, y = facet, fill = modelname, col = modelname, height = 30 * ymax_cov_model / ymax),
                       shape = 18, point_size = 0.5, size = 0.1, .width = c(0.95), alpha = 0.8, show.legend = F) + 
  ggdist::stat_halfeye(data = tb.long[tb.long$category == 'terrain' & tb.long$facet == 10 & tb.long$model == 4, ], 
                       aes(x = x, y = facet, fill = modelname, col = modelname, height = 30 * ymax_cov_model / ymax),
                       shape = 18, point_size = 0.5, size = 0.1, .width = c(0.95), alpha = 0.8, show.legend = F) + 
  ggdist::stat_halfeye(data = tb.long[tb.long$category == 'terrain' & tb.long$facet == 10 & tb.long$model == 5, ], 
                       aes(x = x, y = facet, fill = modelname, col = modelname, height = 30 * ymax_cov_model / ymax),
                       shape = 18, point_size = 0.5, size = 0.1, .width = c(0.95), alpha = 0.8, show.legend = F) +    

  ggdist::stat_halfeye(data = tb.long[tb.long$category == 'terrain' & tb.long$facet == 20 & tb.long$model == 2, ], 
                       aes(x = x, y = facet, fill = modelname, col = modelname, height = 30 * ymax_cov_model / ymax),
                       shape = 18, point_size = 0.5, size = 0.1, .width = c(0.95), alpha = 0.8, show.legend = F) + 
  ggdist::stat_halfeye(data = tb.long[tb.long$category == 'terrain' & tb.long$facet == 20 & tb.long$model == 3, ], 
                       aes(x = x, y = facet, fill = modelname, col = modelname, height = 30 * ymax_cov_model / ymax),
                       shape = 18, point_size = 0.5, size = 0.1, .width = c(0.95), alpha = 0.8, show.legend = F) + 
  ggdist::stat_halfeye(data = tb.long[tb.long$category == 'terrain' & tb.long$facet == 20 & tb.long$model == 4, ], 
                       aes(x = x, y = facet, fill = modelname, col = modelname, height = 30 * ymax_cov_model / ymax),
                       shape = 18, point_size = 0.5, size = 0.1, .width = c(0.95), alpha = 0.8, show.legend = F) + 
  ggdist::stat_halfeye(data = tb.long[tb.long$category == 'terrain' & tb.long$facet == 20 & tb.long$model == 5, ], 
                       aes(x = x, y = facet, fill = modelname, col = modelname, height = 30 * ymax_cov_model / ymax),
                       shape = 18, point_size = 0.5, size = 0.1, .width = c(0.95), alpha = 0.8, show.legend = F) + 
  scale_y_continuous('Terrain', expand = c(0, 0), limits = c(10, 26), 
                     breaks = seq(10, 20, 10) + 2, labels = tb.long.rank[tb.long.rank$category == 'terrain', ]$cov) +
  scale_x_continuous('', breaks = c(-0.4, 0, 0.4, 0.8, 1.2), limits = c(-0.4, 1.2)) +
  coord_cartesian(clip = 'off') + 
  theme_classic() +
  theme(aspect.ratio = 1 / 12 * 2, 
        axis.line = element_line(size = 0.2),
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 13), 
        axis.ticks = element_line(size = 0.2), 
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.title.y = element_text(vjust = 12.5), 
        panel.background = element_rect(fill = 'grey97'), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(), 
        plot.margin = ggplot2::margin(0, 4, 0, 4)) + 
  guides(fill = guide_legend(''))




plots[[3]] <- ggplot() + 
  geom_vline(xintercept = 0, lwd = 0.3, col = 'grey', lty = 'dashed') + 
  geom_hline(yintercept = seq(10, 60, 10), col = 'white', lwd = 0.5) + 

  ggdist::stat_halfeye(data = tb.long[tb.long$category == 'land use' & tb.long$facet == 10 & tb.long$model == 3, ], 
                       aes(x = x, y = facet, fill = modelname, col = modelname, height = 30 * ymax_cov_model / ymax),
                       shape = 18, point_size = 0.5, size = 0.1, .width = c(0.95), alpha = 0.8, show.legend = F) + 
  ggdist::stat_halfeye(data = tb.long[tb.long$category == 'land use' & tb.long$facet == 10 & tb.long$model == 4, ], 
                       aes(x = x, y = facet, fill = modelname, col = modelname, height = 30 * ymax_cov_model / ymax),
                       shape = 18, point_size = 0.5, size = 0.1, .width = c(0.95), alpha = 0.8, show.legend = F) + 
  ggdist::stat_halfeye(data = tb.long[tb.long$category == 'land use' & tb.long$facet == 10 & tb.long$model == 5, ], 
                       aes(x = x, y = facet, fill = modelname, col = modelname, height = 30 * ymax_cov_model / ymax),
                       shape = 18, point_size = 0.5, size = 0.1, .width = c(0.95), alpha = 0.8, show.legend = F) +    

  ggdist::stat_halfeye(data = tb.long[tb.long$category == 'land use' & tb.long$facet == 20 & tb.long$model == 3, ], 
                       aes(x = x, y = facet, fill = modelname, col = modelname, height = 30 * ymax_cov_model / ymax),
                       shape = 18, point_size = 0.5, size = 0.1, .width = c(0.95), alpha = 0.8, show.legend = F) + 
  ggdist::stat_halfeye(data = tb.long[tb.long$category == 'land use' & tb.long$facet == 20 & tb.long$model == 4, ], 
                       aes(x = x, y = facet, fill = modelname, col = modelname, height = 30 * ymax_cov_model / ymax),
                       shape = 18, point_size = 0.5, size = 0.1, .width = c(0.95), alpha = 0.8, show.legend = F) + 
  ggdist::stat_halfeye(data = tb.long[tb.long$category == 'land use' & tb.long$facet == 20 & tb.long$model == 5, ], 
                       aes(x = x, y = facet, fill = modelname, col = modelname, height = 30 * ymax_cov_model / ymax),
                       shape = 18, point_size = 0.5, size = 0.1, .width = c(0.95), alpha = 0.8, show.legend = F) +    

  ggdist::stat_halfeye(data = tb.long[tb.long$category == 'land use' & tb.long$facet == 30 & tb.long$model == 3, ], 
                       aes(x = x, y = facet, fill = modelname, col = modelname, height = 30 * ymax_cov_model / ymax),
                       shape = 18, point_size = 0.5, size = 0.1, .width = c(0.95), alpha = 0.8, show.legend = F) + 
  ggdist::stat_halfeye(data = tb.long[tb.long$category == 'land use' & tb.long$facet == 30 & tb.long$model == 4, ], 
                       aes(x = x, y = facet, fill = modelname, col = modelname, height = 30 * ymax_cov_model / ymax),
                       shape = 18, point_size = 0.5, size = 0.1, .width = c(0.95), alpha = 0.8, show.legend = F) + 
  ggdist::stat_halfeye(data = tb.long[tb.long$category == 'land use' & tb.long$facet == 30 & tb.long$model == 5, ], 
                       aes(x = x, y = facet, fill = modelname, col = modelname, height = 30 * ymax_cov_model / ymax),
                       shape = 18, point_size = 0.5, size = 0.1, .width = c(0.95), alpha = 0.8, show.legend = F) +    

  ggdist::stat_halfeye(data = tb.long[tb.long$category == 'land use' & tb.long$facet == 40 & tb.long$model == 3, ], 
                       aes(x = x, y = facet, fill = modelname, col = modelname, height = 30 * ymax_cov_model / ymax),
                       shape = 18, point_size = 0.5, size = 0.1, .width = c(0.95), alpha = 0.8, show.legend = F) + 
  ggdist::stat_halfeye(data = tb.long[tb.long$category == 'land use' & tb.long$facet == 40 & tb.long$model == 4, ], 
                       aes(x = x, y = facet, fill = modelname, col = modelname, height = 30 * ymax_cov_model / ymax),
                       shape = 18, point_size = 0.5, size = 0.1, .width = c(0.95), alpha = 0.8, show.legend = F) + 
  ggdist::stat_halfeye(data = tb.long[tb.long$category == 'land use' & tb.long$facet == 40 & tb.long$model == 5, ], 
                       aes(x = x, y = facet, fill = modelname, col = modelname, height = 30 * ymax_cov_model / ymax),
                       shape = 18, point_size = 0.5, size = 0.1, .width = c(0.95), alpha = 0.8, show.legend = F) +    

  ggdist::stat_halfeye(data = tb.long[tb.long$category == 'land use' & tb.long$facet == 50 & tb.long$model == 3, ], 
                       aes(x = x, y = facet, fill = modelname, col = modelname, height = 30 * ymax_cov_model / ymax),
                       shape = 18, point_size = 0.5, size = 0.1, .width = c(0.95), alpha = 0.8, show.legend = F) + 
  ggdist::stat_halfeye(data = tb.long[tb.long$category == 'land use' & tb.long$facet == 50 & tb.long$model == 4, ], 
                       aes(x = x, y = facet, fill = modelname, col = modelname, height = 30 * ymax_cov_model / ymax),
                       shape = 18, point_size = 0.5, size = 0.1, .width = c(0.95), alpha = 0.8, show.legend = F) + 
  ggdist::stat_halfeye(data = tb.long[tb.long$category == 'land use' & tb.long$facet == 50 & tb.long$model == 5, ], 
                       aes(x = x, y = facet, fill = modelname, col = modelname, height = 30 * ymax_cov_model / ymax),
                       shape = 18, point_size = 0.5, size = 0.1, .width = c(0.95), alpha = 0.8, show.legend = F) +    

  ggdist::stat_halfeye(data = tb.long[tb.long$category == 'land use' & tb.long$facet == 60 & tb.long$model == 3, ], 
                       aes(x = x, y = facet, fill = modelname, col = modelname, height = 30 * ymax_cov_model / ymax),
                       shape = 18, point_size = 0.5, size = 0.1, .width = c(0.95), alpha = 0.8, show.legend = F) + 
  ggdist::stat_halfeye(data = tb.long[tb.long$category == 'land use' & tb.long$facet == 60 & tb.long$model == 5, ], 
                       aes(x = x, y = facet, fill = modelname, col = modelname, height = 30 * ymax_cov_model / ymax),
                       shape = 18, point_size = 0.5, size = 0.1, .width = c(0.95), alpha = 0.8, show.legend = F) + 
  scale_y_continuous('Land use', expand = c(0, 0), limits = c(10, 67), 
                     breaks = seq(10, 60, 10) + 2, labels = tb.long.rank[tb.long.rank$category == 'land use', ]$cov) +
  scale_x_continuous('', breaks = c(-0.4, 0, 0.4, 0.8, 1.2), limits = c(-0.4, 1.2)) +
  coord_cartesian(clip = 'off') + 
  theme_classic() +
  theme(aspect.ratio = 1 / 12 * 6, 
        axis.line = element_line(size = 0.2),
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 13), 
        axis.ticks = element_line(size = 0.2), 
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.title.y = element_text(vjust = 7), 
        panel.background = element_rect(fill = 'grey97'), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(), 
        plot.margin = ggplot2::margin(0, 4, 0, 4)) + 
  guides(fill = guide_legend(''))




plots[[4]] <- ggplot() + 
  geom_vline(xintercept = 0, lwd = 0.3, col = 'grey', lty = 'dashed') + 
  geom_hline(yintercept = seq(10, 20, 10), col = 'white', lwd = 0.5) + 
  
  ggdist::stat_halfeye(data = tb.long[tb.long$category == 'urban morphology' & tb.long$facet == 10 & tb.long$model == 4, ], 
                       aes(x = x, y = facet, fill = modelname, col = modelname, height = 30 * ymax_cov_model / ymax),
                       shape = 18, point_size = 0.5, size = 0.1, .width = c(0.95), alpha = 0.8, show.legend = F) + 
  
  ggdist::stat_halfeye(data = tb.long[tb.long$category == 'urban morphology' & tb.long$facet == 20 & tb.long$model == 4, ], 
                       aes(x = x, y = facet, fill = modelname, col = modelname, height = 30 * ymax_cov_model / ymax),
                       shape = 18, point_size = 0.5, size = 0.1, .width = c(0.95), alpha = 0.8, show.legend = F) + 
  ggdist::stat_halfeye(data = tb.long[tb.long$category == 'urban morphology' & tb.long$facet == 20 & tb.long$model == 5, ], 
                       aes(x = x, y = facet, fill = modelname, col = modelname, height = 30 * ymax_cov_model / ymax),
                       shape = 18, point_size = 0.5, size = 0.1, .width = c(0.95), alpha = 0.8, show.legend = F) + 
  scale_color_manual(values = c("#00A087", "#3C5488")) + 
  scale_y_continuous('Urban\nmorphology', expand = c(0, 0), limits = c(10, 27), 
                     breaks = seq(10, 20, 10) + 2, labels = tb.long.rank[tb.long.rank$category == 'urban morphology', ]$cov) +
  scale_x_continuous('', breaks = c(-0.4, 0, 0.4, 0.8, 1.2), limits = c(-0.4, 1.2)) +
  coord_cartesian(clip = 'off') + 
  theme_classic() +
  theme(aspect.ratio = 1 / 12 * 2, 
        axis.line = element_line(size = 0.2),
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 13), 
        axis.ticks = element_line(size = 0.2), 
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.title.y = element_text(vjust = 2.2), 
        panel.background = element_rect(fill = 'grey97'), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(), 
        plot.margin = ggplot2::margin(0, 4, 0, 4)) + 
  guides(fill = guide_legend(''))



plots[[5]] <- ggplot() + 
  geom_vline(xintercept = 0, lwd = 0.3, col = 'grey', lty = 'dashed') + 
  geom_hline(yintercept = seq(10, 70, 10), col = 'white', lwd = 0.5) + 
  
  ggdist::stat_halfeye(data = tb.long[tb.long$category == 'urban function' & tb.long$facet == 10 & tb.long$model == 5, ], 
                       aes(x = x, y = facet, fill = modelname, col = modelname, height = 30 * ymax_cov_model / ymax),
                       shape = 18, point_size = 0.5, size = 0.1, .width = c(0.95), alpha = 0.8, show.legend = F) + 
  ggdist::stat_halfeye(data = tb.long[tb.long$category == 'urban function' & tb.long$facet == 20 & tb.long$model == 5, ], 
                       aes(x = x, y = facet, fill = modelname, col = modelname, height = 30 * ymax_cov_model / ymax),
                       shape = 18, point_size = 0.5, size = 0.1, .width = c(0.95), alpha = 0.8, show.legend = F) + 
  ggdist::stat_halfeye(data = tb.long[tb.long$category == 'urban function' & tb.long$facet == 30 & tb.long$model == 5, ], 
                       aes(x = x, y = facet, fill = modelname, col = modelname, height = 30 * ymax_cov_model / ymax),
                       shape = 18, point_size = 0.5, size = 0.1, .width = c(0.95), alpha = 0.8, show.legend = F) + 
  ggdist::stat_halfeye(data = tb.long[tb.long$category == 'urban function' & tb.long$facet == 40 & tb.long$model == 5, ], 
                       aes(x = x, y = facet, fill = modelname, col = modelname, height = 30 * ymax_cov_model / ymax),
                       shape = 18, point_size = 0.5, size = 0.1, .width = c(0.95), alpha = 0.8, show.legend = F) + 
  ggdist::stat_halfeye(data = tb.long[tb.long$category == 'urban function' & tb.long$facet == 50 & tb.long$model == 5, ], 
                       aes(x = x, y = facet, fill = modelname, col = modelname, height = 30 * ymax_cov_model / ymax),
                       shape = 18, point_size = 0.5, size = 0.1, .width = c(0.95), alpha = 0.8, show.legend = F) + 
  ggdist::stat_halfeye(data = tb.long[tb.long$category == 'urban function' & tb.long$facet == 60 & tb.long$model == 5, ], 
                       aes(x = x, y = facet, fill = modelname, col = modelname, height = 30 * ymax_cov_model / ymax),
                       shape = 18, point_size = 0.5, size = 0.1, .width = c(0.95), alpha = 0.8, show.legend = F) + 
  ggdist::stat_halfeye(data = tb.long[tb.long$category == 'urban function' & tb.long$facet == 70 & tb.long$model == 5, ], 
                       aes(x = x, y = facet, fill = modelname, col = modelname, height = 30 * ymax_cov_model / ymax),
                       shape = 18, point_size = 0.5, size = 0.1, .width = c(0.95), alpha = 0.8, show.legend = F) + 
  scale_y_continuous('Urban function', expand = c(0, 0), limits = c(10, 77), 
                     breaks = seq(10, 70, 10) + 2, labels = tb.long.rank[tb.long.rank$category == 'urban function', ]$cov) +
  scale_x_continuous('Posterior distribution of coefficient', breaks = c(-0.4, 0, 0.4, 0.8, 1.2), limits = c(-0.4, 1.2)) +
  coord_cartesian(clip = 'off') + 
  theme_classic() +
  theme(aspect.ratio = 1 / 12 * 7, 
        axis.line = element_line(size = 0.2),
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 13),
        axis.ticks = element_line(size = 0.2), 
        axis.title.y = element_text(vjust = 5.5), 
        panel.background = element_rect(fill = 'grey97'), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(), 
        plot.margin = ggplot2::margin(0, 4, 0, 4)) + 
  guides(fill = guide_legend(''))

