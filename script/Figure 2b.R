
library(sf)
library(raster)
library(foreach)
library(tidyverse)
library(doParallel)
library(exactextractr)
library(units)
library(Metrics)
library(ggplot2)



files <- list.files('result/Randomforest_model', full.names = T)

rank <- bind_rows(foreach(k = 2:5) %do% {  
  files.mod <- files[str_detect(files, paste0('mod', sprintf('%02d', k)))]
  
  bind_rows(foreach(i = 1:length(files.mod)) %do% {
    rf <- readRDS(files.mod[i])
    rf$importance %>% 
      as.data.frame() %>% 
      mutate(cov = rownames(.)) %>% 
      mutate(cov = ifelse(str_detect(cov, 'MP'), 'MP', cov))
  }) %>% 
    mutate(model = as.character(k)) %>% 
    rename(IncMSE = `%IncMSE`)
}) %>% 
  group_by(cov, model) %>% 
  mutate(MSEmean = mean(IncMSE)) %>% 
  ungroup() %>% 
  group_by(cov) %>%
  mutate(MSEmax = max(MSEmean)) %>% 
  ungroup()

name <- data.frame(filename = c("ASTGTM_beijing_100m", "slope_beijing_100m", "globaland_10", "globaland_20", "globaland_30", 
                                "globaland_60", "globaland_80", "globaland_90", "POIdentisy_catering", "POIdentisy_education",
                                "POIdentisy_leisure", "POIdentisy_residential", "POIdentisy_shopping", "POIdentisy_transport",
                                "POIdentisy_working", "roaddensity", "builtArea"), 
                   varname = c('dem', 'slope', 'ld_cropland', 'ld_woodland', 'ld_grassland', 'ld_waters', 'ld_builtup', 
                               'ld_barearea', 'poi_catering', 'poi_education', 'poi_leisure', 'poi_residential', 'poi_shopping',
                               'poi_transport', 'poi_working', 'roaddensity', 'builtarea'))

checktable <- 
  data.frame(cov = c('MP', 'dem', 'slope', 'ld_cropland', 'ld_woodland', 'ld_grassland', 'ld_waters', 'ld_builtup', 
                     'ld_barearea', 'poi_catering', 'poi_education', 'poi_leisure', 'poi_residential', 'poi_shopping',
                     'poi_transport', 'poi_working', 'roaddensity', 'builtarea'), 
             category = c('MP', rep('terrain', 2), rep('land use', 6), rep('urban function', 7), rep('urban morphology', 2)))

rank <- rank %>% 
  left_join(name, by = c('cov' = 'filename')) %>% 
  mutate(varname = ifelse(is.na(varname), 'MP', varname), 
         cov = varname, 
         cov = factor(cov, unique(cov)), 
         varname = NULL) %>% 
  left_join(checktable, by = 'cov') %>% 
  mutate(category = factor(category, rev(c('MP', 'terrain', 'land use', 
                                           'urban function', 'urban morphology')))) %>% 
  arrange(MSEmax) %>% 
  mutate(cov = ifelse(cov == 'dem', 'altitude', cov), 
         cov = ifelse(cov == 'barearea', 'bare area', cov), 
         cov = ifelse(cov == 'roaddensity', 'road density', cov), 
         cov = ifelse(cov == 'builtarea', 'Built-up area', cov), 
         cov = gsub('^ld_', '', cov), 
         cov = gsub('^poi_', '', cov), 
         cov = ifelse(cov == 'builtup', 'Built-up', cov), 
         cov = ifelse(cov == 'shopping', 'commercial', cov), 
         cov = str_to_sentence(cov)) %>% 
  mutate(cov = factor(cov, unique(cov))) %>% 
  mutate(modelname = ifelse(model == 2, '+ terrain', 
                            ifelse(model == 3, '+ landuse', 
                                   ifelse(model == 4, '+ urban morphology', '+ urban function'))), 
         modelname = factor(modelname, c('+ terrain', '+ landuse', '+ urban morphology', '+ urban function')))



plots <- vector('list', 5)

plots[[1]] <- ggplot(rank[rank$category == 'MP', ] %>% mutate(cov = factor(cov, unique(cov)))) + 
  geom_vline(xintercept = 0, col = 'grey', lwd = 0.3, lty = 'dashed') + 
  geom_boxplot(aes(y = cov, x = IncMSE, fill = modelname), 
               width = 1, size = 0.1, col = 'grey30', alpha = 0.8, outlier.size = 0, show.legend = F) + 
  scale_y_discrete('', labels = 'Mobile phone', drop = F) + 
  scale_x_continuous('', breaks = seq(0, 8, 2), limits = c(-0.5, 8), expand = c(0.01, 0.01)) +
  xlab('%IncMSE') + 
  theme_classic() +
  theme(aspect.ratio = 1/12*1, 
        axis.line = element_line(size = 0.2),
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 13), 
        axis.ticks = element_line(size = 0.2), 
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.title.y = element_text(), 
        panel.background = element_rect(fill = 'grey97'), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(), 
        plot.margin = ggplot2::margin(-10, 0, -10, 0)) + 
  guides(fill = guide_legend(''))


plots[[2]] <- ggplot(rank[rank$category == 'terrain', ] %>% mutate(cov = factor(cov, unique(cov)))) + 
  geom_vline(xintercept = 0, col = 'grey', lwd = 0.3, lty = 'dashed') + 
  geom_hline(yintercept = seq(1.5, 1.5, 1), col = 'white', lwd = 0.5) +
  geom_boxplot(aes(y = cov, x = IncMSE, fill = modelname), 
               width = 1, size = 0.1, col = 'grey30', alpha = 0.8, outlier.size = 0, show.legend = F) + 
  scale_y_discrete('', labels = as.character(unique(rank[rank$category == 'terrain', ]$cov)), drop = F) + 
  scale_x_continuous('', breaks = seq(0, 8, 2), limits = c(-0.5, 8), expand = c(0.01, 0.01)) +
  xlab('%IncMSE') + 
  theme_classic() +
  theme(aspect.ratio = 1/12*2, 
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
        plot.margin = ggplot2::margin(-10, 0, -10, 0)) + 
  guides(fill = guide_legend(''))


plots[[3]] <- ggplot(rank[rank$category == 'land use', ] %>% mutate(cov = factor(cov, unique(cov)))) + 
  geom_vline(xintercept = 0, col = 'grey', lwd = 0.3, lty = 'dashed') + 
  geom_hline(yintercept = seq(1.5, 5.5, 1), col = 'white', lwd = 0.5) +
  geom_boxplot(aes(y = cov, x = IncMSE, fill = modelname), 
               width = 1, size = 0.1, col = 'grey30', alpha = 0.8, outlier.size = 0, show.legend = F) + 
  scale_y_discrete('', labels = as.character(unique(rank[rank$category == 'land use', ]$cov)), drop = F) + 
  scale_x_continuous('', breaks = seq(0, 8, 2), limits = c(-0.5, 8), expand = c(0.01, 0.01)) +
  xlab('%IncMSE') + 
  theme_classic() +
  theme(aspect.ratio = 1/12*6, 
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
        plot.margin = ggplot2::margin(-10, 0, -10, 0)) + 
  guides(fill = guide_legend(''))


plots[[4]] <- ggplot(rank[rank$category == 'urban morphology', ] %>% mutate(cov = factor(cov, unique(cov)))) + 
  geom_vline(xintercept = 0, col = 'grey', lwd = 0.3, lty = 'dashed') + 
  geom_hline(yintercept = seq(1.5, 1.5, 1), col = 'white', lwd = 0.5) +
  geom_boxplot(aes(y = cov, x = IncMSE, fill = modelname), 
               width = 0.5, size = 0.1, col = 'grey30', alpha = 0.8, outlier.size = 0, show.legend = F) + 
  scale_y_discrete('', labels = as.character(unique(rank[rank$category == 'urban morphology', ]$cov)), drop = F) + 
  scale_x_continuous('', breaks = seq(0, 8, 2), limits = c(-0.5, 8), expand = c(0.01, 0.01)) +
  xlab('%IncMSE') + 
  theme_classic() +
  theme(aspect.ratio = 1/12*2, 
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
        plot.margin = ggplot2::margin(-10, 0, -10, 0)) + 
  guides(fill = guide_legend(''))


plots[[5]] <- ggplot(rank[rank$category == 'urban function', ] %>% mutate(cov = factor(cov, unique(cov)))) + 
  geom_vline(xintercept = 0, col = 'grey', lwd = 0.3, lty = 'dashed') + 
  geom_hline(yintercept = seq(1.5, 6.5, 1), col = 'white', lwd = 0.5) +
  geom_boxplot(aes(y = cov, x = IncMSE, fill = modelname), 
               width = 0.5, size = 0.15, col = 'grey30', alpha = 0.8, outlier.size = 0, show.legend = F) + 
  scale_y_discrete('', labels = as.character(unique(rank[rank$category == 'urban function', ]$cov)), drop = F) + 
  scale_x_continuous('%IncMSE', breaks = seq(0, 8, 2), limits = c(-0.5, 8), expand = c(0.01, 0.01)) +
  xlab('%IncMSE') + 
  theme_classic() +
  theme(aspect.ratio = 1/12*7, 
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
        plot.margin = ggplot2::margin(-10, 0, -10, 0)) + 
  guides(fill = guide_legend(''))

