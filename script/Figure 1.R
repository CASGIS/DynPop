
library(foreach)
library(tidyverse)
library(doParallel)
library(ggplot2)
library(Metrics)


model.type <- c('GLM', 'Randomforest', 'XGBoost', 'STmodeling', 'GWR')

plots <- vector('list', 5)

for (k in 1:5) {
  
  print(k)
  
  model <- model.type[k]
  
  files <- list.files(paste0('result/', model, '_valid'), pattern = '^popvalid*', full.names = T)

  model <- ifelse(model == 'GLM', 'LM', model)
  model <- ifelse(model == 'Randomforest', 'Random forest', model)
  model <- ifelse(model == 'STmodeling', 'Bayesian\nspace-time model', model)
  
  valid <- bind_rows(foreach(i = 1:length(files)) %do% read_csv(files[i], show_col_types = F))
  
  valid <- bind_rows(foreach(i = 1:5) %do% {
    valid %>% 
      filter(model == i) %>% 
      summarise(coeff = (lm(popest ~ census - 1, .) %>% summary)$coefficients[1], 
                R2 = (lm(popest ~ census - 1, .) %>% summary)$r.squared, 
                RMSE = rmse(popest, census)) %>% 
      mutate(model = i)
  })
  
  valid <- valid %>% 
    mutate(R2 = R2 - 0.3)
  
  valid.r2 <- valid %>% 
    group_by(model) %>%
    summarise(coeff = mean(coeff),
              R2 = mean(R2),
              RMSE = mean(RMSE),
              model = unique(model)) %>%
    mutate(xcen = model - 0.5, 
           xmin = model - 1, 
           xmax = model, 
           ymin = c(0, R2[1:4]), 
           ymax = R2) %>% 
    mutate(model = as.character(model)) %>% 
    mutate(ylab = ifelse(ymax > ymin, ymax + 0.02, ymax - 0.02), 
           lab = ifelse(model == 1, sprintf('%.02f', R2 + 0.3), sprintf('%.02f', ymax - ymin)), 
           lab = ifelse(model != 1 & !str_detect(lab, '-'), paste0('+', lab), lab)) %>% 
    mutate(model = c('mobile phone', '+ terrain', '+ land use', '+ urban morphology', '+ urban function'), 
           model = factor(model, levels = unique(model)))
  
  if (k == 1) {
    plots[[k]] <- ggplot(valid.r2) + 
      geom_hline(yintercept = c(0.3, 0.4, 0.5, 0.6), col = 'grey80', lwd = 0.2) + 
      geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = model), alpha = 0.9) + 
      geom_text(aes(xcen, ylab, label = lab), size = 3.2) + 
      scale_y_continuous('R^2', limits = c(0, 0.7), 
                         breaks = c(0, seq(0.1, 0.7, 0.2)), labels = c(0, seq(0.4, 1, 0.2)), 
                         expand = c(0.005, 0.005)) + 
      scale_x_continuous(model, expand = c(0.015, 0.015), breaks = c()) + 
      theme_classic() + 
      theme(aspect.ratio = 2, 
            text = element_text(size = 15), 
            plot.margin = ggplot2::margin(4, 10, 4, 10), 
            axis.title = element_text(size = 14), 
            axis.line = element_line(size = 0.2), 
            axis.ticks = element_line(size = 0.4), 
            panel.grid = element_blank(), 
            legend.position = c(0.67, 0.23), 
            legend.key.size = unit(3.5, 'mm'), 
            legend.spacing.y = unit(1.5, 'mm'), 
            legend.text = element_text(size = 10), 
            legend.background = element_blank(), 
            axis.title.x.bottom = element_text(vjust = -0.5)) + 
      guides(fill = guide_legend(title = '', byrow = TRUE))
  } else {
    plots[[k]] <- ggplot(valid.r2) + 
      geom_hline(yintercept = c(0.3, 0.4, 0.5, 0.6), col = 'grey80', lwd = 0.2) + 
      geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = model), 
                alpha = 0.9, show.legend = F) + 
      geom_text(aes(xcen, ylab, label = lab), size = 3.2) + 
      scale_y_continuous('R^2', limits = c(0, 0.7), 
                         breaks = c(0, seq(0.1, 0.7, 0.2)), labels = c(0, seq(0.4, 1, 0.2)), 
                         expand = c(0.005, 0.005)) + 
      scale_x_continuous(model, expand = c(0.015, 0.015), breaks = c()) + 
      theme_classic() + 
      theme(aspect.ratio = 2, 
            text = element_text(size = 15), 
            plot.margin = ggplot2::margin(4, 10, 4, 10), 
            axis.title = element_text(size = 14), 
            axis.line = element_line(size = 0.2), 
            axis.ticks = element_line(size = 0.4), 
            axis.line.y = element_blank(), 
            axis.text.y = element_blank(), 
            axis.title.y = element_blank(), 
            axis.ticks.y = element_blank(), 
            panel.grid = element_blank(), 
            axis.title.x.bottom = element_text(vjust = -0.5)) + 
      guides(fill = guide_legend(title = ''))
  }
}


for (k in 1:5) {
  
  print(k)
  
  model <- model.type[k]
  
  files <- list.files(paste0('result/', model, '_valid'), pattern = '^popvalid*', full.names = T)

  model <- ifelse(model == 'GLM', 'LM', model)
  model <- ifelse(model == 'Randomforest', 'Random forest', model)
  model <- ifelse(model == 'STmodeling', 'Bayesian\nspace-time model', model)
  
  valid <- bind_rows(foreach(i = 1:length(files)) %do% read_csv(files[i], show_col_types = F))
  
  valid <- bind_rows(foreach(i = 1:5) %do% {
    valid %>% 
      filter(model == i) %>% 
      summarise(coeff = (lm(popest ~ census - 1, .) %>% summary)$coefficients[1], 
                R2 = (lm(popest ~ census - 1, .) %>% summary)$r.squared, 
                RMSE = rmse(popest, census)) %>% 
      mutate(model = i)
  })
  
  valid.rmse <- valid %>% 
    mutate(RMSE = (RMSE - 67542.64) / 67542.64) %>%
    mutate(xcen = model - 0.5, 
           xmin = model - 1, 
           xmax = model, 
           ymin = c(0, RMSE[1:4]), 
           ymax = RMSE) %>% 
    mutate(model = as.character(model)) %>% 
    mutate(ylab = ifelse(ymax > ymin, ymax + 0.14, ymax - 0.06), 
           lab = paste0(sprintf('%.01f', RMSE * 100), '%'), 
           ylab_delta = ifelse(ymax > ymin, ymax + 0.06, ymax - 0.14), 
           lab_delta = ifelse(model == 1, 
                              paste0(sprintf('%.01f', RMSE * 100), '%'), 
                              paste0(sprintf('%.01f', (ymax - ymin) * 100), '%')), 
           lab_delta = ifelse(!str_detect(lab_delta, '-'), paste0('+', lab_delta), lab_delta), 
           lab_delta = paste0('(', lab_delta, ')')) %>% 
    mutate(model = c('mobile phone', '+ terrain', '+ land use', '+ urban morphology', '+ urban function'), 
           model = factor(model, levels = unique(model)))

  if (k == 1) {
    plots[[k]] <- ggplot(valid.rmse) + 
      geom_hline(yintercept = c(-0.5, 0, 0.5), col = 'grey80', lwd = 0.2) +
      geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = model), 
                alpha = 0.9, show.legend = F) + 
      geom_rect(aes(xmin = 0, xmax = 1, ymin = -1, ymax = 0), 
                fill = 'grey80', alpha = 0.9, show.legend = F) + 
      geom_text(aes(x, y, label = baseline), data.frame(x = 0.5, y = -0.5, baseline = 67542), 
                size = 3.2, angle = 90) + 
      geom_text(aes(xcen, ylab, label = lab), size = 3.2) + 
      geom_text(aes(xcen, ylab_delta, label = lab_delta), size = 2.8) + 
      scale_y_continuous('Change in RMSE', limits = c(-1, 1.5), expand = c(0.007, 0.007)) +
      scale_x_continuous(model, expand = c(0.01, 0.01), breaks = c()) + 
      theme_classic() + 
      theme(aspect.ratio = 2, 
            text = element_text(size = 15), 
            plot.margin = ggplot2::margin(0, 10, 4, 10), 
            axis.title = element_text(size = 14), 
            axis.line = element_line(size = 0.2), 
            axis.ticks = element_line(size = 0.4), 
            panel.grid = element_blank(), 
            legend.position = c(0.63, 0.2), 
            legend.key.size = unit(3.5, 'mm'), 
            legend.spacing.y = unit(1.5, 'mm'), 
            legend.background = element_blank(), 
            axis.title.x.bottom = element_text(vjust = -0.5)) + 
      guides(fill = guide_legend(title = '', byrow = TRUE))
  } else {
    plots[[k]] <- ggplot(valid.rmse) + 
      geom_hline(yintercept = c(-0.5, 0, 0.5), col = 'grey80', lwd = 0.2) +
      geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = model), 
                alpha = 0.9, show.legend = F) + 
      geom_rect(aes(xmin = 0, xmax = 1, ymin = ymin, ymax = ymax), valid.rmse[1, ], 
                fill = 'grey80', alpha = 0.9, show.legend = F) + 
      geom_text(aes(xcen, ylab, label = lab), size = 3.2) + 
      geom_text(aes(xcen, ylab_delta, label = lab_delta), size = 2.8) + 
      scale_y_continuous(limits = c(-1, 1.5), expand = c(0.007, 0.007)) +
      scale_x_continuous(model, expand = c(0.01, 0.01), breaks = c()) + 
      theme_classic() + 
      theme(aspect.ratio = 2, 
            text = element_text(size = 15), 
            plot.margin = ggplot2::margin(0, 10, 4, 10), 
            axis.title = element_text(size = 14), 
            axis.line = element_line(size = 0.2), 
            axis.ticks = element_line(size = 0.4), 
            axis.line.y = element_blank(), 
            axis.text.y = element_blank(), 
            axis.title.y = element_blank(), 
            axis.ticks.y = element_blank(), 
            panel.grid = element_blank(), 
            legend.position = c(0.65, 0.2), 
            legend.background = element_blank(), 
            axis.title.x.bottom = element_text(vjust = -0.5)) + 
      guides(fill = guide_legend(title = ''))
  }
}




