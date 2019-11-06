library(tidyverse)

results.chr <- list.files('~/rwa/party_data/party_tune/cv_results/full_grid_2/')

tuning.grid.tb <- map_dfr(.x = results.chr,
                          .f = function(x){
                                 load(paste0('~/rwa/party_data/party_tune/cv_results/full_grid_2/', x))
                                 wrksp.name.sym <- str_remove(string = x, pattern = '^full\\_grid\\_2\\_') %>%
                                                     str_remove(pattern = '\\.RData$') %>%
                                                       sym()
                                 return(eval(expr(pluck(!!wrksp.name.sym, 'tuning.grid'))))
                               },
                          .id = 'Data.Partition'
                  )

# write a check

load(paste0('~/rwa/party_data/party_tune/cv_results/full_grid_2/', x[2]))



filter(tuning.grid.tb, Data.Partition == 2)



tuning.grid.summary.tb <- group_by(tuning.grid.tb, mtry, mincriterion, ntree) %>%
  summarise(mean.auc = mean(auc), sd.auc = sd(auc)) %>%
    ungroup() %>%
      arrange(desc(mean.auc), sd.auc)

tuning.grid.summary.tb

summary(tuning.grid.summary.tb$mean.auc)

max(tuning.grid.summary.tb$mean.auc)

filter(tuning.grid.summary.tb, mean.auc > 0.83) %>%
  arrange(sd.auc)

summary(tuning.grid.tb$mincriterion)

filter(tuning.grid.summary.tb, mean.auc < 0.6) %>%
  arrange(mean.auc)


tuning.grid.auc.p <- ggplot(data = tuning.grid.summary.tb, aes(x = mtry, y = mincriterion, fill = mean.auc)) +
  geom_point(shape = 21, size = 4) +
  scale_fill_viridis_c() + 
  facet_wrap(facets = 'ntree', ncol = 2)

tuning.grid.auc.p

filter(tuning.grid.summary.tb, mean.auc > 0.6 ) %>%
  ggplot(aes(x = mtry, y = mincriterion, fill = mean.auc)) +
    geom_point(shape = 21, size = 4) +
    scale_fill_viridis_c() + 
    facet_wrap(facets = 'ntree', ncol = 2)


filter(tuning.grid.summary.tb, mean.auc > 0.7 ) %>%
  ggplot(aes(x = mtry, y = mincriterion, fill = mean.auc)) +
    geom_point(shape = 21, size = 4) +
    scale_fill_viridis_c() + 
    facet_wrap(facets = 'ntree', ncol = 2)



filter(tuning.grid.summary.tb, mean.auc > 0.831 ) %>%
  ggplot(aes(x = mtry, y = mincriterion, fill = mean.auc)) +
    geom_point(shape = 21, size = 4) +
    scale_fill_viridis_c() + 
    facet_wrap(facets = 'ntree', ncol = 2)


filter(tuning.grid.summary.tb, mean.auc > 0.8325 ) %>%
  arrange(sd.auc)

summary(tuning.grid.summary.tb$sd.auc)

arrange(tuning.grid.summary.tb, desc(mean.auc))



# ggsave(file = '~/rwa/party_data/party_tune/cv_results/full_grid_1/tuning_grid_mean_auc.pdf')

filter(tuning.grid.summary.tb, mean.auc > 0.83) %>%
ggplot(aes(x = mtry, y = mincriterion, fill = mean.auc)) +
  geom_point(shape = 21, size = 4) +
  scale_fill_viridis_c() + 
  facet_wrap(facets = 'ntree', ncol = 2)

# ggsave(file = '~/rwa/party_data/party_tune/cv_results/full_grid_1/tuning_grid_mean_auc_filter_1.pdf')

ggplot(data = tuning.grid.summary.tb, aes(x = mtry, y = mincriterion, fill = sd.auc)) +
  geom_point(shape = 21, size = 4) +
  scale_fill_viridis_c() + 
  facet_wrap(facets = 'ntree', ncol = 2)

# ggsave(file = '~/rwa/party_data/party_tune/cv_results/full_grid_1/tuning_grid_sd_auc.pdf')

filter(tuning.grid.summary.tb, mean.auc == max(mean.auc))

length(unique(tuning.grid.tb$mtry)) * length(unique(tuning.grid.tb$mincriterion)) * length(unique(tuning.grid.tb$ntree))

group_by(tuning.grid.tb, mtry, mincriterion, ntree) %>%
  summarise(median.auc = median(auc)) %>%
    ungroup() %>%
      ggplot(aes(x = median.auc)) +
        geom_histogram()
