library(tidyverse)

results.chr <- list.files('~/rwa/party_data/party_tune/cv_results/full_grid_3/')

tuning.grid.tb <- map_dfr(.x = results.chr,
                          .f = function(x){
                                 load(paste0('~/rwa/party_data/party_tune/cv_results/full_grid_3/', x))
                                 wrksp.name.sym <- str_remove(string = x, pattern = '^full\\_grid\\_3\\_') %>%
                                                     str_remove(pattern = '\\.RData$') %>%
                                                       sym()
                                 return(eval(expr(pluck(!!wrksp.name.sym, 'tuning.grid'))))
                               },
                          .id = 'Data.Partition'
                  )

# check 1

load(paste0('~/rwa/party_data/party_tune/cv_results/full_grid_3/', results.chr[2]))

results.chr[2]

identical(
  arrange(party.cv.output.data.partition.10$tuning.grid, auc),
  filter(tuning.grid.tb, Data.Partition == 2) %>% select(-Data.Partition) %>% arrange(auc)
)  

# check 5

load(paste0('~/rwa/party_data/party_tune/cv_results/full_grid_3/', results.chr[5]))

results.chr[5]

identical(
  arrange(party.cv.output.data.partition.13$tuning.grid, auc),
  filter(tuning.grid.tb, Data.Partition == 5) %>% select(-Data.Partition) %>% arrange(auc)
)  


# # # 

tuning.grid.summary.tb <- group_by(tuning.grid.tb, mtry, mincriterion, ntree) %>%
  summarise(mean.auc = mean(auc), sd.auc = sd(auc)) %>%
    ungroup() %>%
      arrange(desc(mean.auc), sd.auc)

tuning.grid.summary.tb

summary(tuning.grid.summary.tb$mean.auc)

summary(tuning.grid.summary.tb$sd.auc)

filter(tuning.grid.summary.tb, mean.auc == max(mean.auc))

# Thus our selected tuning parameters are:

mtry.selected <- filter(tuning.grid.summary.tb, mean.auc == max(mean.auc)) %>%
                   pull(mtry)

mtry.selected # 50

mincriterion.selected <- filter(tuning.grid.summary.tb, mean.auc == max(mean.auc)) %>%
                           pull(mincriterion)

mincriterion.selected # 0.45

ntree.selected <- filter(tuning.grid.summary.tb, mean.auc == max(mean.auc)) %>%
                    pull(ntree)

ntree.selected # 1000

# save(list = c('mtry.selected', 'mincriterion.selected', 'ntree.selected'), file = '~/rwa/party_data/party_tune/cv_results/selected_tuning_parameter_values.RData')
