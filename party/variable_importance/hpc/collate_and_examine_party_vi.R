library(glue)
library(tidyverse)

results.chr <- list.files(path = '~/rwa/party_data/variable_importance/')

vi.tb <- map_dfr(.x = results.chr,
                 .f = function(x){
                        load(paste0('~/rwa/party_data/variable_importance/', x))
                         vi.ls.sym <- str_remove(string = x, pattern = '\\.RData') %>%
                                        glue('.ls') %>%                                       
                                          sym()
                         VI.i.tb <- eval(
                                      expr(
                                        map_dfr(!!vi.ls.sym,
                                                function(x){
                                                  tibble(Variable = names(x), Importance = x)
                                                },
                                               .id = 'Permutation'
                                        )
                                      )
                                    )
                         return(VI.i.tb)
                      },
                 .id = 'Forest'
         )

vi.tb

# Check:

results.chr[[4]]

load('~/rwa/party_data/variable_importance/cforest.12.vi.RData')

cf.12.test.tb <- tibble(Variable = names(cforest.12.vi.ls[[6]]),
                       Importance.Manual = cforest.12.vi.ls[[6]]
                )

filter(vi.tb, Forest == 4, Permutation == 6) %>%
  left_join(y = cf.12.test.tb, by = 'Variable') %>%
    mutate(Test = Importance - Importance.Manual) %>%
      pull(Test) %>%
        summary()

# Check passed

results.chr[[7]]

load('~/rwa/party_data/variable_importance/cforest.15.vi.RData')

cf.15.p.9.tb <- tibble(Variable = names(cforest.15.vi.ls[[9]]),
                      Importance.Manual = cforest.15.vi.ls[[9]]
                )

filter(vi.tb, Forest == 7, Permutation == 9) %>%
  left_join(y = cf.15.p.9.tb, by = 'Variable') %>%
    mutate(Test = Importance - Importance.Manual) %>%
      pull(Test) %>%
        unique() == 0

# Check passed

# Plot the distributions of variable importance scores:

# rank the variables by mean importance score

mean.var.imp.tb <- group_by(.data = vi.tb, Variable) %>%
  summarise(mean.Importance = mean(Importance)) %>%
    arrange(desc(mean.Importance))

slice(mean.var.imp.tb, 1:10) %>%
  knitr::kable()

# # # |Variable      | mean.Importance| Description                                                                             |
# |:-------------|---------------:|----------------------------------------------------------------------------------------:|
# 
# |Clim.Terr.C62 | 0.0263946      | Central Synthetic Variable of Cluster has positive correlations with summary statistics of elevation, negative correlations with summary statistics of temperature                                                       |
#                                                                                                                                                                                            
# |NFI.S1.C2     | 0.0073571      | conifer-broadleaf balance                                                               |
#                                                       
# |Clim.Terr.C69 | 0.0064327      | cluster contains the single variable clim09.10yr.mean = 10 yrs mean of mean temperature of the Driest Quarter |
#                                                                                                                                           
# |NFI.S1.C13    | 0.0044445      | presence of gaps in forest canopy                                                       |
#                                                                
# |Clim.Terr.C64 | 0.0039468      | cluster 64 contains the single variable dem.12m.cluster67 = amount of global potential radiation                               |
# 
# |Clim.Terr.C25 | 0.0027536      | cluster 25 dem.12m.cluster25 = integrated moisture index min, Q1 and median                                                    |
# 
# |Clim.Terr.C28 | 0.0013353      | cluster 28 describes Northness and site exposure                                        |
#                                                                             
# |Clim.Terr.C68 | 0.0012791      | Cluster 68 contains the single variable clim08.10yr.mean = 10 yr mean of the mean daily temperatures of the wettest quarters   |
#                                                                                                                                                         
# |Clim.Terr.C18 | 0.0010361      | Cluster 18 contains the single variable dem.12m.cluster18 = Eastness value (all summary statistics except for variance)        |
#                                                                             
# |Clim.Terr.C70 | 0.0009863      | Cluster 70 describes the amount of precipitation (clim12, clim13 \& ap.tot.prcp)        |







# |Variable      | mean.Importance| Description                                                       |
# |:-------------|---------------:|------------------------------------------------------------------:|
# |Clim.Terr.C62 | 0.0263946      | elevation \& 10 yr means of temperature summary statistics        |
# |NFI.S1.C2     | 0.0073571      | conifer-broadleaf balance                                         |
# |Clim.Terr.C69 | 0.0064327      | 10 yrs mean of mean temperature of the Driest Quarter             |
# |NFI.S1.C13    | 0.0044445      | presence of gaps in forest canopy                                 |
# |Clim.Terr.C64 | 0.0039468      | 10 yrs mean of the amounts of global potential radiation          |
# |Clim.Terr.C25 | 0.0027536      | Integrated Moisture Index (lower half of values in plot)          |
# |Clim.Terr.C28 | 0.0013353      | Northness and site exposure                                       |
# |Clim.Terr.C68 | 0.0012791      | 10 yr mean of the mean daily temperatures of the wettest quarters |                            |Clim.Terr.C18 | 0.0010361      | Eastness                                                          |
# |Clim.Terr.C70 | 0.0009863      | 10 yrs mean of amounts of precipitation in different periods      |




  
# boxplots of the variable importance scores obtained from all permutations of all random forests for all variables:

mutate(vi.tb, Variable.ro = fct_relevel(.f = Variable, mean.var.imp.tb$Variable)) %>%
  ggplot(aes(x = Variable.ro, y = Importance)) +
    geom_boxplot() +
      labs(x = 'Explanatory Variable')

# boxplots of the variable importance scores obtained from all permutations of all random forests for the 10 variables with the highest mean variable importance scores:

cf.vi.top.10.b.plots <- mutate(vi.tb, Variable.ro = fct_relevel(.f = Variable, mean.var.imp.tb$Variable)) %>%
  filter(Variable.ro %in% mean.var.imp.tb$Variable[1:10]) %>%
    ggplot(aes(x = Variable.ro, y = Importance)) +
      geom_jitter(alpha = 0.1, colour = 'grey') +
      geom_boxplot(outlier.shape = NA, alpha = 0.5) +
      labs(x = NULL) +
      theme_bw() + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# ggsave(filename = '~/rwa/party_data/variable_importance_plots/cf_vi_top_10.pdf', plot = cf.vi.top.10.b.plots, width = 7, height = 7)

# ggsave(filename = '~/rwa/party_data/variable_importance_plots/cf_vi_top_10.tiff', plot = cf.vi.top.10.b.plots, width = 7, height = 7, compression = 'lzw')

# ggsave(filename = '~/rwa/party_data/variable_importance_plots/cf_vi_top_10.png', plot = cf.vi.top.10.b.plots, width = 7, height = 7)


# we could fit a conditional inference forest to the full data set using only the ~ 6 explanatory variables with highest variable importance scores

# the AUC of this reduced conditional inference forest could be compared to that of the conditional inference forest that contained was supplied with all explanatory variables with the permutational test of equality between AUC values from the pROC package




group_by(.data = vi.tb, Variable) %>%
  summarise(mean.Importance = mean(Importance),
            sd.Importance = sd(Importance)) %>%
    arrange(desc(mean.Importance),
            sd.Importance) %>%
      mutate(Mean.Importance = round(x = mean.Importance, digits = 5),
             SD.Importance   = round(x = sd.Importance, digits = 5)
      ) %>%
        select(Variable, Mean.Importance, SD.Importance) %>%
          slice(1:10) %>%
            write_csv(path = '~/rwa/party_data/variable_importance_tables/cf_vi_top_10.csv')


# | elevation \& 10 yr means of temperature summary statistics        |
# | conifer-broadleaf balance                                         |
# | 10 yrs mean of mean temperature of the Driest Quarter             |
# | presence of gaps in forest canopy                                 |
# | 10 yrs mean of the amounts of global potential radiation          |
# | Integrated Moisture Index (lower half of values in plot)          |
# | Northness and site exposure                                       |
# | 10 yr mean of the mean daily temperatures of the wettest quarters |
# | Eastness                                                          |
# | 10 yrs mean of amounts of precipitation in different periods      |
