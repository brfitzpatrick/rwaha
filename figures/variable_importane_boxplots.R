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
        unique()

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
      labs(x = NULL, y = 'Conditional Variable Importance') +
      theme_bw() + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

cf.vi.top.10.b.plots

ggsave(cf.vi.top.10.b.plots, filename = '~/rwa/figures/files/varimp/varimp_boxplots.pdf', width = 190, height = 130, units = 'mm')

ggsave(cf.vi.top.10.b.plots, filename = '~/rwa/figures/files/varimp/varimp_boxplots.png', width = 190, height = 130, units = 'mm')

ggsave(cf.vi.top.10.b.plots, filename = '~/rwa/figures/files/varimp/varimp_boxplots.tiff', width = 190, height = 130, units = 'mm', compression = 'lzw')


