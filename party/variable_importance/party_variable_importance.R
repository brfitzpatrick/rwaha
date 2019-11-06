
# load the packages

library('party')
library('doMC')
library('iterators')
library('foreach')
library('yardstick')
library('tidyverse')

# load the data

load('~/rwa/party_data/party_tune/cv_initial_workspace/party_cv_input.RData')

# the tuning parameters values selected by cross validation:

mtry.selected <- 100

mincriterion.selected <- 0

ntree.selected <- 500

colnames(Data.Y.X.ID.tb)

# convert response to a factor (required by party::cforest )

Data.tb <- dplyr::mutate(Data.Y.X.ID.tb, RWAM.F = factor(x = RWAM, levels = c('Present', 'Absent')))
  
dplyr::mutate(Data.tb, Equal = (RWAM == as.character(RWAM.F))) %>%
  dplyr::summarise(Test = (FALSE %in% Equal)) %>%
    dplyr::pull(Test)

Data.tb <- dplyr::select(Data.tb, -RWAM) %>%
  dplyr::rename(RWAM = RWAM.F)



n.presence <- filter(Data.tb, RWAM == 'Present') %>%
                nrow()

n.absence <- filter(Data.tb, RWAM == 'Absent') %>%
               nrow()

n.total <- nrow(Data.tb)

n.presence + n.absence == n.total

weights.tb <- select(Data.tb, RWAM) %>%
                mutate(weights = case_when(RWAM == 'Absent' ~ 1,
                                           RWAM == 'Present'~ n.absence/n.presence
                                 )
                )

group_by(weights.tb, RWAM) %>%
  summarise(total.weigths = sum(weights))


# fit party cforest to full data set


cf.vi <- function(){

system.time(
  cf.1 <- cforest(formula = RWAM ~ .,
                  data = select(Data.tb, -CLNR),                    
                  weights = weights.tb$weights,
                  control = cforest_control(teststat = "quad",
                                            testtype = "Univ",
                                            mincriterion = mincriterion.selected, 
                                            replace = FALSE,
                                            mtry = mtry.selected,
                                            ntree = ntree.selected,
                                            fraction = (n.presence/n.total) # aim for ~50% of presence in bag, 
                            ) 
          )
) # 38.4 sec, 2Gb  - 2.3Gb  : 0.3Gb RAM required to load data and run model

# conditional permutation variable importance

system.time(
  cf.1.vi <- varimpAUC(object = cf.1,
                       mincriterion = 0,
                       conditional = TRUE,
                       threshold = 0.2,
                       nperm = 1,
                       OOB = TRUE
             )
)


}

  
# Old time: 3257.146 sec # 54.3 mins

# regarding the warning:

# Warning message:
# In varImp::varImpAUC(...) :
#   ‘varimp’ with non-unity weights might give misleading results

# it is necessary with a severe class imbalance in the response variable to use weights in training the trees on the in-bag samples to ensure trees are trained on approximately balanced data

# here we are assessing variable importance using the out of bag samples and I have been careful to ensure that the out of bag samples contain ~50% of the presences (with the remaining ~50% in bag) and the remainder of the absences

# the class imbalance in the out of bag samples is the reason we are using varimpAUC( )

# for these reasons I believe that the variable imporance scores obtained from varimpAUC( ) are reliable

VI.1.tb <- tibble(Variable = names(cf.1.vi),
                  Importance = cf.1.vi
           ) %>%
             arrange(desc(Importance))

filter(VI.1.tb, Importance > 0) %>%
  slice(1:25) %>%
    knitr::kable()


filter(VI.1.tb, Importance > 0) %>%
  knitr::kable()

    

sort(cf.1.vi, decreasing = TRUE)

cor(Data.tb$dp.mean.temp.10yr.mean, Data.tb$clim11.10yr.mean)

select(Data.tb, -CLNR, -RWAM) %>% cor() %>% caret::findCorrelation(cutoff = 0.8, names = TRUE)












# Prior to starting R RAM = 2Gb
# RAM during 2.5Gb
# RAM post 2.5Gb

system.time(
  cf.1.vi.2 <- varimpAUC(object = cf.1,
                         mincriterion = 0,
                         conditional = TRUE,
                         threshold = 0.2,
                         nperm = 1,
                         OOB = TRUE
               )
)

