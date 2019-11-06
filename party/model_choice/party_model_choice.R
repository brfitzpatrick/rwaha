# load the packages

library('party')
library('doMC')
library('iterators')
library('foreach')
library('yardstick')
library('tidyverse')

# load the data

load('~/rwa/party_data/party_tune/cv_initial_workspace/party_cv_input.RData')

colnames(Data.Y.X.ID.tb)

# the tuning parameters values selected by cross validation:

load('~/rwa/party_data/party_tune/cv_results/selected_tuning_parameter_values.RData')

mtry.selected

mincriterion.selected

ntree.selected

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


# fit party cforest to full data set (in the manuscript I refer to this model as **Model 1**)

system.time(
  cf.full <- cforest(formula = RWAM ~ .,
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
)  # 40 sec

#  #  #  #

# fit party cforest using all explanatory variables except for those that describe direct human modification of and influences upon the environment (in the manuscript I refer to this model as **Model 2**) 

# use the same proportions of variables of this reduced data set as were used in the full data set for the mtry parameter

mtry.nh <- floor((mtry.selected/ncol(select(Data.tb, -CLNR, -RWAM))) * ncol(select(Data.tb, -CLNR, -matches('NFI\\.S2'), -near_dst_b, -near_dst_sr, -near_dst_lr)))

mtry.nh

system.time(
  cf.nh <- cforest(formula = RWAM ~ .,
                   data = select(Data.tb, -CLNR, -matches('NFI\\.S2'), -near_dst_b, -near_dst_sr, -near_dst_lr),                 
                   weights = weights.tb$weights,
                   control = cforest_control(teststat = "quad",
                                             testtype = "Univ",
                                             mincriterion = mincriterion.selected, 
                                             replace = FALSE,
                                             mtry = mtry.nh,
                                             ntree = ntree.selected,
                                             fraction = (n.presence/n.total) # aim for ~50% of presence in bag, 
                            ) 
          )
) 

# compare AUC from cf.full to AUC from cf.nh

library(pROC)

RWAM.Binary <- mutate(Data.tb,
                      RWAM.Binary = case_when(RWAM == 'Present' ~ 1,
                                              RWAM == 'Absent'  ~ 0
                                    )
               ) %>%
                   pull(RWAM.Binary)
       

system.time(cf.full.yhat <- predict(cf.full, type = 'prob')) # 570 sec

cf.full.yhat.present <- map_dbl(cf.full.yhat, function(x){x[,'RWAM.Present']})



system.time(cf.nh.yhat <- predict(cf.nh, type = 'prob'))

cf.nh.yhat.present <- map_dbl(cf.nh.yhat, function(x){x[,'RWAM.Present']})

cf.full.roc <- pROC::roc(response = RWAM.Binary,
                         predictor = cf.full.yhat.present
                     )      

cf.nh.roc <- pROC::roc(response = RWAM.Binary,
                       predictor = cf.nh.yhat.present
                   )      

library(doMC)

registerDoMC(cores = 6)

system.time(
  cf.full.cf.nh.test <- pROC::roc.test(roc1 = cf.full.roc,
                                       roc2 = cf.nh.roc,
                                       method = 'bootstrap',
                                       paired = TRUE,
                                       boot.n = 1e4,
                                       parallel = TRUE
                              )
)               

cf.full.cf.nh.test

#	Bootstrap test for two correlated ROC curves
#
# data:  cf.full.roc and cf.nh.roc
# D = 0.092836, boot.n = 10000, boot.stratified = 1, p-value = 0.926
# alternative hypothesis: true difference in AUC is not equal to 0
# sample estimates:
# AUC of roc1 AUC of roc2 
#   0.8985280   0.8984733 

# Thus the difference between the AUC values for the model fitted to data that did not include any variables that quantify human modifications of the environment (AUC = 0.89847) and the model fitted to data that included all of our explanatory variables (AUC = 0.89853) was not found to differ significantly from zero (p = 0.926) when a permutational test for differences in AUC was applied.

# fit party cforest using only the 6 explanatory variables that had the highest conditional variable importance scores in the cforest fitted to the full data set (in the manuscript I refer to this model as **Model 3**)

mtry.t6 <- floor((mtry.selected/ncol(select(Data.tb, -CLNR, -RWAM))) * 6)

mtry.t6

system.time(
  cf.t6 <- cforest(formula = RWAM ~ .,
                   data = select(Data.tb, RWAM, Clim.Terr.C62, NFI.S1.C2, Clim.Terr.C69, NFI.S1.C13, Clim.Terr.C64, Clim.Terr.C25),                 
                   weights = weights.tb$weights,
                   control = cforest_control(teststat = "quad",
                                             testtype = "Univ",
                                             mincriterion = mincriterion.selected, 
                                             replace = FALSE,
                                             mtry = mtry.t6,
                                             ntree = ntree.selected,
                                             fraction = (n.presence/n.total) # aim for ~50% of presence in bag, 
                            ) 
          )
) 

system.time(cf.t6.yhat <- predict(cf.t6, type = 'prob')) 

cf.t6.yhat.present <- map_dbl(cf.t6.yhat, function(x){x[,'RWAM.Present']})

cf.t6.roc <- pROC::roc(response = RWAM.Binary,
                       predictor = cf.t6.yhat.present
                   )      

system.time(
  cf.full.cf.t6.test <- pROC::roc.test(roc1 = cf.full.roc,
                                       roc2 = cf.t6.roc,
                                       method = 'bootstrap',
                                       paired = TRUE,
                                       boot.n = 1e4,
                                       parallel = TRUE
                              )
)               

cf.full.cf.t6.test


#	Bootstrap test for two correlated ROC curves
#
# data:  cf.full.roc and cf.t6.roc
# D = 11.55, boot.n = 10000, boot.stratified = 1, p-value < 2.2e-16
# alternative hypothesis: true difference in AUC is not equal to 0
# sample estimates:
# AUC of roc1 AUC of roc2 
#    0.898528    0.868101 

#  #  #  #

# fit party cforest using only the 4 explanatory variables that had the highest conditional variable importance scores in the cforest fitted to the full data set (in the manuscript I refer to this model as **Model 4**)

mtry.t4 <- floor((mtry.selected/ncol(select(Data.tb, -CLNR, -RWAM))) * 4)

mtry.t4

# a value of 1 for mtry leaves no room for a choice between potential explanatory variables each time a node is divided

mtry.t4 <- 2

system.time(
  cf.t4 <- cforest(formula = RWAM ~ .,
                   data = select(Data.tb, RWAM, Clim.Terr.C62, NFI.S1.C2, Clim.Terr.C69, NFI.S1.C13),                 
                   weights = weights.tb$weights,
                   control = cforest_control(teststat = "quad",
                                             testtype = "Univ",
                                             mincriterion = mincriterion.selected, 
                                             replace = FALSE,
                                             mtry = mtry.t4,
                                             ntree = ntree.selected,
                                             fraction = (n.presence/n.total) # aim for ~50% of presence in bag, 
                            ) 
          )
) 

system.time(cf.t4.yhat <- predict(cf.t4, type = 'prob')) 

cf.t4.yhat.present <- map_dbl(cf.t4.yhat, function(x){x[,'RWAM.Present']})

cf.t4.roc <- pROC::roc(response = RWAM.Binary,
                       predictor = cf.t4.yhat.present
                   )      

system.time(
  cf.full.cf.t4.test <- pROC::roc.test(roc1 = cf.full.roc,
                                       roc2 = cf.t4.roc,
                                       method = 'bootstrap',
                                       paired = TRUE,
                                       boot.n = 1e4,
                                       parallel = TRUE
                              )
)               

cf.full.cf.t4.test

#	Bootstrap test for two correlated ROC curves
#
# data:  cf.full.roc and cf.t4.roc
# D = 11.568, boot.n = 10000, boot.stratified = 1, p-value < 2.2e-16
# alternative hypothesis: true difference in AUC is not equal to 0
# sample estimates:
# AUC of roc1 AUC of roc2 
#   0.8985280   0.8505975 
