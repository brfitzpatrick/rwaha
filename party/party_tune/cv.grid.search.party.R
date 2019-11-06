#' A function to tune a conditional inference forest by cross validation
#'
#' The current version of this function has been written specifically to tune random forests for modelling the Red Wood Ant occurence data from the Swiss NFI 2009-2018.
#' It would need to be generalised to be applicable to other data.
#'

library(party)
library(tidyverse)
library(parallel)
library(doMC)
library(iterators)
library(foreach)

cv.grid.search.party <- function(all.data = selected.data.tb,
                                   all.partitions = CV.Partitions.tb,
                                   current.partitions = c(1, 2),
                                   tuning.grid = tuning.grid,
                                   n.threads = parallel::detectCores() - 1,
                                   use.foreach = TRUE,
                                   save.forests = TRUE,
                                   write.out.interim.results = FALSE){

  # Check the response variable:

  response.variable <- dplyr::pull(all.data, RWAM)

  unique.response.variable.entries <- unique(response.variable)

  # Typically the first factor level is assumed to be the event. (i.e. in our case a Presence)
  # Consequently I am setting this to be the case below:
  
  if(class(response.variable) == 'character' &
     length(unique.response.variable.entries) == 2 &
     'Present' %in% unique.response.variable.entries &
     'Absent' %in% unique.response.variable.entries
    ){
    # Convert the Response to a Factor
    all.data <- dplyr::mutate(all.data, RWAM.F = factor(x = RWAM, levels = c('Present', 'Absent')))
  
    # Check 0
    if( dplyr::mutate(all.data, Equal = RWAM == as.character(RWAM.F)) %>%
          dplyr::summarise(Test = FALSE %in% Equal) %>%
            dplyr::pull(Test)
      ){
         print('Check 0 Failed')
         return('Error: Check 0 Failed')
    }
    
    all.data <- dplyr::select(all.data, -RWAM) %>%
      dplyr::rename(RWAM = RWAM.F)
    
  } else{
      if(class(response.variable) == 'factor'){
         print('response is a factor, be sure the Presence is the first factor level and Absence is the second level in the ordering of the levels')
      } else{   
          print('Error: Please supply a data.frame or tibble to all.data that contains a column named RWAM which contains the response variable represented as character data with entries of \'Present\' and \'Absent\' or a factor with these levels')
      }
  }     

  # Partition the Data

  Selected.Partitions.Ind.tb <- dplyr::filter(all.partitions, Division.ID %in% paste0('Division ', current.partitions))

  # Check 1
  if(!(nrow(Selected.Partitions.Ind.tb) == length(current.partitions)*nrow(all.data))){
    print('Check 1 Failed')
    return('Error: Check 1 Failed')
  }  

  # Check 2
  if(!identical(sort(unique(Selected.Partitions.Ind.tb$CLNR)), sort(unique(all.data$CLNR)))){
    print('Check 2 Failed')
    return('Error: Check 2 Failed')
  }  

  # Union of the Validation Set and the Observations in the Spatial Exclusion Buffer around the Validation Set:
  
  Buffer.U.Validation.Ind.tb <- dplyr::filter(Selected.Partitions.Ind.tb, !(Set == 'Training'))

  # Test if time and distance variables are included in the design matrix:

  time.vars.present <- colnames(all.data) %>% str_count(pattern = 'time\\.to') %>% sum() > 0

  dist.vars.present <- colnames(all.data) %>% str_count(pattern = 'dist\\.to') %>% sum() > 0

  # If time &/ distance variables are present drop those that give time to or distances to observations in the validation or spatial buffer to prevent data leakage between the training set and either of the validation or buffer sets:
  
  if(time.vars.present == TRUE){
    time.vars.to.drop <- paste0('time.to.', unique(Buffer.U.Validation.Ind.tb$CLNR))
  } else{
      time.vars.to.drop <- character(length = 0)
  }

  if(dist.vars.present == TRUE){
    distance.vars.to.drop <- paste0('dist.to.clnr.', unique(Buffer.U.Validation.Ind.tb$CLNR))
  } else{
      distance.vars.to.drop <- character(length = 0)
  }
  
  Train.Data.tb <- dplyr::filter(all.data, !(CLNR %in% Buffer.U.Validation.Ind.tb$CLNR)) %>%
    dplyr::select(-one_of(c(time.vars.to.drop, distance.vars.to.drop)))

  # Check 2.5
  
  check.2.5 <- left_join(x = Train.Data.tb,  y = Selected.Partitions.Ind.tb, by = 'CLNR') %>%
                 pull(Set) %>%
                   unique()
  
  if( ('Validation' %in% check.2.5) | ('Buffer' %in% check.2.5) ){
    print('Check 2.5 Failed')
    return('Check 2.5 Failed')
  }
  
  if(time.vars.present == TRUE){    
    # Check 3
    if( TRUE %in% ( time.vars.to.drop %in% colnames(Train.Data.tb)) ){
      print('Check 3 Failed')
      return('Error: Check 3 Failed')
    }
  
    # Check 4
    if( FALSE %in% ( time.vars.to.drop %in% colnames(all.data)) ){
      print('Check 4 Failed')
      return('Error: Check 4 Failed')
    }
  }

  
  if(dist.vars.present == TRUE){      
    # Check 5
    if( TRUE %in% ( distance.vars.to.drop %in% colnames(Train.Data.tb)) ){
      print('Check 5 Failed')
      return('Error: Check 5 Failed')
    }
  
    # Check 6
    if( FALSE %in% ( distance.vars.to.drop %in% colnames(all.data)) ){
      print('Check 6 Failed')
      return('Error: Check 6 Failed')
    }
  }
  
  Train.Data.YX.tb <- dplyr::select(Train.Data.tb, -CLNR)

  Validation.Ind.tb <- dplyr::filter(Selected.Partitions.Ind.tb, Set == 'Validation')

  Valid.Data.tb <- dplyr::filter(all.data, (CLNR %in% Validation.Ind.tb$CLNR)) %>%
    dplyr::select(-one_of(c(time.vars.to.drop, distance.vars.to.drop)))

  Prct.Training <- 100*nrow(Train.Data.YX.tb)/nrow(all.data)

  Prct.Validation <- 100*nrow(Valid.Data.tb)/nrow(all.data)

  # Calculate Weights with which observations are sampled for each tree 

  n.presence.train <- filter(Train.Data.YX.tb, RWAM == 'Present') %>%
                        nrow()

  n.absence.train <- filter(Train.Data.YX.tb, RWAM == 'Absent') %>%
                       nrow()

  n.train <- nrow(Train.Data.YX.tb)

  # check for rogue response entries:

  if(!(n.train == (n.presence.train + n.absence.train))){
    print('response contains entries other than \'Present\' and \'Absent\'')
    return('response contains entries other than \'Present\' and \'Absent\'')
  }

  # these weights result in the probability of drawing a presence to be 'in-bag' equal to the probability of drawing an absence to be 'in-bag':
  
  weights.tb <- select(Train.Data.YX.tb, RWAM) %>%
                  mutate(weights = case_when(RWAM == 'Absent' ~ 1,
                                             RWAM == 'Present'~ n.absence.train/n.presence.train
                                   )
                  )
  
  # Conduct the grid search

  if(use.foreach == FALSE){

    tuning.grid$auc <- as.numeric(rep(NA, times = nrow(tuning.grid)))

    models.list <- vector(mode = 'list', length = nrow(tuning.grid))
    
    for(i in 1:nrow(tuning.grid)){
  
      # class.weigths are weights for the outcome classes (in order of the factor levels) 
      
      cf.i <- cforest(formula = RWAM ~ .,
                      data = Train.Data.YX.tb,                    
                      weights = weights.tb$weights,
                      control = cforest_control(teststat = "quad",
                                                testtype = "Univ",
                                                mincriterion = tuning.grid[i, 'mincriterion'], 
                                                replace = FALSE,
                                                mtry = tuning.grid[i, 'mtry'],
                                                ntree = tuning.grid[i, 'ntree'],
                                                fraction = (n.presence.train/n.train) # aim for ~50% of presence in bag, 
                                ) 
              )

      if(save.forests == TRUE){
      
        models.list[[i]] <- cf.i

      }  
  
      validation.set.yhat.i <- predict(object = cf.i, newdata = Valid.Data.tb, type = 'prob') %>%
                                 map_dbl(function(x){x[,'RWAM.Present']})
      
      # estimate If truth is binary, a numeric vector of class probabilities corresponding to the "relevant" class
      
      # yardstick::roc_auc_vec defaults to assuming that the first level of the factor supplied to 'truth' represents the "event"        the predicted probabilities of which are supplied to 'estimate'
    
      tuning.grid[i, 'auc'] <- yardstick::roc_auc_vec(truth = Valid.Data.tb$RWAM, estimate = validation.set.yhat.i)
  
      rm(list = c('rf.i', 'validation.set.yhat.i'))
  
      if(write.out.interim.results == TRUE){
        
        output.i <- list(tuning.grid = tuning.grid,
                         Division.IDs = paste0('Division ', current.partitions),
                         models.list = models.list,
                         Validation.Set.CLNR = Valid.Data.tb$CLNR
                    )
        
        save(list = 'output.i', file = paste0(paste0('P',current.partitions,collapse = ''), '.output.',i, '.RData'))
  
      }  
  
      print(paste0(round(100*i/nrow(tuning.grid),2), ' % of grid search complete'))
    }    
  }

  if(use.foreach == TRUE){

    tge <- function(tuning.grid.row){
           
              cf.i <- cforest(formula = RWAM ~ .,
                              data = Train.Data.YX.tb,                    
                              weights = weights.tb$weights,
                              control = cforest_control(teststat = "quad",
                                                        testtype = "Univ",
                                                        mincriterion = tuning.grid.row['mincriterion'], 
                                                        replace = FALSE,
                                                        mtry = tuning.grid.row['mtry'],
                                                        ntree = tuning.grid.row['ntree'],
                                                        fraction = (n.presence.train/n.train) # aim for ~50% of presences in-bag 
                                        ) 
                      )
                    
              validation.set.yhat.i <- predict(object = cf.i, newdata = Valid.Data.tb, type = 'prob') %>%
                                         map_dbl(function(x){x[,'RWAM.Present']})
           
              AUC <- yardstick::roc_auc_vec(truth = Valid.Data.tb$RWAM, estimate = validation.set.yhat.i)
           
              tuning.grid.row$auc <- AUC

              if(save.forests == TRUE){
      
                model <- cf.i
           
                out.ls <- list(tuning.grid.row, model)

              } else{
           
                  out.ls <- list(tuning.grid.row)

              }
           
           }

    doMC::registerDoMC(cores = n.threads)

    out.ls <- foreach(tgr=iterators::iter(tuning.grid, by='row')) %dopar% tge(tgr)
    
    tuning.grid <- map_dfr(.x = out.ls, 1)

    if(save.forests == TRUE){
      
      models.list <- map(.x = out.ls, 2)
      
    }

  }
  
  selected.tuning.parameters <- dplyr::filter(tuning.grid, auc == max(auc))

  if(save.forests == TRUE){
    output <- list(selected.tuning.parameters = selected.tuning.parameters,
                   tuning.grid = tuning.grid,
                   Division.IDs = paste0('Division ', current.partitions),
                   Training.Set.CLNR = Train.Data.tb$CLNR,
                   Validation.Set.CLNR = Valid.Data.tb$CLNR,
                   models.list = models.list,
                   Prct.Obs.in.Training = Prct.Training,
                   Prct.Obs.in.Validation = Prct.Validation                 
              )
  } else{

      output <- list(selected.tuning.parameters = selected.tuning.parameters,
                     tuning.grid = tuning.grid,
                     Division.IDs = paste0('Division ', current.partitions),
                     Training.Set.CLNR = Train.Data.tb$CLNR,
                     Validation.Set.CLNR = Valid.Data.tb$CLNR,
                     Prct.Obs.in.Training = Prct.Training,
                     Prct.Obs.in.Validation = Prct.Validation                 
                )
  }  
  
  return(output)
  
  }

