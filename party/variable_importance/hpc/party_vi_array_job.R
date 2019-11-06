# collect command arguments (here the job counter a.k.a. the SLURM_ARRAY_TASK_ID)

# job.counter <- commandArgs(trailingOnly=TRUE)

job.counter <- Sys.getenv('SLURM_ARRAY_TASK_ID')

class(job.counter)

job.counter

job.counter.num <- as.numeric(job.counter)

job.counter.num

# load the packages:

.libPaths('/home/fitzpatr/R')

library('party')
library('doMC')
library('iterators')
library('foreach')
library('yardstick')
library('tidyverse')

# set the random seed in the batch job to the numeric value of the job counter:

job.seed <- job.counter.num

job.seed

set.seed(seed = job.seed) 

# load the data
load('/home/fitzpatr/party/data/party_cv_input.RData')

# convert response to a factor (required by party::cforest )

Data.tb <- dplyr::mutate(Data.Y.X.ID.tb, RWAM.F = factor(x = RWAM, levels = c('Present', 'Absent')))
  
dplyr::mutate(Data.tb, Equal = (RWAM == as.character(RWAM.F))) %>%
  dplyr::summarise(Test = (FALSE %in% Equal)) %>%
    dplyr::pull(Test)

Data.tb <- dplyr::select(Data.tb, -RWAM) %>%
  dplyr::rename(RWAM = RWAM.F)

# Calculate the Weights for the Response Observations

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

# load the tuning parameter values selected by cross validation:

load('/home/fitzpatr/party/data/selected_tuning_parameter_values.RData')

mtry.selected

mincriterion.selected

ntree.selected

# fit party cforest to the full data set

system.time(
  cf <- cforest(formula = RWAM ~ .,
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
)

# conditional permutation variable importance

cf.vi <- function(cf.obj = cf,
                   n.perm = 1){

  cf.vi <- varimpAUC(object = cf.obj,
                     mincriterion = 0,
                     conditional = TRUE,
                     threshold = 0.2,
                     nperm = 1,
                     OOB = TRUE
           )

  return(cf.vi)
  
}

# import the number of available CPUs specified at the line
# #SBATCH --cpus-per-task=20
# in the the file `party_vi_array_job.sh`

n.cpus <- Sys.getenv("SLURM_CPUS_PER_TASK")

n.cpus

class(n.cpus)

# we need this to be numeric below so:

n.cpus <- as.numeric(n.cpus)

n.cpus

class(n.cpus)

# register a parallel backend specifying the number of CPUs as the number we imported via Sys.getenv()

doMC::registerDoMC(cores = n.cpus) 

# run the permutations in parallel

system.time(
  cf.vi.ls <- foreach(i = 1:n.cpus) %dopar% cf.vi(cf.obj = cf, n.perm = 1)
)  

assign(x = paste0('cforest.', job.counter.num),
       value = cf) 


assign(x = paste0('cforest.', job.counter.num, '.vi.ls'),
       value = cf.vi.ls)

save(list = c(paste0('cforest.', job.counter.num), paste0('cforest.', job.counter.num, '.vi.ls')),
     file = paste0('/home/fitzpatr/party/results/variable_importance/cforest.', job.counter.num, '.vi.RData')
    ) 
