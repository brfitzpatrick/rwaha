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

# set the random seed in the batch job to a function of the job counter:

set.seed(seed = 1111*job.counter.num) 

# create the grid of tuning parameter values:

tuning.grid.test <- expand.grid(mtry = c(6, 12, 25, 50, 100),
                                mincriterion = c(1, 0.99, 0.95, 0.75, 0.5, 0.25, 0.05, 0),
                                ntree = c(500, 1e3, 5e3, 1e4)
                    )

load('/home/fitzpatr/party/data/party_cv_input.RData')

n.partitions <- CV.Partitions.tb$Division.ID %>% unique() %>% length()

partition.pairs <- combn(x = 1:n.partitions, m = 2)

current.partitions.id <- partition.pairs[,job.counter.num]

current.partitions.id

# commented out to test indexing via job counter:

 system.time(
   cv.gs.output <- cv.grid.search.party(all.data = Data.Y.X.ID.tb,
                                        all.partitions = CV.Partitions.tb,
                                        current.partitions = current.partitions.id,
                                        tuning.grid = tuning.grid.test,
                                        n.threads = 8,
                                        use.foreach = TRUE,
                                        save.forests = FALSE,
                                        write.out.interim.results = FALSE)
 ) 

assign(x = paste0('party.cv.output.data.partition.', job.counter.num),
       value = cv.gs.output) 

save(list = paste0('party.cv.output.data.partition.', job.counter.num),
     file = paste0('/home/fitzpatr/party/results/cv_results/full_grid_1/full_grid_1_party.cv.output.data.partition.', job.counter.num, '.RData')
    ) 
