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

set.seed(seed = 4321*job.counter.num) 

# create the grid of tuning parameter values:

tuning.grid.test <- expand.grid(mtry = 40:55,
                                mincriterion = c(0, 0.001, 0.01, seq(from = 0.05, to = 0.95, by = 0.1), 0.99, 0.999),
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
                                        n.threads = 20,
                                        use.foreach = TRUE,
                                        save.forests = FALSE,
                                        write.out.interim.results = FALSE)
 ) 

assign(x = paste0('party.cv.output.data.partition.', job.counter.num),
       value = cv.gs.output) 

save(list = paste0('party.cv.output.data.partition.', job.counter.num),
     file = paste0('/home/fitzpatr/party/results/cv_results/full_grid_3/full_grid_3_party.cv.output.data.partition.', job.counter.num, '.RData')
    ) 
