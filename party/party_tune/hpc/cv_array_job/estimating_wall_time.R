library(tidyverse)

times.tb <- tribble(~forests, ~trees, ~cpus, ~seconds,
                           2,    5e3,     2, 1331.665,
                           2,    5e3,     2, 1221.338,
                           4,    5e3,     4, 1246.430,
                           4,    5e3,     4, 1248.389,
                           2,    5e2,     2,  133.982,
                           2,    5e2,     2,  123.394)

# so we're looking at ~21 mins per 5e3 tree forest
                    
mutate(times.tb, sec.per.tree = seconds/trees)

# so let's say 0.3 seconds per tree for the deepest trees we will grow (mincriterion = 0, mtry = 140)

# thus the following tuning grid

tuning.grid.test <- expand.grid(mtry = c(6, 12, 25, 50, 100, 140),
                                mincriterion = c(0.999, 0.99, 0.975, 0.95, 0.9, 0.8, 0.6, 0.4, 0.2, 0.1, 0.05, 0.025, 0.01, 0.001, 0),
                                ntree = c(500, 1e3, 5e3, 1e4)
                    )


summarise(tuning.grid.test, total.trees = sum(ntree)) %>%
  mutate(time.estimate.serial = total.trees*0.3/(60^2))

# 124 hrs of wall time seems mildy excessive...

tuning.grid.test <- expand.grid(mtry = c(6, 12, 25, 50, 100),
                                mincriterion = c(0.99, 0.95, 0.75, 0.5, 0.25, 0.05, 0),
                                ntree = c(500, 1e3, 2.5e3, 5e3)
                    )


summarise(tuning.grid.test, total.trees = sum(ntree)) %>%
  mutate(time.estimate.serial = total.trees*0.3/(60^2))

# 26.25 hrs serial

# 13 hrs in two way parallel







