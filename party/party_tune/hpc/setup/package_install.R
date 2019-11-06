.libPaths('/home/fitzpatr/R')

install.packages(pkgs = 'party', repos = 'https://cloud.r-project.org', lib = '/home/fitzpatr/R', Ncpus = 8, dependencies = TRUE)

library('party', lib.loc = '/home/fitzpatr/R')

install.packages(pkgs = 'doMC', repos = 'https://cloud.r-project.org', lib = '/home/fitzpatr/R', Ncpus = 8, dependencies = TRUE)

library('doMC', lib.loc = '/home/fitzpatr/R')

install.packages(pkgs = 'iterators', repos = 'https://cloud.r-project.org', lib = '/home/fitzpatr/R', Ncpus = 8, dependencies = TRUE)

library('iterators', lib.loc = '/home/fitzpatr/R')

install.packages(pkgs = 'foreach', repos = 'https://cloud.r-project.org', lib = '/home/fitzpatr/R', Ncpus = 8, dependencies = TRUE)

library('foreach', lib.loc = '/home/fitzpatr/R')

install.packages(pkgs = 'yardstick', repos = 'https://cloud.r-project.org', lib = '/home/fitzpatr/R', Ncpus = 8, dependencies = TRUE)

library('yardstick', lib.loc = '/home/fitzpatr/R')

install.packages(pkgs = 'tidyverse', repos = 'https://cloud.r-project.org', lib = '/home/fitzpatr/R', Ncpus = 8, dependencies = TRUE)

library('tidyverse', lib.loc = '/home/fitzpatr/R')
