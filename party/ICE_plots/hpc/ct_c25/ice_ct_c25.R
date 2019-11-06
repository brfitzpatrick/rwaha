# load the packages:

.libPaths('/home/fitzpatr/R')

library('party')

library('ICEbox')

library('tidyverse')

# load the inputs

load('/home/fitzpatr/party/ice/input_workspace/ice_inputs.RData')

# calculate the ice object

system.time(
  ice.ct.c25.200p <- ice(object = cf,
                         X = select(Data.tb, -RWAM) %>% as.matrix(),
                         predictor = 'Clim.Terr.C25',
                         predictfcn = party.pred.f,
                         verbose = FALSE,
                         frac_to_build = 1,
                         num_grid_pts =  200
                     )
)

# save the ice object

save(list = 'ice.ct.c25.200p', file = '/home/fitzpatr/party/results/ice_obj/ct_c25/ice.ct.c25.200p.RData')


