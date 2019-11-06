# load the packages:

.libPaths('/home/fitzpatr/R')

library('party')

library('ICEbox')

library('tidyverse')

# load the inputs

load('/home/fitzpatr/party/ice/ice_inputs.RData')

# profile ICEbox::ice( ) on 5% of the observations

system.time(
  ice.Clim.Terr.C62.5ptc <- ice(object = cf,
                                X = select(Data.tb, -RWAM, -CLNR) %>% as.matrix(),
                                predictor = 'Clim.Terr.C62',
                                predictfcn = party.pred.f,
                                verbose = TRUE,
                                frac_to_build = 0.05
                            )
)

save.image('ice_profile.RData')

# profile ICEbox::ice( ) on 10% of the observations

system.time(
  ice.Clim.Terr.C62.10ptc <- ice(object = cf,
                                X = select(Data.tb, -RWAM, -CLNR) %>% as.matrix(),
                                predictor = 'Clim.Terr.C62',
                                predictfcn = party.pred.f,
                                verbose = TRUE,
                                frac_to_build = 0.1
                            )
)

save.image('ice_profile_2.RData')
