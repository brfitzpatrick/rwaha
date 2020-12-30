# load the packages
library('ICEbox')
library('party')
library('tidyverse')
library('gridExtra')
library('grid')
library('cowplot')
library('glue')

# load the input data

load('~/rwa/party_data/party_tune/cv_initial_workspace/party_cv_input.RData')

# source the plot_ice() funtion

source('~/rwaha/functions/plot_ice.R')

# ICE plots of the 6 explanatory variable with the highest conditional variable importance scores:
  # 1) Clim.Terr.C62
  # 2) NFI.S1.C2
  # 3) Clim.Terr.C69
  # 4) NFI.S1.C13
  # 5) Clim.Terr.C64
  # 6) Clim.Terr.C25

# 1) Clim.Terr.C62

load('~/rwa/party_data/ice/objects/ice_obj/ct_c62/ice.ct.c62.200p.RData')

ct.c62.ice.p <- plot_ice(ice.obj = ice.ct.c62.200p,
                         alpha.dist.to.obs.power = 12,
                         alpha.scale.range = c(0,0.05),
                         center.curves = TRUE,
                         centered.quantile = 0.01,
                         plot.pd.curve = FALSE,
                         y.axis.title = 'Centred Predicted Probability of Ant Mound Occurrence'  
                )

ct.c62.ice.p

# 2) NFI.S1.C2

load('~/rwa/party_data/ice/objects/ice_obj/nfi_s1_c2/ice.nfi.s1.c2.200p.RData')

nfi.s1.c2.ice.p <- plot_ice(ice.obj = ice.nfi.s1.c2.200p,
         alpha.dist.to.obs.power = 12,
         alpha.scale.range = c(0,0.05),
         center.curves = TRUE,
         centered.quantile = 0.001,
         plot.pd.curve = FALSE,
         y.axis.title = 'Centred Predicted Probability of Ant Mound Occurrence'  
)

nfi.s1.c2.ice.p

# 3) Clim.Terr.C69

load('~/rwa/party_data/ice/objects/ice_obj/ct_c69/ice.ct.c69.200p.RData')

ct.c69.ice.p <- plot_ice(ice.obj = ice.ct.c69.200p,
         alpha.dist.to.obs.power = 12,
         alpha.scale.range = c(0,0.05),
         center.curves = TRUE,
         centered.quantile = 0.01,
         plot.pd.curve = FALSE,
         y.axis.title = 'Centred Predicted Probability of Ant Mound Occurrence'  
)

# 4) NFI.S1.C13

load('~/rwa/party_data/ice/objects/ice_obj/nfi_s1_c13/ice.nfi.s1.c13.200p.RData')

nfi.s1.c13.ice.p <- plot_ice(ice.obj = ice.nfi.s1.c13.200p,
         alpha.dist.to.obs.power = 12,
         alpha.scale.range = c(0,0.05),
         center.curves = TRUE,
         centered.quantile = 0.01,
         plot.pd.curve = FALSE,
         y.axis.title = 'Centred Predicted Probability of Ant Mound Occurrence'  
)

# 5) Clim.Terr.C64

load('~/rwa/party_data/ice/objects/ice_obj/ct_c64/ice.ct.c64.200p.RData')

ct.c64.ice.p <- plot_ice(ice.obj = ice.ct.c64.200p,
         alpha.dist.to.obs.power = 12,
         alpha.scale.range = c(0,0.05),
         center.curves = TRUE,
         centered.quantile = 0.01,
         plot.pd.curve = FALSE,
         y.axis.title = 'Centred Predicted Probability of Ant Mound Occurrence'  
)

## looks like there could be two groups of curves in this plot...

ct.c64.int.ct.c62.ice.p <- plot_ice(ice.obj = ice.ct.c64.200p,
         alpha.dist.to.obs.power = 12,
         alpha.scale.range = c(0,0.05),
         facet.cols.by = 'Clim.Terr.C62',
         facet.cols.threshold = quantile(x = Data.Y.X.ID.tb$Clim.Terr.C62, probs = 0.6),
         center.curves = TRUE,
         centered.quantile = 0.01,
         plot.pd.curve = FALSE,
         y.axis.title = 'Centred Predicted Probability of Ant Mound Occurrence'  
)

# 6) Clim.Terr.C25

load('~/rwa/party_data/ice/objects/ice_obj/ct_c25/ice.ct.c25.200p.RData')

ct.c25.ice.p <- plot_ice(ice.obj = ice.ct.c25.200p,
         alpha.dist.to.obs.power = 12,
         alpha.scale.range = c(0,0.05),
         center.curves = TRUE,
         centered.quantile = 0.01,
         plot.pd.curve = FALSE,
         y.axis.title = 'Centred Predicted Probability of Ant Mound Occurrence'  
)

## this is another plot where it appears that there could be two groups of curves

ct.c25.int.ct.c62.ice.p <- plot_ice(ice.obj = ice.ct.c25.200p,
         alpha.dist.to.obs.power = 12,
         alpha.scale.range = c(0,0.05),
         facet.cols.by = 'Clim.Terr.C62',
         facet.cols.threshold = quantile(x = Data.Y.X.ID.tb$Clim.Terr.C62, probs = 0.6),
         center.curves = TRUE,
         centered.quantile = 0.01,
         plot.pd.curve = FALSE,
         y.axis.title = 'Centred Predicted Probability of Ant Mound Occurrence'  
)


## Panel of 6 ICE plots:

# 1) Clim.Terr.C62

ct.c62.ice.p <- ct.c62.ice.p + ylab(NULL)

# 2) NFI.S1.C2

nfi.s1.c2.ice.p <- nfi.s1.c2.ice.p + ylab(NULL)

# 3) Clim.Terr.C69

ct.c69.ice.p <- ct.c69.ice.p + ylab(NULL)

# 4) NFI.S1.C13

nfi.s1.c13.ice.p <- nfi.s1.c13.ice.p + ylab(NULL)

# 5) Clim.Terr.C64

ct.c64.ice.p <- ct.c64.ice.p + ylab(NULL)

# 6) Clim.Terr.C25

ct.c25.ice.p <- ct.c25.ice.p + ylab(NULL)

# panel

# a two column figure should be 190mm wide

plot.1 <- grid.arrange(plot_grid(ct.c62.ice.p, nfi.s1.c2.ice.p, ct.c69.ice.p, nfi.s1.c13.ice.p, ct.c64.ice.p, ct.c25.ice.p, ncol=3, align = 'v', labels = LETTERS[1:6], hjust = -0.125), left = 'Centred Predicted Probability of Ant Mound Occurrence')

ggsave(plot.1, filename = '~/rwa/figures/files/ICE_panel/Top_6_Imp_Var_ICE_Panel.pdf', width = 190, height = 130, units = 'mm')

ggsave(plot.1, filename = '~/rwa/figures/files/ICE_panel/Top_6_Imp_Var_ICE_Panel.png', width = 190, height = 130, units = 'mm')

ggsave(plot.1, filename = '~/rwa/figures/files/ICE_panel/Top_6_Imp_Var_ICE_Panel.tiff', width = 190, height = 130, units = 'mm', compression = 'lzw')

### panel of ICE plots displaying apparrent interactions

# load the input data

load('~/rwa/party_data/party_tune/cv_initial_workspace/party_cv_input.RData')

# source the plot_ice() funtion

source('~/rwaha/functions/plot_ice.R')

load('~/rwa/party_data/ice/objects/ice_obj/ct_c64/ice.ct.c64.200p.RData')

load('~/rwa/party_data/ice/objects/ice_obj/ct_c25/ice.ct.c25.200p.RData')

ice.ct.c64.int.ct.c62.p <- plot_ice(ice.obj = ice.ct.c64.200p,
                                    alpha.dist.to.obs.power = 12,
                                    alpha.scale.range = c(0,0.05),
                                    facet.cols.by = 'Clim.Terr.C62',
                                    facet.cols.threshold = quantile(x = Data.Y.X.ID.tb$Clim.Terr.C62, probs = 0.6) %>%
                                                             round(digits = 2) %>%
                                                               as.numeric(),
                                    center.curves = TRUE,
                                    centered.quantile = 0.01,
                                    plot.pd.curve = FALSE,
                                    y.axis.title = NULL,
                                    facet.threshold.rounding.digits = 2
) + ylab(NULL)

ice.ct.c25.int.ct.c62.p <- plot_ice(ice.obj = ice.ct.c25.200p,
                                    alpha.dist.to.obs.power = 12,
                                    alpha.scale.range = c(0,0.05),
                                    facet.cols.by = 'Clim.Terr.C62',
                                    facet.cols.threshold = quantile(x = Data.Y.X.ID.tb$Clim.Terr.C62, probs = 0.6) %>%
                                                             round(digits = 2) %>%
                                                               as.numeric(),
                                    center.curves = TRUE,
                                    centered.quantile = 0.01,
                                    plot.pd.curve = FALSE,
                                    y.axis.title = NULL,
                                    facet.threshold.rounding.digits = 2                                    
) + ylab(NULL)


ice.int.plots <- grid.arrange(plot_grid(ice.ct.c64.int.ct.c62.p, ice.ct.c25.int.ct.c62.p, nrow = 2, align = 'v'), left = 'Centred Predicted Probability of Ant Mound Occurrence')

ggsave(ice.int.plots, filename = '~/rwa/figures/files/ICE_int_panel/ICE_Int.pdf', width = 190, height = 130, units = 'mm')

ggsave(ice.int.plots, filename = '~/rwa/figures/files/ICE_int_panel/ICE_Int.png', width = 190, height = 130, units = 'mm')

ggsave(ice.int.plots, filename = '~/rwa/figures/files/ICE_int_panel/ICE_Int.tiff', width = 190, height = 130, units = 'mm', compression = 'lzw')

### 

ice.ct.c64.int.ct.c62.p <- plot_ice(ice.obj = ice.ct.c64.200p,
                                    alpha.dist.to.obs.power = 12,
                                    alpha.scale.range = c(0,0.05),
                                    facet.cols.by = 'Clim.Terr.C62',
                                    facet.cols.threshold = quantile(x = Data.Y.X.ID.tb$Clim.Terr.C62, probs = 0.6) %>%
                                                             round(digits = 2) %>%
                                                               as.numeric(),
                                    center.curves = TRUE,
                                    centered.quantile = 0.01,
                                    plot.pd.curve = FALSE,
                                    y.axis.title = 'Centred Predicted Probability of Ant Mound Occurrence',
                                    facet.threshold.rounding.digits = 2
) + theme(axis.title = element_text(size = 20), strip.text = element_text(size = 20))

ice.ct.c64.int.ct.c62.p

ggsave(ice.ct.c64.int.ct.c62.p, filename = '~/rwa/figures/files/ICE_int_panel/ICE_Int_CT.C64_CT.C62.pdf')

ggsave(ice.ct.c64.int.ct.c62.p, filename = '~/rwa/figures/files/ICE_int_panel/ICE_Int_CT.C64_CT.C62.png')

ggsave(ice.ct.c64.int.ct.c62.p, filename = '~/rwa/figures/files/ICE_int_panel/ICE_Int_CT.C64_CT.C62.tiff', compression = 'lzw')


### Figure 1


# 1) Clim.Terr.C62

ct.c62.ice.p <- plot_ice(ice.obj = ice.ct.c62.200p,
                         alpha.dist.to.obs.power = 12,
                         alpha.scale.range = c(0,0.05),
                         center.curves = TRUE,
                         centered.quantile = 0.01,
                         plot.pd.curve = FALSE,
                         y.axis.title = 'Centred Predicted Probability of Ant Mound Occurrence'  
                )

# 2) NFI.S1.C2

nfi.s1.c2.ice.p <- plot_ice(ice.obj = ice.nfi.s1.c2.200p,
         alpha.dist.to.obs.power = 12,
         alpha.scale.range = c(0,0.05),
         center.curves = TRUE,
         centered.quantile = 0.001,
         plot.pd.curve = FALSE,
         y.axis.title = 'Centred Predicted Probability of Ant Mound Occurrence'  
)

# 3) Clim.Terr.C69

ct.c69.ice.p <- plot_ice(ice.obj = ice.ct.c69.200p,
         alpha.dist.to.obs.power = 12,
         alpha.scale.range = c(0,0.05),
         center.curves = TRUE,
         centered.quantile = 0.01,
         plot.pd.curve = FALSE,
         y.axis.title = 'Centred Predicted Probability of Ant Mound Occurrence'  
)

# 4) NFI.S1.C13

nfi.s1.c13.ice.p <- plot_ice(ice.obj = ice.nfi.s1.c13.200p,
         alpha.dist.to.obs.power = 12,
         alpha.scale.range = c(0,0.05),
         center.curves = TRUE,
         centered.quantile = 0.01,
         plot.pd.curve = FALSE,
         y.axis.title = 'Centred Predicted Probability of Ant Mound Occurrence'  
)

# 5) Clim.Terr.C64

ct.c64.ice.p <- plot_ice(ice.obj = ice.ct.c64.200p,
         alpha.dist.to.obs.power = 12,
         alpha.scale.range = c(0,0.05),
         center.curves = TRUE,
         centered.quantile = 0.01,
         plot.pd.curve = FALSE,
         y.axis.title = 'Centred Predicted Probability of Ant Mound Occurrence'  
)

## looks like there could be two groups of curves in this plot...

ct.c64.int.ct.c62.ice.p <- plot_ice(ice.obj = ice.ct.c64.200p,
         alpha.dist.to.obs.power = 12,
         alpha.scale.range = c(0,0.05),
         facet.cols.by = 'Clim.Terr.C62',
         facet.cols.threshold = quantile(x = Data.Y.X.ID.tb$Clim.Terr.C62, probs = 0.6),
         center.curves = TRUE,
         centered.quantile = 0.01,
         plot.pd.curve = FALSE,
         y.axis.title = 'Centred Predicted Probability of Ant Mound Occurrence'  
)

# 6) Clim.Terr.C25

ct.c25.ice.p <- plot_ice(ice.obj = ice.ct.c25.200p,
         alpha.dist.to.obs.power = 12,
         alpha.scale.range = c(0,0.05),
         center.curves = TRUE,
         centered.quantile = 0.01,
         plot.pd.curve = FALSE,
         y.axis.title = 'Centred Predicted Probability of Ant Mound Occurrence'  
)

## this is another plot where it appears that there could be two groups of curves

ct.c25.int.ct.c62.ice.p <- plot_ice(ice.obj = ice.ct.c25.200p,
         alpha.dist.to.obs.power = 12,
         alpha.scale.range = c(0,0.05),
         facet.cols.by = 'Clim.Terr.C62',
         facet.cols.threshold = quantile(x = Data.Y.X.ID.tb$Clim.Terr.C62, probs = 0.6),
         center.curves = TRUE,
         centered.quantile = 0.01,
         plot.pd.curve = FALSE,
         y.axis.title = 'Centred Predicted Probability of Ant Mound Occurrence'  
)



## Panel of 6 ICE plots:

m.adj.v <- rep(0.5,4)

# 1) Clim.Terr.C62

ct.c62.ice.p <- ct.c62.ice.p + ylab(NULL) + theme(plot.margin = unit(m.adj.v, "cm"))

# 2) NFI.S1.C2

nfi.s1.c2.ice.p <- nfi.s1.c2.ice.p + ylab(NULL) + theme(plot.margin = unit(m.adj.v, "cm"))

# 3) Clim.Terr.C69

ct.c69.ice.p <- ct.c69.ice.p + ylab(NULL) + theme(plot.margin = unit(m.adj.v, "cm"))

# 4) NFI.S1.C13

nfi.s1.c13.ice.p <- nfi.s1.c13.ice.p + ylab(NULL) + theme(plot.margin = unit(m.adj.v, "cm"))

# 5) Clim.Terr.C64

ct.c64.ice.p <- ct.c64.ice.p + ylab(NULL) + theme(plot.margin = unit(m.adj.v, "cm"))

# 6) Clim.Terr.C25

ct.c25.ice.p <- ct.c25.ice.p + ylab(NULL) + theme(plot.margin = unit(m.adj.v, "cm"))

# panel

# a two column figure should be 190mm wide

plot.1 <- grid.arrange(
            plot_grid(
              ct.c62.ice.p,
              nfi.s1.c2.ice.p,
              ct.c69.ice.p,
              nfi.s1.c13.ice.p,
              ct.c64.ice.p,
              ct.c25.ice.p,
              ncol=3,
              align = 'v',
              labels = purrr::map(.x = letters[1:6], .f = function(x){glue('(', x, ')')}),
              label_size = 12,
              label_fontface = 'plain', 
              vjust = 1.5,
              hjust = 0.3
            ),
            left = 'Centred Predicted Probability of Ant Mound Occurrence\n'
          )

 ggsave(plot.1, filename = '~/rwa/manuscript/diversity_and_distributions_submission/final_submission/figures/uk_spelling_centred/Figures_1.pdf', width = 190, height = 130, units = 'mm')

 ggsave(plot.1, filename = '~/rwa/manuscript/diversity_and_distributions_submission/final_submission/figures/uk_spelling_centred/Figures_1.tiff', width = 190, height = 130, units = 'mm', compression = 'lzw')



### Figure 2

ice.ct.c64.int.ct.c62.p <- plot_ice(ice.obj = ice.ct.c64.200p,
                                    alpha.dist.to.obs.power = 12,
                                    alpha.scale.range = c(0,0.05),
                                    facet.cols.by = 'Clim.Terr.C62',
                                    facet.cols.threshold = quantile(x = Data.Y.X.ID.tb$Clim.Terr.C62, probs = 0.6) %>%
                                                             round(digits = 2) %>%
                                                               as.numeric(),
                                    center.curves = TRUE,
                                    centered.quantile = 0.01,
                                    plot.pd.curve = FALSE,
                                    y.axis.title = NULL,
                                    facet.threshold.rounding.digits = 2
) + ylab(NULL)

ice.ct.c25.int.ct.c62.p <- plot_ice(ice.obj = ice.ct.c25.200p,
                                    alpha.dist.to.obs.power = 12,
                                    alpha.scale.range = c(0,0.05),
                                    facet.cols.by = 'Clim.Terr.C62',
                                    facet.cols.threshold = quantile(x = Data.Y.X.ID.tb$Clim.Terr.C62, probs = 0.6) %>%
                                                             round(digits = 2) %>%
                                                               as.numeric(),
                                    center.curves = TRUE,
                                    centered.quantile = 0.01,
                                    plot.pd.curve = FALSE,
                                    y.axis.title = NULL,
                                    facet.threshold.rounding.digits = 2                                    
) + ylab(NULL)

ice.int.plots <- grid.arrange(plot_grid(ice.ct.c64.int.ct.c62.p, ice.ct.c25.int.ct.c62.p, nrow = 2, align = 'v'), left = 'Centred Predicted Probability of Ant Mound Occurrence')

ggsave(ice.int.plots, filename = '~/rwa/manuscript/diversity_and_distributions_submission/final_submission/figures/uk_spelling_centred/Figure_2.pdf', width = 190, height = 130, units = 'mm')

ggsave(ice.int.plots, filename = '~/rwa/manuscript/diversity_and_distributions_submission/final_submission/figures/uk_spelling_centred/Figure_2.tiff', width = 190, height = 130, units = 'mm', compression = 'lzw')


