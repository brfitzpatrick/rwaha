## Red Wood Ant (RWA) Occurrence Map

### Run this code with R 3.6

#### Plot of RWA Presence/Absence in NFI plots overlayed onto a relief map of Switerzland with the major lakes and rivers marked.

#### This figure should occupy approximately the top half of an A4 page in the portrait orientation i.e. a 2-column fitting image.

#### A double column figure should be 190 mm wide.

#### Image formats: EPS, PDF, TIFF or JPEG

### Load the Packages

library(tidyverse)
library(sf)
library(stars)
library(ggspatial)

## Load the Prepared Data

load('~/rwa/data/ants/Preparations_of_Ameisen_Data_2009-2017_pourWSL_22.03.2018_xlsx/Red_Wood_Ant_Mound_Occurence.RData')

RWAMO.tb <- mutate(RWAMO.tb, Ant.Mound = case_when(RWAM.PA == 0 ~ 'Absent',
                                                  RWAM.PA == 1 ~ 'Present'
                                        )
            ) %>%
              filter(!is.na(Ant.Mound)) %>%
                arrange(Ant.Mound)

relief.clip.stars <- read_stars(.x = '/home/ben/rwa/data/Swiss_Map_Elements/Relief1000_smooth.tif')

rivers.sf <- read_sf('~/rwa/data/Swiss_Map_Elements/lakes_rivers/fluesse.shp')

lakes.sf <- read_sf('~/rwa/data/Swiss_Map_Elements/lakes_rivers/Grosse_Seen.shp')

st_crs(relief.clip.stars) == st_crs(rivers.sf)

st_crs(relief.clip.stars) == st_crs(lakes.sf)

### The NFI Plot Centroids have been provided using the projection: EPSG 21781 = ch1903 / lv03.

raster::crs("+init=epsg:21781")

Mounds.sf <- select(RWAMO.tb, X, Y, Ant.Mound) %>%
               st_as_sf(x = .,
                        coords = c('X', 'Y'),
                        crs = 21781
               ) %>%
                 st_transform(x = ., crs = st_crs(relief.clip.stars))

st_crs(Mounds.sf) == st_crs(relief.clip.stars)

### If we had wanted different transparencies for Presences and Absences

Mounds.sf <- mutate(Mounds.sf,
                    Ant.Mound.alpha = case_when(Ant.Mound == 'Present' ~ 0.75,
                                                Ant.Mound == 'Absent' ~ 0.75
                                      )
             )

RWA.Occurrence.p <- ggplot() +
  geom_stars(data = relief.clip.stars, alpha = 0.25, show.legend = FALSE) +
  scale_fill_gradient(low = 'black', high = 'white', na.value = 'white') +
  coord_equal() +
  geom_sf(data = rivers.sf, colour = 'lightgrey') +
  geom_sf(data = lakes.sf, colour = 'lightgrey', fill = 'lightgrey') +
  geom_sf(data = Mounds.sf,
          aes(colour = Ant.Mound,
              shape = Ant.Mound,
              size = Ant.Mound,
              alpha = Ant.Mound.alpha
             ),
          show.legend = 'point'
          ) +
  coord_sf(datum = NA) +
  scale_colour_manual(values = c('darkgrey', 'black')) +
  scale_size_manual(values = c(0.5, 1)) +
  scale_alpha_identity(guide = FALSE) +
  annotation_scale(width_hint = 0.3) +
  annotation_north_arrow(location = "tr", which_north = "true") + 
  theme_void() +
  labs(x = NULL, y = NULL, shape = 'Ant Mound  ', colour = 'Ant Mound  ', size = 'Ant Mound  ')

RWA.Occurrence.p

ggsave(filename = '~/rwa/figures/files/map/Formica_rufa_group_Occurrence_Map.pdf', plot = RWA.Occurrence.p, width = 190, height = 120, units = 'mm')

ggsave(filename = '~/rwa/figures/files/map/Formica_rufa_group_Occurrence_Map.tiff', plot = RWA.Occurrence.p, width = 190, height = 120, units = 'mm', compression = 'lzw')

ggsave(filename = '~/rwa/figures/files/map/Formica_rufa_group_Occurrence_Map.jpg', plot = RWA.Occurrence.p, width = 190, height = 120, units = 'mm')

ggsave(filename = '~/rwa/figures/files/map/Formica_rufa_group_Occurrence_Map.png', plot = RWA.Occurrence.p, width = 190, height = 120, units = 'mm')

### Colour Version

RWA.Occurrence.Colour.p <- ggplot() +
  geom_stars(data = relief.clip.stars, alpha = 0.25, show.legend = FALSE) +
  scale_fill_gradient(low = 'black', high = 'white', na.value = 'white') +
  coord_equal() +
  geom_sf(data = rivers.sf, colour = '#92c5de') + 
  geom_sf(data = lakes.sf, colour = '#92c5de', fill = '#92c5de') + 
  geom_sf(data = Mounds.sf,
          aes(colour = Ant.Mound,
              shape = Ant.Mound,
              size = Ant.Mound,
              alpha = Ant.Mound.alpha
             ),
          show.legend = 'point'
          ) +
  coord_sf(datum = NA) +
  scale_colour_manual(values = c('#5aae61', '#40004b')) + 
  scale_size_manual(values = c(1.5, 3)) +
  scale_alpha_identity(guide = FALSE) +
  annotation_scale(width_hint = 0.3, text_cex = 2) +
  annotation_north_arrow(location = "tr", which_north = "true", height = unit(2, "cm"), width = unit(2, "cm")) + 
  theme_void() +
  theme(legend.text = element_text(size = 20), legend.title = element_text(size = 20)) + 
  labs(x = NULL, y = NULL, shape = 'Ant Mound  ', colour = 'Ant Mound  ', size = 'Ant Mound  ')

RWA.Occurrence.Colour.p

ggsave(filename = '~/rwa/figures/files/map/Formica_rufa_group_Occurrence_Map_Colour.pdf', plot = RWA.Occurrence.Colour.p)

ggsave(filename = '~/rwa/figures/files/map/Formica_rufa_group_Occurrence_Map_Colour.tiff', plot = RWA.Occurrence.Colour.p, compression = 'lzw')

ggsave(filename = '~/rwa/figures/files/map/Formica_rufa_group_Occurrence_Map_Colour.jpg', plot = RWA.Occurrence.Colour.p)

ggsave(filename = '~/rwa/figures/files/map/Formica_rufa_group_Occurrence_Map_Colour.png', plot = RWA.Occurrence.Colour.p)







