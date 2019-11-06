#' Partition Binary, Geostatistical Data for Cross Validation
#'
#' This function finds a set of observations that contains a user specified minimum number of presences (`target.n.present`) and has a ratio of presences to absences within a tolerance limit (`tol`) of this ratio for the full set of observations being partitioned.
#' Observations are selected by an iterative grid search started at the location of a user specified observation (`start.clnr`).
#' The search proceeds by dividing the study area into square cells of side lengths `cell.res` and searching the neighbouring cells of the cell that the user specified starting observation occupies.
#' The ratio of presences to absences in each potential pair of cells formed by the union of the original cell and each of its neighbours is computed and compared to the overall ratio of presences to absences.
#' The neighbouring cell selected to be included in the set is choosen to satisfy two critera.
#' First, it must result in a ratio of presences to absences in the revised set that is within a tolerance `tol.dur` of the ratio closest to the overall ratio that can be achieved by including one of the current neighbouring cells.
#' Second, of the cells that satisfy the first condition the cell that contains the most observations in selected.
#' The selected pair of cells then becomes the current selection and the process is repeated considering all neighbours of these two cells.
#' Once the set of cells consists of more than four cells the process is elaborated slightly to tend the selections towards continuous groups of cells without gaps in the central regions of the groups.
#' Once this stage passed a convex hull that encloses the centroids of all cells in the current selection and the centroid of the neighbouring cell currently being considered is calculated for each neighbouring cell being considered in this iteration.
#' The ratios of presences to absences among the observations that occupy the sets of cells with centroids within these convex hulls are then computed.
#' The new neighbouring cell to include is selected as that which results in the convex hull that in turn results in the set of points that satisfy the pair of hierarchical conditions described above.
#' This process continues until both the number of presences in the selection is greater than `target.n.present` and the ratio of presence to absences in the selection is within `tol` of the ratio of presences to absences in the full data.
#' This can sometimes result in the number of presences in the selection exceeding `target.n.present`.
#'
#' @param start.clnr The unique observation identifier (CLNR) of the observation at which the process is started. The cell which this observation occupies is the first cell considered in the iterative search.
#' @param pts A tibble containing the coordinates of each observation and the observation itself with presences coded as 1's and absences coded as 0's.
#' @param cell.res The side length of the square cells into which the study area is divided for the iterative search.
#' @param tol tolerance: the allowable difference between the ratio of presences to absences in the final selection and this ratio in the full data
#' @param tol.dur tolerance during: used in choosing a cell to include in the current set of cells, cells are only considered for inclusion if their inclusion would result in a ratio of presences to absences in the revised selection that is within `tol.dur` of best of the ratios that would result from inclusion of any one of the neighbouring cells currently being considered for inclusion.
#' @param plot Logical, should a plot depicting the full set of observations and the selected observations be produced? The selected observations are coloured red.
#' @param target.n.present Minimum number of presences that should be contained in the final selection.
#' @param target.ratio The ratio of presences to absences (number of presences/number of absences) to aim for in the final selection of observations.
#' @param m.q.nb.o Maximum queen neighbour order to consider when examining neighbouring cells (order here referes to the number of squares on a chess board that the queen piece would move from the focal square to a neighbouring square).
#' @param relief.rast A hillshaded terrain surface of the study area to use as the background for the plot. If a relief raster is not provided the plot is produced with a blank background.
#'
#' @return A tibble containing the observations that constitute the selection.

chull.partitioner <- function(start.clnr = 142147, pts = RWAMPA.df, cell.res = 6000, tol = 0.005, tol.dur = 0.00001, plot = TRUE, target.n.present = 15, m.q.nb.o = 2, relief.rast = NULL, target.ratio = 290/6051){
  
  # calculate overall number of presences / number of absences

  pa.counts.tb <- dplyr::summarise(.data = pts, Presences = sum(RWAM.PA == 1), Absences = sum(RWAM.PA == 0))

  total.n.present <- dplyr::pull(pa.counts.tb, Presences)
  
  total.n.absent <- dplyr::pull(pa.counts.tb, Absences)

  # create the raster

  rast <- raster::raster(xmn = floor(min(pts$X)-cell.res/2), 
                         xmx = ceiling(max(pts$X)+cell.res/2), 
                         ymn = floor(min(pts$Y)-cell.res/2), 
                         ymx = ceiling(max(pts$Y)+cell.res/2), 
                         resolution = cell.res)

  raster::values(rast) <- 1:raster::ncell(rast)

  # start cell from start point CLNR

  start.cell <- raster::extract(x = rast, y = filter(pts, CLNR == start.clnr) %>% select(X,Y))
  
  # add column to pts that contains ID of raster cell which each point intersects

  pts$cellID <- raster::extract(x = rast, y = select(pts, X, Y))

  # check presence/absence in start.cell

  start.cell.pts <- dplyr::filter(.data = pts, cellID == start.cell)

  start.cell.counts <- dplyr::summarise(.data = start.cell.pts, Presences = sum(RWAM.PA == 1), Absences = sum(RWAM.PA == 0))

  start.cell.n.present <- pull(start.cell.counts, Presences)

  start.cell.n.absent <- pull(start.cell.counts, Absences)

  if(start.cell.n.absent > 0){ratio <- start.cell.n.present/start.cell.n.absent}

  if(start.cell.n.absent == 0){ratio <- start.cell.n.present}

  n.present <- start.cell.n.present

  counter <- 1

  cells <- start.cell 

  while(n.present < target.n.present | abs(ratio - target.ratio) > tol ){    

    # search first order queen 

    cells.sum.tb <- cells.sum(rast = rast, cells = cells, nb = 8, pts = pts, target.ratio = target.ratio)
   
    selection <- cell.select(cells.sum.tb = cells.sum.tb, cells = cells, tol.dur = tol.dur)

   # if all such cells are empty

   if(length(selection) == 0 & m.q.nb.o > 1){
   
    # search 2nd order queen neighbours
   
    q2.nb <- matrix(data = c( 1,  1,  1,  1,  1,
                              1,  1,  1,  1,  1,
                              1,  1,  0,  1,  1,
                              1,  1,  1,  1,  1,
                              1,  1,  1,  1,  1), 
                      ncol = 5, 
                      byrow = TRUE)
   
     cells.sum.tb <- cells.sum(rast = rast, cells = cells, nb = q2.nb, , pts = pts, target.ratio = target.ratio)
   
     selection <- cell.select(cells.sum.tb = cells.sum.tb, cells = cells, tol.dur = tol.dur)
                  
   }

   # if all such cells are empty:

   if(length(selection) == 0 & m.q.nb.o > 2){
   
    # search 3rd order queen neighbours
   
    q3.nb <- matrix(data = c( 1, 1, 1, 1, 1, 1, 1,
                              1, 1, 1, 1, 1, 1, 1,
                              1, 1, 1, 1, 1, 1, 1, 
                              1, 1, 1, 0, 1, 1, 1,
                              1, 1, 1, 1, 1, 1, 1,
                              1, 1, 1, 1, 1, 1, 1,
                              1, 1, 1, 1, 1, 1, 1), 
                      ncol = 7, 
                      byrow = TRUE)
   
     cells.sum.tb <- cells.sum(rast = rast, cells = cells, nb = q3.nb, pts = pts, target.ratio = target.ratio)
   
     selection <- cell.select(cells.sum.tb = cells.sum.tb, cells = cells, tol.dur = tol.dur)
                  
   }

   if(length(selection) == 0){
     print('Error: Reached a point where no pixels within the neighbourhoods of current pixels contain observations. Please try a starting location more centrally located within the study region or a larger resolution grid.')
   break()
   }

   cells <- c(selection, cells) %>% unique()

   counts.i.tb <- filter(pts, cellID %in% cells) %>%
     summarise(Present = sum(RWAM.PA == 1),
               Absent = sum(RWAM.PA == 0))

   n.present <- pull(counts.i.tb, Present)

   n.absent <- pull(counts.i.tb, Absent)

   ratio <- n.present/n.absent

   total <- n.present + n.absent

   print(paste0('iteration ', counter, ', total points in selection = ', total, ', total presences in selection = ', n.present, ', ratio = ', ratio))

   counter <- counter + 1

  }

  if(plot == TRUE){

    if(class(relief.rast) == 'RasterLayer'){

      raster::plot(relief.rast, col = grey(level = seq(from = 0, to = 1, length.out = 1e3), alpha = 0.1), legend = FALSE)

    } else{

       raster::plot(rast, col = 'white', legend = FALSE)

    }  
 
    select(pts, CLNR, X, Y, RWAM.PA) %>%
      filter(RWAM.PA == 0) %>%
        with(., points(x = X, y = Y, pch = 16, cex = 0.25, col = 'darkgrey'))
 
    select(pts, CLNR, X, Y, RWAM.PA) %>%
      filter(RWAM.PA == 1) %>%
        with(., points(x = X, y = Y, pch = 17, cex = 0.5, col = 'darkgrey'))

    select(pts, CLNR, X, Y, RWAM.PA, cellID) %>%
      filter(RWAM.PA == 0 & cellID %in% cells) %>%
        with(., points(x = X, y = Y, pch = 16, cex = 0.5, col = 'red'))
 
    select(pts, CLNR, X, Y, RWAM.PA, cellID) %>%
      filter(RWAM.PA == 1 & cellID %in% cells) %>%
        with(., points(x = X, y = Y, pch = 17, cex = 0.75, col = 'red'))  
     
  }

  output.tb <- select(pts, CLNR, X, Y, RWAM.PA, cellID) %>%
      filter(cellID %in% cells)

   dplyr::summarise(output.tb, 
                    Absent = sum(RWAM.PA == 0),
                    Present = sum(RWAM.PA == 1)
          ) %>%
                dplyr::mutate(Ratio = Present/Absent, 
                              Ratio.diff = abs(Ratio - target.ratio),
                              Total = Present + Absent) %>%
                  print()

  return(output.tb)

}

cells.sum <- function(rast, cells, nb = 8, pts = pts, target.ratio = target.ratio, chull.centroids = TRUE){
  
  qnb.1 <- raster::adjacent(x = rast, cells = cells, directions = nb, pairs = TRUE, include = FALSE, id = FALSE)[,'to'] %>% unique()
  
  cells.sum.tb <- purrr::map_df(.x = qnb.1, 
                                .f = function(x){
                                       cells.i <- c(cells, x)
                                       if(chull.centroids == TRUE){
                                         if(length(cells.i) > 4){
                                           centroids.i <- raster::xyFromCell(object = rast, cell = cells.i)
                                           chull.rows.i <- chull(x = centroids.i)  
                                           chull.df.i <- centroids.i[chull.rows.i, ]  
                                           bound.poly.i <- Polygon(coords = chull.df.i)  
                                           bound.polys.i <- Polygons(srl = list(bound.poly.i), ID = 1)  
                                           bound.polys.sp.i <- SpatialPolygons(Srl = list(bound.polys.i))  
                                           cells.in.bound.poly.i <-  raster::cellFromPolygon(object = rast, p = bound.polys.sp.i)[[1]]
                                           proposed.cells.i <- c(cells, cells.in.bound.poly.i) %>% unique()
                                         } else{
                                             proposed.cells.i <- c(cells, x) %>% unique()
                                         }
                                       } else{
                                           proposed.cells.i <- c(cells, cells.in.bound.poly.i) %>% unique()
                                       }
                                       proposed.plus.previous.cells.summary <- dplyr::filter(.data = pts, cellID %in% proposed.cells.i) %>% 
                                                                                 dplyr::summarise(Absent = sum(RWAM.PA == 0),
                                                                                                  Present = sum(RWAM.PA == 1)
                                                                                         ) %>%
                                                                                         dplyr::mutate(Cell = x, 
                                                                                                       Ratio = Present/Absent, 
                                                                                                       Ratio.diff = abs(Ratio - target.ratio),
                                                                                                       Total = Present + Absent
                                                                                         )                                        
                                      n.obs.proposed.cells.only <- dplyr::filter(.data = pts, cellID == x) %>%
                                        nrow()
                                      cells.sum.tb <- mutate(proposed.plus.previous.cells.summary, n.points.added = n.obs.proposed.cells.only)
                                     }
                  )
  
  return(cells.sum.tb)
  
}

cell.select <- function(cells.sum.tb = cells.sum.tb, cells = cells, tol.dur = 0.005){
  
  out.vec <- filter(cells.sum.tb, !(Cell %in% cells) & !is.na(n.points.added) & !is.na(Ratio.diff) & n.points.added > 0) %>%
     filter(abs(Ratio.diff - min(Ratio.diff)) <= tol.dur) %>%
       filter(Total == max(Total)) %>%
         pull(Cell) %>%
           unique()
  
  return(out.vec)
  
}

