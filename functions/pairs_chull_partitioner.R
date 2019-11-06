#' Pairs Convex Hull Partitioner 
#'
#' Partition Binary, Geostatistical Data for Cross Validation.
#' This function is an elaboration of the similarly named `chull_partitioner( )`.
#' The elaboration allows a pair of spatially separated starting points to be supplied to `start.clnr`.
#' The function then proceeds to grow a pair of spatially separate regions of cells, one that starts with the first member of the pair of starting points and the other that starts with the other member of this pair of points.
#' The regions of cells are grown iteratively.
#' At each iteration one (or more) neighbouring cells are added to only one of the two regions.
#' The region that gets cells added to it in an iteration and the cells that are added are selected as those which bring the ratio of presences to absences in the union of the two regions closest to that supplied to `target.ratio` (typically the ratio of presences to absences in the full data set).
#' Ties are broken by selecting the member of the tied cells that would most increase the number of observations in the union of cells any further ties are broken at random.
#'
#' The motivation for making this elaboration was to parition data with a non-stationary spatial dependence structure, namely data in which there are regions where the ratio of presences to absences is much higher than other regions.
#' With this function a pair of starting observations can be supplied such that one member of the pair is in a region with a higher than averge density of presences and the other member of this pair is in a region with lower than average density of presences.
#' The function can then select a hold out set for use in cross validation that has a ratio of presences to absences that is within `tol` of the values supplied to `target.ratio`.
#' It does this by selecting obseravtions from these two spatially separate regions.
#'
#' @param start.clnr The unique observation identifiers (CLNR) for the pair of observations at which the process is started. The cell which each observation occupies is the first cell considered in the iterative search started at that observation.
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

pairs.chull.partitioner <- function(start.clnr = c(78596, 13680), pts = RWAMPA.df, cell.res = 6000, tol = 0.005, tol.dur = 0.00001, plot = TRUE, target.n.present = 15, m.q.nb.o = 2, relief.rast = NULL, target.ratio = 290/6051){
  
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

  start.cells <- raster::extract(x = rast, y = filter(pts, CLNR %in% start.clnr) %>% select(X,Y))
  
  # add column to pts that contains ID of raster cell which each point intersects

  pts$cellID <- raster::extract(x = rast, y = select(pts, X, Y))

  # check presence/absence in start.cells

  start.cell.pts <- dplyr::filter(.data = pts, cellID %in% start.cells)

  start.cell.counts <- dplyr::summarise(.data = start.cell.pts, Presences = sum(RWAM.PA == 1), Absences = sum(RWAM.PA == 0))

  start.cell.n.present <- pull(start.cell.counts, Presences)

  start.cell.n.absent <- pull(start.cell.counts, Absences)

  if(start.cell.n.absent > 0){ratio <- start.cell.n.present/start.cell.n.absent}

  if(start.cell.n.absent == 0){ratio <- start.cell.n.present}

  n.present <- start.cell.n.present

  counter <- 1

  A.cells <- start.cells[1]

  B.cells <- start.cells[2]

  while(n.present < target.n.present | abs(ratio - target.ratio) > tol ){    

    # search 1st order queen neighbours of cells.a

    # search 2nd order queen neighbours of cells.a

    # search 3rd order queen neighbours of cells.a

    # search 1st order queen neighbours of cells.b

    # search 2nd order queen neighbours of cells.b

    # search 3rd order queen neighbours of cells.b

    # choose one cell to add which is either a neighbour of cells.a or a neighbour of cells.b

    # choose this cell as that which best improves the ratio in the union of cells.a and cells.b
   
    # Cells A
    
    A.cells.sum.tb <- pairs.cells.sum(rast = rast, a.cells = A.cells, b.cells = B.cells, focus = 'a', nb = 8, pts = pts, target.ratio = target.ratio)
    
    A.selection <- cell.select(cells.sum.tb = A.cells.sum.tb, cells = A.cells, tol.dur = tol.dur)

    # if all such cells are empty

    if(length(A.selection) == 0 & m.q.nb.o > 1){
      
      # search 2nd order queen neighbours
      
      q2.nb <- matrix(data = c( 1,  1,  1,  1,  1,
                              1,  1,  1,  1,  1,
                              1,  1,  0,  1,  1,
                              1,  1,  1,  1,  1,
                              1,  1,  1,  1,  1), 
                      ncol = 5, 
                      byrow = TRUE)
      
      A.cells.sum.tb <- pairs.cells.sum(rast = rast, a.cells = A.cells, b.cells = B.cells, focus = 'a', nb = q2.nb, pts = pts, target.ratio = target.ratio)
      A.selection <- cell.select(cells.sum.tb = A.cells.sum.tb, cells = A.cells, tol.dur = tol.dur)
                  
    }

    # if all such cells are empty:

    if(length(A.selection) == 0 & m.q.nb.o > 2){
      
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
      
      A.cells.sum.tb <- pairs.cells.sum(rast = rast, a.cells = A.cells, b.cells = B.cells, focus = 'a', nb = q3.nb, pts = pts, target.ratio = target.ratio)
      
      A.selection <- cell.select(cells.sum.tb = A.cells.sum.tb, cells = A.cells, tol.dur = tol.dur)
                  
    }
        
    # Cells B
    
    B.cells.sum.tb <- pairs.cells.sum(rast = rast, a.cells = A.cells, b.cells = B.cells, focus = 'b', nb = 8, pts = pts, target.ratio = target.ratio)

    B.selection <- cell.select(cells.sum.tb = B.cells.sum.tb, cells = B.cells, tol.dur = tol.dur)

    # if all such cells are empty

    if(length(B.selection) == 0 & m.q.nb.o > 1){
      
      # search 2nd order queen neighbours
      
      q2.nb <- matrix(data = c( 1,  1,  1,  1,  1,
                              1,  1,  1,  1,  1,
                              1,  1,  0,  1,  1,
                              1,  1,  1,  1,  1,
                              1,  1,  1,  1,  1), 
                      ncol = 5, 
                      byrow = TRUE)
      
      B.cells.sum.tb <- pairs.cells.sum(rast = rast, a.cells = A.cells, b.cells = B.cells, focus = 'b', nb = q2.nb, pts = pts, target.ratio = target.ratio)

      B.selection <- cell.select(cells.sum.tb = B.cells.sum.tb, cells = B.cells, tol.dur = tol.dur)
      
    }

    # if all such cells are empty:

    if(length(B.selection) == 0 & m.q.nb.o > 2){
      
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
      
      B.cells.sum.tb <- pairs.cells.sum(rast = rast, a.cells = A.cells, b.cells = B.cells, focus = 'b', nb = q3.nb, pts = pts, target.ratio = target.ratio)

      B.selection <- cell.select(cells.sum.tb = B.cells.sum.tb, cells = B.cells, tol.dur = tol.dur)
                  
    }

    if(length(A.selection) == 0 & length(B.selection) == 0){
      print('Error: Reached a point where no pixels within the neighbourhoods of current pixels contain observations. Please try a starting location more centrally located within the study region or a larger resolution grid.')
      break()
    }

    AB.plus.new.A.sum <- dplyr::filter(.data = pts, cellID %in% c(A.cells, B.cells, A.selection)) %>% 
                           dplyr::summarise(Absent = sum(RWAM.PA == 0),
                                            Present = sum(RWAM.PA == 1)
                                  ) %>%
                             dplyr::mutate(Ratio = Present/Absent, 
                                           Ratio.diff = abs(Ratio - target.ratio),
                                           Total = Present + Absent
                                    )

    ratio.diff.with.new.A <- pull(AB.plus.new.A.sum, Ratio.diff)

    total.with.new.A <- pull(AB.plus.new.A.sum, Total)
    
    AB.plus.new.B.sum <- dplyr::filter(.data = pts, cellID %in% c(A.cells, B.cells, B.selection)) %>% 
                           dplyr::summarise(Absent = sum(RWAM.PA == 0),
                                            Present = sum(RWAM.PA == 1)
                                  ) %>%
                             dplyr::mutate(Ratio = Present/Absent, 
                                           Ratio.diff = abs(Ratio - target.ratio),
                                           Total = Present + Absent
                                    )

    ratio.diff.with.new.B <- pull(AB.plus.new.B.sum, Ratio.diff)

    total.with.new.B <- pull(AB.plus.new.B.sum, Total)


    if(ratio.diff.with.new.A < ratio.diff.with.new.B){

      A.cells <- c(A.cells, A.selection)       

    }  

    if(ratio.diff.with.new.A > ratio.diff.with.new.B){

      B.cells <- c(B.cells, B.selection)       
      
    }  

    if(ratio.diff.with.new.A == ratio.diff.with.new.B){
    
      if(total.with.new.A > total.with.new.B){
        A.cells <- c(A.cells, A.selection)
      }
    
      if(total.with.new.A < total.with.new.B){
        B.cells <- c(B.cells, B.selection)       
      }
    
      if(total.with.new.A == total.with.new.B){
        choice <- sample(x = 1:2, size = 1)
        if(choice == 1){
          A.cells <- c(A.cells, A.selection)       
        } else{
            B.cells <- c(B.cells, B.selection)       
        }        
      }    
    }  

    counts.i.tb <- filter(pts, cellID %in% c(A.cells, B.cells)) %>%
      summarise(Present = sum(RWAM.PA == 1),
                Absent  = sum(RWAM.PA == 0))

   n.present <- pull(counts.i.tb, Present)

   n.absent <- pull(counts.i.tb, Absent)

   ratio <- n.present/n.absent

   total <- n.present + n.absent

   print(paste0('iteration ', counter, ', total points in selection = ', total, ', total presences in selection = ', n.present, ', ratio = ', ratio))

   counter <- counter + 1

  }

  cells <- c(A.cells, B.cells)

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

pairs.cells.sum <- function(rast, a.cells, b.cells, focus = 'a', nb = 8, pts = pts, target.ratio = target.ratio){

  if(focus == 'a'){
    a.nb <- raster::adjacent(x = rast, cells = a.cells, directions = nb, pairs = TRUE, include = FALSE, id = FALSE)[,'to'] %>% unique()

    cells.sum.tb <- purrr::map_df(.x = a.nb, 
                                  .f = function(x){
                                         cells.i <- c(a.cells, x)
                                         if(length(cells.i) > 4){
                                           centroids.i <- raster::xyFromCell(object = rast, cell = cells.i)
                                           chull.rows.i <- chull(x = centroids.i)  
                                           chull.df.i <- centroids.i[chull.rows.i, ]  
                                           bound.poly.i <- Polygon(coords = chull.df.i)  
                                           bound.polys.i <- Polygons(srl = list(bound.poly.i), ID = 1)  
                                           bound.polys.sp.i <- SpatialPolygons(Srl = list(bound.polys.i))  
                                           cells.in.bound.poly.i <-  raster::cellFromPolygon(object = rast, p = bound.polys.sp.i)[[1]]
                                           proposed.cells.i <- c(a.cells, b.cells, cells.in.bound.poly.i) %>% unique()
                                         } else{
                                             proposed.cells.i <- c(a.cells, b.cells, x) %>% unique()
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
  }

  if(focus == 'b'){
    b.nb <- raster::adjacent(x = rast, cells = b.cells, directions = nb, pairs = TRUE, include = FALSE, id = FALSE)[,'to'] %>% unique()

    cells.sum.tb <- purrr::map_df(.x = b.nb, 
                                  .f = function(x){
                                         cells.i <- c(b.cells, x)
                                         if(length(cells.i) > 4){
                                           centroids.i <- raster::xyFromCell(object = rast, cell = cells.i)
                                           chull.rows.i <- chull(x = centroids.i)  
                                           chull.df.i <- centroids.i[chull.rows.i, ]  
                                           bound.poly.i <- Polygon(coords = chull.df.i)  
                                           bound.polys.i <- Polygons(srl = list(bound.poly.i), ID = 1)  
                                           bound.polys.sp.i <- SpatialPolygons(Srl = list(bound.polys.i))  
                                           cells.in.bound.poly.i <-  raster::cellFromPolygon(object = rast, p = bound.polys.sp.i)[[1]]
                                           proposed.cells.i <- c(a.cells, b.cells, cells.in.bound.poly.i) %>% unique()
                                         } else{
                                             proposed.cells.i <- c(a.cells, b.cells, x) %>% unique()
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
  }
  
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
