#' Buffered Monte Carlo Join Count Tests
#'
#' Conducts permutational join count tests for spatial dependence between same coloured joins within user specified disk or ring buffers using user specified spatial neighbourhood weighting schemes from the options of Binary, Inverse Distance and Inverse Squared Distance.
#' 
#' @param in.rad = inner.radius

#' @param out.rad = outer.radius

#' @param crds = coordinates of response observations

#' @param rsp = response
#'
#' @param nb.wght.schm = neighbourhood weighting schemens, a character vector containing one or more of the following characters 'B' (Binary neighbourhood weights), 'ID' (Inverse Distance weights) and 'ISD' (Inverse Square Distance weights).

db.mc.jct <- function(in.rad = 0,
                      out.rad = 2400,
                      crds = select(Points.in.1st.Poly.tb, X, Y) %>% as.matrix(),
                      rsp = Points.in.1st.Poly.tb$RWAM,
                      nb.wght.schm = c('B', 'ID', 'IDS'),
                      n.perm = 1e3, 
                      p.figs = 5,                      
                      quiet = TRUE){
  require(spdep)
  if(!(class(rsp) == 'factor')){
    rsp <- factor(rsp)
  }
  nb.obj <- dnearneigh(x = crds, d1 = in.rad, d2 = out.rad)
  # Binary Weights List
  if('B' %in% nb.wght.schm){
    bin.lstw <- nb2listw(neighbours = nb.obj, zero.policy = TRUE, style = 'B')
    bw.jc.mct.grt <- joincount.mc(fx = rsp, listw = bin.lstw, nsim = n.perm, zero.policy = TRUE, alternative = 'greater')
    bw.jc.mct.lss <- joincount.mc(fx = rsp, listw = bin.lstw, nsim = n.perm, zero.policy = TRUE, alternative = 'less')
    print(paste0('Binary Weights (Alt. Greater): ', names(bw.jc.mct.grt[[1]]$statistic), ' = ', round(bw.jc.mct.grt[[1]]$statistic, 2), ', p = ', round(bw.jc.mct.grt[[1]]$p.value, p.figs)))  
    print(paste0('Binary Weights (Alt. Greater): ', names(bw.jc.mct.grt[[2]]$statistic), ' = ', round(bw.jc.mct.grt[[2]]$statistic, 2), ', p = ', round(bw.jc.mct.grt[[2]]$p.value, p.figs)))
    print(paste0('Binary Weights (Alt. Less): ', names(bw.jc.mct.lss[[1]]$statistic), ' = ', round(bw.jc.mct.lss[[1]]$statistic, 2), ', p = ', round(bw.jc.mct.lss[[1]]$p.value, p.figs)))
    print(paste0('Binary Weights (Alt. Less): ', names(bw.jc.mct.lss[[2]]$statistic), ' = ', round(bw.jc.mct.lss[[2]]$statistic, 2), ', p = ', round(bw.jc.mct.lss[[2]]$p.value, p.figs)))
  }
  # Inverse Distance Weights List
  if('ID' %in% nb.wght.schm){
    nb.dsts <- nbdists(nb = nb.obj, coords = crds)
    inv.dst.weights  <- lapply(nb.dsts, function(x) 1/(x/1000))
    if(quiet == TRUE){
      suppressWarnings(inv.dst.lstw <- nb2listw(neighbours = nb.obj, glist = inv.dst.weights, style = 'B', zero.policy = TRUE))
    } else{
                       inv.dst.lstw <- nb2listw(neighbours = nb.obj, glist = inv.dst.weights, style = 'B', zero.policy = TRUE)
    }
    inv.dst.jc.mct.grt <- joincount.mc(fx = rsp, listw = inv.dst.lstw, nsim = n.perm, zero.policy = TRUE, alternative = 'greater')
    inv.dst.jc.mct.lss <- joincount.mc(fx = rsp, listw = inv.dst.lstw, nsim = n.perm, zero.policy = TRUE, alternative = 'less')
    print(paste0('Inverse Distance Weights (Alt. Greater): ', names(inv.dst.jc.mct.grt[[1]]$statistic), ' = ', round(inv.dst.jc.mct.grt[[1]]$statistic, 2), ', p = ', round(inv.dst.jc.mct.grt[[1]]$p.value, p.figs)))
    print(paste0('Inverse Distance Weights (Alt. Greater): ', names(inv.dst.jc.mct.grt[[2]]$statistic), ' = ', round(inv.dst.jc.mct.grt[[2]]$statistic, 2), ', p = ', round(inv.dst.jc.mct.grt[[2]]$p.value, p.figs)))
    print(paste0('Inverse Distance Weights (Alt. Less): ', names(inv.dst.jc.mct.lss[[1]]$statistic), ' = ', round(inv.dst.jc.mct.lss[[1]]$statistic, 2), ', p = ', round(inv.dst.jc.mct.lss[[1]]$p.value, p.figs)))
    print(paste0('Inverse Distance Weights (Alt. Less): ', names(inv.dst.jc.mct.lss[[2]]$statistic), ' = ', round(inv.dst.jc.mct.lss[[2]]$statistic, 2), ', p = ', round(inv.dst.jc.mct.lss[[2]]$p.value, p.figs)))
  }
  # Inverse Square Distance Weights List
  if('IDS' %in% nb.wght.schm){
    inv.sq.dst.weights <- lapply(nb.dsts, function(x) 1/((x/1000)^2))  
    if(quiet == TRUE){
      suppressWarnings(inv.sq.dst.lstw <- nb2listw(neighbours = nb.obj, glist = inv.sq.dst.weights, style = 'B', zero.policy = TRUE))
    } else{
                       inv.sq.dst.lstw <- nb2listw(neighbours = nb.obj, glist = inv.sq.dst.weights, style = 'B', zero.policy = TRUE)  
    }  
    inv.sq.dst.jc.mct.grt <- joincount.mc(fx = rsp, listw = inv.sq.dst.lstw, nsim = n.perm, zero.policy = TRUE, alternative = 'greater')  
    inv.sq.dst.jc.mct.lss <- joincount.mc(fx = rsp, listw = inv.sq.dst.lstw, nsim = n.perm, zero.policy = TRUE, alternative = 'less')  
    print(paste0('Inverse Square Distance Weights (Alt. Greater): ', names(inv.sq.dst.jc.mct.grt[[1]]$statistic), ' = ', round(inv.sq.dst.jc.mct.grt[[1]]$statistic, 2), ', p = ', round(inv.sq.dst.jc.mct.grt[[1]]$p.value, p.figs)))  
    print(paste0('Inverse Square Distance Weights (Alt. Greater): ', names(inv.sq.dst.jc.mct.grt[[2]]$statistic), ' = ', round(inv.sq.dst.jc.mct.grt[[2]]$statistic, 2), ', p = ', round(inv.sq.dst.jc.mct.grt[[2]]$p.value, p.figs)))  
    print(paste0('Inverse Square Distance Weights (Alt. Less): ', names(inv.sq.dst.jc.mct.lss[[1]]$statistic), ' = ', round(inv.sq.dst.jc.mct.lss[[1]]$statistic, 2), ', p = ', round(inv.sq.dst.jc.mct.lss[[1]]$p.value, p.figs)))  
    print(paste0('Inverse Square Distance Weights (Alt. Less): ', names(inv.sq.dst.jc.mct.lss[[2]]$statistic), ' = ', round(inv.sq.dst.jc.mct.lss[[2]]$statistic, 2), ', p = ', round(inv.sq.dst.jc.mct.lss[[2]]$p.value, p.figs)))  
  }
}
