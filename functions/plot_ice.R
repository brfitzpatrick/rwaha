#' A function to produce Individual Conditional Expectation (ICE) plots
#'
#' Longer description goes here...
#' 
#' @param ice.obj the output of \code{\link[ICEbox]{ice}}
#' @param col.by the name of the variable mapped to colour in the plot. If you do not wish to colour the lines by a variable leave this argument blank (or supply it with `NULL`).
#' @param facet.rows.by the name of the variable that will be used to define the rows of the grid of facets (small multiples). Leave blank to produce a single column of small multiples.
#' @param facet.rows.threshold the threshold value of \code{facet.rows.by} that will be used to divide the data between the rows of the grid of facets  (small multiples). If left blank (i.e. supplied with the default `NULL` value) the median of the faceting variable will be used.
#' @param facet.cols.by the name of the variable that will be used to define the columns of the grid of facets (small multiples). Leave blank to produce a single column of small multiples.
#' @param facet.cols.threshold the threshold value of \code{facet.cols.by} that will be used to divide the data between the columns of the grid of facets (small multiples). If left blank (i.e. supplied with the default `NULL` value) the median of the faceting variable will be used.
#' @param facet.threshold.rounding.digits the number of digits to which the facet threshold will be rounded. Defaults to a value of 2 which equates to two decimal places.
#' @param plot.pd.curve plot the partial dependence curve (TRUE/FALSE)

#' @param alpha.dist.to.obs map the displacement between the focal observation of each line and the horizontal coordinate to the transparency of the line (TRUE/FALSE)
#' @param alpha.dist.to.obs.power power to which the distance from the focal observation is raised before mapping this distance to the transparency of the associated line in the plot
#' @param alpha.value ignored if `alpha.dist.to.obs = TRUE`
#' @param y.axis.title character vector with which to label the vertical axis/axes of the plot(s)
#' @param x.axis.title character vector with which to label the horizontal axis/axes of the plot(s)
#' @param plot.order.low.to.high draw the curves in the order of the associated values of the variable mapped to line colour. If `TRUE` the lines associated with higher values of the variable mapped to colour are drawn later and consequently on top of lines associated with lower values of this variable.
#' 
#' @references \href{https://doi.org/10.1080/10618600.2014.907095}{Goldstein, A., Kapelner, A., Bleich, J., & Pitkin, E. (2015). Peeking Inside the Black Box: Visualizing Statistical Learning With Plots of Individual Conditional Expectation. Journal of Computational and Graphical Statistics, 24(1), 44–65.}

library(ICEbox)
library(tidyverse)

plot_ice <- function(ice.obj = ice.elev,
                      colour.by = NULL,
                      facet.rows.by = NULL,
                      facet.rows.threshold = NULL,
                      facet.cols.by = NULL,
                      facet.cols.threshold = NULL,
                      facet.threshold.rounding.digits = 2, 
                      alpha.dist.to.obs = TRUE,
                      alpha.dist.to.obs.power = 3,
                      alpha.scale.range = c(0,0.1),
                      alpha.value = NULL,
                      center.curves = TRUE,
                      centered.quantile = 0,
                      plot.order.low.to.high = TRUE,
                      plot.pd.curve = TRUE,
                      pdc.thick.line.col = 'white',
                      pdc.thin.line.col = 'black',
                      pdc.thick.line.width = 2,
                      pdc.thin.line.width = 0.5,
                      pdc.thick.line.alpha = 0.5,
                      pdc.thin.line.alpha = 1,
                      x.axis.title = NULL,
                      y.axis.title = NULL  
                     ){
  if(length(x.axis.title) == 0){
    x.axis.title <- ice.obj$predictor
  }
  if(length(y.axis.title) == 0){
    if(center.curves){
      y.axis.title <- 'centered yhat'
    } else{
        y.axis.title <- 'yhat'
      }
  }
  predictor.sym <- sym(ice.obj$predictor)
  ice.curves.mat.rnc <- ice.obj$ice_curves # rnc = renamed columns
  colnames(ice.curves.mat.rnc) <- paste0('CP', 1:ncol(ice.curves.mat.rnc))
  ic.key.tb <- tibble(New.CN = colnames(ice.curves.mat.rnc), Old.CN = colnames(ice.obj$ice_curves), gridpts = ice.obj$gridpts)
  # summary(as.numeric(ic.key.tb$Old.CN) - ic.key.tb$gridpts) # i.e. these are basically identical
  ice.tb <- cbind(ice.obj$Xice, ice.curves.mat.rnc) %>%
    as_tibble() %>%
      mutate(Curve = 1:nrow(.)) %>%
        gather(ic.key.tb$New.CN, key = 'New.CN', value = 'yhat') %>% 
          left_join(y = ic.key.tb, by = 'New.CN')
  if(alpha.dist.to.obs){
    predictor.range <- abs(max(ice.obj$gridpts) - min(ice.obj$gridpts))
    ice.tb <- mutate(.data = ice.tb,
                     dist.to.obs = (!!predictor.sym) - gridpts,
                     abs.dist.to.obs = abs(dist.to.obs),
                     curve.alpha = (1 - abs.dist.to.obs/predictor.range)^alpha.dist.to.obs.power
              )
  } else{
      ice.tb <- mutate(.data = ice.tb, curve.alpha = alpha.value)
    }  
  if(class(facet.rows.by) == 'character'){
    facet.rows.by.sym <- sym(facet.rows.by)
      if(!(class(facet.rows.threshold) %in% c('numeric', 'integer'))){
        facet.rows.threshold <- pull(ice.tb, !!facet.rows.by.sym) %>%
                                  median() %>%
                                    round(digits = facet.threshold.rounding.digits)
      } 
  }
  if(class(facet.cols.by) == 'character'){
    facet.cols.by.sym <- sym(facet.cols.by)
    if(!(class(facet.cols.threshold) %in% c('numeric', 'integer'))){
      facet.cols.threshold <- pull(ice.tb, !!facet.cols.by.sym) %>%
                                median() %>%
                                  round(digits = facet.threshold.rounding.digits)
    }
  }
  if(center.curves){
    center.at <- quantile(x = ice.obj$gridpts, probs = centered.quantile)
    ice.tb <- mutate(ice.tb, dist.to.center.at = abs(gridpts - center.at))
    min.dist.to.center.at.tb <- group_by(ice.tb, Curve) %>%
      summarise(min.dist.to.center.at = min(dist.to.center.at))
    ice.tb <- left_join(x = ice.tb, y = min.dist.to.center.at.tb, by = 'Curve')
    ice.vert.offsets.tb <- filter(ice.tb, dist.to.center.at == min.dist.to.center.at) %>%
      group_by(Curve) %>%
        summarise(yhat.offset = mean(yhat))    
    ice.tb <- left_join(x = ice.tb, y = ice.vert.offsets.tb, by = 'Curve') %>%
      mutate(yhat.recentered = yhat - yhat.offset) %>%
        select(-yhat) %>%
          rename(yhat = yhat.recentered) 
    pd.dist.to.center.at.tb <- tibble(gridpts = ice.obj$gridpts,
                                      yhat = ice.obj$pdp) %>%
                                 mutate(dist.to.center.at = abs(gridpts - center.at))
    pd.min.dist.to.center.at <- pull(pd.dist.to.center.at.tb, dist.to.center.at) %>%
                                  min()
    pd.vertical.offset <- filter(pd.dist.to.center.at.tb, dist.to.center.at == pd.min.dist.to.center.at) %>%
      pull(yhat) %>%
        mean()
  } else{
      pd.vertical.offset <- 0
    }
  if(class(facet.rows.by) == 'character' & class(facet.cols.by) == 'character'){
    if(facet.rows.by == facet.cols.by){
      V.q1 <- pull(ice.tb, (!!facet.cols.by.sym)) %>%
                quantile(probs = 0.25)
      V.q2 <- pull(ice.tb, (!!facet.cols.by.sym)) %>%
                quantile(probs = 0.5)
      V.q3 <- pull(ice.tb, (!!facet.cols.by.sym)) %>%
                quantile(probs = 0.75)
      int.fac.var.chr <- paste0(facet.cols.by,' range')
      int.fac.var.sym <- sym(int.fac.var.chr)
      ice.tb <- mutate(.data = ice.tb,
                       (!!int.fac.var.sym) := case_when(
                                 (!!facet.cols.by.sym) <= V.q1 ~ '0% - 25%',
                                 V.q1 < (!!facet.cols.by.sym) & (!!facet.cols.by.sym) <= V.q2 ~ '26% - 50%',
                                 V.q2 < (!!facet.cols.by.sym) & (!!facet.cols.by.sym) <= V.q3 ~ '51% - 75%',
                                 V.q3 < (!!facet.cols.by.sym) ~ '76% - 100%'
                               )
                )    
    }
  }
  if(class(colour.by) == 'character'){
    colour.by.sym <- sym(colour.by)
    if(plot.order.low.to.high){
      Curve.ro <- group_by(ice.tb, Curve) %>%
        summarise(u.colour.by = unique((!!colour.by.sym))) %>%
          arrange(u.colour.by) %>%
            mutate(plot.order = 1:nrow(.))
    } else{
      Curve.ro <- group_by(ice.tb, Curve) %>%
        summarise(u.colour.by = unique((!!colour.by.sym))) %>%
          arrange(desc(u.colour.by)) %>%
            mutate(plot.order = 1:nrow(.))
      }    
    plot <- left_join(x = ice.tb, y = Curve.ro, by = 'Curve') %>%
      arrange(plot.order) %>%
        ggplot(aes(x = gridpts, y = yhat, group = Curve, colour = (!!colour.by.sym), alpha = curve.alpha)) +
          geom_line() +
          scale_colour_viridis_c() + 
          theme_bw() +
          scale_alpha_continuous(range = alpha.scale.range) +
          guides(alpha = FALSE) + 
          labs(x = x.axis.title, y = y.axis.title)
  } else{
      plot <- ggplot(data = ice.tb, aes(x = gridpts, y = yhat, group = Curve, alpha = curve.alpha)) +
                geom_line() +
                theme_bw() +
                scale_alpha_continuous(range = alpha.scale.range) +
                guides(alpha = FALSE) + 
                labs(x = x.axis.title, y = y.axis.title)
  }  
  if( (class(facet.rows.by) == 'character') & !(class(facet.cols.by) == 'character')){  
    plot <- plot +
      facet_grid(rows = vars((!!facet.rows.by.sym) > (!!facet.rows.threshold)),
                 cols = NULL,
                 scales = 'fixed',
                 labeller = labeller(.rows = label_both, .cols = NULL)
      )
    if(plot.pd.curve){
      pd.row.1.tb <- tibble(gridpts = ice.obj$gridpts,
                            yhat = ice.obj$pdp - pd.vertical.offset,
                            plot.order = 0,
                            curve.alpha = 1,
                            (!!facet.rows.by.sym) := facet.rows.threshold - 0.1, 
                            Curve = 1
                     )       
      pd.row.2.tb <- tibble(gridpts = ice.obj$gridpts,
                            yhat = ice.obj$pdp - pd.vertical.offset,
                            plot.order = 0,
                            curve.alpha = 1,
                            (!!facet.rows.by.sym) := facet.rows.threshold + 0.1, 
                            Curve = 1
                     )      
      plot <- plot +
        geom_line(data = pd.row.1.tb, colour = pdc.thick.line.col, size = pdc.thick.line.width, alpha = pdc.thick.line.alpha) +
        geom_line(data = pd.row.1.tb, colour = pdc.thin.line.col, size = pdc.thin.line.width, alpha = pdc.thin.line.alpha) +
        geom_line(data = pd.row.2.tb, colour = pdc.thick.line.col, size = pdc.thick.line.width, alpha = pdc.thick.line.alpha) +
        geom_line(data = pd.row.2.tb, colour = pdc.thin.line.col, size = pdc.thin.line.width, alpha = pdc.thin.line.alpha)
    }    
  }
  if( !(class(facet.rows.by) == 'character') & (class(facet.cols.by) == 'character')){  
    plot <- plot +
              facet_grid(cols = vars((!!facet.cols.by.sym) > (!!facet.cols.threshold)),
                         rows = NULL,
                         scales = 'fixed',
                         labeller = labeller(.cols = label_both, .rows = NULL)
              )    
    if(plot.pd.curve){
      pd.row.1.tb <- tibble(gridpts = ice.obj$gridpts,
                            yhat = ice.obj$pdp - pd.vertical.offset,
                            plot.order = 0,
                            curve.alpha = 1,
                            (!!facet.cols.by.sym) := facet.cols.threshold - 0.1,
                            Curve = 1
                     )       
      pd.row.2.tb <- tibble(gridpts = ice.obj$gridpts,
                            yhat = ice.obj$pdp - pd.vertical.offset,
                            plot.order = 0,
                            curve.alpha = 1,
                            (!!facet.cols.by.sym) := facet.cols.threshold + 0.1,
                            Curve = 1
                     )       
      plot <- plot +
        geom_line(data = pd.row.1.tb, colour = pdc.thick.line.col, size = pdc.thick.line.width, alpha = pdc.thick.line.alpha) +
        geom_line(data = pd.row.1.tb, colour = pdc.thin.line.col, size = pdc.thin.line.width, alpha = pdc.thin.line.alpha) +
        geom_line(data = pd.row.2.tb, colour = pdc.thick.line.col, size = pdc.thick.line.width, alpha = pdc.thick.line.alpha) +
        geom_line(data = pd.row.2.tb, colour = pdc.thin.line.col, size = pdc.thin.line.width, alpha = pdc.thin.line.alpha)
    }
  }  
  if( (class(facet.rows.by) == 'character') & (class(facet.cols.by) == 'character') ){
    if( !(facet.rows.by == facet.cols.by) ){  
      plot <- plot +
                facet_grid(rows = vars((!!facet.rows.by.sym) > (!!facet.rows.threshold)),
                           cols = vars((!!facet.cols.by.sym) > (!!facet.cols.threshold)),
                           scales = 'fixed',
                           labeller = labeller(.cols = label_both, .rows = label_both)
                )
      if(plot.pd.curve){
        pd.row.1.tb <- tibble(gridpts = ice.obj$gridpts,
                              yhat = ice.obj$pdp - pd.vertical.offset,
                              plot.order = 0,
                              curve.alpha = 1,
                              (!!facet.rows.by.sym) := facet.rows.threshold - 0.1,
                              (!!facet.cols.by.sym) := facet.cols.threshold - 0.1,
                              Curve = 1
                       )       
        pd.row.2.tb <- tibble(gridpts = ice.obj$gridpts,
                              yhat = ice.obj$pdp - pd.vertical.offset,
                              plot.order= 0,
                              curve.alpha = 1,
                              (!!facet.rows.by.sym) := facet.rows.threshold - 0.1,
                              (!!facet.cols.by.sym) := facet.cols.threshold + 0.1,
                              Curve = 1
                       )
        pd.row.3.tb <- tibble(gridpts = ice.obj$gridpts,
                              yhat = ice.obj$pdp - pd.vertical.offset,
                              plot.order= 0,
                              curve.alpha = 1,
                              (!!facet.rows.by.sym) := facet.rows.threshold + 0.1,
                              (!!facet.cols.by.sym) := facet.cols.threshold - 0.1,
                              Curve = 1
                       )       
        pd.row.4.tb <- tibble(gridpts = ice.obj$gridpts,
                              yhat = ice.obj$pdp - pd.vertical.offset,
                              plot.order= 0,
                              curve.alpha = 1,
                              (!!facet.rows.by.sym) := facet.rows.threshold + 0.1,
                              (!!facet.cols.by.sym) := facet.cols.threshold + 0.1,
                              Curve = 1
                       )      
        plot <- plot +
          geom_line(data = pd.row.1.tb, colour = pdc.thick.line.col, size = pdc.thick.line.width, alpha = pdc.thick.line.alpha) +
          geom_line(data = pd.row.1.tb, colour = pdc.thin.line.col, size = pdc.thin.line.width, alpha = pdc.thin.line.alpha) +
          geom_line(data = pd.row.2.tb, colour = pdc.thick.line.col, size = pdc.thick.line.width, alpha = pdc.thick.line.alpha) +
          geom_line(data = pd.row.2.tb, colour = pdc.thin.line.col, size = pdc.thin.line.width, alpha = pdc.thin.line.alpha) +
          geom_line(data = pd.row.3.tb, colour = pdc.thick.line.col, size = pdc.thick.line.width, alpha = pdc.thick.line.alpha) +
          geom_line(data = pd.row.3.tb, colour = pdc.thin.line.col, size = pdc.thin.line.width, alpha = pdc.thin.line.alpha) +
          geom_line(data = pd.row.4.tb, colour = pdc.thick.line.col, size = pdc.thick.line.width, alpha = pdc.thick.line.alpha) +
          geom_line(data = pd.row.4.tb, colour = pdc.thin.line.col, size = pdc.thin.line.width, alpha = pdc.thin.line.alpha) 
      }
    } 
    if( (facet.rows.by == facet.cols.by) ){
      plot <- plot +
                facet_wrap(facets = vars(!!int.fac.var.sym),
                           nrow = 2,
                           scales = 'fixed',
                           labeller = labeller(.cols = label_both, .rows = NULL)
                )
      if(plot.pd.curve){
        pd.row.1.tb <- tibble(gridpts = ice.obj$gridpts,
                              yhat = ice.obj$pdp - pd.vertical.offset,
                              plot.order = 0,
                              curve.alpha = 1,                            
                              (!!int.fac.var.sym) := '0% - 25%',
                              Curve = 1
                       )       
        pd.row.2.tb <- tibble(gridpts = ice.obj$gridpts,
                              yhat = ice.obj$pdp - pd.vertical.offset,
                              plot.order = 0,
                              curve.alpha = 1,                            
                              (!!int.fac.var.sym) := '26% - 50%',
                              Curve = 1
                       )
        pd.row.3.tb <- tibble(gridpts = ice.obj$gridpts,
                              yhat = ice.obj$pdp - pd.vertical.offset,
                              plot.order = 0,
                              curve.alpha = 1,                            
                              (!!int.fac.var.sym) := '51% - 75%',
                              Curve = 1
                       )       
        pd.row.4.tb <- tibble(gridpts = ice.obj$gridpts,
                              yhat = ice.obj$pdp - pd.vertical.offset,
                              plot.order = 0,
                              curve.alpha = 1,                            
                              (!!int.fac.var.sym) := '76% - 100%',
                              Curve = 1
                       )      
        plot <- plot +
          geom_line(data = pd.row.1.tb, colour = pdc.thick.line.col, size = pdc.thick.line.width, alpha = pdc.thick.line.alpha) +
          geom_line(data = pd.row.1.tb, colour = pdc.thin.line.col, size = pdc.thin.line.width, alpha = pdc.thin.line.alpha) +
          geom_line(data = pd.row.2.tb, colour = pdc.thick.line.col, size = pdc.thick.line.width, alpha = pdc.thick.line.alpha) +
          geom_line(data = pd.row.2.tb, colour = pdc.thin.line.col, size = pdc.thin.line.width, alpha = pdc.thin.line.alpha) +
          geom_line(data = pd.row.3.tb, colour = pdc.thick.line.col, size = pdc.thick.line.width, alpha = pdc.thick.line.alpha) +
          geom_line(data = pd.row.3.tb, colour = pdc.thin.line.col, size = pdc.thin.line.width, alpha = pdc.thin.line.alpha) +
          geom_line(data = pd.row.4.tb, colour = pdc.thick.line.col, size = pdc.thick.line.width, alpha = pdc.thick.line.alpha) +
          geom_line(data = pd.row.4.tb, colour = pdc.thin.line.col, size = pdc.thin.line.width, alpha = pdc.thin.line.alpha) 
      }
    }
  }  
  if( !(class(facet.rows.by) == 'character') & !(class(facet.cols.by) == 'character')){  
    if(plot.pd.curve){
       pd.row.1.tb <- tibble(gridpts = ice.obj$gridpts,
                             yhat = ice.obj$pdp - pd.vertical.offset,
                             plot.order = 0,
                             curve.alpha = 1,
                             Curve = 1
                      )       
       plot <- plot +
         geom_line(data = pd.row.1.tb, colour = pdc.thick.line.col, size = pdc.thick.line.width, alpha = pdc.thick.line.alpha) +
         geom_line(data = pd.row.1.tb, colour = pdc.thin.line.col, size = pdc.thin.line.width, alpha = pdc.thin.line.alpha)    
    }    
  }  
  return(plot)  
}
