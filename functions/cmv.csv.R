#' A function to plot values of explanatory variables in a cluster against the central synthetic variable that summarises the information in cluster.
#' Subplots are arranged by the squared loading metric of relatedness between the explanatory variable in the subplot and the cluster's central synthetic variable.
#' CMV = Cluster Member Variables
#' CSV = Cluster Synthetic Variable

cmv.csv <- function(csvn = 'cluster3', # cluser synthetic variable name ,
                    data = NFI.Set.2.tb, # raw (unclustered) data
                    tree = NFI.Set.2.hclust.ct, # tree in clustvar format
                    data.description = 'NFI Set 2',
                    facet.ncol = 2, 
                    num.point.alpha = 0.05,
                    box.point.alpha = 0.2
           ){
  cluster.number <- str_extract(string = csvn, '[0-9]+') %>% as.integer()
  # member variables' names
  mvs <- which(tree$cluster == cluster.number) %>% names() 
  # cluster's central synthetic variable
  CSV.tb <- tree$scores %>%
    as_tibble() %>%
      select(csvn) %>%
        rename(csv = csvn)
  # variable loadings
  if(length(mvs) > 1){
  VL.tb <- tibble(squared.loading = tree$var[[csvn]]['squared loading'],
                  Variable = rownames(tree$var[[csvn]])
           ) 
  } else{
  VL.tb <- tibble(squared.loading = tree$var[[csvn]]['squared loading'],
                  Variable = mvs)
  }
  # categorial members of cluster
  data.chr <- select(data, mvs) %>%
    select_if(is.character) %>%
      bind_cols(CSV.tb)
  # numeric members of cluster
  data.num <- select(data, mvs) %>%
    select_if(is.numeric) %>%
      bind_cols(CSV.tb)
  # plotting cluster's central synthetic variable against categorical members of the cluster
  if(ncol(data.chr) > 1){
    cvp <- gather(data = data.chr, -csv, key = 'Variable', value = 'Value') %>%
             left_join(y = VL.tb, by = 'Variable') %>%
               mutate(Variable.ro = fct_reorder(.f = Variable, .x = squared.loading, .fun = unique, .desc = TRUE)) %>%
                 ggplot(aes(x = Value, y = csv)) +
                   geom_jitter(height = 0, width = 0.4, alpha = box.point.alpha, colour = 'grey') + 
                   geom_boxplot(fill = NA, outlier.shape = NA) + 
                   facet_wrap(. ~ Variable.ro, scales = 'free', ncol = facet.ncol) +
                   theme_bw() + 
                   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
                   labs(y = 'Cluster\'s Central Synthetic Variable', x = '', title = paste(data.description, 'Cluster', str_extract(string = csvn, pattern = '\\d+'), sep = ' '))
  } else{
      cvp <- 'no categorical variables in this cluster'
  }
  # plotting cluster's central synthetic variable against numeric members of the cluster
  if(ncol(data.num) > 1){
    nvp <- gather(data = data.num, -csv, key = 'Variable', value = 'Value') %>%
             left_join(y = VL.tb, by = 'Variable') %>%
               mutate(Variable.ro = fct_reorder(.f = Variable, .x = squared.loading, .fun = unique, .desc = TRUE)) %>%
                 ggplot(aes(x = Value, y = csv)) +
                   geom_point(alpha = num.point.alpha) + 
                   facet_wrap(. ~ Variable.ro, scales = 'free', ncol = facet.ncol) +
                   theme_bw() + 
                   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
                   labs(y = 'Cluster\'s Central Synthetic Variable', x = '', title = paste(data.description, 'Cluster', str_extract(string = csvn, pattern = '\\d+'), sep = ' '))
  } else{
      nvp <- 'no numeric variables in this cluster'
  }
  return(list(cat.var.plot = cvp, num.var.plot = nvp))
}
