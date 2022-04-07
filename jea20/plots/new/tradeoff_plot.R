tradeoff_plot <- function(dataframes,
                          relative_to,
                          objective = "avg_km1",
                          order = NULL,
                          point_size_scaling = 1, 
                          ncol = 3,
                          legend_col = 4,
                          latex_export = F,
                          small_size = F) {
  # Remove all timeout instances
  relative_to <- relative_to[relative_to$timeout == F,]
  relative_to <- droplevels(relative_to)
  relative_to <- relative_to %>% mutate(avg_km1 = ifelse(avg_km1 == 0, 1, avg_km1))
  relative_to <- relative_to %>% mutate(avg_cut = ifelse(avg_cut == 0, 1, avg_cut))
  for ( i in seq(1,length(dataframes)) ) {
    dataframes[[i]] <-  dataframes[[i]][ dataframes[[i]]$timeout == F,]
    dataframes[[i]] <- droplevels(dataframes[[i]])
    dataframes[[i]] <- dataframes[[i]] %>% mutate(avg_km1 = ifelse(avg_km1 == 0, 1, avg_km1))
    dataframes[[i]] <- dataframes[[i]] %>% mutate(avg_cut = ifelse(avg_cut == 0, 1, avg_cut))
  }
  
  # restrict benchmark set to all instances for which we currently have results
  semi_join_filter = semi_join(relative_to, dataframes[[1]], by=c('graph','k'))
  if ( length(dataframes) > 1  ) {
    for ( i in seq(2,length(dataframes)) ) {
      semi_join_filter = semi_join(semi_join_filter, dataframes[[i]], by=c('graph','k'))
    } 
  }
  
  # apply the semi_join_filter to all data frames
  relative_to = semi_join(relative_to, semi_join_filter, by=c('graph','k'))
  relative_to = relative_to[with(relative_to, order(graph,k)), ]
  for ( i in seq(1,length(dataframes)) ) {
    dataframes[[i]] = semi_join(dataframes[[i]], semi_join_filter, by=c('graph','k'))
    dataframes[[i]] = dataframes[[i]][with(dataframes[[i]], order(graph,k)), ]
  }
  
  # Compute relative running time and quality
  dataframes[[1]]$rel_time <- relative_to$avg_time / dataframes[[1]]$avg_time
  dataframes[[1]]$rel_obj <- relative_to[[objective]] / dataframes[[1]][[objective]] - 1
  result <- dataframes[[1]]
  if ( length(dataframes) > 1  ) {
    for ( i in seq(2,length(dataframes)) ) {
      dataframes[[i]]$rel_time <- relative_to$avg_time / dataframes[[i]]$avg_time
      dataframes[[i]]$rel_obj <- relative_to[[objective]] / dataframes[[i]][[objective]] - 1
      result <- rbind(result, dataframes[[i]])
    } 
  }
  
  if ( !is.null(order) ) {
    result$algorithm <- factor(result$algorithm, levels = order)
  }
  
  scaling <- 1000
  relative_algo_name <- relative_to$algorithm[1]
  result$rel_obj <- result$rel_obj * scaling
  result <- result %>% mutate(rel_obj = ifelse(infeasible == T, -10 * scaling, rel_obj))
  y_breaks <- c(-10 * scaling, -1 * scaling, -0.1 * scaling, -0.01 * scaling, 0, 0.01 * scaling, 0.1 * scaling, 1 * scaling )
  y_labels <- c(ifelse(latex_export, "\\ding{55}", "imb"), "-1", "-0.1", "-0.01", "0", "0.01", "0.1", "1")
  contains_imbalanced_results <- any(result$infeasible)
  tradeoff_plot = ggplot(result, aes(x=rel_time, y=rel_obj, color=algorithm)) +
    geom_point(size = point_size_scaling * 0.5 * plot_point_size(latex_export), alpha = 0.25) +
    scale_x_continuous(trans = "log10", breaks = c(0.01, 1, 100, 10000),
                       labels = pow_text(10, seq(from = -2, to = 4, by = 2), latex_export)) +
    scale_y_continuous(trans = pseudo_log_trans(base = 10), breaks = y_breaks, labels = y_labels) + 
    facet_wrap(~algorithm, ncol = ncol) +
    geom_hline(yintercept = 0, size = 0.5 * plot_line_size(latex_export)) +
    geom_vline(xintercept = 1, size = 0.5 * plot_line_size(latex_export))
  if ( contains_imbalanced_results  ) {
    tradeoff_plot <- tradeoff_plot + geom_hline(yintercept = -3.33 * scaling, size = 0.25 * plot_line_size(latex_export))
  }
  tradeoff_plot <- tradeoff_plot + theme_bw(base_size = 10) +
    scale_color_manual(values=algo_color_mapping, drop = F) +
    scale_fill_manual(values=algo_color_mapping, drop = F) +
    labs(y=paste("Improvement over", relative_algo_name, sep = " "), 
         x=paste("Speedup over", relative_algo_name, sep=" ")) +
    create_theme(latex_export, small_size, x_axis_text_hjust = 1) +
    guides(colour = guide_legend(title=NULL, ncol = legend_col, byrow = F,keyheight = .5, 
                                 override.aes = list(size = 4 * plot_point_size(latex_export), alpha = 1), title.hjust=0))
  return(tradeoff_plot)    
}