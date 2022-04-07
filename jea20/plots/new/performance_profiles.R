lam_trans = function() trans_new('lam',
                                 transform = function(x) -1 * x ^ -0.25,
                                 inverse = function(x) 1/(x^4)) 

performace_plot <- function(dataframes, 
                            objective = "avg_km1", 
                            effectivenes_test = F, 
                            hide_y_axis_title = F,
                            show_infeasible_tick = T,
                            show_timeout_tick = T,
                            widths = c(4,2,1,1),
                            title = NULL,
                            legend_top_margin = 0,
                            legend_col = 2,
                            show_legend = TRUE,
                            small_legend = F,
                            small_ticks = F,
                            latex_export = F,
                            small_size = F) {
  
  # Compute Performance Profile Plots
  worst_ratio <- computeWorstRatioForAllFiltered(dataframes, objective = objective, effectivenes_test = effectivenes_test)
  performance_profile = computePerformanceProfile(dataframes, objective,  
                                                  worst_ratio = worst_ratio,
                                                  effectivenes_test = effectivenes_test)
  plot_km1_quality <-  performanceProfilePlot(performance_profile,
                                              worst_ratio = worst_ratio,
                                              hide_y_axis_title = hide_y_axis_title,
                                              show_infeasible_tick = show_infeasible_tick,
                                              show_timeout_tick = show_timeout_tick,
                                              widths = widths,
                                              title = title,
                                              legend_col = legend_col,
                                              show_legend = show_legend,
                                              small_legend = small_legend,
                                              small_ticks = small_ticks,
                                              latex_export = latex_export,
                                              small_size = small_size) 
  return(plot_km1_quality)
}

performace_plot_legend <- function(dataframes, 
                                   legend_col = NULL,
                                   latex_export = F,
                                   small_size = F) {
  
  # Compute Performance Profile Plots
  worst_ratio <- computeWorstRatioForAllFiltered(dataframes, objective = "avg_km1", effectivenes_test = F)
  performance_profile = computePerformanceProfile(dataframes, "avg_km1",  
                                                  effectivenes_test = F,
                                                  worst_ratio = worst_ratio)
  plot_km1_quality <-  performanceProfilePlot(performance_profile,
                                              worst_ratio = worst_ratio,
                                              hide_y_axis_title = FALSE,
                                              show_infeasible_tick = TRUE,
                                              show_timeout_tick = TRUE,
                                              widths = c(3,2,1,1),
                                              legend_col = legend_col,
                                              show_legend = T,
                                              latex_export = latex_export,
                                              small_size = small_size) 
  
  leg <- ggpubr::get_legend(plot_km1_quality)
  return(as_ggplot(leg))
}

performanceProfilePlot = function(profile_plots,
                                  hide_y_axis = F,
                                  hide_y_axis_title = F,
                                  show_infeasible_tick = T,
                                  show_timeout_tick = T,
                                  worst_ratio = 999999999,
                                  widths = c(4,2,1,1),
                                  legend_col = NULL,
                                  title = NULL,
                                  show_legend = T,
                                  small_legend = F,
                                  small_ticks = F,
                                  latex_export = F,
                                  small_size=F) {
  if ( ( show_infeasible_tick | show_timeout_tick ) & length(widths) < 4  ) {
    widths <- c(widths,1)
  }
  
  profile_plots$plotted_tau <- as.numeric(lapply(profile_plots$tau, FUN = plotted_tau, worst_ratio, widths))
  
  if ( is.null(legend_col) ) {
    legend_col <- 4
    if ( latex_export ) {
      legend_col <- 2
    } 
  }
  
  if( small_size ) {
    y_breaks = c(0.01,0.2,0.4,0.6,0.8,1.0)
  } else {
    y_breaks = c(0.01,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)
  }
  x_breaks = c(1.0,1.05,1.1,1.5,2)
  x_labels = c(1.0,1.05,1.1,1.5,2)
  
  base_10 <- max(floor(log10(worst_ratio)),2)
  x_breaks <- c(x_breaks, 10, 10^base_10)
  x_labels <- c(x_labels, pow_text(10,1,latex_export),pow_text(10,base_10,latex_export))
  x_limit <-  sum(widths) + 0.25
  if ( show_infeasible_tick | show_timeout_tick  ) {
    infeasible_value <- max(worst_ratio,100) + 1
    timeout_value <- max(worst_ratio,100) + 2
    reduce_limit <- F
    if ( !show_infeasible_tick ) {
      timeout_value <- infeasible_value
      profile_plots <- profile_plots %>% mutate(plotted_tau = 
                                                  ifelse(tau > max(worst_ratio,100), plotted_tau(infeasible_value, worst_ratio, widths), plotted_tau))
      reduce_limit <- T
    }
    if ( reduce_limit | !show_timeout_tick ) {
      x_limit <-  plotted_tau(infeasible_value + 1, worst_ratio, widths)
    }
    
    x_min <- ifelse(show_infeasible_tick, infeasible_value - 1, infeasible_value)
    x_max <- ifelse(show_timeout_tick, timeout_value + 1, infeasible_value + 1)
    x_breaks <- add_infeasible_break(x_breaks, infeasible_value, show_infeasible_tick, latex_export)
    x_labels <- add_infeasible_label(x_labels, infeasible_value, show_infeasible_tick, latex_export)
    x_breaks <- add_timeout_break(x_breaks, timeout_value, show_timeout_tick, latex_export)
    x_labels <- add_timeout_label(x_labels, timeout_value, show_timeout_tick, latex_export)
  } 
  
  p = ggplot(profile_plots, aes(x=plotted_tau, y=rho, color=algorithm)) +
    scale_color_manual(values=algo_color_mapping, drop = F) +
    scale_y_continuous(breaks = y_breaks) +
    scale_x_continuous(breaks = as.numeric(lapply(x_breaks, FUN = plotted_tau, worst_ratio, widths)), labels = x_labels,
                       expand = c(0, 0), limits=c(-0.02,x_limit))
  
  x_ticks <- c(1.01,1.02,1.03,1.04,1.06,1.07,1.08,1.09,1.25,1.75)
  x_ticks <- as.numeric(lapply(x_ticks, FUN = plotted_tau, worst_ratio, widths))
  p = p +     
    annotate("segment", x=x_ticks, y=-0.065, xend=x_ticks, yend=-0.05, size=.5, col="black") +
    geom_vline(aes(xintercept=plotted_tau(1.01, worst_ratio, widths)), colour="grey", linetype="11", size=.5) +
    geom_vline(aes(xintercept=plotted_tau(1.02, worst_ratio, widths)), colour="grey", linetype="11", size=.5) +
    geom_vline(aes(xintercept=plotted_tau(1.03, worst_ratio, widths)), colour="grey", linetype="11", size=.5) +
    geom_vline(aes(xintercept=plotted_tau(1.04, worst_ratio, widths)), colour="grey", linetype="11", size=.5) +
    geom_vline(aes(xintercept=plotted_tau(1.06, worst_ratio, widths)), colour="grey", linetype="11", size=.5) +
    geom_vline(aes(xintercept=plotted_tau(1.07, worst_ratio, widths)), colour="grey", linetype="11", size=.5) +
    geom_vline(aes(xintercept=plotted_tau(1.08, worst_ratio, widths)), colour="grey", linetype="11", size=.5) +
    geom_vline(aes(xintercept=plotted_tau(1.09, worst_ratio, widths)), colour="grey", linetype="11", size=.5) +
    geom_vline(aes(xintercept=plotted_tau(1.25, worst_ratio, widths)), colour="grey", linetype="11", size=.5) +
    geom_vline(aes(xintercept=plotted_tau(1.75, worst_ratio, widths)), colour="grey", linetype="11", size=.5)
  
  
  p = p + geom_blank(data = profile_plots, aes(color=algorithm)) +
    geom_line(aes(color=algorithm),size=2*plot_line_size(latex_export)) +
    theme_bw(base_size = 10) +
    expand_limits(y=c(0.0,1.0)) +
    coord_cartesian(ylim = c(0, 1), clip="off") +
    labs(x="Quality relative to best", y="Fraction of Instances") +
    ggtitle(title) +
    create_theme(latex_export, small_ticks, small_size,  legend_position = "bottom", panel_grid_size = 0.5)
  
  p = p + geom_vline(aes(xintercept=plotted_tau(1.1, worst_ratio, widths)), colour="black", size=.5) +
    geom_vline(aes(xintercept=plotted_tau(2, worst_ratio, widths)), colour="black", size=.5) 
  if ( show_infeasible_tick | show_timeout_tick ) {
    p = p + geom_vline(aes(xintercept=plotted_tau(10^base_10, worst_ratio, widths)), colour="black", size=.5) 
  }
  
  if(hide_y_axis_title) {
    p =  p  +  theme(axis.title.y = element_blank(),
                     axis.text.y = element_blank())
  }
  
  if (!show_legend) {
    p = p + theme(legend.position = "none")
  } else {
    p = p + theme(legend.text = element_text(size = legend_text_size(latex_export, small_size | small_legend)))
  }
  
  if (small_size) {
    p = p +  guides(colour = guide_legend(title=NULL, ncol = legend_col, byrow = F,keyheight = .5, keywidth = .75, title.hjust=0),linetype = guide_legend(title=NULL))
  } else {
    p = p +  guides(colour = guide_legend(title=NULL, ncol = legend_col, byrow = F,keyheight = .5, title.hjust=0), linetype = guide_legend(title=NULL))
  }
  
  return(p)
}

computeWorstRatioForAllFiltered = function(dataframes, objective,
                                           effectivenes_test = F){
  for(i in 1:length(dataframes)) {
    # account for zero values
    dataframes[[i]] = dataframes[[i]] %>% mutate(!!(rlang::sym(objective)) := ifelse(!!(rlang::sym(objective)) %==% 0, 1, !!(rlang::sym(objective))))
    # correct sort order
    if ( effectivenes_test ) {
      dataframes[[i]] = dataframes[[i]][with(dataframes[[i]], order(graph,k,instance)), ]
    } else {
      dataframes[[i]] = dataframes[[i]][with(dataframes[[i]], order(graph,k)), ]
    }
    dataframes[[i]]$ratio = -1
  }
  
  #sanity checks
  for(i in 2:length(dataframes)) {
    if ( effectivenes_test ) {
      stopifnot(dataframes[[1]]$graph == dataframes[[i]]$graph, 
                dataframes[[1]]$k == dataframes[[i]]$k, 
                dataframes[[1]]$instance == dataframes[[i]]$instance)
    } else {
      stopifnot(dataframes[[1]]$graph == dataframes[[i]]$graph, 
                dataframes[[1]]$k == dataframes[[i]]$k)
    }
  }
  
  worst_ratio = 0
  # find minima
  global_mins_min = dataframes[[1]][[objective]]
  for(i in 2:length(dataframes)) {
    global_mins_min = pmin(global_mins_min, dataframes[[i]][[objective]])
  }
  
  # compute ratios
  for(i in 1:length(dataframes)) {
    dataframes[[i]]$ratio = dataframes[[i]][[objective]] / global_mins_min
  }
  for(i in 1:length(dataframes)) {
    worst_ratio = max(c(worst_ratio,dataframes[[i]][dataframes[[i]]$ratio < Inf,]$ratio),na.rm = T)
  }
  
  return(worst_ratio)
}

computePerformanceProfile = function(dataframes, 
                                     objective,
                                     effectivenes_test = F,
                                     worst_ratio){
  for(i in 1:length(dataframes)) {
    # Account for zero values
    dataframes[[i]] = dataframes[[i]] %>% mutate(!!(rlang::sym(objective)) := ifelse(!!(rlang::sym(objective)) %==% 0, 1, !!(rlang::sym(objective))))
    # Correct sort order
    if ( effectivenes_test ) {
      dataframes[[i]] = dataframes[[i]][with(dataframes[[i]], order(graph,k,instance)), ]
    } else {
      dataframes[[i]] = dataframes[[i]][with(dataframes[[i]], order(graph,k)), ]
    }
  }
  
  #Sanity checks
  for(i in 2:length(dataframes)) {
    if ( effectivenes_test ) {
      stopifnot(dataframes[[1]]$graph == dataframes[[i]]$graph, 
                dataframes[[1]]$k == dataframes[[i]]$k, 
                dataframes[[1]]$instance == dataframes[[i]]$instance)
    } else {
      stopifnot(dataframes[[1]]$graph == dataframes[[i]]$graph, 
                dataframes[[1]]$k == dataframes[[i]]$k)
    }
  }
  
  # Find minima
  global_mins_min = dataframes[[1]][[objective]]
  for(i in 2:length(dataframes)) {
    global_mins_min = pmin(global_mins_min, dataframes[[i]][[objective]])
  }
  
  # Compute ratios
  for(i in 1:length(dataframes)) {
    dataframes[[i]]$ratio =  dataframes[[i]][[objective]] / global_mins_min
  }
  
  # Set a large infeasible ratio for infeasible solutions
  infeasible_value = max(worst_ratio,100) + 1
  timeout_value = max(worst_ratio,100) + 2
  for(i in 1:length(dataframes)) {
    dataframes[[i]] = dataframes[[i]] %>% mutate(ratio = ifelse(timeout == TRUE, timeout_value, ratio))
    dataframes[[i]] = dataframes[[i]] %>% mutate(ratio = ifelse(infeasible == TRUE, infeasible_value, ratio))
  }
  
  # The result
  result = data.frame(algorithm= character(0), n =integer(0), rho = numeric(0), tau= numeric(0))
  
  # First plot frame 1.0 - 1.1 in 0.01 steps
  first_window_seq = seq(from=1, to=1.1, by=0.001)
  for(i in 1:length(dataframes)) {
    for (tau in first_window_seq){
      temp_result = (dataframes[[i]] %>% tally(as.numeric(ratio) %<=% as.numeric(tau)))
      temp_result$rho = as.numeric(temp_result$n) / nrow(dataframes[[i]])
      temp_result$tau = tau
      temp_result$algorithm = as.factor(unique(dataframes[[i]]$algorithm))
      result = rbind(result,temp_result)
    }
  }
  
  # Second plot frame 1.1 - 2.0 in 0.1 steps
  second_window_seq = seq(from=1.1, to=2.0, by=0.01)
  for(i in 1:length(dataframes)) {
    for (tau in second_window_seq){
      temp_result = (dataframes[[i]] %>% tally(as.numeric(ratio) %<=% as.numeric(tau)))
      temp_result$rho = as.numeric(temp_result$n) / nrow(dataframes[[i]])
      temp_result$tau = tau
      temp_result$algorithm = as.factor(unique(dataframes[[i]]$algorithm))
      result = rbind(result,temp_result)
    }
  }
  
  # Third plot frame part 1
  third_window_seq = seq(from=2.0, to=max(100,min(worst_ratio,500)), by=1)
  for(i in 1:length(dataframes)) {
    for (tau in third_window_seq){
      temp_result = (dataframes[[i]] %>% tally(as.numeric(ratio) %<=% as.numeric(tau)))
      temp_result$rho = as.numeric(temp_result$n) / nrow(dataframes[[i]])
      temp_result$tau = tau
      temp_result$algorithm = as.factor(unique(dataframes[[i]]$algorithm))
      result = rbind(result,temp_result)
    }
  }
  
  # Fourth plot frame part 2
  if ( worst_ratio > 500  ) {
    fourth_window_seq = seq(from=min(worst_ratio,500), to=worst_ratio, by=100)
    df = foreach (i = 1:length(dataframes),.combine=rbind) %dopar% {
      tmp = data.frame(algorithm= character(0), n =integer(0), rho = numeric(0), tau= numeric(0))
      for (tau in fourth_window_seq){
        temp_result = (dataframes[[i]] %>% tally(as.numeric(ratio) %<=% as.numeric(tau)))
        temp_result$rho = as.numeric(temp_result$n) / nrow(dataframes[[i]])
        temp_result$tau = tau
        temp_result$algorithm = as.factor(unique(dataframes[[i]]$algorithm))
        tmp = rbind(tmp,temp_result)
      }
      return(tmp)
    }  
  }
  
  # Fifth plot frame part 2
  fifth_window_seq = seq(from=max(100,worst_ratio), to=timeout_value+1, by=0.01)
  df = foreach (i = 1:length(dataframes),.combine=rbind) %dopar% {
    tmp = data.frame(algorithm= character(0), n =integer(0), rho = numeric(0), tau= numeric(0))
    for (tau in fifth_window_seq){
      temp_result = (dataframes[[i]] %>% tally(as.numeric(ratio) %<=% as.numeric(tau)))
      temp_result$rho = as.numeric(temp_result$n) / nrow(dataframes[[i]])
      temp_result$tau = tau
      temp_result$algorithm = as.factor(unique(dataframes[[i]]$algorithm))
      tmp = rbind(tmp,temp_result)
    }
    return(tmp)
  }  
  
  result = rbind(df, result)
  
  # have the ones only once
  ones = result[result$rho %==% 1.0,]
  ones = ones[with(ones, order(tau)), ]
  ones = ones[!duplicated(ones[c("algorithm","rho")]),]
  
  result = result[result$rho %!=% 1.0,]
  result = rbind(result, ones)
  return(result)
} 

plotted_tau <- function(tau, worst_ratio, widths) {
  worst_ratio <- max(100, worst_ratio)
  if ( tau <= 1.1 ) {
    return( widths[1] * ( (tau - 1) / 0.1) )
  } else if ( tau > 1.1 & tau <= 2 ) {
    return(widths[1] + widths[2] * ( ( tau - 1.1 ) / 0.9 ) )
  } else if ( tau > 2 & tau <= worst_ratio ) {
    return(sum(widths[1:2]) + widths[3] * ( log10( 1 + tau - 2 ) / log10(1 + worst_ratio - 2) ) )
  } else if (length(widths) == 4) {
    return(sum(widths[1:3]) + widths[4] * ( ( tau - worst_ratio ) / 2 ) )
  } else {
    return(sum(widths[1:3]) + ( ( tau - worst_ratio ) / 2 ) )
  }
}



