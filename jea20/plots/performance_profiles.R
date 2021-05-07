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
                            show_legend = TRUE,
                            latex_export = F,
                            small_size = F) {
  
  # Compute Performance Profile Plots
  worst_ratio <- computeWorstRatioForAllFiltered(dataframes, objective = objective)
  performance_profile = computePerformanceProfile(dataframes, objective,  
                                                  worst_ratio = worst_ratio)
  plot_km1_quality <-  performanceProfilePlot(performance_profile,
                                              worst_ratio = worst_ratio,
                                              hide_y_axis_title = hide_y_axis_title,
                                              show_infeasible_tick = show_infeasible_tick,
                                              show_timeout_tick = show_timeout_tick,
                                              widths = widths,
                                              latex_export = latex_export,
                                              small_size = small_size) 
  
  # Combine Performance Profile Plots
  if (show_infeasible_tick | show_timeout_tick) {
    combined_plot <- egg::ggarrange(plot_km1_quality[[1]]+theme(plot.margin =  margin(5.5, -2.9, 0, 5.5)),
                                    plot_km1_quality[[2]]+theme(legend.position = "none", plot.margin =  margin(5.5, -2.9, 0, 0)),
                                    plot_km1_quality[[3]]+theme(plot.margin =  margin(5.5, -2.9, 0, 0)),
                                    plot_km1_quality[[4]]+theme(plot.margin =  margin(5.5, 0, 0, 0)),
                                    widths = widths, ncol = 4, draw=F)
  } else {
    combined_plot <- egg::ggarrange(plot_km1_quality[[1]]+theme(plot.margin =  margin(5.5, -2.9, 0, 5.5)),
                                    plot_km1_quality[[2]]+theme(legend.position = "none", plot.margin =  margin(5.5, -2.9, 0, 0)),
                                    plot_km1_quality[[3]]+theme(plot.margin =  margin(5.5, 0, 0, 0)),
                                    widths = widths[1:3], ncol = 3, draw=F)  
  }
  
  combined_plot <- annotate_figure(combined_plot, 
                                   bottom = text_grob("Quality relative to best", 
                                                      vjust = -1.5, size = axis_title_size(latex_export, small_size)))
  if ( !is.null(title) ) {
    combined_plot <- annotate_figure(combined_plot, 
                                     top = text_grob(title, vjust = 1.5, size = plot_title_size(latex_export)))
  }
  
  
  # Add Legend
  leg <- ggpubr::get_legend(plot_km1_quality[[2]])
  if ( show_legend ) {
    combined_plot <- plot_grid(combined_plot, 
                               ggpubr::as_ggplot(leg) + theme(plot.margin =  margin(legend_top_margin,0,0,0)), 
                               ncol = 1, rel_heights = c(5,1))
  }
  return(combined_plot)
}

performace_plot_legend <- function(dataframes, 
                                   legend_col = NULL,
                                   latex_export = F,
                                   small_size = F) {
  
  # Compute Performance Profile Plots
  worst_ratio <- computeWorstRatioForAllFiltered(dataframes, objective = "avg_km1", effectivenes_test = effectivenes_test)
  performance_profile = computePerformanceProfile(dataframes, "avg_km1",  
                                                  effectivenes_test = effectivenes_test,
                                                  worst_ratio = worst_ratio)
  plot_km1_quality <-  performanceProfilePlot(performance_profile,
                                              worst_ratio = worst_ratio,
                                              hide_y_axis_title = FALSE,
                                              show_infeasible_tick = TRUE,
                                              show_timeout_tick = TRUE,
                                              widths = c(3,2,1,1),
                                              legend_col = legend_col,
                                              latex_export = latex_export,
                                              small_size = small_size) 
  
  leg <- ggpubr::get_legend(plot_km1_quality[[2]])
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
                                  latex_export = F,
                                  small_size=F) {
  if ( is.null(legend_col) ) {
    legend_col <- 4
    if ( latex_export ) {
      legend_col <- 2
    } 
  }
  
  original_aspect_ratio <- 1.236068 / 33.0
  
  # First Plot from tau = 1 to tau = 1.1
  if( small_size ) {
    y_scale = scale_y_continuous(breaks=c(0.01,0.2,0.4,0.6,0.8,1.0))
    x_breaks = c(1.0,1.05,1.1)
    x_labels = c(1.0,1.05,"")
  } else {
    y_scale = scale_y_continuous(breaks=c(0.01,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0))
    x_breaks = c(1.0,1.05,1.1)
    x_labels = c(1.0,1.05,"")
  }
  
  a = ggplot(profile_plots[profile_plots$tau %<=% 1.1,], aes(x=tau, y=rho, color=algorithm)) + 
    scale_color_manual(values=algo_color_mapping, drop = F) +
    scale_x_continuous(breaks=x_breaks,labels=x_labels,expand = c(0, 0), limits=c(0.999,1.1)) +
    annotate("segment", x=c(1.01,1.02,1.03,1.04,1.05,1.06,1.07,1.08,1.09), y=-0.065, xend=c(1.01,1.02,1.03,1.04,1.05,1.06,1.07,1.08,1.09), yend=-0.05, size=.5,
             col="black") +
    geom_vline(aes(xintercept=1.01), colour="grey", linetype="11", size=.5) +
    geom_vline(aes(xintercept=1.02), colour="grey", linetype="11", size=.5) +
    geom_vline(aes(xintercept=1.03), colour="grey", linetype="11", size=.5) +
    geom_vline(aes(xintercept=1.04), colour="grey", linetype="11", size=.5) +
    geom_vline(aes(xintercept=1.05), colour="grey", linetype="11", size=.5) +
    geom_vline(aes(xintercept=1.06), colour="grey", linetype="11", size=.5) +
    geom_vline(aes(xintercept=1.07), colour="grey", linetype="11", size=.5) +
    geom_vline(aes(xintercept=1.08), colour="grey", linetype="11", size=.5) +
    geom_vline(aes(xintercept=1.09), colour="grey", linetype="11", size=.5) +
    coord_cartesian(ylim = c(0, 1), clip="off") +
    y_scale +
    geom_line(aes(color=algorithm),size=2*plot_line_size(latex_export)) +
    theme_bw(base_size = 10) +
    expand_limits(y=c(0.0,1.0)) +
    labs(x="", y="Fraction of instances") +
    create_theme(latex_export, small_size, aspect_ratio = 2.15/ ( original_aspect_ratio * ( (widths[[1]] / sum(widths)) * 100.0 )  ),  legend_position = "none", panel_grid_size = 0.5) +
    theme(axis.title.x = element_blank())
  
  if(hide_y_axis_title) {
    a=  a +  theme(axis.title.y = element_blank(),
                   axis.text.y = element_blank())
  }
  
  # Second Plot from tau = 1.1 to tau = 2
  b = ggplot(profile_plots[profile_plots$tau %<=% 2.0 & profile_plots$tau %>=% 1.1,], aes(x=tau, y=rho, color=algorithm)) +
    scale_color_manual(values=algo_color_mapping, drop = F) +
    scale_x_continuous(breaks=c(1.1,1.5,2.0),expand = c(0, 0), limits=c(1.1,2.0),labels=c(1.1,1.5,2.0)) +
    annotate("segment", x=c(1.25, 1.5, 1.75), y=-0.065, xend=c(1.25,1.5,1.75), yend=-0.05, size=.5,
             col="black") +
    geom_blank(data = profile_plots, aes(color=algorithm)) +
    geom_vline(aes(xintercept=1.25), colour="grey", linetype="11", size=.5) +
    geom_vline(aes(xintercept=1.5), colour="grey", linetype="11", size=.5) +
    geom_vline(aes(xintercept=1.75), colour="grey", linetype="11", size=.5) +
    y_scale +
    geom_line(aes(color=algorithm),size=2*plot_line_size(latex_export)) +
    theme_bw(base_size = 10) +
    expand_limits(y=c(0.0,1.0)) +
    coord_cartesian(ylim = c(0, 1), clip="off") +
    labs(x="", y="") +
    create_theme(latex_export, small_size, aspect_ratio = 2.15/ ( original_aspect_ratio * ( (widths[[2]] / sum(widths)) * 100.0 )  ),  legend_position = "bottom", panel_grid_size = 0.5) +
    theme(axis.text.y = element_blank(),
          axis.title.y = element_blank())
  if (small_size) {
    b = b +  guides(colour = guide_legend(title=NULL, ncol = legend_col, byrow = F,keyheight = .5, keywidth = .75, title.hjust=0),linetype = guide_legend(title=NULL))
  } else {
    b = b +  guides(colour = guide_legend(title=NULL, ncol = legend_col, byrow = F,keyheight = .5, title.hjust=0), linetype = guide_legend(title=NULL))
  }
  
  
  # Third Plot from tau = 2 to worst_ratio
  base_10 <- max(floor(log10(worst_ratio)),2)
  x_breaks <- c(2,10,10^base_10)
  x_labels <- c("",pow_text(10,1,latex_export),pow_text(10,base_10,latex_export))
  x_limits <- c(2,max(worst_ratio,10^base_10))
  if ( !show_infeasible_tick & !show_timeout_tick ) {
    if ( base_10 > 2 ) {
      x_limits <- c(2,max(worst_ratio,10^(base_10 + 3)))
    } else {
      x_limits <- c(2,max(worst_ratio,10^(base_10 + 0.75)))
    }
  }
  c = ggplot(profile_plots[profile_plots$tau %>=% 2.0 & profile_plots$tau %<=% max(worst_ratio,100),], aes(x=tau, y=rho, color=algorithm))  +
    geom_line(aes(color=algorithm),size=2*plot_line_size(latex_export)) +
    scale_color_manual(values=algo_color_mapping, drop = F) +
    scale_x_continuous(trans="lam",breaks=x_breaks,labels=x_labels,expand = c(0, 0),limits=x_limits) +
    y_scale +
    expand_limits(y=c(0.0,1.0)) +
    coord_cartesian(ylim = c(0, 1), clip="off") +
    theme_bw(base_size = 10) +
    labs(x="", y="ddd") +
    create_theme(latex_export, small_size, aspect_ratio = 2.15/ ( original_aspect_ratio * ( (widths[[3]] / sum(widths)) * 100.0 )  ),  legend_position = "none", panel_grid_size = 0.5) +
    theme(axis.text.y = element_blank(),
          axis.title.y = element_blank())

  if ( show_infeasible_tick | show_timeout_tick  ) {
    # Fourth Plot showing infeasible and timeout instances
    infeasible_value <- max(worst_ratio,100) + 1
    timeout_value <- max(worst_ratio,100) + 2
    y_breaks <- c(max(worst_ratio,100))
    y_labels <- c("")
    x_min <- ifelse(show_infeasible_tick, infeasible_value - 1, infeasible_value)
    x_max <- ifelse(show_timeout_tick, timeout_value + 1, infeasible_value + 1)
    y_breaks <- add_infeasible_break(y_breaks, infeasible_value, show_infeasible_tick, latex_export)
    y_labels <- add_infeasible_label(y_labels, infeasible_value, show_infeasible_tick, latex_export)
    y_breaks <- add_timeout_break(y_breaks, timeout_value, show_timeout_tick, latex_export)
    y_labels <- add_timeout_label(y_labels, timeout_value, show_timeout_tick, latex_export)
    
    d = ggplot(profile_plots[profile_plots$tau %>=% x_min & profile_plots$tau %<=% x_max,], aes(x=tau, y=rho, color=algorithm))  +
      geom_line(aes(color=algorithm),size=2*plot_line_size(latex_export)) +
      scale_color_manual(values=algo_color_mapping, drop = F) +
      scale_x_continuous(breaks=y_breaks,labels=y_labels,expand = c(0, 0),limits=c(x_min,x_max)) +
      y_scale +
      expand_limits(y=c(0.0,1.0)) +
      coord_cartesian(ylim = c(0, 1), clip="off") +
      theme_bw(base_size = 10) +
      labs(x="", y="ddd")
    if ( latex_export ) {
      d <- d + create_theme(latex_export, small_size, aspect_ratio = 2.15/ ( original_aspect_ratio * ( (widths[[4]] / sum(widths)) * 100.0 )  ),  legend_position = "none", panel_grid_size = 0.5)
    } else {
      d <- d + create_theme(latex_export, small_size, aspect_ratio = 2.15/ ( original_aspect_ratio * ( (widths[[4]] / sum(widths)) * 100.0 )  ),  
                            legend_position = "none", panel_grid_size = 0.5, x_axis_text_angle = 30, x_axis_text_hjust = 1)
    }
    d <- d + theme(axis.text.y = element_blank(),
                   axis.title.y = element_blank()) 
    return(list(a,b,c,d))
  } else {
    return(list(a,b,c))
  }
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

