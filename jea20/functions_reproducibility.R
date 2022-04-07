library(ggplot2)
library(scales)
library(plyr)
library(dplyr)
library(RColorBrewer)
library(fpCompare)
library(doParallel)
library(reshape)
library(pracma)
library(gridExtra)

graphclass = function(row) {
  if(grepl("*dual*", row['graph'])){
    return("Dual")
  } else if (grepl("*primal*", row['graph'])) {
    return("Primal")
  } else if (grepl("sat14*", row['graph'])) {
    return("Literal")
  } else if (grepl("*mtx*", row['graph'])) {
    return("SPM")
  }  else if (grepl("*ISPD98*", row['graph'])) {
    return("ISPD")
  } else {
    return("DAC")
  }
}

csv_aggreg = function(df) data.frame(min_km1 = min(df$km1, na.rm=TRUE),
                                     avg_km1 = mean(df$km1, na.rm=TRUE),
                                     min_cut = min(df$cut, na.rm=TRUE),
                                     avg_cut = mean(df$cut, na.rm=TRUE),
                                     min_imbalance = min(df$imbalance, na.rm=TRUE),
                                     avg_imbalance = mean(df$imbalance, na.rm=TRUE),
                                     min_total_time = min(as.numeric(df$totalPartitionTime), na.rm=TRUE),
                                     avg_total_time = mean(as.numeric(df$totalPartitionTime), na.rm=TRUE),
                                     avg_time = mean(as.numeric(df$totalPartitionTime), na.rm=TRUE),
                                     timeout = all(as.logical(df$timeout)),
                                     failed = all(as.logical(df$failed)))

csv_speed_up_aggreg = function(df) data.frame(min_km1 = min(df$km1, na.rm=TRUE),
                                     avg_km1 = mean(df$km1, na.rm=TRUE),
                                     min_imbalance = min(df$imbalance, na.rm=TRUE),
                                     avg_imbalance = mean(df$imbalance, na.rm=TRUE),
                                     min_total_time = min(as.numeric(df$totalPartitionTime), na.rm=TRUE),
                                     avg_total_time = mean(as.numeric(df$totalPartitionTime), na.rm=TRUE),
                                     avg_time = mean(as.numeric(df$totalPartitionTime), na.rm=TRUE),
                                     min_preprocessing_time = min(as.numeric(df$preprocessing_time), na.rm=TRUE),
                                     avg_preprocessing_time = mean(as.numeric(df$preprocessing_time), na.rm=TRUE),
                                     min_coarsening_time = min(as.numeric(df$coarsening_time), na.rm=TRUE),
                                     avg_coarsening_time = mean(as.numeric(df$coarsening_time), na.rm=TRUE),
                                     min_initial_partitioning_time = min(as.numeric(df$initial_partitioning_time), na.rm=TRUE),
                                     avg_initial_partitioning_time = mean(as.numeric(df$initial_partitioning_time), na.rm=TRUE),
                                     min_batch_uncontraction_time = min(as.numeric(df$batch_uncontraction_time), na.rm=TRUE),
                                     avg_batch_uncontraction_time = mean(as.numeric(df$batch_uncontraction_time), na.rm=TRUE),
                                     min_label_propagation_time = min(as.numeric(df$label_propagation_time), na.rm=TRUE),
                                     avg_label_propagation_time = mean(as.numeric(df$label_propagation_time), na.rm=TRUE),
                                     min_fm_time = min(as.numeric(df$fm_time), na.rm=TRUE),
                                     avg_fm_time = mean(as.numeric(df$fm_time), na.rm=TRUE))

aggreg_data <- function(data, timelimit, epsilon) {
  # Invalidate all objectives of imbalanced solutions
  data <- data %>% mutate(cut = ifelse(imbalance > epsilon + .Machine$double.eps, NA, cut)) %>% 
    mutate(km1 = ifelse(imbalance > epsilon + .Machine$double.eps, NA, km1))
  # Invalidate and modify all results of timeout instances
  data <- data %>% mutate(timeout = ifelse(as.numeric(totalPartitionTime) >= timelimit, TRUE, FALSE)) %>%
    mutate(cut = ifelse(timeout == TRUE, NA, cut)) %>% 
    mutate(km1 = ifelse(timeout == TRUE, NA, km1)) %>% 
    mutate(imbalance = ifelse(timeout == TRUE, 1.0, imbalance)) %>% 
    mutate(totalPartitionTime = ifelse(timeout == TRUE, timelimit, totalPartitionTime)) 
  # Invalidate and modify all results of timeout instances
  if ( !"failed" %in% colnames(data) ) {
    data$failed <- "no"
  }
  data <- data %>% mutate(failed = ifelse(failed == "yes", TRUE, FALSE)) %>% 
    mutate(cut = ifelse(failed == TRUE, NA, cut)) %>% 
    mutate(km1 = ifelse(failed == TRUE, NA, km1)) %>% 
    mutate(totalPartitionTime = ifelse(failed == TRUE, timelimit, totalPartitionTime))
  if ( !"num_threads" %in% colnames(data) ) {
    data$num_threads <- 1
  }
  data <- ddply(data, c("graph", "k", "epsilon",  "num_threads"), csv_aggreg)
  data <- data %>% mutate(avg_km1 = ifelse(is.na(avg_km1), Inf, avg_km1)) %>% 
    mutate(min_km1 = ifelse(is.na(min_km1), Inf, min_km1))%>% 
    mutate(min_cut = ifelse(is.na(min_cut), Inf, min_cut))%>% 
    mutate(avg_cut = ifelse(is.na(avg_cut), Inf, avg_cut))
  data <- data %>% mutate(infeasible = ifelse(min_imbalance > epsilon + .Machine$double.eps & failed == F &
                                                ( min_imbalance != 1.0 | avg_time < timelimit ), TRUE, FALSE)) 
  data <- data %>% mutate(invalid = ifelse(failed == T | infeasible == T | timeout == T, TRUE, FALSE))
  return(data)
}

csv_speed_up_aggreg = function(df) data.frame(min_km1 = min(df$km1, na.rm=TRUE),
                                              avg_km1 = mean(df$km1, na.rm=TRUE),
                                              min_imbalance = min(df$imbalance, na.rm=TRUE),
                                              avg_imbalance = mean(df$imbalance, na.rm=TRUE),
                                              min_total_time = min(as.numeric(df$totalPartitionTime), na.rm=TRUE),
                                              avg_total_time = mean(as.numeric(df$totalPartitionTime), na.rm=TRUE),
                                              avg_time = mean(as.numeric(df$totalPartitionTime), na.rm=TRUE),
                                              min_preprocessing_time = min(as.numeric(df$preprocessing_time), na.rm=TRUE),
                                              avg_preprocessing_time = mean(as.numeric(df$preprocessing_time), na.rm=TRUE),
                                              min_coarsening_time = min(as.numeric(df$coarsening_time), na.rm=TRUE),
                                              avg_coarsening_time = mean(as.numeric(df$coarsening_time), na.rm=TRUE),
                                              min_initial_partitioning_time = min(as.numeric(df$initial_partitioning_time), na.rm=TRUE),
                                              avg_initial_partitioning_time = mean(as.numeric(df$initial_partitioning_time), na.rm=TRUE),
                                              min_batch_uncontraction_time = min(as.numeric(df$batch_uncontraction_time), na.rm=TRUE),
                                              avg_batch_uncontraction_time = mean(as.numeric(df$batch_uncontraction_time), na.rm=TRUE),
                                              min_label_propagation_time = min(as.numeric(df$label_propagation_time), na.rm=TRUE),
                                              avg_label_propagation_time = mean(as.numeric(df$label_propagation_time), na.rm=TRUE),
                                              min_fm_time = min(as.numeric(df$fm_time), na.rm=TRUE),
                                              avg_fm_time = mean(as.numeric(df$fm_time), na.rm=TRUE))

aggreg_speed_up_data <- function(data) {
  data <- ddply(data, c("graph", "k", "epsilon",  "num_threads"), csv_speed_up_aggreg)
  data$timeout <- FALSE
  data$failed <- FALSE
  data$infeasible <- FALSE
  data$invalid <- FALSE
  return(data)
}

# Computes the geometric mean of a vector
gm_mean = function(x, na.rm=TRUE, zero.propagate = FALSE){
  if(any(x < 0, na.rm = TRUE)){
    return(NaN)
  }
  if(zero.propagate){
    if(any(x == 0, na.rm = TRUE)){
      return(0)
    }
    return(exp(mean(log(x), na.rm = na.rm)))
  } else {
    return(exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x)))
  }
}

harmonic_mean = function(x) {
  return(length(x) / sum( 1.0 / x[x > 0] ))
}

removeSeed <- function(df, seed) {
  return(df[df$seed != seed,])
}

createVirtualInstance <- function(faster_algo, total_time_faster_algo, total_time_slower_algo) {
  virtual_instances_faster_algo <- data.frame()
  algo <- faster_algo
  algo <- droplevels(algo)
  current_sample <- sample_n(algo, 1)
  current_time <- current_sample$totalPartitionTime[[1]]
  while ( nrow(algo) > 0 & total_time_faster_algo + current_time <= total_time_slower_algo ) {
    virtual_instances_faster_algo <- rbind(virtual_instances_faster_algo, current_sample)
    total_time_faster_algo <- total_time_faster_algo + current_time  
    algo <- removeSeed(algo, current_sample$seed[[1]])
    current_sample <- sample_n(algo, 1)
    current_time <- current_sample$totalPartitionTime[[1]]
  }
  
  if ( nrow(algo) > 0 ) {
    last_sample_probability <- (total_time_slower_algo - total_time_faster_algo) / current_time
    rnd_num <- runif(1)
    if ( last_sample_probability >= rnd_num ) {
      virtual_instances_faster_algo <- rbind(virtual_instances_faster_algo, current_sample)
    } 
  }
  
  return(virtual_instances_faster_algo)
}

shift_legend <- function(p) {
  # ...
  # to grob
  gp <- ggplotGrob(p)
  facet.panels <- grep("^panel", gp[["layout"]][["name"]])
  empty.facet.panels <- sapply(facet.panels, function(i) "zeroGrob" %in% class(gp[["grobs"]][[i]]))
  empty.facet.panels <- facet.panels[empty.facet.panels]
  
  # establish name of empty panels
  empty.facet.panels <- gp[["layout"]][empty.facet.panels, ]
  names <- empty.facet.panels$name
  # example of names:
  #[1] "panel-3-2" "panel-3-3"
  
  # now we just need a simple call to reposition the legend
  reposition_legend(p, 'center', panel=names, plot = T)
}


tradeoff_plot <- function(dataframes,
                          relative_to,
                          objective = "avg_km1",
                          order = NULL,
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
  for ( i in seq(2,length(dataframes)) ) {
    semi_join_filter = semi_join(semi_join_filter, dataframes[[i]], by=c('graph','k'))
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
  for ( i in seq(2,length(dataframes)) ) {
    dataframes[[i]]$rel_time <- relative_to$avg_time / dataframes[[i]]$avg_time
    dataframes[[i]]$rel_obj <- relative_to[[objective]] / dataframes[[i]][[objective]] - 1
    result <- rbind(result, dataframes[[i]])
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
  tradeoff_plot = ggplot(result, aes(x=rel_time, y=rel_obj, color=algorithm)) +
    geom_point(size = 0.5 * plot_point_size(latex_export), alpha = 0.25) +
    scale_x_continuous(trans = "log10", breaks = c(0.01, 1, 100, 10000),
                       labels = pow_text(10, seq(from = -2, to = 4, by = 2), latex_export)) +
    scale_y_continuous(trans = pseudo_log_trans(base = 10), breaks = y_breaks, labels = y_labels) + 
    facet_wrap(~algorithm, ncol = ncol) +
    geom_hline(yintercept = 0, size = 0.5 * plot_line_size(latex_export)) +
    geom_hline(yintercept = -3.33 * scaling, size = 0.25 * plot_line_size(latex_export)) +
    geom_vline(xintercept = 1, size = 0.5 * plot_line_size(latex_export)) +
    theme_bw(base_size = 10) +
    scale_color_manual(values=algo_color_mapping, drop = F) +
    scale_fill_manual(values=algo_color_mapping, drop = F) +
    labs(y=paste("Improvement over", relative_algo_name, sep = " "), 
         x=paste("Speedup over", relative_algo_name, sep=" ")) +
    create_theme(latex_export, small_size, x_axis_text_hjust = 1) +
    guides(colour = guide_legend(title=NULL, ncol = legend_col, byrow = F,keyheight = .5, 
                                 override.aes = list(size = 4 * plot_point_size(latex_export), alpha = 1), title.hjust=0))
  return(tradeoff_plot)    
}



