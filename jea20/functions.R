library(purrr)
library(ggplot2)
library(scales)
library(RSQLite)
library(DBI)
library(dbConnect)
library(sqldf)
library(data.table)
library(plyr)
library(dplyr)
library(dtplyr)
library(grid)
library(magrittr)
library(RColorBrewer)
library(ggpubr)
library(stringr)
library(fpCompare)
library(doParallel)
library(doMPI)
library(doSNOW)
library(anchors)
library(reshape)
library(pracma)
library(varhandle)
library(stringi)
library(ggpubr)
library(gridExtra)
library(cowplot)

if ( !exists( "tikzDeviceLoaded" ) ) {  
  library(tikzDevice) #if not installed call install.packages("tikzDevice", repos="http://R-Forge.R-project.org")
  
  options(tikzLatexPackages = c(getOption('tikzLatexPackages')
                                , "\\usepackage[utf8]{inputenc}"
                                , "\\usepackage[T1]{fontenc}"
                                , "\\usepackage{preview} "
                                , "\\usepackage{latexsym,amsmath,amssymb,mathtools,textcomp,pifont,marvosym,eufrak}"
                                , "\\usepackage{xcolor}"
                                ,paste("\\input{/home/tobias/kahypar-jea/macros.tex}",sep="")
  )
  )
  tikzDeviceLoaded = T
}

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



effectivenessTestDataFrame <- function(num_virtual_instances,
                                       algo_1, algo_name_1, algo_2, algo_name_2,
                                       output_csv_file,
                                       timelimit = 28800) {
  registerDoParallel(cores = 8)
  virtual_instances <- foreach(instance=1:num_virtual_instances, .combine = rbind) %dopar% {
    virtual_instances <- data.frame()
    for ( graph in levels(factor(algo_1$graph)) ) {
      graph_algo_1 <- algo_1[algo_1$graph == graph,]
      graph_algo_2 <- algo_2[algo_2$graph == graph,]
      for ( k in levels(factor(algo_1$k)) ) {
        instance_algo_1 <- graph_algo_1[graph_algo_1$k == k,]
        instance_algo_2 <- graph_algo_2[graph_algo_2$k == k,]
        
        virtual_instances_algo_1 <- sample_n(instance_algo_1, 1)
        virtual_instances_algo_2 <- sample_n(instance_algo_2, 1)
        total_time_algo_1 <- virtual_instances_algo_1$totalPartitionTime[[1]]
        total_time_algo_2 <- virtual_instances_algo_2$totalPartitionTime[[1]]
        instance_algo_1 <- removeSeed(instance_algo_1, virtual_instances_algo_1$seed[[1]])
        instance_algo_2 <- removeSeed(instance_algo_2, virtual_instances_algo_2$seed[[1]])
        if ( total_time_algo_1 < total_time_algo_2 ) {
          sum_total_time_algo_1 <- sum(instance_algo_1$totalPartitionTime)
          if ( total_time_algo_1 + sum_total_time_algo_1 <= total_time_algo_2 ) {
            virtual_instances_algo_1 <- rbind(virtual_instances_algo_1, instance_algo_1)
          } else {
            virtual_instances_algo_1 <- rbind(virtual_instances_algo_1, 
                                              createVirtualInstance(instance_algo_1, total_time_algo_1, total_time_algo_2))
          }
        } else {
          sum_total_time_algo_2 <- sum(instance_algo_2$totalPartitionTime)
          if ( total_time_algo_2 + sum_total_time_algo_2 <= total_time_algo_1 ) {
            total_time_algo_2 <- total_time_algo_2 + sum_total_time_algo_2
            virtual_instances_algo_2 <- rbind(virtual_instances_algo_2, instance_algo_2)
          } else {
            virtual_instances_algo_2 <- rbind(virtual_instances_algo_2, 
                                              createVirtualInstance(instance_algo_2, total_time_algo_2, total_time_algo_1))    
          }   
        }
        virtual_instances_algo_1 <- aggreg_data(virtual_instances_algo_1, timelimit, 0.03)
        virtual_instances_algo_2 <- aggreg_data(virtual_instances_algo_2, timelimit, 0.03)
        virtual_instances_algo_1$instance <- instance
        virtual_instances_algo_2$instance <- instance
        virtual_instances_algo_1$algo <- algo_name_1
        virtual_instances_algo_2$algo <- algo_name_2
        virtual_instances <- rbind(virtual_instances,virtual_instances_algo_1, virtual_instances_algo_2)
      } 
    }
    return(virtual_instances)
  }
  
  write.csv(virtual_instances, file = output_csv_file, quote = F, row.names = F)
  return(virtual_instances)
}

effectivenessTestPerformanceProfile <- function(effectiveness_test_df,
                                                algo_name_1, algo_name_2,
                                                objective = "min_km1",
                                                hide_y_axis_title = F,
                                                show_infeasible_tick = T,
                                                show_timeout_tick = T,
                                                widths = c(4,2,1,1),
                                                title = NULL,
                                                legend_top_margin = 0,
                                                show_legend = TRUE,
                                                latex_export = F,
                                                small_size = F) {
  virtual_algo_1 <- effectiveness_test_df[effectiveness_test_df$algo == algo_name_1,]
  virtual_algo_1 <- droplevels(virtual_algo_1)
  virtual_algo_2 <- effectiveness_test_df[effectiveness_test_df$algo == algo_name_2,]
  virtual_algo_2 <- droplevels(virtual_algo_2)
  virtual_algo_1$algorithm <- virtual_algo_1$algo
  virtual_algo_2$algorithm <- virtual_algo_2$algo
  
  quality <- performace_plot(list(virtual_algo_1, 
                                  virtual_algo_2), 
                             objective = objective, 
                             hide_y_axis_title = hide_y_axis_title,
                             show_infeasible_tick = show_infeasible_tick,
                             show_timeout_tick = show_timeout_tick,
                             widths = widths,
                             legend_top_margin = legend_top_margin,
                             latex_export = latex_export,
                             small_size = small_size)
  return(quality) 
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



