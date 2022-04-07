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
    tmp_virtual_instances <- data.frame()
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
        tmp_virtual_instances <- rbind(tmp_virtual_instances,virtual_instances_algo_1, virtual_instances_algo_2)
      } 
    }
    return(tmp_virtual_instances)
  }
  
  write.csv2(virtual_instances, file = output_csv_file, quote = F, row.names = F)
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
                                                latex_export = F,
                                                small_legend = F,
                                                small_ticks = F,
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
                             effectivenes_test = T,
                             hide_y_axis_title = hide_y_axis_title,
                             show_infeasible_tick = show_infeasible_tick,
                             show_timeout_tick = show_timeout_tick,
                             widths = widths,
                             latex_export = latex_export,
                             small_legend = small_legend,
                             small_ticks = small_ticks,
                             small_size = small_size)
  return(quality) 
}
