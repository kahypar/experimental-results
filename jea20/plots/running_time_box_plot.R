sqrt5_trans = function() trans_new("sqrt5", function(x) x^(1/5), function(x) x^5)

compute_avg_time_per_pin <- function(dataframe, instances) {
  tmp_data <- dataframe
  tmp_data <- join(tmp_data, instances, by = "graph", type = "left")
  tmp_data$avg_time_per_pin <- ( tmp_data$avg_time * 10^6 ) / tmp_data$pins
  dataframe$avg_time_per_pin <- tmp_data$avg_time_per_pin
  return(dataframe)
}

running_time_box_plot <- function(dataframes, 
                                  show_infeasible_tick = T,
                                  show_timeout_tick = T,
                                  order = NULL,
                                  latex_export = F,
                                  small_size = F) {
  
  # Prepare data frame containing all relevant data for the plot
  result <- data.frame(algorithm = character(),
                       avg_time = double(),
                       plotted_avg_time = double(),
                       infeasible = logical(),
                       timeout = logical())
  max_time <- 0
  for ( i in 1:length(dataframes) ) {
    tmp_df <- dataframes[[i]][,c("algorithm", "avg_time", "infeasible", "timeout")]
    tmp_df <- tmp_df %>% mutate(plotted_avg_time = ifelse(infeasible == T | timeout == T, 0, avg_time))
    tmp_max_time <- max(tmp_df$plotted_avg_time)
    max_time <- max(max_time, tmp_max_time)
    result <- rbind(result, tmp_df)
  }
  
  # Create y-axis ticks
  y_breaks <- c(0)
  y_labels <- c(to_latex_math_mode("0", latex_export))
  max_exp_time <- ceil(log10(max_time))
  max_pow10_time <- 10^max_exp_time
  y_breaks <- c(0, 10^seq(from = 0, to = max_exp_time))
  y_labels <- c(to_latex_math_mode("0", latex_export), pow_text(10, seq(from = 0, to = max_exp_time), latex_export))
  
  contains_invalid_results <- any(result$infeasible) | any(result$timeout)
  if ( contains_invalid_results ) {
    infeasible_value = max_pow10_time * 1.25
    timeout_value = max_pow10_time * 1.6
    result <- result %>% mutate(plotted_avg_time = ifelse(timeout == T, timeout_value, plotted_avg_time))
    result <- result %>% mutate(plotted_avg_time = ifelse(infeasible == T, infeasible_value, plotted_avg_time))
    y_breaks <- add_infeasible_break(y_breaks, infeasible_value, show_infeasible_tick, latex_export)
    y_labels <- add_infeasible_label(y_labels, infeasible_value, show_infeasible_tick, latex_export)
    y_breaks <- add_timeout_break(y_breaks, timeout_value, show_timeout_tick, latex_export)
    y_labels <- add_timeout_label(y_labels, timeout_value, show_timeout_tick, latex_export)
    y_limits <- c(0, timeout_value * 1.01)
  } else {
    y_limits <- c(0, max_pow10_time * 1.1)
  }
  
  if ( !is.null(levels) ) {
    result$algorithm <- factor(result$algorithm, levels = order)
  }
  
  # Create Running Time Box Plot
  gmean_time_aggreg = function(df) data.frame(gmean_time = gm_mean(df$avg_time))
  result_gmean <- ddply(result, c("algorithm"), gmean_time_aggreg)
  running_time = ggplot(result, aes(x=algorithm, y=avg_time)) +
    geom_jitter(aes(y = plotted_avg_time, fill=algorithm), size = plot_point_size(latex_export), alpha = 0.33, pch = 21, width = 0.3) +
    stat_boxplot(aes(color = algorithm), geom ='errorbar', width = 0.6) +
    geom_boxplot(aes(color = algorithm), outlier.shape = NA, alpha = 0.75) +
    geom_text(aes(x = algorithm, y = 0, label=round(gmean_time, 2), group = algorithm), result_gmean, 
              size = plot_text_size(latex_export),  position = position_dodge(width=0.75)) +
    scale_y_continuous(breaks=y_breaks, labels = y_labels, limits = y_limits) +
    coord_trans(y = "sqrt5") + 
    theme_bw(base_size = 10) +
    scale_color_manual(values=algo_color_mapping, drop = F) +
    scale_fill_manual(values=algo_color_mapping, drop = F) +
    labs(x="", y=paste("Running Time",to_latex_math_mode("[s]",latex_export),sep=" ")) +
    create_theme(latex_export, small_size, legend_position = "none", x_axis_text_angle = 15, x_axis_text_hjust = 1)
  if ( contains_invalid_results ) {
    running_time <- running_time + geom_hline(yintercept = (max_pow10_time + infeasible_value) / 2, size = 0.5, alpha = 0.5)
  }
  
  return(running_time)
}

running_time_per_pin_box_plot <- function(dataframes, 
                                          show_infeasible_tick = T,
                                          show_timeout_tick = T,
                                          order = NULL,
                                          title = NULL,
                                          use_sampling = F,
                                          sampling_factor = 1.0,
                                          text_angle = 15,
                                          show_legend = T,
                                          latex_export = F,
                                          small_size = F) {
  
  # Prepare data frame containing all relevant data for the plot
  result <- data.frame(algorithm = character(),
                       avg_time_per_pin = double(),
                       plotted_avg_time = double(),
                       infeasible = logical(),
                       timeout = logical())
  max_time <- 0
  for ( i in 1:length(dataframes) ) {
    tmp_df <- dataframes[[i]][,c("algorithm", "avg_time_per_pin", "infeasible", "timeout")]
    tmp_df <- tmp_df %>% mutate(plotted_avg_time = ifelse(infeasible == T | timeout == T, 0, avg_time_per_pin))
    tmp_max_time <- max(tmp_df$plotted_avg_time)
    max_time <- max(max_time, tmp_max_time)
    result <- rbind(result, tmp_df)
  }
  
  # Create y-axis ticks
  y_breaks <- c(0)
  y_labels <- c(to_latex_math_mode("0", latex_export))
  max_exp_time <- ceil(log10(max_time))
  max_pow10_time <- 10^max_exp_time
  y_breaks <- c(0, 10^seq(from = 0, to = max_exp_time - 1))
  y_labels <- c(to_latex_math_mode("0", latex_export), pow_text(10, seq(from = 0, to = max_exp_time - 1), latex_export))
  
  contains_infeasible_results <- any(result$infeasible)
  contains_timeout_results <- any(result$timeout)
  contains_invalid_results <- contains_infeasible_results | contains_timeout_results
  contains_only_one_invalid_type <- xor(contains_infeasible_results, contains_timeout_results)
  if ( contains_invalid_results ) {
    infeasible_factor <- 2
    timeout_factor <- 7.5
    if ( contains_only_one_invalid_type ) {
      infeasible_factor <- 1.5
      timeout_factor <- 1.5
    }
    infeasible_value = ( infeasible_factor * max_pow10_time )
    timeout_value = ( timeout_factor * max_pow10_time  )
    result <- result %>% mutate(plotted_avg_time = ifelse(timeout == T, timeout_value, plotted_avg_time))
    result <- result %>% mutate(plotted_avg_time = ifelse(infeasible == T, infeasible_value, plotted_avg_time))
    y_breaks <- add_infeasible_break(y_breaks, infeasible_value, show_infeasible_tick, latex_export)
    y_labels <- add_infeasible_label(y_labels, infeasible_value, show_infeasible_tick, latex_export)
    y_breaks <- add_timeout_break(y_breaks, timeout_value, show_timeout_tick, latex_export)
    y_labels <- add_timeout_label(y_labels, timeout_value, show_timeout_tick, latex_export)
    y_limits <- c(0, timeout_value * 1.01)
  } else {
    y_limits <- c(0, max_pow10_time * 1.1)
  }
  
  if ( !is.null(levels) ) {
    result$algorithm <- factor(result$algorithm, levels = order)
  }
  
  if ( use_sampling ) {
    sampled_result <- data.frame(algorithm = character(),
                             avg_time_per_pin = double(),
                             plotted_avg_time = double(),
                             infeasible = logical(),
                             timeout = logical())
    for ( algorithm in levels(result$algorithm) ) {
      tmp_data <- result[result$algorithm == algorithm,]
      median_time <- median(tmp_data$plotted_avg_time)
      non_sampled_data <- tmp_data[tmp_data$plotted_avg_time > max_pow10_time,]
      sampled_data <- tmp_data[tmp_data$plotted_avg_time <= max_pow10_time,]
      sampled_data <- sample_n(sampled_data, nrow(sampled_data) * sampling_factor)
      sampled_result <- rbind(sampled_result, non_sampled_data)
      sampled_result <- rbind(sampled_result, sampled_data)
    }
  }
  
  # Create Running Time Box Plot
  gmean_time_aggreg = function(df) data.frame(gmean_time = gm_mean(df$avg_time_per_pin))
  result_gmean <- ddply(result, c("algorithm"), gmean_time_aggreg)
  running_time = ggplot(result, aes(x=algorithm, y=avg_time_per_pin)) 
  if ( use_sampling ) {
    running_time <- running_time + geom_jitter(data = sampled_result, aes(y = plotted_avg_time, fill=algorithm), size = plot_point_size(latex_export), alpha = 0.33, pch = 21, width = 0.3)
  } else {
    running_time <- running_time + geom_jitter(aes(y = plotted_avg_time, fill=algorithm), size = plot_point_size(latex_export), alpha = 0.33, pch = 21, width = 0.3)
  }
  running_time <- running_time + stat_boxplot(aes(color = algorithm), geom ='errorbar', width = 0.6) +
    geom_boxplot(aes(color = algorithm), outlier.shape = NA, alpha = 0.75) +
    geom_text(aes(x = algorithm, y = 0.5 * max_pow10_time, label=round(gmean_time, 2), group = algorithm), result_gmean, 
              size = plot_text_size(latex_export),  position = position_dodge(width=0.75)) +
    scale_y_continuous(trans = pseudo_log_trans(base = 10), breaks=y_breaks, labels = y_labels, limits = y_limits) +
    theme_bw(base_size = 10) +
    scale_color_manual(values=algo_color_mapping, drop = F) +
    scale_fill_manual(values=algo_color_mapping, drop = F) +
    labs(x="", y=paste("Time per pin",to_latex_math_mode("[\\mu s]",latex_export),sep=" ")) +
    create_theme(latex_export, small_size, legend_position = "none", x_axis_text_angle = text_angle, x_axis_text_hjust = 1)
  if ( contains_invalid_results ) {
    running_time <- running_time + geom_hline(yintercept = max_pow10_time, size = 0.5, alpha = 0.5)
  }
  
  if ( !show_legend ) {
    running_time <- running_time + theme(
      axis.text.x = element_text(angle = text_angle, hjust = 1, size = axis_text_size(latex_export, small_size), color = "white"),)
  }
  
  if ( !is.null(title) ) {
    running_time <- running_time + ggtitle(title)
  }
  
  return(running_time)
}
