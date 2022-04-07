library(zoo)
library(psych)
library(data.table)

compute_speed_up <- function(data,
                             relative_to,
                             objectives) {
  # Sanity Check
  stopifnot(relative_to$graph == data$graph, relative_to$k == data$k)
  
  # Compute Speed Up for all objectives
  targets <- c("min", "avg")
  for ( target in targets ) {
    for ( obj in objectives ) {
      time_column_name <- paste(target,obj,"time", sep="_")
      speed_up_column_name <- paste(target,obj,"speed_up",sep="_")
      data[speed_up_column_name] <- relative_to[time_column_name] / data[time_column_name]
    } 
  }
  return(data)
}

add_single_threaded_time <- function(data, 
                                     relative_to, 
                                     objectives) {
  # Sanity Check
  stopifnot(relative_to$graph == data$graph, relative_to$k == data$k)
  
  # Compute Speed Up for all objectives
  targets <- c("min", "avg")
  for ( target in targets ) {
    for ( obj in objectives ) {
      time_column_name <- paste(target,obj,"time", sep="_")
      single_threaded_column_name <- paste("single_threaded",target,obj,"time",sep="_")
      data[single_threaded_column_name] <- relative_to[time_column_name]
    } 
  }
  return(data)
}
pad_with_white_latex_text <- function(text, n, pad = "x") {
  text <- str_pad(text, n, "right", pad = pad)
  for ( i in seq(1:length(text)) ) {
    if (grepl(text[[i]], "#") != -1) {
      text[[i]] <- str_replace(text[[i]], pad, paste("\\\\\\textcolor{white}{", pad, sep = ""))
      text[[i]] <- stri_replace_last_fixed(text[[i]], pad, paste(pad,"}", sep = ""))
    }
  }
  return(text)
}


detailed_speed_up_vs_single_threaded_plot <- function(dataframes, 
                                                      relative_to,
                                                      target,
                                                      objective,
                                                      plot_title = "",
                                                      window_size = 25,
                                                      point_alpha = 0.5,
                                                      show_max_speed_up_line = F,
                                                      order = c("1", "4", "16", "64"),
                                                      show_legend = T,
                                                      hide_y_axis = F,
                                                      latex_export = F,
                                                      small_ticks = F,
                                                      small_size = F) {
  # Calculate Speed Ups and add Single-Threaded Running Time
  speed_up_obj <- paste(target,objective,"speed_up", sep="_")
  single_threaded_time <- paste("single_threaded",target,objective,"time",sep="_")
  relative_to_time <- paste(target,objective,"time",sep="_")
  for(i in 1:length(dataframes)) {
    dataframes[[i]] <- compute_speed_up(dataframes[[i]], relative_to, c(objective))
    dataframes[[i]] <- add_single_threaded_time(dataframes[[i]], relative_to, c(objective))
    dataframes[[i]] <- dataframes[[i]][order(dataframes[[i]][single_threaded_time]),]
    dataframes[[i]]$rolling_gmean_speed_up <- rollapply(dataframes[[i]][speed_up_obj], window_size, geometric.mean, fill = NA, align = "center", partial = TRUE)
  }
  relative_to <- relative_to[order(relative_to[relative_to_time]),]
  
  # Create data for Speed Up Plot
  data <- bind_rows(dataframes)
  data <- data[data$num_threads != 1,]
  data <- droplevels(data)
  speed_up_df <- data[, c("graph", "num_threads", "algorithm", speed_up_obj, single_threaded_time, "rolling_gmean_speed_up")]
  names(speed_up_df)[names(speed_up_df) == speed_up_obj] <- "speed_up"
  names(speed_up_df)[names(speed_up_df) == single_threaded_time] <- "single_threaded_time"
  
  # Handle Superlinear Speed Ups
  max_num_threads <- max(speed_up_df$num_threads)
  max_speed_up <- max(speed_up_df$speed_up)
  next_pow_2 <- 2^(ceil(log2(max_speed_up)))
  speed_up_df <- speed_up_df %>% 
    mutate(speed_up = ifelse(speed_up > max_num_threads, 
                             max_num_threads + max_num_threads * (speed_up - max_num_threads) / 
                               (next_pow_2 - max_num_threads), speed_up)) %>% 
    mutate(rolling_gmean_speed_up = ifelse(rolling_gmean_speed_up > max_num_threads, 
                                           max_num_threads + max_num_threads * (rolling_gmean_speed_up - max_num_threads) / 
                                             (next_pow_2 - max_num_threads), rolling_gmean_speed_up))
  
  if ( !is.null(order) ) {
    speed_up_df$num_threads <- as.character(speed_up_df$num_threads)
    speed_up_df$num_threads <- factor(speed_up_df$num_threads, levels = order)
    speed_up_df <- droplevels(speed_up_df)
  }
  speed_up_df$title <- plot_title
  speed_up = ggplot(speed_up_df, aes(x=single_threaded_time, y=speed_up, color=num_threads)) +
    scale_x_continuous(trans = "log10", breaks=c(1,10,100,1000,10000), 
                       labels=c(pow_text(10,0,latex_export), pow_text(10,1,latex_export), pow_text(10,2,latex_export), pow_text(10,3,latex_export), pow_text(10,4,latex_export))) +
    scale_y_continuous(trans = pseudo_log_trans(base = 2), breaks=c(0,1,2,4,8,16,32,64,128), labels=c(0,1,2,4,8,16,32,64,next_pow_2), limits = c(0,ifelse(next_pow_2 > 64, 128, 64))) +
    geom_rect(xmin = -Inf, xmax = 1.5, ymin = 5.8, ymax = Inf, fill = "white", colour = "transparent")
  if ( show_max_speed_up_line  ) {
    speed_up <- speed_up + geom_hline(yintercept = 64, size = .25, linetype = "dashed") +
      geom_hline(yintercept = 0, size = .25, alpha = 0.0)
  } else {
    speed_up <- speed_up + geom_hline(yintercept = 64, size = .25, alpha = 0.0)+
      geom_hline(yintercept = 0, size = .25, alpha = 0.0)
  }
  speed_up <- speed_up + geom_point(size = plot_point_size(latex_export), alpha = point_alpha) +
    geom_text(aes(label = title), x = -Inf, y = Inf, hjust = -0.1, vjust = 1.75, check_overlap = T, colour = "black", size = plot_text_size(latex_export)) +
    geom_line(aes(y = rolling_gmean_speed_up), size = 2 * plot_line_size(latex_export)) +
    scale_color_manual(values=algo_color_mapping, drop = F) +
    scale_fill_manual(values=algo_color_mapping, drop = F) +
    theme_bw(base_size = 10) +
    labs(x=paste("Single-Threaded Time of Component",to_latex_math_mode("[s]",latex_export),sep=" "), 
         y="Speed Up") +
    create_theme(latex_export, small_ticks, small_size, legend_position = "bottom") +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_blank()) +
    guides(colour = guide_legend(title=NULL, ncol = length(levels(factor(speed_up_df$num_threads))), byrow = 
                                   F, title.position = "top"))
  
  if (hide_y_axis  ) {
    speed_up <- speed_up + theme(axis.text.y = element_blank(),
                                 axis.ticks.y = element_blank())
  }
  
  if (!show_legend) {
    speed_up <-  speed_up + theme(legend.position = "none")
  }
  
  return(speed_up)
}
