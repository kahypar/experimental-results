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

stage = function(row) {
  if(grepl("*total*", row['objective'])){
    return("Total Computation")
  } else if(grepl("*avg_time*", row['objective'])){
    return("Total Computation")
  } else if(grepl("*preprocessing*", row['objective'])){
    return("Preprocessing")
  } else if(grepl("*community_detection*", row['objective'])){
    return("Community Detection")
  } else if(grepl("*coarsening*", row['objective'])){
    return("Coarsening")
  } else if(grepl("*initial_partitioning*", row['objective'])){
    return("Initial Partitioning")
  } else if(grepl("*refinement*", row['objective'])){
    return("Refinement")
  } else if(grepl("*label_propagation*", row['objective'])){
    return("Localized Label Propagation")
  } else if(grepl("*global_fm*", row['objective'])){
    return("Global FM")
  } else if(grepl("*fm*", row['objective'])){
    return("Localized FM")
  } else if(grepl("*batch_uncontraction*", row['objective'])){
    return("Batch Uncontractions")
  }
  return(row['objective'])
}

stages_levels <- c("Total Computation", "Preprocessing", "Community Detection", "Coarsening", 
                   "Initial Partitioning", "Refinement", "Batch Uncontractions",
                   "Localized Label Propagation", "Global FM", "Localized FM")

detailed_speed_up_vs_single_threaded_plot <- function(dataframes, 
                                                      relative_to,
                                                      objectives, 
                                                      latex_export = F,
                                                      small_size = F) {
  # Calculate Speed Ups and add Single-Threaded Running Time
  for(i in 1:length(dataframes)) {
    dataframes[[i]] <- compute_speed_up(dataframes[[i]], relative_to, objectives)
    dataframes[[i]] <- add_single_threaded_time(dataframes[[i]], relative_to, objectives)
  }
  
  # Create data for Speed Up Plot
  data <- bind_rows(dataframes)
  data <- data[data$num_threads != 1,]
  data <- droplevels(data)
  speed_up_df <- data.frame(graph = character(),
                            k = integer(),
                            num_threads = integer(),
                            algorithm = character(),
                            objective = character(),
                            speed_up = double(),
                            single_threaded_time = double(),
                            hmean_speed_up = double())
  for ( objective in objectives ) {
    speed_up_obj <- paste("avg", objective, "speed_up", sep = "_")
    single_threaded_time <- paste("single_threaded_avg", objective, "time", sep = "_")
    df <- data[, c("graph", "k", "num_threads", "algorithm", speed_up_obj, single_threaded_time)]
    names(df)[names(df) == speed_up_obj] <- "speed_up"
    names(df)[names(df) == single_threaded_time] <- "single_threaded_time"
    df$objective <- objective
    df$hmean_speed_up <- apply(df, 1, function(row) {
      t <- as.numeric(row["num_threads"])
      time <- row["single_threaded_time"]
      return(harmonic_mean(df[df$num_threads == t & df$single_threaded_time >= as.double(time),]$speed_up))
    })
    speed_up_df <- rbind(speed_up_df, df)
  }
  
  # Handle Superlinear Speed Ups
  max_num_threads <- max(speed_up_df$num_threads)
  max_speed_up <- max(speed_up_df$speed_up)
  next_pow_2 <- 2^(ceil(log2(max_speed_up)))
  speed_up_df <- speed_up_df %>% 
    mutate(speed_up = ifelse(speed_up > max_num_threads, 
                             max_num_threads + max_num_threads * (speed_up - max_num_threads) / 
                               (next_pow_2 - max_num_threads), speed_up)) %>% 
    mutate(hmean_speed_up = ifelse(hmean_speed_up > max_num_threads, 
                                   max_num_threads + max_num_threads * (hmean_speed_up - max_num_threads) / 
                                     (next_pow_2 - max_num_threads), hmean_speed_up))
  
  # Transform objectives into readable objective description
  speed_up_df$objective_desc <- as.character(apply(speed_up_df, 1, function(x) stage(x)))
  if ( latex_export ) {
    # Pads each objective description with 'x' such that each description has the same length
    # => fixes alignment problems in latex export
    max_length_str <- max(nchar(speed_up_df$objective_desc))
    speed_up_df$objective_desc <- pad_with_white_latex_text(speed_up_df$objective_desc, max_length_str)
    speed_up_df$objective_desc = factor(speed_up_df$objective_desc, levels = pad_with_white_latex_text(stages_levels, max_length_str)) 
  } else {
    speed_up_df$objective_desc = factor(speed_up_df$objective_desc, levels = stages_levels) 
  }
  
  
  speed_up = ggplot(speed_up_df, aes(x=single_threaded_time, y=speed_up, color=algorithm)) +
    scale_x_continuous(trans = "log10", breaks=c(1,10,100,1000,10000), 
                       labels=c(pow_text(10,0,latex_export), pow_text(10,1,latex_export), pow_text(10,2,latex_export), pow_text(10,3,latex_export), pow_text(10,4,latex_export))) +
    scale_y_continuous(trans = pseudo_log_trans(base = 2), breaks=c(0,1,2,4,8,16,32,64,128), labels=c(0,1,2,4,8,16,32,64,next_pow_2)) +
    geom_rect(xmin = -Inf, xmax = 1.5, ymin = 5.8, ymax = Inf, fill = "white", colour = "transparent") +
    geom_hline(yintercept = 64, size = .25, linetype = "dashed") +
    geom_text(aes(label = objective_desc), x = -Inf, y = Inf, hjust = -0.1, vjust = 1.75, check_overlap = T, colour = "black", size = plot_text_size(latex_export)) +
    geom_point(size = plot_point_size(latex_export), alpha = 0.5) +
    geom_line(aes(y = hmean_speed_up), size = 2 * plot_line_size(latex_export)) +
    facet_wrap(vars(objective_desc), ncol=3, scales = "free_x") +
    scale_color_manual(values=algo_color_mapping, drop = F) +
    scale_fill_manual(values=algo_color_mapping, drop = F) +
    theme_bw(base_size = 10) +
    labs(x=paste("Single-Threaded Running Time of Component",to_latex_math_mode("[s]",latex_export),sep=" "), 
         y=paste("Harmonic Mean Speed Up", ifelse(latex_export, "$[\\ge x]$", "[>= 0]"), sep=" ")) +
    create_theme(latex_export, small_size, legend_position = "bottom") +
    guides(colour = guide_legend(title=NULL, ncol = length(levels(factor(data$algorithm))), byrow = 
                                   F, title.position = "top"))
  return(speed_up)
}
