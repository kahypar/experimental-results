source("../functions.R")
source("../plots/plots_common.R")
source("../plots/running_time_box_plot.R")
source("../plots/relative_running_time_plot.R")
source("../plots/performance_profiles.R")

############## SETUP DATA FRAMES ############## 

# Read Data Frames
instances <- read.csv("instances.csv", header = TRUE)
instances$pins <- 2 * instances$m
kaffpa_strong_f <- aggreg_data(read.csv("cut_kaffpa_strong_graph_f.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
kaffpa_strong_g <- aggreg_data(read.csv("cut_kaffpa_strong_graph_g.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
kaffpa_strong_f$type <- "WebSocial"
kaffpa_strong_g$type <- "Dimacs"
kaffpa_strong_f$graph <- paste(kaffpa_strong_f$graph, ".hgr", sep = "")
kaffpa_strong_g$graph <- paste(kaffpa_strong_g$graph, ".hgr", sep = "")
kaffpa_strong <- rbind(kaffpa_strong_f, kaffpa_strong_g)
kaffpa_strong_opt_f <- aggreg_data(read.csv("cut_kaffpa_strong_opt_graph_f.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
kaffpa_strong_opt_g <- aggreg_data(read.csv("cut_kaffpa_strong_opt_graph_g.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
kaffpa_strong_opt_f$type <- "WebSocial"
kaffpa_strong_opt_g$type <- "Dimacs"
kaffpa_strong_opt_f$graph <- paste(kaffpa_strong_opt_f$graph, ".hgr", sep = "")
kaffpa_strong_opt_g$graph <- paste(kaffpa_strong_opt_g$graph, ".hgr", sep = "")
kaffpa_strong_opt <- rbind(kaffpa_strong_opt_f, kaffpa_strong_opt_g)
kaffpa_strongs_f <- aggreg_data(read.csv("cut_kaffpa_strongs_graph_f.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
kaffpa_strongs_g <- aggreg_data(read.csv("cut_kaffpa_strongs_graph_g.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
kaffpa_strongs_f$type <- "WebSocial"
kaffpa_strongs_g$type <- "Dimacs"
kaffpa_strongs_f$graph <- paste(kaffpa_strongs_f$graph, ".hgr", sep = "")
kaffpa_strongs_g$graph <- paste(kaffpa_strongs_g$graph, ".hgr", sep = "")
kaffpa_strongs <- rbind(kaffpa_strongs_f, kaffpa_strongs_g)
kaffpa_strongs_opt_f <- aggreg_data(read.csv("cut_kaffpa_strongs_opt_graph_f.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
kaffpa_strongs_opt_g <- aggreg_data(read.csv("cut_kaffpa_strongs_opt_graph_g.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
kaffpa_strongs_opt_f$type <- "WebSocial"
kaffpa_strongs_opt_g$type <- "Dimacs"
kaffpa_strongs_opt_f$graph <- paste(kaffpa_strongs_opt_f$graph, ".hgr", sep = "")
kaffpa_strongs_opt_g$graph <- paste(kaffpa_strongs_opt_g$graph, ".hgr", sep = "")
kaffpa_strongs_opt <- rbind(kaffpa_strongs_opt_f, kaffpa_strongs_opt_g)
kahypar_k_f <- aggreg_data(read.csv("cut_kahypar_k_graph_f.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
kahypar_k_g <- aggreg_data(read.csv("cut_kahypar_k_graph_g.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
kahypar_k_f$type <- "WebSocial"
kahypar_k_g$type <- "Dimacs"
kahypar_k <- rbind(kahypar_k_f, kahypar_k_g)
kahypar_r_f <- aggreg_data(read.csv("cut_kahypar_r_graph_f.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
kahypar_r_g <- aggreg_data(read.csv("cut_kahypar_r_graph_g.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
kahypar_r_f$type <- "WebSocial"
kahypar_r_g$type <- "Dimacs"
kahypar_r <- rbind(kahypar_r_f, kahypar_r_g)

# Compute Average Time per Pin
kahypar_r_f <- compute_avg_time_per_pin(kahypar_r_f, instances)
kahypar_r_g <- compute_avg_time_per_pin(kahypar_r_g, instances)
kahypar_r <- compute_avg_time_per_pin(kahypar_r, instances)
kahypar_k_f <- compute_avg_time_per_pin(kahypar_k_f, instances)
kahypar_k_g <- compute_avg_time_per_pin(kahypar_k_g, instances)
kahypar_k <- compute_avg_time_per_pin(kahypar_k, instances)
kaffpa_strong_f <- compute_avg_time_per_pin(kaffpa_strong_f, instances)
kaffpa_strong_g <- compute_avg_time_per_pin(kaffpa_strong_g, instances)
kaffpa_strong <- compute_avg_time_per_pin(kaffpa_strong, instances)
kaffpa_strongs_f <- compute_avg_time_per_pin(kaffpa_strongs_f, instances)
kaffpa_strongs_g <- compute_avg_time_per_pin(kaffpa_strongs_g, instances)
kaffpa_strongs <- compute_avg_time_per_pin(kaffpa_strongs, instances)
kaffpa_strong_opt_f <- compute_avg_time_per_pin(kaffpa_strong_opt_f, instances)
kaffpa_strong_opt_g <- compute_avg_time_per_pin(kaffpa_strong_opt_g, instances)
kaffpa_strong_opt <- compute_avg_time_per_pin(kaffpa_strong_opt, instances)
kaffpa_strongs_opt_f <- compute_avg_time_per_pin(kaffpa_strongs_opt_f, instances)
kaffpa_strongs_opt_g <- compute_avg_time_per_pin(kaffpa_strongs_opt_g, instances)
kaffpa_strongs_opt <- compute_avg_time_per_pin(kaffpa_strongs_opt, instances)

# Set Algorithm Name
kahypar_r_f$algorithm <- "$r$KaHyPar"
kahypar_r_g$algorithm <- "$r$KaHyPar"
kahypar_r$algorithm <- "$r$KaHyPar"
kahypar_k_f$algorithm <- "$k$KaHyPar"
kahypar_k_g$algorithm <- "$k$KaHyPar"
kahypar_k$algorithm <- "$k$KaHyPar"
kaffpa_strong_f$algorithm <- "KaFFPa-Strong"
kaffpa_strong_g$algorithm <- "KaFFPa-Strong"
kaffpa_strong$algorithm <- "KaFFPa-Strong"
kaffpa_strong_opt_f$algorithm <- "KaFFPa-Strong*"
kaffpa_strong_opt_g$algorithm <- "KaFFPa-Strong*"
kaffpa_strong_opt$algorithm <- "KaFFPa-Strong*"
kaffpa_strongs_f$algorithm <- "KaFFPa-StrongS"
kaffpa_strongs_g$algorithm <- "KaFFPa-StrongS"
kaffpa_strongs$algorithm <- "KaFFPa-StrongS"
kaffpa_strongs_opt_f$algorithm <- "KaFFPa-StrongS*"
kaffpa_strongs_opt_g$algorithm <- "KaFFPa-StrongS*"
kaffpa_strongs_opt$algorithm <- "KaFFPa-StrongS*"

# Specify Colors of Algorithms in Plots
palette <- brewer.pal(n = 9, name = "Set1")
algo_color_mapping <- c("$k$KaHyPar" = palette[[1]],
                        "$r$KaHyPar" = palette[[2]],
                        "KaFFPa-Strong" = "#B8DBD9",
                        "KaFFPa-Strong*" = "#30BCED",
                        "KaFFPa-StrongS" = "#FAA916",
                        "KaFFPa-StrongS*" = "#AE8D0A")

order <- c("$k$KaHyPar", "$r$KaHyPar", "KaFFPa-Strong", "KaFFPa-Strong*", "KaFFPa-StrongS", "KaFFPa-StrongS*")
tikz("~/kahypar-jea/tikz_plots/cut_web_graphs_running_time_kaffpa_strongs.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(running_time_per_pin_box_plot(list(kaffpa_strongs_f,
                                         kaffpa_strongs_opt_f), 
                                    show_infeasible_tick = F,
                                    show_timeout_tick = T,
                                    order = order,
                                    use_sampling = T,
                                    sampling_factor = 0.5,
                                    text_angle = 30,
                                    latex_export = T,
                                    small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/cut_dimacs_graph_running_time_kaffpa_strong.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(running_time_per_pin_box_plot(list(kaffpa_strong_g,
                                         kaffpa_strong_opt_g), 
                                    show_infeasible_tick = F,
                                    show_timeout_tick = T,
                                    order = order,
                                    use_sampling = T,
                                    sampling_factor = 0.5,
                                    text_angle = 30,
                                    latex_export = T,
                                    small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/cut_web_graphs_running_time_kahypar_vs_kaffpa.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(running_time_per_pin_box_plot(list(kahypar_k_f,
                                         kahypar_r_f,
                                         kaffpa_strongs_opt_f), 
                                    show_infeasible_tick = F,
                                    show_timeout_tick = T,
                                    order = order,
                                    use_sampling = T,
                                    sampling_factor = 0.5,
                                    text_angle = 30,
                                    latex_export = T,
                                    small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/cut_dimacs_running_time_kahypar_vs_kaffpa.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(running_time_per_pin_box_plot(list(kahypar_k_g,
                                         kahypar_r_g,
                                         kaffpa_strong_opt_g,
                                         kaffpa_strongs_opt_g), 
                                    show_infeasible_tick = F,
                                    show_timeout_tick = T,
                                    order = order,
                                    use_sampling = T,
                                    sampling_factor = 0.5,
                                    text_angle = 30,
                                    latex_export = T,
                                    small_size = F))
dev.off()

############## Performance Profile Plot ############## 

scaling <- 1.25

tikz("~/kahypar-jea/tikz_plots/cut_web_graphs_kaffpa_strongs.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(performace_plot(list(kaffpa_strongs_f,
                           kaffpa_strongs_opt_f), 
                      objective = "avg_cut", 
                      hide_y_axis_title = F,
                      show_infeasible_tick = F,
                      show_timeout_tick = T,
                      widths = c(3,2,1,1),
                      legend_top_margin = -25,
                      latex_export = T,
                      small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/cut_dimacs_graph_kaffpa_strong.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(performace_plot(list(kaffpa_strong_g,
                           kaffpa_strong_opt_g), 
                      objective = "avg_cut", 
                      hide_y_axis_title = F,
                      show_infeasible_tick = F,
                      show_timeout_tick = T,
                      widths = c(3,2,1,1),
                      legend_top_margin = -25,
                      latex_export = T,
                      small_size = F))
dev.off()


tikz("~/kahypar-jea/tikz_plots/cut_web_graphs_kahypar_vs_kaffpa.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(performace_plot(list(kahypar_k_f,
                           kahypar_r_f,
                           kaffpa_strongs_opt_f), 
                      objective = "avg_cut", 
                      hide_y_axis_title = F,
                      show_infeasible_tick = F,
                      show_timeout_tick = T,
                      widths = c(3,2,1,1),
                      legend_top_margin = -25,
                      latex_export = T,
                      small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/cut_web_graphs_kahypar_k_vs_kaffpa.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(performace_plot(list(kahypar_k_f,
                           kaffpa_strongs_opt_f), 
                      objective = "avg_cut", 
                      hide_y_axis_title = F,
                      show_infeasible_tick = F,
                      show_timeout_tick = T,
                      widths = c(3,2,1,1),
                      legend_top_margin = -25,
                      latex_export = T,
                      small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/cut_web_graphs_kahypar_r_vs_kaffpa.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(performace_plot(list(kahypar_r_f,
                           kaffpa_strongs_opt_f), 
                      objective = "avg_cut", 
                      hide_y_axis_title = F,
                      show_infeasible_tick = F,
                      show_timeout_tick = T,
                      widths = c(3,2,1,1),
                      legend_top_margin = -25,
                      latex_export = T,
                      small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/cut_dimacs_kahypar_vs_kaffpa.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(performace_plot(list(kahypar_k_g,
                           kahypar_r_g,
                           kaffpa_strong_opt_g,
                           kaffpa_strongs_opt_g), 
                      objective = "avg_cut", 
                      hide_y_axis_title = F,
                      show_infeasible_tick = F,
                      show_timeout_tick = T,
                      widths = c(3,2,1,1),
                      legend_top_margin = -25,
                      latex_export = T,
                      small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/cut_dimacs_kahypar_k_vs_kaffpa.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(performace_plot(list(kahypar_k_g,
                           kaffpa_strong_opt_g,
                           kaffpa_strongs_opt_g), 
                      objective = "avg_cut", 
                      hide_y_axis_title = F,
                      show_infeasible_tick = F,
                      show_timeout_tick = T,
                      widths = c(3,2,1,1),
                      legend_top_margin = -25,
                      latex_export = T,
                      small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/cut_dimacs_kahypar_r_vs_kaffpa.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(performace_plot(list(kahypar_r_g,
                           kaffpa_strong_opt_g,
                           kaffpa_strongs_opt_g), 
                      objective = "avg_cut", 
                      hide_y_axis_title = F,
                      show_infeasible_tick = F,
                      show_timeout_tick = T,
                      widths = c(3,2,1,1),
                      legend_top_margin = -25,
                      latex_export = T,
                      small_size = F))
dev.off()



