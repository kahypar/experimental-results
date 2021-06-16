source("../functions.R")
source("../plots/plots_common.R")
source("../plots/running_time_box_plot.R")
source("../plots/relative_running_time_plot.R")
source("../plots/performance_profiles.R")

############## SETUP DATA FRAMES ##############

# Read Data Frames
instances <- read.csv("instances.csv", header = TRUE)
instances$pins <- 2 * instances$m
kaffpa_strong_opt_f <- aggreg_data(read.csv("cut_kaffpa_strong_websocial.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
kaffpa_strong_opt_g <- aggreg_data(read.csv("cut_kaffpa_strong_dimacs.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
kaffpa_strong_opt_f$type <- "WebSocial"
kaffpa_strong_opt_g$type <- "Dimacs"
kaffpa_strong_opt_f$graph <- paste(kaffpa_strong_opt_f$graph, ".hgr", sep = "")
kaffpa_strong_opt_g$graph <- paste(kaffpa_strong_opt_g$graph, ".hgr", sep = "")
kaffpa_strongs_opt_f <- aggreg_data(read.csv("cut_kaffpa_strongs_websocial.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
kaffpa_strongs_opt_g <- aggreg_data(read.csv("cut_kaffpa_strongs_dimacs.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
kaffpa_strongs_opt_f$type <- "WebSocial"
kaffpa_strongs_opt_g$type <- "Dimacs"
kaffpa_strongs_opt_f$graph <- paste(kaffpa_strongs_opt_f$graph, ".hgr", sep = "")
kaffpa_strongs_opt_g$graph <- paste(kaffpa_strongs_opt_g$graph, ".hgr", sep = "")
kahypar_k_f <- aggreg_data(read.csv("cut_kKahypar_websocial.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
kahypar_k_g <- aggreg_data(read.csv("cut_kKahypar_dimacs.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
kahypar_k_f$type <- "WebSocial"
kahypar_k_g$type <- "Dimacs"

# Compute Average Time per Pin
kahypar_k_f <- compute_avg_time_per_pin(kahypar_k_f, instances)
kahypar_k_g <- compute_avg_time_per_pin(kahypar_k_g, instances)
kaffpa_strong_opt_f <- compute_avg_time_per_pin(kaffpa_strong_opt_f, instances)
kaffpa_strong_opt_g <- compute_avg_time_per_pin(kaffpa_strong_opt_g, instances)
kaffpa_strongs_opt_f <- compute_avg_time_per_pin(kaffpa_strongs_opt_f, instances)
kaffpa_strongs_opt_g <- compute_avg_time_per_pin(kaffpa_strongs_opt_g, instances)

# Set Algorithm Name
kahypar_k_f$algorithm <- "$k$KaHyPar"
kahypar_k_g$algorithm <- "$k$KaHyPar"
kaffpa_strong_opt_f$algorithm <- "KaFFPa-Strong"
kaffpa_strong_opt_g$algorithm <- "KaFFPa-Strong"
kaffpa_strongs_opt_f$algorithm <- "KaFFPa-StrongS"
kaffpa_strongs_opt_g$algorithm <- "KaFFPa-StrongS"

# Specify Colors of Algorithms in Plots
palette <- brewer.pal(n = 9, name = "Set1")
algo_color_mapping <- c("$k$KaHyPar" = palette[[1]],
                        "KaFFPa-Strong" = "#30BCED",
                        "KaFFPa-StrongS" = "#AE8D0A")


############## Running Time Box Plot ##############

scaling <- 1.225

order <- c("$k$KaHyPar", "KaFFPa-Strong", "KaFFPa-StrongS")
tikz("~/kahypar-jea/tikz_plots/cut_web_graphs_running_time_kahypar_vs_kaffpa.tex",
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(running_time_per_pin_box_plot(list(kahypar_k_f,
                                         kaffpa_strong_opt_f,
                                         kaffpa_strongs_opt_f),
                                    show_infeasible_tick = F,
                                    show_timeout_tick = T,
                                    order = order,
                                    text_angle = 30,
                                    latex_export = T,
                                    small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/cut_dimacs_running_time_kahypar_vs_kaffpa.tex",
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(running_time_per_pin_box_plot(list(kahypar_k_g,
                                         kaffpa_strong_opt_g,
                                         kaffpa_strongs_opt_g),
                                    show_infeasible_tick = F,
                                    show_timeout_tick = T,
                                    order = order,
                                    text_angle = 30,
                                    latex_export = T,
                                    small_size = F))
dev.off()

############## Performance Profile Plot ##############

scaling <- 1.29

tikz("~/kahypar-jea/tikz_plots/cut_web_graphs_kahypar_k_vs_kaffpa.tex",
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(performace_plot(list(kahypar_k_f,
                           kaffpa_strong_opt_f,
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

compareQualityOfPartitioners(taus <- c(1.0),
                             objective <- "avg_cut",
                             kahypar_k_g,
                             kaffpa_strong_opt_g,
                             kaffpa_strongs_opt_g)

nrow(kaffpa_strongs_opt_f[kaffpa_strongs_opt_f$timeout == T,])
