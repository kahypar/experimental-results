args = commandArgs(trailingOnly=TRUE)
working_dir <- args[[1]] # "/home/tobias/journal-experiments/experimental-results/jea20"
setwd(working_dir)

source("functions_reproducibility.R")
source("plots/new/plots_common.R")
source("plots/new/running_time_box_plot.R")
source("plots/new/relative_running_time_plot.R")
source("plots/new/performance_profiles.R")
source("plots/new/effectiveness_tests.R")
source("plots/new/tradeoff_plot.R")

############## SETUP DATA FRAMES ##############

# Read Data Frames
instances <- read.csv("graph_partitioning/instances.csv", header = TRUE)
instances$pins <- 2 * instances$m
kaffpa_strong_opt_f <- aggreg_data(read.csv("graph_partitioning/cut_kaffpa_strong_websocial.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
kaffpa_strong_opt_g <- aggreg_data(read.csv("graph_partitioning/cut_kaffpa_strong_dimacs.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
kaffpa_strong_opt_f$type <- "WebSocial"
kaffpa_strong_opt_g$type <- "Dimacs"
kaffpa_strong_opt_f$graph <- paste(kaffpa_strong_opt_f$graph, ".hgr", sep = "")
kaffpa_strong_opt_g$graph <- paste(kaffpa_strong_opt_g$graph, ".hgr", sep = "")
kaffpa_strongs_opt_f <- aggreg_data(read.csv("graph_partitioning/cut_kaffpa_strongs_websocial.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
kaffpa_strongs_opt_g <- aggreg_data(read.csv("graph_partitioning/cut_kaffpa_strongs_dimacs.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
kaffpa_strongs_opt_f$type <- "WebSocial"
kaffpa_strongs_opt_g$type <- "Dimacs"
kaffpa_strongs_opt_f$graph <- paste(kaffpa_strongs_opt_f$graph, ".hgr", sep = "")
kaffpa_strongs_opt_g$graph <- paste(kaffpa_strongs_opt_g$graph, ".hgr", sep = "")
kahypar_k_f <- aggreg_data(read.csv("graph_partitioning/cut_kKahypar_websocial.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
kahypar_k_g <- aggreg_data(read.csv("graph_partitioning/cut_kKahypar_dimacs.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
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
kahypar_k_f$algorithm <- "kKaHyPar"
kahypar_k_g$algorithm <- "kKaHyPar"
kaffpa_strong_opt_f$algorithm <- "KaFFPa-Strong"
kaffpa_strong_opt_g$algorithm <- "KaFFPa-Strong"
kaffpa_strongs_opt_f$algorithm <- "KaFFPa-StrongS"
kaffpa_strongs_opt_g$algorithm <- "KaFFPa-StrongS"

# Specify Colors of Algorithms in Plots
palette <- brewer.pal(n = 9, name = "Set1")
algo_color_mapping <- c("kKaHyPar" = palette[[1]],
                        "KaFFPa-Strong" = "#30BCED",
                        "KaFFPa-StrongS" = "#AE8D0A")


############## Running Time Box Plot ##############

scaling <- 1.225

order <- c("kKaHyPar", "KaFFPa-Strong", "KaFFPa-StrongS")
p1 <- running_time_per_pin_box_plot(list(kahypar_k_f,
                                         kaffpa_strong_opt_f,
                                         kaffpa_strongs_opt_f),
                                    show_infeasible_tick = F,
                                    show_timeout_tick = T,
                                    order = order,
                                    text_angle = 30,
                                    latex_export = F,
                                    small_size = T)


p2 <- running_time_per_pin_box_plot(list(kahypar_k_g,
                                         kaffpa_strong_opt_g,
                                         kaffpa_strongs_opt_g),
                                    show_infeasible_tick = F,
                                    show_timeout_tick = T,
                                    order = order,
                                    text_angle = 30,
                                    latex_export = F,
                                    small_size = T)
print(p2)

############## Performance Profile Plot ##############

p3 <- performace_plot(list(kahypar_k_f,
                           kaffpa_strong_opt_f,
                           kaffpa_strongs_opt_f),
                      objective = "avg_cut",
                      hide_y_axis_title = F,
                      show_infeasible_tick = F,
                      show_timeout_tick = T,
                      widths = c(3,2,1,1),
                      legend_top_margin = -10,
                      latex_export = F,
                      small_legend = T,
                      small_ticks = T,
                      small_size = T)


p4 <- performace_plot(list(kahypar_k_g,
                           kaffpa_strong_opt_g,
                           kaffpa_strongs_opt_g),
                      objective = "avg_cut",
                      hide_y_axis_title = F,
                      show_infeasible_tick = F,
                      show_timeout_tick = T,
                      widths = c(3,2,1,1),
                      legend_top_margin = -10,
                      latex_export = F,
                      small_legend = T,
                      small_ticks = T,
                      small_size = T)

performance_plot_1 <- egg::ggarrange(p3,p1, widths = c(1,1), ncol = 2,  draw=F)
pdf(file = "img/figure_14.pdf", width = 3 * 2.1666, height = 1.5 * 1.666666) 
print(performance_plot_1)
dev.off()

performance_plot_2 <- egg::ggarrange(p4,p2, widths = c(1,1), ncol = 2,  draw=F)
pdf(file = "img/figure_15.pdf", width = 3 * 2.1666, height = 1.5 * 1.666666) 
print(performance_plot_2)
dev.off()
