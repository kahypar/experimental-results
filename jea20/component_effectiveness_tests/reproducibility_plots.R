args = commandArgs(trailingOnly=TRUE)
working_dir <- args[[1]] # "/home/tobias/journal-experiments/experimental-results/jea20"
setwd(working_dir)

source("functions_reproducibility.R")
source("plots/new/plots_common.R")
source("plots/new/running_time_box_plot.R")
source("plots/new/relative_running_time_plot.R")
source("plots/new/performance_profiles.R")
source("plots/new/effectiveness_tests.R")

############## SETUP DATA FRAMES ##############

# Specify Colors of Algorithms in Plots
palette <- brewer.pal(n = 9, name = "Set1")
palette[[6]] <- "#AE8D0A"
algo_color_mapping <- c("kKaHyPar" = palette[[1]],
                        "kKaHyPar-S" = palette[[3]],
                        "kKaHyPar-CAC-S" = palette[[5]],
                        "kKaHyPar-F-CAC-S" = palette[[7]])

# Precomputed Effectiveness Tests
eff_km1_kahypar_k_s <- read.csv("component_effectiveness_tests/effectiveness_tests/km1_kKahypar_s.csv", header = T)
eff_km1_kahypar_k_cac_s <- read.csv("component_effectiveness_tests/effectiveness_tests/km1_kKahypar_cac_s.csv", header = T)
eff_km1_kahypar_k_f_cac_s <- read.csv("component_effectiveness_tests/effectiveness_tests/km1_kKahypar_f_cac_s.csv", header = T)

p1 <- effectivenessTestPerformanceProfile(eff_km1_kahypar_k_s, 
                                          "$k$KaHyPar", "kKaHyPar",
                                          "$k$KaHyPar$-$S", "kKaHyPar-S",
                                          objective = "min_km1", 
                                          hide_y_axis_title = F,
                                          show_infeasible_tick = F,
                                          show_timeout_tick = T,
                                          widths = c(4,2,1),
                                          latex_export = F,
                                          small_legend = T,
                                          small_ticks = T,
                                          small_size = T)

p2 <- effectivenessTestPerformanceProfile(eff_km1_kahypar_k_cac_s, 
                                          "$k$KaHyPar", "kKaHyPar",
                                          "$k$KaHyPar$-$CAC$-$S", "kKaHyPar-CAC-S",
                                          objective = "min_km1", 
                                          hide_y_axis_title = T,
                                          show_infeasible_tick = F,
                                          show_timeout_tick = T,
                                          widths = c(4,2,1),
                                          latex_export = F,
                                          small_legend = T,
                                          small_ticks = T,
                                          small_size = T)

p3 <- effectivenessTestPerformanceProfile(eff_km1_kahypar_k_f_cac_s, 
                                          "$k$KaHyPar", "kKaHyPar",
                                          "$k$KaHyPar$-$F$-$CAC$-$S", "kKaHyPar-F-CAC-S",
                                          objective = "min_km1", 
                                          hide_y_axis_title = T,
                                          show_infeasible_tick = F,
                                          show_timeout_tick = T,
                                          widths = c(4,2,1),
                                          latex_export = F,
                                          small_legend = T,
                                          small_ticks = T,
                                          small_size = T)

eff_plot <- egg::ggarrange(p1,p2,p3, widths = c(1,1,1), ncol = 3,  draw=F)
pdf(file = "img/figure_8.pdf", width = 3 * 2.1666, height = 1.5 * 1.666666) 
print(eff_plot)
dev.off()

############## SETUP DATA FRAMES ##############

# Read Data Frames
instances <- read.csv("component_effectiveness_tests/instances.csv", header = TRUE)
km1_kahypar_k <- aggreg_data(read.csv("component_effectiveness_tests/km1_kKaHyPar.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
km1_kahypar_k_s <- aggreg_data(read.csv("component_effectiveness_tests/km1_kKahypar-s.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
km1_kahypar_k_cac_s <- aggreg_data(read.csv("component_effectiveness_tests/km1_kKahypar-cac-s.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
km1_kahypar_k_f_cac_s <- aggreg_data(read.csv("component_effectiveness_tests/km1_kKahypar-f-cac-s.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)

# Set Algorithm Name
km1_kahypar_k$algorithm <- "kKaHyPar"
km1_kahypar_k_s$algorithm <- "kKaHyPar-S"
km1_kahypar_k_cac_s$algorithm <- "kKaHyPar-CAC-S"
km1_kahypar_k_f_cac_s$algorithm <- "kKaHyPar-F-CAC-S"

# Compute Average Time per Pin
km1_kahypar_k <- compute_avg_time_per_pin(km1_kahypar_k, instances)
km1_kahypar_k_s <- compute_avg_time_per_pin(km1_kahypar_k_s, instances)
km1_kahypar_k_cac_s <- compute_avg_time_per_pin(km1_kahypar_k_cac_s, instances)
km1_kahypar_k_f_cac_s <- compute_avg_time_per_pin(km1_kahypar_k_f_cac_s, instances)

# restrict benchmark set to all instances for which we currently have results
semi_join_filter = semi_join(km1_kahypar_k_s, km1_kahypar_k, by=c('graph','k'))

# apply the semi_join_filter to all data frames
km1_kahypar_k = semi_join(km1_kahypar_k, semi_join_filter, by=c('graph','k'))
km1_kahypar_k$graph <- as.character(km1_kahypar_k$graph)


############## Running Time Box Plot ##############

order <- c("kKaHyPar", "kKaHyPar-S", "kKaHyPar-CAC-S", "kKaHyPar-F-CAC-S")
p4 <- running_time_per_pin_box_plot(list(km1_kahypar_k,
                                         km1_kahypar_k_s,
                                         km1_kahypar_k_cac_s,
                                         km1_kahypar_k_f_cac_s),
                                    show_infeasible_tick = F,
                                    show_timeout_tick = T,
                                    order = order,
                                    latex_export = F,
                                    small_size = T)

p5 <- performace_plot(list(km1_kahypar_k,
                           km1_kahypar_k_s,
                           km1_kahypar_k_cac_s,
                           km1_kahypar_k_f_cac_s),
                      objective = "avg_km1",
                      hide_y_axis_title = F,
                      show_infeasible_tick = F,
                      show_timeout_tick = T,
                      widths = c(3,2,1,1),
                      legend_top_margin = -10,
                      latex_export = F,
                      small_legend = T,
                      small_ticks = T,
                      small_size = T)

performance_plot <- egg::ggarrange(p5,p4, widths = c(1,1), ncol = 2,  draw=F)
pdf(file = "img/figure_7.pdf", width = 3 * 2.1666, height = 1.5 * 1.666666) 
print(performance_plot)
dev.off()
