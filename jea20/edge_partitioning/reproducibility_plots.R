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
instances <- read.csv("edge_partitioning/instances.csv", header = TRUE)
kahypar_k <- aggreg_data(read.csv("edge_partitioning/km1_kKaHyPar.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
patoh_d <- aggreg_data(read.csv("edge_partitioning/km1_patoh_d.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
patoh_q <- aggreg_data(read.csv("edge_partitioning/km1_patoh_q.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
hmetis_r <- aggreg_data(read.csv("edge_partitioning/km1_hmetis-r.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
hmetis_k <- aggreg_data(read.csv("edge_partitioning/km1_hmetis-k.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
zoltan_alg_d <- aggreg_data(read.csv("edge_partitioning/km1_zoltan-algd.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
mondriaan <- aggreg_data(read.csv("edge_partitioning/km1_mondriaan.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
mondriaan <- mondriaan %>% mutate(infeasible = ifelse(failed == TRUE, TRUE, FALSE))
hype <- aggreg_data(read.csv("edge_partitioning/km1_hype.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)

# Compute Average Time per Pin
kahypar_k <- compute_avg_time_per_pin(kahypar_k, instances)
patoh_d <- compute_avg_time_per_pin(patoh_d, instances)
patoh_q <- compute_avg_time_per_pin(patoh_q, instances)
hmetis_r <- compute_avg_time_per_pin(hmetis_r, instances)
hmetis_k <- compute_avg_time_per_pin(hmetis_k, instances)
zoltan_alg_d <- compute_avg_time_per_pin(zoltan_alg_d, instances)
mondriaan <- compute_avg_time_per_pin(mondriaan, instances)
hype <- compute_avg_time_per_pin(hype, instances)

# Set Algorithm Name
kahypar_k$algorithm <- "kKaHyPar"
patoh_d$algorithm <- "PaToH-D"
patoh_q$algorithm <- "PaToH-Q"
hmetis_r$algorithm <- "hMETIS-R"
hmetis_k$algorithm <- "hMETIS-K"
zoltan_alg_d$algorithm <- "Zoltan-AlgD"
mondriaan$algorithm <- "Mondriaan"
hype$algorithm <- "HYPE"

# Specify Colors of Algorithms in Plots
palette <- brewer.pal(n = 9, name = "Set1")
palette[[6]] <- "#AE8D0A"
algo_color_mapping <- c("kKaHyPar" = palette[[1]],
                        "PaToH-D" = palette[[3]],
                        "PaToH-Q" = palette[[4]],
                        "hMETIS-R" = palette[[5]],
                        "hMETIS-K" = palette[[6]],
                        "Zoltan-AlgD" = palette[[7]],
                        "Mondriaan" = palette[[8]],
                        "HYPE" = palette[[9]])

############## Running Time Box Plot ##############


order <- c("HYPE", "PaToH-D", "PaToH-Q", "Mondriaan", "Zoltan-AlgD", "hMETIS-R", "hMETIS-K", "kKaHyPar")
p1 <- running_time_per_pin_box_plot(list(kahypar_k,
                                         patoh_d,
                                         patoh_q,
                                         hmetis_r,
                                         hmetis_k,
                                         zoltan_alg_d,
                                         mondriaan,
                                         hype),
                                    show_infeasible_tick = T,
                                    show_timeout_tick = T,
                                    order = order,
                                    text_angle = 30,
                                    latex_export = F,
                                    small_size = T)

############## Performance Profile Plot (ALL) ##############

p2 <- performace_plot(list(kahypar_k,
                           patoh_d,
                           patoh_q,
                           hmetis_r,
                           hmetis_k,
                           zoltan_alg_d,
                           mondriaan,
                           hype),
                      objective = "avg_km1",
                      hide_y_axis_title = F,
                      show_infeasible_tick = T,
                      show_timeout_tick = F,
                      widths = c(3,2,1,1),
                      legend_top_margin = -10,
                      latex_export = F,
                      small_legend = T,
                      small_ticks = T,
                      small_size = T)

performance_plot <- egg::ggarrange(p2,p1, widths = c(1,1), ncol = 2,  draw=F)
pdf(file = "img/figure_13.pdf", width = 3 * 2.1666, height = 1.5 * 1.666666) 
print(performance_plot)
dev.off()
