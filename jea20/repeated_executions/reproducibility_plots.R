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
kahypar_k <- aggreg_data(read.csv("repeated_executions/km1_kKaHyPar.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
kahypar_e <- aggreg_data(read.csv("repeated_executions/km1_kKaHyPar_E.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
patoh_d <- aggreg_data(read.csv("repeated_executions/km1_patoh_d.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
patoh_q <- aggreg_data(read.csv("repeated_executions/km1_patoh_q.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
hmetis_r <- aggreg_data(read.csv("repeated_executions/km1_hmetis-r.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
hmetis_k <- aggreg_data(read.csv("repeated_executions/km1_hmetis-k.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)

# Set Algorithm Name
kahypar_k$algorithm <- "kKaHyPar"
kahypar_e$algorithm <- "kKaHyPar-E"
patoh_d$algorithm <- "PaToH-D"
patoh_q$algorithm <- "PaToH-Q"
hmetis_r$algorithm <- "hMETIS-R"
hmetis_k$algorithm <- "hMETIS-K"

# Specify Colors of Algorithms in Plots
palette <- brewer.pal(n = 9, name = "Set1")
palette[[6]] <- "#AE8D0A"
algo_color_mapping <- c("kKaHyPar" = palette[[1]],
                        "kKaHyPar-E" = "#333333",
                        "PaToH-D" = palette[[3]],
                        "PaToH-Q" = palette[[4]],
                        "hMETIS-R" = palette[[5]],
                        "hMETIS-K" = palette[[6]])

############## Performance Profile Plot (ALL) ############## 

scaling <- 1.29

p1 <- performace_plot(list(kahypar_e,
                           kahypar_k,
                           patoh_d,
                           patoh_q,
                           hmetis_r,
                           hmetis_k), 
                      objective = "min_km1",
                      hide_y_axis_title = F,
                      show_infeasible_tick = T,
                      show_timeout_tick = F,
                      widths = c(3,2,1,1),
                      legend_top_margin = -10,
                      latex_export = F,
                      small_legend = T,
                      small_ticks = T,
                      small_size = T)

kahypar_k_1 <- read.csv("repeated_executions/km1_kKaHyPar.csv", header = TRUE)
kahypar_k_1 <- kahypar_k_1[kahypar_k_1$seed == 1,]
kahypar_k_1 <- aggreg_data(kahypar_k_1, timelimit = 30000, epsilon = 0.03)
kahypar_k_1$algorithm <- "kKaHyPar"

p2 <- performace_plot(list(kahypar_k_1,
                           patoh_d,
                           patoh_q,
                           hmetis_r,
                           hmetis_k), 
                      objective = "min_km1",
                      hide_y_axis_title = F,
                      show_infeasible_tick = T,
                      show_timeout_tick = F,
                      widths = c(3,2,1,1),
                      legend_top_margin = -10,
                      latex_export = F,
                      small_legend = T,
                      small_ticks = T,
                      small_size = T)


performance_plot <- egg::ggarrange(p1,p2, widths = c(1,1), ncol = 2,  draw=F)
pdf(file = "img/figure_11.pdf", width = 3 * 2.1666, height = 1.5 * 1.666666) 
print(performance_plot)
dev.off()
