source("../functions.R")
source("../plots/plots_common.R")
source("../plots/running_time_box_plot.R")
source("../plots/relative_running_time_plot.R")
source("../plots/performance_profiles.R")

############## SETUP DATA FRAMES ############## 

# Read Data Frames
kahypar_k <- aggreg_data(read.csv("km1_kKaHyPar.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
kahypar_e <- aggreg_data(read.csv("km1_kKaHyPar_E.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
patoh_d <- aggreg_data(read.csv("km1_patoh_d.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
patoh_q <- aggreg_data(read.csv("km1_patoh_q.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
hmetis_r <- aggreg_data(read.csv("km1_hmetis-r.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
hmetis_k <- aggreg_data(read.csv("km1_hmetis-k.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)

# Set Algorithm Name
kahypar_k$algorithm <- "$k$KaHyPar"
kahypar_e$algorithm <- "$k$KaHyPar-E"
patoh_d$algorithm <- "PaToH-D"
patoh_q$algorithm <- "PaToH-Q"
hmetis_r$algorithm <- "hMETIS-R"
hmetis_k$algorithm <- "hMETIS-K"

# Specify Colors of Algorithms in Plots
palette <- brewer.pal(n = 9, name = "Set1")
palette[[6]] <- "#AE8D0A"
algo_color_mapping <- c("$k$KaHyPar" = palette[[1]],
                        "$k$KaHyPar-E" = "#333333",
                        "PaToH-D" = palette[[3]],
                        "PaToH-Q" = palette[[4]],
                        "hMETIS-R" = palette[[5]],
                        "hMETIS-K" = palette[[6]])

############## Performance Profile Plot (ALL) ############## 

scaling <- 1.25

tikz("~/kahypar-jea/tikz_plots/repeated_executions_min_km1_overall.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(performace_plot(list(kahypar_e,
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
                      legend_top_margin = -5,
                      latex_export = T,
                      small_size = F))
dev.off()

kahypar_k_1 <- read.csv("km1_kKaHyPar.csv", header = TRUE)
kahypar_k_1 <- kahypar_k_1[kahypar_k_1$seed == 1,]
kahypar_k_1 <- aggreg_data(kahypar_k_1, timelimit = 30000, epsilon = 0.03)
kahypar_k_1$algorithm <- "$k$KaHyPar"

tikz("~/kahypar-jea/tikz_plots/repeated_executions_min_km1_overall_1.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(performace_plot(list(kahypar_k_1,
                           patoh_d,
                           patoh_q,
                           hmetis_r,
                           hmetis_k), 
                      objective = "min_km1", 
                      hide_y_axis_title = F,
                      show_infeasible_tick = T,
                      show_timeout_tick = F,
                      widths = c(3,2,1,1),
                      legend_top_margin = -5,
                      latex_export = T,
                      small_size = F))
dev.off()

