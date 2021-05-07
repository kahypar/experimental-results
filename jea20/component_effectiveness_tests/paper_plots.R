source("../functions.R")
source("../plots/plots_common.R")
source("../plots/running_time_box_plot.R")
source("../plots/relative_running_time_plot.R")
source("../plots/performance_profiles.R")

############## SETUP DATA FRAMES ############## 

# Read Data Frames
instances <- read.csv("instances.csv", header = TRUE)
km1_kahypar_k <- aggreg_data(read.csv("km1_kKaHyPar.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
km1_kahypar_k_s <- aggreg_data(read.csv("km1_kkahypar-s.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
km1_kahypar_k_cac_s <- aggreg_data(read.csv("km1_kkahypar-cac-s.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
km1_kahypar_k_f_cac_s <- aggreg_data(read.csv("km1_kkahypar-f-cac-s.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)

# Compute Average Time per Pin
km1_kahypar_k <- compute_avg_time_per_pin(km1_kahypar_k, instances)
km1_kahypar_k_s <- compute_avg_time_per_pin(km1_kahypar_k_s, instances)
km1_kahypar_k_cac_s <- compute_avg_time_per_pin(km1_kahypar_k_cac_s, instances)
km1_kahypar_k_f_cac_s <- compute_avg_time_per_pin(km1_kahypar_k_f_cac_s, instances)

# Set Algorithm Name
km1_kahypar_k$algorithm <- "$k$KaHyPar"
km1_kahypar_k_s$algorithm <- "$k$KaHyPar$-$S"
km1_kahypar_k_cac_s$algorithm <- "$k$KaHyPar$-$CAC$-$S"
km1_kahypar_k_f_cac_s$algorithm <- "$k$KaHyPar$-$F$-$CAC$-$S"

# Specify Colors of Algorithms in Plots
palette <- brewer.pal(n = 9, name = "Set1")
palette[[6]] <- "#AE8D0A"
algo_color_mapping <- c("$k$KaHyPar" = palette[[1]],
                        "$k$KaHyPar$-$S" = palette[[3]],
                        "$k$KaHyPar$-$CAC$-$S" = palette[[5]],
                        "$k$KaHyPar$-$F$-$CAC$-$S" = palette[[7]])

# restrict benchmark set to all instances for which we currently have results
semi_join_filter = semi_join(km1_kahypar_k_s, km1_kahypar_k, by=c('graph','k'))

# apply the semi_join_filter to all data frames
km1_kahypar_k = semi_join(km1_kahypar_k, semi_join_filter, by=c('graph','k'))
km1_kahypar_k$graph <- as.character(km1_kahypar_k$graph)


############## Running Time Box Plot ############## 

scaling <- 1.16

order <- c("$k$KaHyPar", "$k$KaHyPar$-$S", "$k$KaHyPar$-$CAC$-$S", "$k$KaHyPar$-$F$-$CAC$-$S")
tikz("~/kahypar-jea/tikz_plots/km1_kahypar_k_running_time.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(running_time_per_pin_box_plot(list(km1_kahypar_k, 
                                         km1_kahypar_k_s, 
                                         km1_kahypar_k_cac_s, 
                                         km1_kahypar_k_f_cac_s), 
                                    show_infeasible_tick = F,
                                    show_timeout_tick = T,
                                    order = order,
                                    latex_export = T,
                                    small_size = F))
dev.off()

############## Performance Profile Plot (kKaHyPar) ############## 

scaling <- 1.25

tikz("~/kahypar-jea/tikz_plots/km1_kahypar_k_component_quality.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(performace_plot(list(km1_kahypar_k, 
                           km1_kahypar_k_s,
                           km1_kahypar_k_cac_s,
                           km1_kahypar_k_f_cac_s), 
                      objective = "avg_km1", 
                      hide_y_axis_title = F,
                      show_infeasible_tick = F,
                      show_timeout_tick = T,
                      widths = c(3,2,1,1),
                      legend_top_margin = -10,
                      latex_export = T,
                      small_size = F))
dev.off()
