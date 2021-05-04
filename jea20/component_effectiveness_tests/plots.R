source("../functions.R")
source("../plots/plots_common.R")
source("../plots/running_time_box_plot.R")
source("../plots/relative_running_time_plot.R")
source("../plots/performance_profiles.R")

############## SETUP DATA FRAMES ############## 

# Read Data Frames
cut_kahypar_r <- aggreg_data(read.csv("cut_rKaHyPar.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
cut_kahypar_r_s <- aggreg_data(read.csv("cut_rkahypar-s.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
cut_kahypar_r_cac_s <- aggreg_data(read.csv("cut_rkahypar-cac-s.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
cut_kahypar_r_f_cac_s <- aggreg_data(read.csv("cut_rkahypar-f-cac-s.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
km1_kahypar_r <- aggreg_data(read.csv("km1_rKaHyPar.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
km1_kahypar_r_s <- aggreg_data(read.csv("km1_rkahypar-s.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
km1_kahypar_r_cac_s <- aggreg_data(read.csv("km1_rkahypar-cac-s.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
km1_kahypar_r_f_cac_s <- aggreg_data(read.csv("km1_rkahypar-f-cac-s.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
cut_kahypar_k <- aggreg_data(read.csv("cut_kKaHyPar.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
cut_kahypar_k_s <- aggreg_data(read.csv("cut_kkahypar-s.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
cut_kahypar_k_cac_s <- aggreg_data(read.csv("cut_kkahypar-cac-s.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
cut_kahypar_k_f_cac_s <- aggreg_data(read.csv("cut_kkahypar-f-cac-s.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
km1_kahypar_k <- aggreg_data(read.csv("km1_kKaHyPar.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
km1_kahypar_k_s <- aggreg_data(read.csv("km1_kkahypar-s.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
km1_kahypar_k_cac_s <- aggreg_data(read.csv("km1_kkahypar-cac-s.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
km1_kahypar_k_f_cac_s <- aggreg_data(read.csv("km1_kkahypar-f-cac-s.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)

# Set Algorithm Name
cut_kahypar_r$algorithm <- "$r$KaHyPar"
cut_kahypar_r_s$algorithm <- "$r$KaHyPar$-$S"
cut_kahypar_r_cac_s$algorithm <- "$r$KaHyPar$-$CAC$-$S"
cut_kahypar_r_f_cac_s$algorithm <- "$r$KaHyPar$-$F$-$CAC$-$S"
km1_kahypar_r$algorithm <- "$r$KaHyPar"
km1_kahypar_r_s$algorithm <- "$r$KaHyPar$-$S"
km1_kahypar_r_cac_s$algorithm <- "$r$KaHyPar$-$CAC$-$S"
km1_kahypar_r_f_cac_s$algorithm <- "$r$KaHyPar$-$F$-$CAC$-$S"
cut_kahypar_k$algorithm <- "$k$KaHyPar"
cut_kahypar_k_s$algorithm <- "$k$KaHyPar$-$S"
cut_kahypar_k_cac_s$algorithm <- "$k$KaHyPar$-$CAC$-$S"
cut_kahypar_k_f_cac_s$algorithm <- "$k$KaHyPar$-$F$-$CAC$-$S"
km1_kahypar_k$algorithm <- "$k$KaHyPar"
km1_kahypar_k_s$algorithm <- "$k$KaHyPar$-$S"
km1_kahypar_k_cac_s$algorithm <- "$k$KaHyPar$-$CAC$-$S"
km1_kahypar_k_f_cac_s$algorithm <- "$k$KaHyPar$-$F$-$CAC$-$S"

# Specify Colors of Algorithms in Plots
palette <- brewer.pal(n = 9, name = "Set1")
palette[[6]] <- "#AE8D0A"
algo_color_mapping <- c("$k$KaHyPar" = palette[[1]],
                        "$r$KaHyPar" = palette[[2]],
                        "$k$KaHyPar$-$S" = palette[[3]],
                        "$r$KaHyPar$-$S" = palette[[4]],
                        "$k$KaHyPar$-$CAC$-$S" = palette[[5]],
                        "$r$KaHyPar$-$CAC$-$S" = palette[[6]],
                        "$k$KaHyPar$-$F$-$CAC$-$S" = palette[[7]],
                        "$r$KaHyPar$-$F$-$CAC$-$S" = palette[[8]])

# restrict benchmark set to all instances for which we currently have results
semi_join_filter = semi_join(cut_kahypar_r_s, cut_kahypar_r, by=c('graph','k'))
semi_join_filter = semi_join(semi_join_filter, km1_kahypar_r, by=c('graph','k'))
semi_join_filter = semi_join(semi_join_filter, cut_kahypar_k, by=c('graph','k'))
semi_join_filter = semi_join(semi_join_filter, km1_kahypar_k, by=c('graph','k'))

# apply the semi_join_filter to all data frames
cut_kahypar_r = semi_join(cut_kahypar_r, semi_join_filter, by=c('graph','k'))
km1_kahypar_r = semi_join(km1_kahypar_r, semi_join_filter, by=c('graph','k'))
cut_kahypar_k = semi_join(cut_kahypar_k, semi_join_filter, by=c('graph','k'))
km1_kahypar_k = semi_join(km1_kahypar_k, semi_join_filter, by=c('graph','k'))

cut_kahypar_r$graph <- as.character(cut_kahypar_r$graph)
km1_kahypar_r$graph <- as.character(km1_kahypar_r$graph)
cut_kahypar_k$graph <- as.character(cut_kahypar_k$graph)
km1_kahypar_k$graph <- as.character(km1_kahypar_k$graph)


############## Running Time Box Plot ############## 

scaling <- 1.25

order <- c("$r$KaHyPar", "$r$KaHyPar$-$S", "$r$KaHyPar$-$CAC$-$S", "$r$KaHyPar$-$F$-$CAC$-$S")
tikz("~/kahypar-jea/tikz_plots/cut_kahypar_r_running_time.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(running_time_box_plot(list(cut_kahypar_r, 
                                 cut_kahypar_r_s, 
                                 cut_kahypar_r_cac_s, 
                                 cut_kahypar_r_f_cac_s), 
                            show_infeasible_tick = T,
                            show_timeout_tick = T,
                            order = order,
                            latex_export = T,
                            small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/km1_kahypar_r_running_time.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(running_time_box_plot(list(km1_kahypar_r, 
                                 km1_kahypar_r_s, 
                                 km1_kahypar_r_cac_s, 
                                 km1_kahypar_r_f_cac_s), 
                            show_infeasible_tick = T,
                            show_timeout_tick = T,
                            order = order,
                            latex_export = T,
                            small_size = F))
dev.off()

order <- c("$k$KaHyPar", "$k$KaHyPar$-$S", "$k$KaHyPar$-$CAC$-$S", "$k$KaHyPar$-$F$-$CAC$-$S")
tikz("~/kahypar-jea/tikz_plots/cut_kahypar_k_running_time.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(running_time_box_plot(list(cut_kahypar_k, 
                                 cut_kahypar_k_s, 
                                 cut_kahypar_k_cac_s, 
                                 cut_kahypar_k_f_cac_s), 
                            show_infeasible_tick = T,
                            show_timeout_tick = T,
                            order = order,
                            latex_export = T,
                            small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/km1_kahypar_k_running_time.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(running_time_box_plot(list(km1_kahypar_k, 
                                 km1_kahypar_k_s, 
                                 km1_kahypar_k_cac_s, 
                                 km1_kahypar_k_f_cac_s), 
                            show_infeasible_tick = F,
                            show_timeout_tick = T,
                            order = order,
                            latex_export = T,
                            small_size = F))
dev.off()


  ############## Performance Profile Plot (rKaHyPar) ############## 

scaling <- 1.25

tikz("~/kahypar-jea/tikz_plots/cut_kahypar_r_vs_kahypar_r_s.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(performace_plot(list(cut_kahypar_r, 
                          cut_kahypar_r_s), 
                objective = "avg_cut", 
                hide_y_axis_title = F,
                show_infeasible_tick = F,
                show_timeout_tick = F,
                widths = c(3,2,1),
                legend_top_margin = -20,
                latex_export = T,
                small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/km1_kahypar_r_vs_kahypar_r_s.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(performace_plot(list(km1_kahypar_r, 
                           km1_kahypar_r_s), 
                      objective = "avg_km1", 
                      hide_y_axis_title = F,
                      show_infeasible_tick = F,
                      show_timeout_tick = F,
                      widths = c(3,2,1),
                      legend_top_margin = -20,
                      latex_export = T,
                      small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/cut_kahypar_r_vs_kahypar_r_cac_s.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(performace_plot(list(cut_kahypar_r, 
                           cut_kahypar_r_cac_s), 
                      objective = "avg_cut", 
                      hide_y_axis_title = F,
                      show_infeasible_tick = F,
                      show_timeout_tick = F,
                      widths = c(3,2,1),
                      legend_top_margin = -20,
                      latex_export = T,
                      small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/km1_kahypar_r_vs_kahypar_r_cac_s.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(performace_plot(list(km1_kahypar_r, 
                           km1_kahypar_r_cac_s), 
                      objective = "avg_km1", 
                      hide_y_axis_title = F,
                      show_infeasible_tick = F,
                      show_timeout_tick = F,
                      widths = c(3,2,1),
                      legend_top_margin = -20,
                      latex_export = T,
                      small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/cut_kahypar_r_vs_kahypar_r_f_cac_s.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(performace_plot(list(cut_kahypar_r, 
                           cut_kahypar_r_f_cac_s), 
                      objective = "avg_cut", 
                      hide_y_axis_title = F,
                      show_infeasible_tick = F,
                      show_timeout_tick = F,
                      widths = c(3,2,1),
                      legend_top_margin = -20,
                      latex_export = T,
                      small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/km1_kahypar_r_vs_kahypar_r_f_cac_s.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(performace_plot(list(km1_kahypar_r, 
                           km1_kahypar_r_f_cac_s), 
                      objective = "avg_km1", 
                      hide_y_axis_title = F,
                      show_infeasible_tick = F,
                      show_timeout_tick = F,
                      widths = c(3,2,1),
                      legend_top_margin = -20,
                      latex_export = T,
                      small_size = F))
dev.off()


############## Performance Profile Plot (kKaHyPar) ############## 

scaling <- 1.25

tikz("~/kahypar-jea/tikz_plots/cut_kahypar_k_vs_kahypar_k_s.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(performace_plot(list(cut_kahypar_k, 
                           cut_kahypar_k_s), 
                      objective = "avg_cut", 
                      hide_y_axis_title = F,
                      show_infeasible_tick = F,
                      show_timeout_tick = F,
                      widths = c(3,2,1),
                      legend_top_margin = -20,
                      latex_export = T,
                      small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/km1_kahypar_k_vs_kahypar_k_s.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(performace_plot(list(km1_kahypar_k, 
                           km1_kahypar_k_s), 
                      objective = "avg_km1", 
                      hide_y_axis_title = F,
                      show_infeasible_tick = F,
                      show_timeout_tick = T,
                      widths = c(3,2,1,1),
                      legend_top_margin = -20,
                      latex_export = T,
                      small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/cut_kahypar_k_vs_kahypar_k_cac_s.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(performace_plot(list(cut_kahypar_k, 
                           cut_kahypar_k_cac_s), 
                      objective = "avg_cut", 
                      hide_y_axis_title = F,
                      show_infeasible_tick = F,
                      show_timeout_tick = F,
                      widths = c(3,2,1),
                      legend_top_margin = -20,
                      latex_export = T,
                      small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/km1_kahypar_k_vs_kahypar_k_cac_s.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(performace_plot(list(km1_kahypar_k, 
                           km1_kahypar_k_cac_s), 
                      objective = "avg_km1", 
                      hide_y_axis_title = F,
                      show_infeasible_tick = F,
                      show_timeout_tick = T,
                      widths = c(3,2,1,1),
                      legend_top_margin = -20,
                      latex_export = T,
                      small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/cut_kahypar_k_vs_kahypar_k_f_cac_s.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(performace_plot(list(cut_kahypar_k, 
                           cut_kahypar_k_f_cac_s), 
                      objective = "avg_cut", 
                      hide_y_axis_title = F,
                      show_infeasible_tick = F,
                      show_timeout_tick = F,
                      widths = c(3,2,1),
                      legend_top_margin = -20,
                      latex_export = T,
                      small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/km1_kahypar_k_vs_kahypar_k_f_cac_s.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(performace_plot(list(km1_kahypar_k, 
                           km1_kahypar_k_f_cac_s), 
                      objective = "avg_km1", 
                      hide_y_axis_title = F,
                      show_infeasible_tick = F,
                      show_timeout_tick = T,
                      widths = c(3,2,1,1),
                      legend_top_margin = -20,
                      latex_export = T,
                      small_size = F))
dev.off()


