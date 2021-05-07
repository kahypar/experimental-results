source("../functions.R")
source("../plots/plots_common.R")
source("../plots/running_time_box_plot.R")
source("../plots/relative_running_time_plot.R")
source("../plots/performance_profiles.R")

############## SETUP DATA FRAMES ############## 

# Specify Colors of Algorithms in Plots
palette <- brewer.pal(n = 9, name = "Set1")
palette[[6]] <- "#AE8D0A"
algo_color_mapping <- c("$k$KaHyPar" = palette[[1]],
                        "$k$KaHyPar$-$S" = palette[[3]],
                        "$k$KaHyPar$-$CAC$-$S" = palette[[5]],
                        "$k$KaHyPar$-$F$-$CAC$-$S" = palette[[7]])

# Precomputed Effectiveness Tests
eff_km1_kahypar_k_s <- read.csv("effectiveness_tests/km1_kahypar_k_s.csv", header = T)
eff_km1_kahypar_k_cac_s <- read.csv("effectiveness_tests/km1_kahypar_k_cac_s.csv", header = T)
eff_km1_kahypar_k_f_cac_s <- read.csv("effectiveness_tests/km1_kahypar_k_f_cac_s.csv", header = T)

scaling <- 1

tikz("~/kahypar-jea/tikz_plots/eff_km1_kahypar_k_s.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(effectivenessTestPerformanceProfile(eff_km1_kahypar_k_s, 
                                          "$k$KaHyPar",
                                          "$k$KaHyPar$-$S",
                                          objective = "min_km1", 
                                          hide_y_axis_title = F,
                                          show_infeasible_tick = F,
                                          show_timeout_tick = F,
                                          widths = c(3,2,1),
                                          legend_top_margin = -20,
                                          latex_export = T,
                                          small_size = T))
dev.off()

tikz("~/kahypar-jea/tikz_plots/eff_km1_kahypar_k_cac_s.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(effectivenessTestPerformanceProfile(eff_km1_kahypar_k_cac_s, 
                                          "$k$KaHyPar",
                                          "$k$KaHyPar$-$CAC$-$S",
                                          objective = "min_km1", 
                                          hide_y_axis_title = T,
                                          show_infeasible_tick = F,
                                          show_timeout_tick = F,
                                          widths = c(3,2,1),
                                          legend_top_margin = -20,
                                          latex_export = T,
                                          small_size = T))
dev.off()

tikz("~/kahypar-jea/tikz_plots/eff_km1_kahypar_k_f_cac_s.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(effectivenessTestPerformanceProfile(eff_km1_kahypar_k_f_cac_s, 
                                          "$k$KaHyPar",
                                          "$k$KaHyPar$-$F$-$CAC$-$S",
                                          objective = "min_km1", 
                                          hide_y_axis_title = T,
                                          show_infeasible_tick = F,
                                          show_timeout_tick = F,
                                          widths = c(3,2,1),
                                          legend_top_margin = -20,
                                          latex_export = T,
                                          small_size = T))
dev.off()

