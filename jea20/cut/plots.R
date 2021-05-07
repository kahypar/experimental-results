source("../functions.R")
source("../plots/plots_common.R")
source("../plots/running_time_box_plot.R")
source("../plots/relative_running_time_plot.R")
source("../plots/performance_profiles.R")

############## SETUP DATA FRAMES ############## 

# Read Data Frames
instances <- read.csv("instances.csv", header = TRUE)
kahypar_r <- aggreg_data(read.csv("cut_rKaHyPar.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
kahypar_k <- aggreg_data(read.csv("cut_kKaHyPar.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
patoh_d <- aggreg_data(read.csv("cut_patoh_d.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
patoh_q <- aggreg_data(read.csv("cut_patoh_q.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
hmetis_r <- aggreg_data(read.csv("cut_hmetis-r.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
hmetis_k <- aggreg_data(read.csv("cut_hmetis-k.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
zoltan_alg_d <- aggreg_data(read.csv("cut_zoltan-algd.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
mondriaan <- aggreg_data(read.csv("cut_mondriaan.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)

# Compute Average Time per Pin
kahypar_r <- compute_avg_time_per_pin(kahypar_r, instances)
kahypar_k <- compute_avg_time_per_pin(kahypar_k, instances)
patoh_d <- compute_avg_time_per_pin(patoh_d, instances)
patoh_q <- compute_avg_time_per_pin(patoh_q, instances)
hmetis_r <- compute_avg_time_per_pin(hmetis_r, instances)
hmetis_k <- compute_avg_time_per_pin(hmetis_k, instances)
zoltan_alg_d <- compute_avg_time_per_pin(zoltan_alg_d, instances)
mondriaan <- compute_avg_time_per_pin(mondriaan, instances)

# Graph Classes
kahypar_r$type <- as.factor(apply(kahypar_r, 1, function(x) graphclass(x)))
kahypar_k$type <- as.factor(apply(kahypar_k, 1, function(x) graphclass(x)))
patoh_d$type <- as.factor(apply(patoh_d, 1, function(x) graphclass(x)))
patoh_q$type <- as.factor(apply(patoh_q, 1, function(x) graphclass(x)))
hmetis_r$type <- as.factor(apply(hmetis_r, 1, function(x) graphclass(x)))
hmetis_k$type <- as.factor(apply(hmetis_k, 1, function(x) graphclass(x)))
zoltan_alg_d$type <- as.factor(apply(zoltan_alg_d, 1, function(x) graphclass(x)))
mondriaan$type <- as.factor(apply(mondriaan, 1, function(x) graphclass(x)))

# Set Algorithm Name
kahypar_r$algorithm <- "$r$KaHyPar"
kahypar_k$algorithm <- "$k$KaHyPar"
patoh_d$algorithm <- "PaToH-D"
patoh_q$algorithm <- "PaToH-Q"
hmetis_r$algorithm <- "hMETIS-R"
hmetis_k$algorithm <- "hMETIS-K"
zoltan_alg_d$algorithm <- "Zoltan-AlgD"
mondriaan$algorithm <- "Mondriaan"

# Specify Colors of Algorithms in Plots
palette <- brewer.pal(n = 9, name = "Set1")
palette[[6]] <- "#AE8D0A"
algo_color_mapping <- c("$k$KaHyPar" = palette[[1]],
                        "$r$KaHyPar" = palette[[2]],
                        "PaToH-D" = palette[[3]],
                        "PaToH-Q" = palette[[4]],
                        "hMETIS-R" = palette[[5]],
                        "hMETIS-K" = palette[[6]],
                        "Zoltan-AlgD" = palette[[7]],
                        "Mondriaan" = palette[[8]])

############## Trade-Off Plot ##############

order <- c("$r$KaHyPar", "hMETIS-R", "hMETIS-K", "PaToH-D", "PaToH-Q", "Mondriaan", "Zoltan-AlgD")
scaling <- 2.5
tikz("~/kahypar-jea/tikz_plots/cut_tradeoff.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(tradeoff_plot(list(kahypar_r, 
                         patoh_d,
                         patoh_q,
                         hmetis_r,
                         hmetis_k,
                         zoltan_alg_d,
                         mondriaan),
                    relative_to = kahypar_k,
                    objective = "avg_cut",
                    order = order,
                    ncol = 3,
                    legend_col = 4,
                    latex_export = T,
                    small_size = F
))
dev.off()

  ############## Running Time Box Plot ############## 

scaling <- 1.25

order <- c("PaToH-D", "PaToH-Q", "Mondriaan", "Zoltan-AlgD", "hMETIS-R", "hMETIS-K", "$k$KaHyPar", "$r$KaHyPar")
tikz("~/kahypar-jea/tikz_plots/cut_running_time_overall.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(running_time_per_pin_box_plot(list(kahypar_k,
                                         kahypar_r, 
                                         patoh_d,
                                         patoh_q,
                                         hmetis_r,
                                         hmetis_k,
                                         zoltan_alg_d,
                                         mondriaan), 
                                    show_infeasible_tick = T,
                                    show_timeout_tick = T,
                                    order = order,
                                    use_sampling = T,
                                    sampling_factor = 0.5,
                                    text_angle = 30,
                                    latex_export = T,
                                    small_size = F))
dev.off()

############## Running Time Box Plot (Instance Types) ############## 

scaling <- 1.3

order <- c("PaToH-D", "PaToH-Q", "Mondriaan", "Zoltan-AlgD", "hMETIS-R", "hMETIS-K", "$k$KaHyPar", "$r$KaHyPar")
tikz("~/kahypar-jea/tikz_plots/cut_running_time_dac.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
type <- "DAC"
print(running_time_per_pin_box_plot(list(kahypar_k[kahypar_k$type == type,],
                                         kahypar_r[kahypar_r$type == type,], 
                                         patoh_d[patoh_d$type == type,],
                                         patoh_q[patoh_q$type == type,],
                                         hmetis_r[hmetis_r$type == type,],
                                         hmetis_k[hmetis_k$type == type,],
                                         zoltan_alg_d[zoltan_alg_d$type == type,],
                                         mondriaan[mondriaan$type == type,]), 
                                    show_infeasible_tick = T,
                                    show_timeout_tick = T,
                                    order = order,
                                    title = "$\\ocut$ -- \\DAC",
                                    text_angle = 30,
                                    show_legend = F,
                                    latex_export = T,
                                    small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/cut_running_time_ispd.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
type <- "ISPD"
print(running_time_per_pin_box_plot(list(kahypar_k[kahypar_k$type == type,],
                                         kahypar_r[kahypar_r$type == type,], 
                                         patoh_d[patoh_d$type == type,],
                                         patoh_q[patoh_q$type == type,],
                                         hmetis_r[hmetis_r$type == type,],
                                         hmetis_k[hmetis_k$type == type,],
                                         zoltan_alg_d[zoltan_alg_d$type == type,],
                                         mondriaan[mondriaan$type == type,]), 
                                    show_infeasible_tick = T,
                                    show_timeout_tick = F,
                                    order = order,
                                    title = "$\\ocut$ -- \\ISPD",
                                    text_angle = 30,
                                    show_legend = F,
                                    latex_export = T,
                                    small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/cut_running_time_primal.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
type <- "Primal"
print(running_time_per_pin_box_plot(list(kahypar_k[kahypar_k$type == type,],
                                         kahypar_r[kahypar_r$type == type,], 
                                         patoh_d[patoh_d$type == type,],
                                         patoh_q[patoh_q$type == type,],
                                         hmetis_r[hmetis_r$type == type,],
                                         hmetis_k[hmetis_k$type == type,],
                                         zoltan_alg_d[zoltan_alg_d$type == type,],
                                         mondriaan[mondriaan$type == type,]), 
                                    show_infeasible_tick = T,
                                    show_timeout_tick = T,
                                    order = order,
                                    title = "$\\ocut$ -- \\Primal",
                                    text_angle = 30,
                                    show_legend = F,
                                    latex_export = T,
                                    small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/cut_running_time_literal.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
type <- "Literal"
print(running_time_per_pin_box_plot(list(kahypar_k[kahypar_k$type == type,],
                                         kahypar_r[kahypar_r$type == type,], 
                                         patoh_d[patoh_d$type == type,],
                                         patoh_q[patoh_q$type == type,],
                                         hmetis_r[hmetis_r$type == type,],
                                         hmetis_k[hmetis_k$type == type,],
                                         zoltan_alg_d[zoltan_alg_d$type == type,],
                                         mondriaan[mondriaan$type == type,]), 
                                    show_infeasible_tick = T,
                                    show_timeout_tick = T,
                                    order = order,
                                    title = "$\\ocut$ -- \\Literal",
                                    text_angle = 30,
                                    show_legend = F,
                                    latex_export = T,
                                    small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/cut_running_time_dual.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
type <- "Dual"
print(running_time_per_pin_box_plot(list(kahypar_k[kahypar_k$type == type,],
                                         kahypar_r[kahypar_r$type == type,], 
                                         patoh_d[patoh_d$type == type,],
                                         patoh_q[patoh_q$type == type,],
                                         hmetis_r[hmetis_r$type == type,],
                                         hmetis_k[hmetis_k$type == type,],
                                         zoltan_alg_d[zoltan_alg_d$type == type,],
                                         mondriaan[mondriaan$type == type,]), 
                                    show_infeasible_tick = T,
                                    show_timeout_tick = T,
                                    order = order,
                                    title = "$\\ocut$ -- \\Dual",
                                    text_angle = 30,
                                    show_legend = T,
                                    latex_export = T,
                                    small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/cut_running_time_spm.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
type <- "SPM"
print(running_time_per_pin_box_plot(list(kahypar_k[kahypar_k$type == type,],
                                         kahypar_r[kahypar_r$type == type,], 
                                         patoh_d[patoh_d$type == type,],
                                         patoh_q[patoh_q$type == type,],
                                         hmetis_r[hmetis_r$type == type,],
                                         hmetis_k[hmetis_k$type == type,],
                                         zoltan_alg_d[zoltan_alg_d$type == type,],
                                         mondriaan[mondriaan$type == type,]), 
                                    show_infeasible_tick = T,
                                    show_timeout_tick = T,
                                    order = order,
                                    title = "$\\ocut$ -- \\SPM",
                                    text_angle = 30,
                                    show_legend = T,
                                    latex_export = T,
                                    small_size = F))
dev.off()

############## Running Time Box Plot (Number of Blocks) ############## 

scaling <- 1.3

order <- c("PaToH-D", "PaToH-Q", "Mondriaan", "Zoltan-AlgD", "hMETIS-R", "hMETIS-K", "$k$KaHyPar", "$r$KaHyPar")
tikz("~/kahypar-jea/tikz_plots/cut_running_time_k_2.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
k <- 2
print(running_time_per_pin_box_plot(list(kahypar_k[kahypar_k$k == k,],
                                         kahypar_r[kahypar_r$k == k,], 
                                         patoh_d[patoh_d$k == k,],
                                         patoh_q[patoh_q$k == k,],
                                         hmetis_r[hmetis_r$k == k,],
                                         hmetis_k[hmetis_k$k == k,],
                                         zoltan_alg_d[zoltan_alg_d$k == k,],
                                         mondriaan[mondriaan$k == k,]), 
                                    show_infeasible_tick = T,
                                    show_timeout_tick = T,
                                    order = order,
                                    title = "$\\ocut$ -- $k = 2$",
                                    text_angle = 30,
                                    show_legend = F,
                                    latex_export = T,
                                    small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/cut_running_time_k_4.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
k <- 4
print(running_time_per_pin_box_plot(list(kahypar_k[kahypar_k$k == k,],
                                         kahypar_r[kahypar_r$k == k,], 
                                         patoh_d[patoh_d$k == k,],
                                         patoh_q[patoh_q$k == k,],
                                         hmetis_r[hmetis_r$k == k,],
                                         hmetis_k[hmetis_k$k == k,],
                                         zoltan_alg_d[zoltan_alg_d$k == k,],
                                         mondriaan[mondriaan$k == k,]), 
                                    show_infeasible_tick = T,
                                    show_timeout_tick = T,
                                    order = order,
                                    title = "$\\ocut$ -- $k = 4$",
                                    text_angle = 30,
                                    show_legend = F,
                                    latex_export = T,
                                    small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/cut_running_time_k_8.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
k <- 8
print(running_time_per_pin_box_plot(list(kahypar_k[kahypar_k$k == k,],
                                         kahypar_r[kahypar_r$k == k,], 
                                         patoh_d[patoh_d$k == k,],
                                         patoh_q[patoh_q$k == k,],
                                         hmetis_r[hmetis_r$k == k,],
                                         hmetis_k[hmetis_k$k == k,],
                                         zoltan_alg_d[zoltan_alg_d$k == k,],
                                         mondriaan[mondriaan$k == k,]), 
                                    show_infeasible_tick = T,
                                    show_timeout_tick = T,
                                    order = order,
                                    title = "$\\ocut$ -- $k = 8$",
                                    text_angle = 30,
                                    show_legend = F,
                                    latex_export = T,
                                    small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/cut_running_time_k_16.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
k <- 16
print(running_time_per_pin_box_plot(list(kahypar_k[kahypar_k$k == k,],
                                         kahypar_r[kahypar_r$k == k,], 
                                         patoh_d[patoh_d$k == k,],
                                         patoh_q[patoh_q$k == k,],
                                         hmetis_r[hmetis_r$k == k,],
                                         hmetis_k[hmetis_k$k == k,],
                                         zoltan_alg_d[zoltan_alg_d$k == k,],
                                         mondriaan[mondriaan$k == k,]), 
                                    show_infeasible_tick = T,
                                    show_timeout_tick = T,
                                    order = order,
                                    title = "$\\ocut$ -- $k = 16$",
                                    text_angle = 30,
                                    show_legend = F,
                                    latex_export = T,
                                    small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/cut_running_time_k_32.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
k <- 32
print(running_time_per_pin_box_plot(list(kahypar_k[kahypar_k$k == k,],
                                         kahypar_r[kahypar_r$k == k,], 
                                         patoh_d[patoh_d$k == k,],
                                         patoh_q[patoh_q$k == k,],
                                         hmetis_r[hmetis_r$k == k,],
                                         hmetis_k[hmetis_k$k == k,],
                                         zoltan_alg_d[zoltan_alg_d$k == k,],
                                         mondriaan[mondriaan$k == k,]), 
                                    show_infeasible_tick = T,
                                    show_timeout_tick = T,
                                    order = order,
                                    title = "$\\ocut$ -- $k = 32$",
                                    text_angle = 30,
                                    show_legend = F,
                                    latex_export = T,
                                    small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/cut_running_time_k_64.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
k <- 64
print(running_time_per_pin_box_plot(list(kahypar_k[kahypar_k$k == k,],
                                         kahypar_r[kahypar_r$k == k,], 
                                         patoh_d[patoh_d$k == k,],
                                         patoh_q[patoh_q$k == k,],
                                         hmetis_r[hmetis_r$k == k,],
                                         hmetis_k[hmetis_k$k == k,],
                                         zoltan_alg_d[zoltan_alg_d$k == k,],
                                         mondriaan[mondriaan$k == k,]), 
                                    show_infeasible_tick = T,
                                    show_timeout_tick = T,
                                    order = order,
                                    title = "$\\ocut$ -- $k = 64$",
                                    text_angle = 30,
                                    show_legend = T,
                                    latex_export = T,
                                    small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/cut_running_time_k_128.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
k <- 128
print(running_time_per_pin_box_plot(list(kahypar_k[kahypar_k$k == k,],
                                         kahypar_r[kahypar_r$k == k,], 
                                         patoh_d[patoh_d$k == k,],
                                         patoh_q[patoh_q$k == k,],
                                         hmetis_r[hmetis_r$k == k,],
                                         hmetis_k[hmetis_k$k == k,],
                                         zoltan_alg_d[zoltan_alg_d$k == k,],
                                         mondriaan[mondriaan$k == k,]), 
                                    show_infeasible_tick = T,
                                    show_timeout_tick = T,
                                    order = order,
                                    title = "$\\ocut$ -- $k = 128$",
                                    text_angle = 30,
                                    show_legend = T,
                                    latex_export = T,
                                    small_size = F))
dev.off()


  ############## Performance Profile Plot (rKaHyPar) ############## 

scaling <- 1.25

tikz("~/kahypar-jea/tikz_plots/cut_overall.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(performace_plot(list(kahypar_k, 
                           kahypar_r, 
                           patoh_d,
                           patoh_q,
                           hmetis_r,
                           hmetis_k,
                           zoltan_alg_d,
                           mondriaan), 
                      objective = "avg_cut", 
                      hide_y_axis_title = F,
                      show_infeasible_tick = T,
                      show_timeout_tick = T,
                      widths = c(3,2,1,1),
                      legend_top_margin = -5,
                      latex_export = T,
                      small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/cut_overall_only_k_kahypar.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(performace_plot(list(kahypar_k,
                           patoh_d,
                           patoh_q,
                           hmetis_r,
                           hmetis_k,
                           zoltan_alg_d,
                           mondriaan), 
                      objective = "avg_cut", 
                      hide_y_axis_title = F,
                      show_infeasible_tick = T,
                      show_timeout_tick = T,
                      widths = c(3,2,1,1),
                      legend_top_margin = -5,
                      latex_export = T,
                      small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/cut_overall_only_r_kahypar.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(performace_plot(list(kahypar_r,
                           patoh_d,
                           patoh_q,
                           hmetis_r,
                           hmetis_k,
                           zoltan_alg_d,
                           mondriaan), 
                      objective = "avg_cut", 
                      hide_y_axis_title = F,
                      show_infeasible_tick = T,
                      show_timeout_tick = T,
                      widths = c(3,2,1,1),
                      legend_top_margin = -5,
                      latex_export = T,
                      small_size = F))
dev.off()

############## Performance Profile Plot (Instance Types) ############## 

tikz("~/kahypar-jea/tikz_plots/cut_dac.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
type <- "DAC"
print(performace_plot(list(kahypar_k[kahypar_k$type == type,],
                           kahypar_r[kahypar_r$type == type,], 
                           patoh_d[patoh_d$type == type,],
                           patoh_q[patoh_q$type == type,],
                           hmetis_r[hmetis_r$type == type,],
                           hmetis_k[hmetis_k$type == type,],
                           zoltan_alg_d[zoltan_alg_d$type == type,],
                           mondriaan[mondriaan$type == type,]), 
                      objective = "avg_cut", 
                      hide_y_axis_title = F,
                      show_infeasible_tick = F,
                      show_timeout_tick = F,
                      widths = c(3,2,1),
                      title = "$\\ocut$ -- \\DAC",
                      show_legend = FALSE,
                      latex_export = T,
                      small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/cut_ispd.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
type <- "ISPD"
print(performace_plot(list(kahypar_k[kahypar_k$type == type,],
                           kahypar_r[kahypar_r$type == type,], 
                           patoh_d[patoh_d$type == type,],
                           patoh_q[patoh_q$type == type,],
                           hmetis_r[hmetis_r$type == type,],
                           hmetis_k[hmetis_k$type == type,],
                           zoltan_alg_d[zoltan_alg_d$type == type,],
                           mondriaan[mondriaan$type == type,]), 
                      objective = "avg_cut", 
                      hide_y_axis_title = F,
                      show_infeasible_tick = T,
                      show_timeout_tick = F,
                      widths = c(3,2,1,1),
                      title = "$\\ocut$ -- \\ISPD",
                      show_legend = FALSE,
                      latex_export = T,
                      small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/cut_primal.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
type <- "Primal"
print(performace_plot(list(kahypar_k[kahypar_k$type == type,],
                           kahypar_r[kahypar_r$type == type,], 
                           patoh_d[patoh_d$type == type,],
                           patoh_q[patoh_q$type == type,],
                           hmetis_r[hmetis_r$type == type,],
                           hmetis_k[hmetis_k$type == type,],
                           zoltan_alg_d[zoltan_alg_d$type == type,],
                           mondriaan[mondriaan$type == type,]), 
                      objective = "avg_cut", 
                      hide_y_axis_title = F,
                      show_infeasible_tick = T,
                      show_timeout_tick = T,
                      widths = c(3,2,1,1),
                      title = "$\\ocut$ -- \\Primal",
                      show_legend = FALSE,
                      latex_export = T,
                      small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/cut_literal.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
type <- "Literal"
print(performace_plot(list(kahypar_k[kahypar_k$type == type,],
                           kahypar_r[kahypar_r$type == type,], 
                           patoh_d[patoh_d$type == type,],
                           patoh_q[patoh_q$type == type,],
                           hmetis_r[hmetis_r$type == type,],
                           hmetis_k[hmetis_k$type == type,],
                           zoltan_alg_d[zoltan_alg_d$type == type,],
                           mondriaan[mondriaan$type == type,]), 
                      objective = "avg_cut", 
                      hide_y_axis_title = F,
                      show_infeasible_tick = T,
                      show_timeout_tick = T,
                      widths = c(3,2,1,1),
                      title = "$\\ocut$ -- \\Literal",
                      show_legend = FALSE,
                      latex_export = T,
                      small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/cut_dual.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
type <- "Dual"
print(performace_plot(list(kahypar_k[kahypar_k$type == type,],
                           kahypar_r[kahypar_r$type == type,], 
                           patoh_d[patoh_d$type == type,],
                           patoh_q[patoh_q$type == type,],
                           hmetis_r[hmetis_r$type == type,],
                           hmetis_k[hmetis_k$type == type,],
                           zoltan_alg_d[zoltan_alg_d$type == type,],
                           mondriaan[mondriaan$type == type,]), 
                      objective = "avg_cut", 
                      hide_y_axis_title = F,
                      show_infeasible_tick = T,
                      show_timeout_tick = T,
                      widths = c(3,2,1,1),
                      title = "$\\ocut$ -- \\Dual",
                      show_legend = FALSE,
                      latex_export = T,
                      small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/cut_spm.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
type <- "SPM"
print(performace_plot(list(kahypar_k[kahypar_k$type == type,],
                           kahypar_r[kahypar_r$type == type,], 
                           patoh_d[patoh_d$type == type,],
                           patoh_q[patoh_q$type == type,],
                           hmetis_r[hmetis_r$type == type,],
                           hmetis_k[hmetis_k$type == type,],
                           zoltan_alg_d[zoltan_alg_d$type == type,],
                           mondriaan[mondriaan$type == type,]), 
                      objective = "avg_cut", 
                      hide_y_axis_title = F,
                      show_infeasible_tick = T,
                      show_timeout_tick = T,
                      widths = c(3,2,1,1),
                      title = "$\\ocut$ -- \\SPM",
                      show_legend = FALSE,
                      latex_export = T,
                      small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/cut_instance_types_legend.tex", 
     width = 2 * 2.1666 * scaling, height = 1, pointsize = 12)
print(performace_plot_legend(list(kahypar_k[kahypar_k$type == type,],
                                  kahypar_r[kahypar_r$type == type,], 
                                  patoh_d[patoh_d$type == type,],
                                  patoh_q[patoh_q$type == type,],
                                  hmetis_r[hmetis_r$type == type,],
                                  hmetis_k[hmetis_k$type == type,],
                                  zoltan_alg_d[zoltan_alg_d$type == type,],
                                  mondriaan[mondriaan$type == type,]), 
                             legend_col = 5,
                             latex_export = T,
                             small_size = F))
dev.off()

############## Performance Profile Plot (Number of Blocks) ############## 

tikz("~/kahypar-jea/tikz_plots/cut_k_2.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
k <- 2
print(performace_plot(list(kahypar_k[kahypar_k$k == k,],
                           kahypar_r[kahypar_r$k == k,], 
                           patoh_d[patoh_d$k == k,],
                           patoh_q[patoh_q$k == k,],
                           hmetis_r[hmetis_r$k == k,],
                           hmetis_k[hmetis_k$k == k,],
                           zoltan_alg_d[zoltan_alg_d$k == k,],
                           mondriaan[mondriaan$k == k,]), 
                      objective = "avg_cut", 
                      hide_y_axis_title = F,
                      show_infeasible_tick = T,
                      show_timeout_tick = T,
                      widths = c(3,2,1,1),
                      title = "$\\ocut$ -- $k = 2$",
                      show_legend = FALSE,
                      latex_export = T,
                      small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/cut_k_4.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
k <- 4
print(performace_plot(list(kahypar_k[kahypar_k$k == k,],
                           kahypar_r[kahypar_r$k == k,], 
                           patoh_d[patoh_d$k == k,],
                           patoh_q[patoh_q$k == k,],
                           hmetis_r[hmetis_r$k == k,],
                           hmetis_k[hmetis_k$k == k,],
                           zoltan_alg_d[zoltan_alg_d$k == k,],
                           mondriaan[mondriaan$k == k,]), 
                      objective = "avg_cut", 
                      hide_y_axis_title = F,
                      show_infeasible_tick = T,
                      show_timeout_tick = T,
                      widths = c(3,2,1,1),
                      title = "$\\ocut$ -- $k = 4$",
                      show_legend = FALSE,
                      latex_export = T,
                      small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/cut_k_8.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
k <- 8
print(performace_plot(list(kahypar_k[kahypar_k$k == k,],
                           kahypar_r[kahypar_r$k == k,], 
                           patoh_d[patoh_d$k == k,],
                           patoh_q[patoh_q$k == k,],
                           hmetis_r[hmetis_r$k == k,],
                           hmetis_k[hmetis_k$k == k,],
                           zoltan_alg_d[zoltan_alg_d$k == k,],
                           mondriaan[mondriaan$k == k,]), 
                      objective = "avg_cut", 
                      hide_y_axis_title = F,
                      show_infeasible_tick = T,
                      show_timeout_tick = T,
                      widths = c(3,2,1,1),
                      title = "$\\ocut$ -- $k = 8$",
                      show_legend = FALSE,
                      latex_export = T,
                      small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/cut_k_16.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
k <- 16
print(performace_plot(list(kahypar_k[kahypar_k$k == k,],
                           kahypar_r[kahypar_r$k == k,], 
                           patoh_d[patoh_d$k == k,],
                           patoh_q[patoh_q$k == k,],
                           hmetis_r[hmetis_r$k == k,],
                           hmetis_k[hmetis_k$k == k,],
                           zoltan_alg_d[zoltan_alg_d$k == k,],
                           mondriaan[mondriaan$k == k,]), 
                      objective = "avg_cut", 
                      hide_y_axis_title = F,
                      show_infeasible_tick = T,
                      show_timeout_tick = T,
                      widths = c(3,2,1,1),
                      title = "$\\ocut$ -- $k = 16$",
                      show_legend = FALSE,
                      latex_export = T,
                      small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/cut_k_32.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
k <- 32
print(performace_plot(list(kahypar_k[kahypar_k$k == k,],
                           kahypar_r[kahypar_r$k == k,], 
                           patoh_d[patoh_d$k == k,],
                           patoh_q[patoh_q$k == k,],
                           hmetis_r[hmetis_r$k == k,],
                           hmetis_k[hmetis_k$k == k,],
                           zoltan_alg_d[zoltan_alg_d$k == k,],
                           mondriaan[mondriaan$k == k,]), 
                      objective = "avg_cut", 
                      hide_y_axis_title = F,
                      show_infeasible_tick = T,
                      show_timeout_tick = T,
                      widths = c(3,2,1,1),
                      title = "$\\ocut$ -- $k = 32$",
                      show_legend = FALSE,
                      latex_export = T,
                      small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/cut_k_64.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
k <- 64
print(performace_plot(list(kahypar_k[kahypar_k$k == k,],
                           kahypar_r[kahypar_r$k == k,], 
                           patoh_d[patoh_d$k == k,],
                           patoh_q[patoh_q$k == k,],
                           hmetis_r[hmetis_r$k == k,],
                           hmetis_k[hmetis_k$k == k,],
                           zoltan_alg_d[zoltan_alg_d$k == k,],
                           mondriaan[mondriaan$k == k,]), 
                      objective = "avg_cut", 
                      hide_y_axis_title = F,
                      show_infeasible_tick = T,
                      show_timeout_tick = T,
                      widths = c(3,2,1,1),
                      title = "$\\ocut$ -- $k = 64$",
                      show_legend = FALSE,
                      latex_export = T,
                      small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/cut_k_128.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
k <- 128
print(performace_plot(list(kahypar_k[kahypar_k$k == k,],
                           kahypar_r[kahypar_r$k == k,], 
                           patoh_d[patoh_d$k == k,],
                           patoh_q[patoh_q$k == k,],
                           hmetis_r[hmetis_r$k == k,],
                           hmetis_k[hmetis_k$k == k,],
                           zoltan_alg_d[zoltan_alg_d$k == k,],
                           mondriaan[mondriaan$k == k,]), 
                      objective = "avg_cut", 
                      hide_y_axis_title = F,
                      show_infeasible_tick = T,
                      show_timeout_tick = T,
                      widths = c(3,2,1,1),
                      title = "$\\ocut$ -- $k = 128$",
                      show_legend = FALSE,
                      latex_export = T,
                      small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/cut_k_legend.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(performace_plot_legend(list(kahypar_k[kahypar_k$k == k,],
                                  kahypar_r[kahypar_r$k == k,], 
                                  patoh_d[patoh_d$k == k,],
                                  patoh_q[patoh_q$k == k,],
                                  hmetis_r[hmetis_r$k == k,],
                                  hmetis_k[hmetis_k$k == k,],
                                  zoltan_alg_d[zoltan_alg_d$k == k,],
                                  mondriaan[mondriaan$k == k,]),  
                             legend_col = 2,
                             latex_export = T,
                             small_size = F))
dev.off()


############## Performance Profile Plot (Individual Comparisons - KaHyPar-K) ############## 

tikz("~/kahypar-jea/tikz_plots/cut_kahypar_k_vs_kahypar_r.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(performace_plot(list(kahypar_k,
                           kahypar_r), 
                      objective = "avg_cut", 
                      hide_y_axis_title = F,
                      show_infeasible_tick = T,
                      show_timeout_tick = T,
                      widths = c(3,2,1,1),
                      legend_top_margin = -25,
                      latex_export = T,
                      small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/cut_kahypar_k_vs_hmetis_r.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(performace_plot(list(kahypar_k,
                           hmetis_r), 
                      objective = "avg_cut", 
                      hide_y_axis_title = F,
                      show_infeasible_tick = T,
                      show_timeout_tick = T,
                      widths = c(3,2,1,1),
                      legend_top_margin = -25,
                      latex_export = T,
                      small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/cut_kahypar_k_vs_hmetis_k.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(performace_plot(list(kahypar_k,
                           hmetis_k), 
                      objective = "avg_cut", 
                      hide_y_axis_title = F,
                      show_infeasible_tick = T,
                      show_timeout_tick = T,
                      widths = c(3,2,1,1),
                      legend_top_margin = -25,
                      latex_export = T,
                      small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/cut_kahypar_k_vs_patoh_d.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(performace_plot(list(kahypar_k,
                           patoh_d), 
                      objective = "avg_cut", 
                      hide_y_axis_title = F,
                      show_infeasible_tick = T,
                      show_timeout_tick = T,
                      widths = c(3,2,1,1),
                      legend_top_margin = -25,
                      latex_export = T,
                      small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/cut_kahypar_k_vs_patoh_q.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(performace_plot(list(kahypar_k,
                           patoh_q), 
                      objective = "avg_cut", 
                      hide_y_axis_title = F,
                      show_infeasible_tick = T,
                      show_timeout_tick = T,
                      widths = c(3,2,1,1),
                      legend_top_margin = -25,
                      latex_export = T,
                      small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/cut_kahypar_k_vs_zoltan_alg_d.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(performace_plot(list(kahypar_k,
                           zoltan_alg_d), 
                      objective = "avg_cut", 
                      hide_y_axis_title = F,
                      show_infeasible_tick = T,
                      show_timeout_tick = T,
                      widths = c(3,2,1,1),
                      legend_top_margin = -25,
                      latex_export = T,
                      small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/cut_kahypar_k_vs_mondriaan.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(performace_plot(list(kahypar_k,
                           mondriaan), 
                      objective = "avg_cut", 
                      hide_y_axis_title = F,
                      show_infeasible_tick = T,
                      show_timeout_tick = T,
                      widths = c(3,2,1,1),
                      legend_top_margin = -25,
                      latex_export = T,
                      small_size = F))
dev.off()

############## Performance Profile Plot (Individual Comparisons - KaHyPar-R) ############## 

tikz("~/kahypar-jea/tikz_plots/cut_kahypar_r_vs_hmetis_r.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(performace_plot(list(kahypar_r,
                           hmetis_r), 
                      objective = "avg_cut", 
                      hide_y_axis_title = F,
                      show_infeasible_tick = T,
                      show_timeout_tick = T,
                      widths = c(3,2,1,1),
                      legend_top_margin = -25,
                      latex_export = T,
                      small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/cut_kahypar_r_vs_hmetis_k.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(performace_plot(list(kahypar_r,
                           hmetis_k), 
                      objective = "avg_cut", 
                      hide_y_axis_title = F,
                      show_infeasible_tick = T,
                      show_timeout_tick = T,
                      widths = c(3,2,1,1),
                      legend_top_margin = -25,
                      latex_export = T,
                      small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/cut_kahypar_r_vs_patoh_d.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(performace_plot(list(kahypar_r,
                           patoh_d), 
                      objective = "avg_cut", 
                      hide_y_axis_title = F,
                      show_infeasible_tick = T,
                      show_timeout_tick = T,
                      widths = c(3,2,1,1),
                      legend_top_margin = -25,
                      latex_export = T,
                      small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/cut_kahypar_r_vs_patoh_q.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(performace_plot(list(kahypar_r,
                           patoh_q), 
                      objective = "avg_cut", 
                      hide_y_axis_title = F,
                      show_infeasible_tick = T,
                      show_timeout_tick = T,
                      widths = c(3,2,1,1),
                      legend_top_margin = -25,
                      latex_export = T,
                      small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/cut_kahypar_r_vs_zoltan_alg_d.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(performace_plot(list(kahypar_r,
                           zoltan_alg_d), 
                      objective = "avg_cut", 
                      hide_y_axis_title = F,
                      show_infeasible_tick = T,
                      show_timeout_tick = T,
                      widths = c(3,2,1,1),
                      legend_top_margin = -25,
                      latex_export = T,
                      small_size = F))
dev.off()

tikz("~/kahypar-jea/tikz_plots/cut_kahypar_r_vs_mondriaan.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(performace_plot(list(kahypar_r,
                           mondriaan), 
                      objective = "avg_cut", 
                      hide_y_axis_title = F,
                      show_infeasible_tick = T,
                      show_timeout_tick = T,
                      widths = c(3,2,1,1),
                      legend_top_margin = -25,
                      latex_export = T,
                      small_size = F))
dev.off()

