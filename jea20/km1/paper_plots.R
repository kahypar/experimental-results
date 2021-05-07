source("../functions.R")
source("../plots/plots_common.R")
source("../plots/running_time_box_plot.R")
source("../plots/relative_running_time_plot.R")
source("../plots/performance_profiles.R")

############## SETUP DATA FRAMES ############## 

# Read Data Frames
instances <- read.csv("instances.csv", header = TRUE)
kahypar_k <- aggreg_data(read.csv("km1_kKaHyPar.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
patoh_d <- aggreg_data(read.csv("km1_patoh_d.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
patoh_q <- aggreg_data(read.csv("km1_patoh_q.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
hmetis_r <- aggreg_data(read.csv("km1_hmetis-r.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
hmetis_k <- aggreg_data(read.csv("km1_hmetis-k.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
zoltan_alg_d <- aggreg_data(read.csv("km1_zoltan-algd.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
mondriaan <- aggreg_data(read.csv("km1_mondriaan.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
hype <- aggreg_data(read.csv("km1_hype.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)

# Compute Average Time per Pin
kahypar_k <- compute_avg_time_per_pin(kahypar_k, instances)
patoh_d <- compute_avg_time_per_pin(patoh_d, instances)
patoh_q <- compute_avg_time_per_pin(patoh_q, instances)
hmetis_r <- compute_avg_time_per_pin(hmetis_r, instances)
hmetis_k <- compute_avg_time_per_pin(hmetis_k, instances)
zoltan_alg_d <- compute_avg_time_per_pin(zoltan_alg_d, instances)
mondriaan <- compute_avg_time_per_pin(mondriaan, instances)
hype <- compute_avg_time_per_pin(hype, instances)

# Graph Classes
kahypar_k$type <- as.factor(apply(kahypar_k, 1, function(x) graphclass(x)))
patoh_d$type <- as.factor(apply(patoh_d, 1, function(x) graphclass(x)))
patoh_q$type <- as.factor(apply(patoh_q, 1, function(x) graphclass(x)))
hmetis_r$type <- as.factor(apply(hmetis_r, 1, function(x) graphclass(x)))
hmetis_k$type <- as.factor(apply(hmetis_k, 1, function(x) graphclass(x)))
zoltan_alg_d$type <- as.factor(apply(zoltan_alg_d, 1, function(x) graphclass(x)))
mondriaan$type <- as.factor(apply(mondriaan, 1, function(x) graphclass(x)))
hype$type <- as.factor(apply(hype, 1, function(x) graphclass(x)))

# Set Algorithm Name
kahypar_k$algorithm <- "$k$KaHyPar"
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
algo_color_mapping <- c("$k$KaHyPar" = palette[[1]],
                        "PaToH-D" = palette[[3]],
                        "PaToH-Q" = palette[[4]],
                        "hMETIS-R" = palette[[5]],
                        "hMETIS-K" = palette[[6]],
                        "Zoltan-AlgD" = palette[[7]],
                        "Mondriaan" = palette[[8]],
                        "HYPE" = palette[[9]])


############## Trade-Off Plot ##############

order <- c("hMETIS-R", "hMETIS-K", "PaToH-D", "PaToH-Q", "HYPE", "Mondriaan", "Zoltan-AlgD")
scaling <- 2.5
tikz("~/kahypar-jea/tikz_plots/km1_tradeoff.tex", 
     width = 2.1666 * scaling, height = 1 * scaling, pointsize = 12)
print(tradeoff_plot(list(patoh_d,
                         patoh_q,
                         hmetis_r,
                         hmetis_k,
                         zoltan_alg_d,
                         mondriaan,
                         hype),
                    relative_to = kahypar_k,
                    objective = "avg_km1",
                    order = order,
                    ncol = 4,
                    legend_col = 4,
                    latex_export = T,
                    small_size = F
                    ))
dev.off()

############## Running Time Box Plot ############## 

scaling <- 1.16

order <- c("HYPE", "PaToH-D", "PaToH-Q", "Mondriaan", "Zoltan-AlgD", "hMETIS-R", "hMETIS-K", "$k$KaHyPar")
tikz("~/kahypar-jea/tikz_plots/km1_running_time_overall.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(running_time_per_pin_box_plot(list(kahypar_k,
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
                                    use_sampling = F,
                                    text_angle = 30,
                                    latex_export = T,
                                    small_size = F))
dev.off()

############## Performance Profile Plot (ALL) ############## 

scaling <- 1.25

tikz("~/kahypar-jea/tikz_plots/km1_overall_only_k_kahypar.tex", 
     width = 2.1666 * scaling, height = 1.666 * scaling, pointsize = 12)
print(performace_plot(list(kahypar_k,
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
                      show_timeout_tick = T,
                      widths = c(3,2,1,1),
                      legend_top_margin = -5,
                      latex_export = T,
                      small_size = F))
dev.off()
