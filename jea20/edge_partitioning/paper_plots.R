source("../functions.R")
source("../plots/plots_common.R")
source("../plots/running_time_box_plot.R")
source("../plots/relative_running_time_plot.R")
source("../plots/performance_profiles.R")

############## SETUP DATA FRAMES ############## 

# Read Data Frames
instances <- dbGetQuery(dbConnect(SQLite(), dbname="instances.db"), "select * from ex1")
kahypar_k <- aggreg_data(read.csv("km1_kKaHyPar_edge_partitioning.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
patoh_d <- aggreg_data(read.csv("km1_patoh_d_edge_partitioning.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
patoh_q <- aggreg_data(read.csv("km1_patoh_q_edge_partitioning.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
hmetis_r <- aggreg_data(read.csv("km1_r_hmetis_edge_partitioning.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
hmetis_k <- aggreg_data(read.csv("km1_k_hmetis_edge_partitioning.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
zoltan_alg_d <- aggreg_data(read.csv("km1_zoltan_algd_edge_partitioning.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
mondriaan <- aggreg_data(read.csv("km1_mondriaan_edge_partitioning.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)
hype <- aggreg_data(read.csv("km1_hype_edge_partitioning.csv", header = TRUE), timelimit = 28800, epsilon = 0.03)

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

############## Running Time Box Plot ############## 

scaling <- 1.16

order <- c("HYPE", "PaToH-D", "PaToH-Q", "Mondriaan", "Zoltan-AlgD", "hMETIS-R", "hMETIS-K", "$k$KaHyPar")
tikz("~/kahypar-jea/tikz_plots/edge_partitioning_running_time_overall.tex", 
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
                                    show_timeout_tick = F,
                                    order = order,
                                    text_angle = 30,
                                    latex_export = T,
                                    small_size = F))
dev.off()

############## Performance Profile Plot (ALL) ############## 

scaling <- 1.25

tikz("~/kahypar-jea/tikz_plots/edge_partitioning_km1_kahypar_k.tex", 
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
                      legend_top_margin = 0,
                      latex_export = T,
                      small_size = F))
dev.off()


