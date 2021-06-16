library(purrr)
library(ggplot2)
library(scales)
library(RSQLite)
library(DBI)
library(dbConnect)
library(sqldf)
library(data.table)
library(plyr)
library(dplyr)
library(dtplyr)
library(stringr)

graphclass = function(row) {
  if(grepl("*dual*", row['graph'])){
    return("\\texttt{Dual}")
  } else if (grepl("*primal*", row['graph'])) {
    return("\\texttt{Primal}")
  } else if (grepl("sat14*", row['graph'])) {
    return("\\texttt{Literal}")
  } else if (grepl("*mtx*", row['graph'])) {
    return("\\texttt{SPM}")
  }  else if (grepl("*ISPD98*", row['graph'])) {
    return("\\texttt{ISPD}")
  } else {
    return("\\texttt{DAC}")
  }
}

set_a <- read.csv("instances.csv")
set_a$set <- "Set A"
set_b <- read.csv("set_b.csv")
set_b <- set_a[set_a$graph %in% set_b$graph,]
set_b$set <- "Set B"
set_c <- read.csv("set_c.csv")
set_c <- set_a[set_a$graph %in% set_c$graph,]
set_c$set <- "Set C"

benchmarks <- rbind(set_a, set_b)
benchmarks <- rbind(benchmarks, set_c)
benchmarks$type <- as.factor(apply(benchmarks, 1, function(x) graphclass(x)))
benchmarks$type <- factor(benchmarks$type, levels = c("\\texttt{ISPD}", "\\texttt{DAC}", "\\texttt{SPM}",
                                                      "\\texttt{Literal}", "\\texttt{Primal}", "\\texttt{Dual}"))
names(benchmarks)[names(benchmarks) == "HNs"] <- "$|V|$"
names(benchmarks)[names(benchmarks) == "HEs"] <- "$|E|$"
names(benchmarks)[names(benchmarks) == "pins"] <- "$|P|$"
names(benchmarks)[names(benchmarks) == "medHEsize"] <- "$\\medsize$"
names(benchmarks)[names(benchmarks) == "maxHEsize"] <- "$\\maxsize{e}$"
names(benchmarks)[names(benchmarks) == "medHNdegree"] <- "$\\meddeg$"
names(benchmarks)[names(benchmarks) == "maxHnDegree"] <- "$\\maxdeg{v}$"
benchmarks <- melt(benchmarks, id.vars = c("graph", "set", "type"), measure.vars = c("$|V|$", "$|E|$", "$|P|$", "$\\medsize$", "$\\maxsize{e}$", "$\\meddeg$", "$\\maxdeg{v}$"))
names(benchmarks)[names(benchmarks) == "variable"] <- "objective"

tikz("benchmark_stats.tex", width = 3.5 * 0.95, height = 2.5 * 0.95, pointsize = 12)
stats = ggplot(benchmarks, aes(x=objective, y=value, colour=set)) +
  geom_point(size = 0.1, position = position_jitterdodge(jitter.width = 0.25)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5) +
  scale_y_continuous(trans = "log10", 
                     breaks=c(0,10,100,1000,10000,100000,1000000,10000000,100000000,1000000000,10000000000),
                     labels=c("$10^0$","$10^1$","$10^2$","$10^3$","$10^4$","$10^5$","$10^6$","$10^7$","$10^8$","$10^9$","$10^{10}$")) +
  theme_bw(base_size = 10) +
  labs(x=NULL, y=NULL) +
  theme(aspect.ratio =2/(1+sqrt(5)),
        legend.position = "bottom",
        legend.margin = margin(-5,0,0,0),
        panel.grid.major = element_line(linetype="dotted",size = 0.25, 
                                        color = "grey"),
        panel.grid.minor =element_blank(),
        axis.line = element_line(size = 0.2, color = "black"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 8, margin = margin(5,0,0,0))) +
  guides(colour = guide_legend(title = NULL, ncol = 2, byrow =  F, 
                               keywidth = .75, keyheight = .75, legend.margin =-.5, title.position = "left", title.hjust=0))
print(stats)
dev.off()

tikz("~/kahypar-jea/tikz_plots/benchmark_stats_per_type.tex", width = 9 * 0.615, height = 6.125 * 0.615, pointsize = 12)
stats = ggplot(benchmarks, aes(x=objective, y=value, colour=set)) +
  geom_point(size = 0.1, position = position_jitterdodge(jitter.width = 0.25)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5) +
  scale_y_continuous(trans = "log10", 
                     breaks=c(1,10,100,1000,10000,100000,1000000,10000000,100000000,1000000000,10000000000),
                     labels=c("$1$","$10$","$10^2$","$10^3$","$10^4$","$10^5$","$10^6$","$10^7$","$10^8$","$10^9$","$10^{10}$")) +
  facet_wrap(~type, ncol = 3) +
  theme_bw(base_size = 10) +
  labs(x=NULL, y=NULL) +
  theme(aspect.ratio =2/(1+sqrt(5)),
        legend.position = "bottom",
        legend.margin = margin(-5,0,0,0),
        panel.grid.major = element_line(linetype="dotted",size = 0.25, 
                                        color = "grey"),
        panel.grid.minor =element_blank(),
        axis.line = element_line(size = 0.2, color = "black"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 8, margin = margin(5,0,0,0))) +
  guides(colour = guide_legend(title = NULL, ncol = 3, byrow =  F, 
                               keywidth = .75, keyheight = .75, legend.margin =-.5, title.position = "left", title.hjust=0))
print(stats)
dev.off()

