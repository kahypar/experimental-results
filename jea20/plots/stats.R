compareQualityOfPartitioners <- function(taus, objective, ...) {
  dataframes <- list(...)
  worst_ratio <- computeWorstRatioForAllFiltered(dataframes, objective = objective, effectivenes_test = F)
  all_km1 = computePerformanceProfile(dataframes, objective = objective,  worst_ratio = worst_ratio, effectivenes_test = F)
  performance_ratios <- all_km1
  
  for ( algo in levels(factor(performance_ratios$algorithm)) ) {
    for ( tau in taus ) {
      values <- performance_ratios[performance_ratios$algorithm == algo & performance_ratios$tau == tau,]$rho
      if (length(values) >= 1) {
        t <- round(as.double(tau) * 100, digits = 0)
        value <- as.double(values[[1]]) * 100.0
        print(paste("algo=", algo, "tau=", t, "percent=", value, sep = " "))
      } 
    }
  }
}