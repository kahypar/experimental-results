
axis_text_size <- function(latex_export = F,
                           small_size = F) {
  if ( small_size ) {
    return(6)
  } else if ( latex_export ) {
    return(8)
  } else {
    return(10)
  }
}

legend_text_size <- function(latex_export = F,
                           small_size = F) {
  if ( small_size ) {
    return(6)
  } else if ( latex_export ) {
    return(8)
  } else {
    return(10)
  }
}

axis_title_size <- function(latex_export = F,
                            small_size = F) {
  if ( small_size ) {
    return(6)
  } else if ( latex_export ) {
    return(8)
  } else {
    return(10)
  }
}

plot_text_size <- function(latex_export = F) {
  if ( latex_export ) {
    return(2.5)
  } else {
    return(4)
  }
}

plot_title_size <- function(latex_export = F) {
  if ( latex_export ) {
    return(10)
  } else {
    return(12)
  }
}

plot_point_size <- function(latex_export = F) {
  if ( latex_export ) {
    return(0.5)
  } else {
    return(1)
  }
}

plot_line_size <- function(latex_export = F) {
  if ( latex_export ) {
    return(0.5)
  } else {
    return(0.75)
  }
}


create_theme <- function(latex_export = F,
                         small_size = F,
                         aspect_ratio = 2/(1+sqrt(5)),
                         legend_position = "bottom",
                         x_axis_text_angle = 0,
                         x_axis_text_hjust = 0.5,
                         panel_grid_size = 0.25) {
  return(theme(aspect.ratio = aspect_ratio,
               legend.position = legend_position,
               legend.background = element_blank(),
               legend.title=element_blank(),
               legend.margin = margin(-5, 0, 0, 0),
               legend.key=element_blank(),
               legend.text = element_text(size = legend_text_size(latex_export, small_size)),
               legend.box = NULL,
               legend.title.align = 0.5,
               strip.background = element_blank(),
               strip.text = element_blank(),
               panel.grid.major = element_line(linetype="11",size = panel_grid_size, color = "grey"),
               panel.grid.minor =element_blank(),
               plot.title = element_text(size = plot_title_size(latex_export), hjust = 0.5),
               axis.line = element_line(size = 0.2, color = "black"),
               axis.title.y = element_text(size = axis_title_size(latex_export, small_size), vjust = 1.5, color = "black"),
               axis.title.x =  element_text(size = axis_title_size(latex_export, small_size), vjust = 1.5, color = "black"),
               axis.text.x = element_text(angle = x_axis_text_angle, hjust = x_axis_text_hjust, size = axis_text_size(latex_export, small_size), color = "black"),
               axis.text.y = element_text(size = axis_text_size(latex_export, small_size), color = "black")))
}

to_latex_math_mode <- function(x, latex_export = F) {
  if ( latex_export ) {
    return(paste("$",x,"$",sep=""))
  } else {
    return(x)
  }
}

pow_text <- function(base, exp, latex_export = F) {
  if ( latex_export ) {
    x <- paste(base,"^{",exp,"}",sep="")
  } else {
    x <- paste(base,"^",exp,sep="")
  }
  return(to_latex_math_mode(x, latex_export))
}

add_infeasible_break <- function(breaks, infeasible_value, show_infeasible_tick = F, latex_export = F) {
  if ( show_infeasible_tick ) {
    breaks <- c(breaks, infeasible_value)
  }
  return(breaks)
}

add_infeasible_label <- function(labels, infeasible_value, show_infeasible_tick = F, latex_export = F) {
  if ( show_infeasible_tick ) {
    if ( latex_export ) {
      labels <- c(labels, "\\ding{55}")
    } else {
      labels <- c(labels, "infeasible")
    }
  }
  return(labels)
}

add_timeout_break <- function(breaks, timeout_value, show_timeout_tick = F, latex_export = F) {
  if ( show_timeout_tick ) {
    breaks <- c(breaks, timeout_value)
  }
  return(breaks)
}

add_timeout_label <- function(labels, timeout_value, show_timeout_tick = F, latex_export = F) {
  if ( show_timeout_tick ) {
    if ( latex_export ) {
      labels <- c(labels, "\\ClockLogo")
    } else {
      labels <- c(labels, "timeout")
    }
  }
  return(labels)
}
