# ggplot2 version of denstrip function
#
# @include ggplot2
# @examples
#
# x <- rnorm(1000)  # sample data
# 
# mydens <- ggdenstrip(dat = x, at = 1,
#                      colmax = "black",
#                      ticks = c(-1, 0, 1))
# 
# empty_plot <- ggplot() + theme_bw()
# 
# ggpubr::ggarrange(empty_plot, mydens,
#                   heights = c(2, 0.7),
#                   ncol = 1, nrow = 2)
# 
# ## pass density directly 
# # beta distribution
# x <- seq(0, 1, by=0.01)
# mydens2 <- ggdenstrip(x = x,
#                       dens = dbeta(x, 5, 10))
# 
# ggpubr::ggarrange(empty_plot, mydens2,
#                   heights = c(2, 0.7),
#                   ncol = 1, nrow = 2)
# 
# # normal distribution
# x <- seq(-1, 1, by=0.01)
# mydens3 <- ggdenstrip(x = x,
#                       dens = dnorm(x, 0.5, 0.15))
# # dens = dnorm(x, -0.5, 0.15))
# 
# ggpubr::ggarrange(empty_plot, mydens3,
#                   heights = c(2, 0.7),
#                   ncol = 1, nrow = 2)
# 
ggdenstrip <- function(dat = NA,   # sample data
                       x = NA,     # x-coords
                       dens = NA,  # density values
                       at = 0,     # y-axis location
                       width = NULL,
                       colmax = "black",  # colour maximum
                       colmin = "white",  # colour minimum
                       scale = 1,
                       x_scale = FALSE,  # rescale to interval [0, 1]
                       gamma = 1,
                       n = 512,
                       horiz = TRUE,
                       ticks = NULL,  ##TODO:
                       tlen = 1.5,
                       tcol = colmax,
                       twd = 0.5, ...) {
  
  if (!any(is.na(dat))) {
    
    # kernel estimation
    strip_dens <- density(dat, n = n, ...)
    
    # normalize and apply scale
    dens <- strip_dens$y / max(strip_dens$y) * scale
    
    x <- strip_dens$x
    
    # # Optional ticks
    # if (!is.null(ticks)) {
    #   tick_df <- data.frame(
    #     x = ticks,
    #     xend = ticks,
    #     y = at - width * tlen / 2,
    #     yend = at + width * tlen / 2
    #   )
    #   
    #   p + geom_segment(data = tick_df,
    #                    aes(x = x, xend = xend, y = y, yend = yend),
    #                    color = tcol,
    #                    linewidth = twd)
    # }
  }
  
  if (x_scale) {
    x_range <- max(x) - min(x)
    x <- (x - min(x))/x_range
  }
  
  # default width
  if (is.null(width)) {
    width <- diff(range(x)) / (length(x) - 1)
  }
  
  df <- data.frame(
    x = x,
    dens = dens,
    y = at)
  
  p <-
    ggplot(df, aes(x = x, y = y)) +
    geom_tile(aes(fill = dens), height = width,
              width = width) +
      scale_fill_continuous(low=colmin, high=colmax) +
      theme_void() +
      theme(
        show.legend = FALSE,
        legend.position = "none")
      # xlim(0, 1)
      # ylim(0.5, 1.6)
  
  return(p)
}
