#' Plot Output Functions
#' @export
#' @rdname density

density_plot_reg <- function(filtered_df) {

  aa <- filtered_df
  #res$date <- as.Date(res$date)
  a <- aa %>%  pull(Close)
  returns <- diff(a)/a[-length(a)]
  bb <- as.data.frame(returns)
  q <- quantile(returns,probs = c(0.1,0.9))

 ggplot(bb, aes_string("returns")) +
    geom_density(color="black",size=1) + labs(x = "returns", y = "density") +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"),
      legend.title = element_blank(),
      legend.key = element_rect(colour = "transparent", fill = "white")) +
    stat_function(fun = dnorm, args = list(mean = mean(bb$returns), sd = sd(bb$returns)),
                  aes(linetype = "Normal distribution"),size = 0.7,colour = "red") +
    geom_vline(xintercept = q, linetype = "longdash")


}


