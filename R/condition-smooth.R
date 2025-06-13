#' Create a plot of the Condition fit incorporating the log depth smoother
#'
#' @param fit_lst A list of a fit from [sdmTMB] and a data frame
#' of the data used in that fit as returned by [condition_fit()]
#'
#' @returns A [ggplot2::ggplot()] object
#' @export
condition_smooth <- \(fit_lst){

  fit <- fit_lst$fit
  ds <- fit_lst$ds

  g <- plot_smooth(fit, select = 1, ggplot = TRUE, rug = FALSE) +
    scale_x_continuous(trans = "exp",
                       breaks = c(log(100),
                                  log(200),
                                  log(300),
                                  log(400),
                                  log(500)),
                       labels = c("100",
                                  "200",
                                  "300",
                                  "400",
                                  "500")) +
    scale_y_continuous(breaks = c(0.97, 1, 1.03, 1.06, 1.09)) +
    coord_cartesian(expand = FALSE) +
    ylab(ifelse(fr(),
                "Facteur de condition prédit",
                "Predicted condition factor")) +
    xlab(ifelse(fr(),
                "Profondeur (m)",
                "Depth (m)")) +
    geom_rug(data = fit$data,
             mapping = aes_string(x = "log_depth"),
             sides = "t",
             inherit.aes = FALSE,
             alpha = 0.01,
             size = 0.5) +
    theme_pbs()

  g
}