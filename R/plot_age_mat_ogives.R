#' Create a 2-panel plot (one for each sex) of maturity ogives for
#' several different scenarios
#'
#' @param d A data frame created  in the document code chunk `calc-maturities`
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_age_mat_ogives <- function(d){

  female_cols <- brewer.pal(n = 8, name = "YlOrRd")
  female_cols <- female_cols[4:7]
  female_cols <- rev(female_cols)

  male_cols <- brewer.pal(n = 9, name = "YlGnBu")
  male_cols <- male_cols[4:8]
  male_cols <- rev(male_cols)

  plot_ogive_by_sex <- function(d,
                                plot_sex = tr("Female"),
                                col_vec = female_cols,
                                ogive_size = 1.3,
                                ogive_alpha = 0.4) {

    models <- unique(d$model)
    # Translate sex
    d <- d |>
      mutate(sex = tr(sex)) |>
      mutate(age = factor(age))

    p <- d |>
      filter(sex == !!plot_sex) |>
      ggplot() +
      geom_point(aes(age,
                     prop_mature,
                     colour = model),
                 alpha = 0.7) +
      scale_color_manual(values = col_vec,
                         name = "Model") +
      ylab(tr("Proportion mature")) +
      xlab(tr("Age")) +
      ggtitle(plot_sex) +
      ggsidekick::theme_sleek() +
      theme(axis.text.x = element_text(size = 7))

    add_ogive <- \(p, i){

      p + geom_function(
        data = NULL,
        size = ogive_size,
        alpha = ogive_alpha,
        fun = \(x){
          plogis(x,
                 d[d$model == models[i] & d$sex == plot_sex, ]$age50mat[1],
                 d[d$model == models[i] & d$sex == plot_sex, ]$sd50mat[1])
        },
        colour = col_vec[i],
        inherit.aes = FALSE) +
        geom_vline(data = NULL,
                   size = ogive_size / 2,
                   lty = "dashed",
                   alpha = ogive_alpha,
                   xintercept = d[d$model == models[i] &
                                          d$sex == plot_sex, ]$age50mat[1],
                   colour = col_vec[i])
    }

    for (i in seq_along(models)){
      p <- p |>
        add_ogive(i)
    }

    p + coord_cartesian(expand = FALSE,
                        xlim = c(0, 26))
  }

  p1 <- plot_ogive_by_sex(d,
                                 plot_sex = tr("Male"),
                                 col_vec = male_cols) +
    theme(legend.title = element_blank(),
          axis.title.x = element_blank(),
          legend.position = c(0.7, 0.25))

  p2 <- plot_ogive_by_sex(d,
                                 plot_sex = tr("Female"),
                                 col_vec = female_cols) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          legend.title = element_blank(),
          axis.title.x = element_blank(),
          legend.position = c(0.7, 0.25))

  xlab <- ggplot(data.frame(l = tr("Age"), x = 13, y = 1)) +
    geom_text(aes(x, y, label = l))+
    theme_void()

  grid.arrange(grobs = list(p1, p2),
               ncol = 2,
               # left = paste0(tr("Length"), " (cm)"),
               bottom = tr("Age (years)"))
}