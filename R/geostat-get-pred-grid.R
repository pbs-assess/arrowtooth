geostat_get_pred_grid <- \(

  dr = "/srv/arrowtooth/arrowtooth-nongit/data",
  fn_mdl_output = file.path(dr, "geo-delta-gamma-nodepth.rds"),
  fn_syn_grid = file.path(dr, "synoptic_grid.rds"),
  ...){

  if(!file.exists(fn_mdl_output)){
    stop("Geostatistical spatial output file does not exist:\n",
         fn_mdl_output)
  }
  if(!file.exists(fn_syn_grid)){
    stop("Synoptic grid file does not exist:\n",
         fn_syn_grid)
  }

  mdl_output <- readRDS(fn_mdl_output) |>
    as_tibble()
  grid_output <- readRDS(fn_syn_grid) |>
    as_tibble()

  m <- mdl_output$`arrowtooth flounder`$fit

  # Pass `years` in ...
  nd <- make_grid(grid_output,
                  ...) |>
    na.omit() |>
    mutate(year = as.integer(year),
           log_depth = log(depth))

  predict(m, newdata = nd)
}