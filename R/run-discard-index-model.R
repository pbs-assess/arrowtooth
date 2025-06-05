#' Run the discard index GLMM model
#'
#' @param dfleet A list, as output from [cpue_extraction()]
#' @param params A parameter list, as returned by [cpue_extraction()] with its
#' argument `ret_params` set to `TRUE`
#'
#' @return The model object, and a CSV is written with the output
#' @export
run_discard_index_model <- \(dfleet,
                             params,
                             fn_csv = "/srv/arrowtooth/arrowtooth-nongit/data"){

  dfleet$year_locality <- paste(dfleet$year_factor, dfleet$locality)

  formulas <- tibble::tibble(
    formula = c(
      "cpue ~ 0 + year_factor",
      "cpue ~ 0 + year_factor + depth",
      "cpue ~ 0 + year_factor + month",
      "cpue ~ 0 + year_factor + latitude",
      "cpue ~ 0 + year_factor + (1 | locality)",
      "cpue ~ 0 + year_factor + (1 | vessel)",
      "cpue ~ 0 + year_factor + depth + month + latitude + (1 | locality) + (1 | vessel)",
      "cpue ~ 0 + year_factor + depth + month + latitude + (1 | locality) + (1 | vessel) + (1 | year_locality)"
    ),
    formula_version = c(
      "Unstandardized",
      "Depth",
      "Month",
      "Latitude",
      "Locality",
      "Vessel",
      "Full (without\nlocality-year effects)",
      "Full standardization"
    )
  )

  torun <- expand.grid(formula = formulas$formula,
                       area = params$area_name,
                       stringsAsFactors = FALSE)
  torun <- torun |>
    inner_join(formulas, by = "formula")

  if(params$skip_single_variable_models){
    torun <- torun |>
      filter(formula_version %in% c("Unstandardized",
                                    "Full standardization"))
  }

  if(params$parallel){
    library("doParallel")
    registerDoParallel(cores = floor(parallel::detectCores()/2L))
  }
  fn_model <- file.path(fn_csv,
                        paste0("cpue-models-arrowtooth-",
                               params$era,
                               "-",
                               params$area_name,
                               "-",
                               lubridate::as_date(lubridate::now()),
                               ".rds"))
  fn_predictions <- file.path(fn_csv,
                        paste0("cpue-predictions-arrowtooth-",
                               params$era,
                               "-",
                               params$area_name,
                               "-",
                               lubridate::as_date(now()),
                               ".csv"))

  if(!file.exists(fn_model)){
    if(params$discard_only){

      fit_func <- \(dat, formula = cpue ~ year_factor, ...){
        glmmTMB::glmmTMB(
          as.formula(formula),
          data = dat,
          family = Gamma(link = "log"),
          control = glmmTMB::glmmTMBControl(optCtrl = list(iter.max = 2000,
                                                           eval.max = 2000),
                                            profile = TRUE,
                                            collect = FALSE),
          ...)
      }
    }else{
      fit_func <- gfplot::fit_cpue_index_glmmtmb
    }
    system.time({
      model <- plyr::mlply(torun, \(formula, area, formula_version){
        message("Fitting model ", formula)
        fit_func(dfleet, as.formula(formula))
      },
      .parallel = params$parallel)
    })
    saveRDS(model, fn_model)
  } else {
    model <- readRDS(fn_model)
  }

  predictions <- plyr::ldply(model, predict_cpue_index_tweedie)
  write_csv(predictions, fn_predictions)

  list(model = model,
       predictions = predictions)
}