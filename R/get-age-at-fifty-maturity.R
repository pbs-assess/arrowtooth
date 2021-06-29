#' Title
#'
#' @param surv Output from [gfdata::get_survey_samples()]
#' @param surv_series A vector of integers representing the survey ids to use
#' 1:4 = c("SYN QCS", "OTHER HS MSA", "SYN HS", "SYN WCVI")
#' @param maturity_code cutoff value for maturity code. See table in `details` section
#' This value and above is considered mature and below this value is considered immature
#' @param maturity_convention_code Type of maturity scale to use. Flatfish is code 4 (default)
#' @param start_year First year to include
#' @param end_year Last year to include
#' @param sex One of "both", "female" or "male"
#'
#' @details The maturity code table for maturity_convention_code 4 (flatfish):
#' MATURITY_CODE,SPECIMEN_SEX_CODE,MATURITY_NAME,MATURITY_DESC
#' 1, 1, IMMATURE, "TESTES VERY SMALL, STRING-LIKE AND SOMEWHAT TRANSLUCENT OR PINKISH IN COLOUR"
#' 1, 2, IMMATURE, "OVARIES VERY SMALL, TRANSLUCENT OR PINKISH  AND SOMEWHAT GELATINOUS IN TEXTURE"
#' 2, 1, MATURING, "TESTES ENLARGING, A DISTINCT BULGE EVIDENT BUT STILL TRANSLUCENT OR PINKISH IN COLOUR"
#' 2, 2, MATURING, "OVARIES RELATIVELY SMALL, PINKISH-YELLOW OR CREAM IN COLOUR, GRANULAR IN TEXTURE.  NO DISTINCT EGGS VISIBLE"
#' 3, 1, DEVELOPING, "TESTES ENLARGING, BROWN-WHITE OR WHITE IN COLOUR, FIRM IN TEXTURE"
#' 3, 2, DEVELOPING, "OVARIES LARGE, CREAM OR YELLOW IN COLOUR CONTAINING OPAQUE EGGS THAT CAN BE DISTINGUISED BY DIRECT OBSERVATION.  SEX MAY BE DETERMINED EXTERNALLY"
#' 4, 1, RIPE, "TESTES LARGE, WHITE AND EASILY BROKEN. NO SPERM EVIDENT"
#' 4, 2, GRAVID, "OVARIES CONTAINING PARTLY OR WHOLLY TRANSLUCENT EGGS.  SEX EASILY DETERMINED EXTERNALLY"
#' 5, 1, SPAWNING, "TESTES LARGE, WHITE AND SPERM EVIDENT"
#' 5, 2, RIPE, "OVARIES CONTAINING ENTIRELY TRANSLUCENT, MATURE OVA.  EGGS LOOSE AND WILL RUN FROM OVIDUCTS UNDER SLIGHT PRESSURE"
#' 6, 1, SPENT, "TESTES FLACCID, SHRUNKEN AND YELLOW-BROWN IN COLOUR. SPERM DUCTS ENLARGED AND A SMALL AMOUNT OF SPRM MAY BE PRESENT"
#' 6, 2, SPENT, "OVARIES LARGE, FLACCID AND PURPLE IN COLOUR; A FEW TRANSLUCENT EGGS MAY BE LEFT.  OVARIAN MEMBRANE VERY VASCULAR (BLOODSHOT) AND SAC-LIKE"
#' 7, 1, RESTING, "TESTES FIRM, SMALL AND YELLOW-BROWN IN COLOUR.  SPERM DUCTS SMALL"
#' 7, 2, RESTING, "OVARIES CONTRACTED AND FIRM, PINKISH GREY TO CREAM-YELLOW IN COLOUR AND MAY APPEAR GRANULAR IN TEXTURE BUT NO DISTINCT EGGS ARE VISIBLE"
#'
#' @return A list of two vectors, one for male and one for female where the vector items are age-at-50% and sd-at-50% maturity.
#' @export
#'
#' @examples
#' \dontrun
#' survey_samples <- gfdata::get_survey_samples("arrowtooth flounder")
#' get_age_at_50_mat(survey_samples)
get_age_at_50_mat <- function(surv,
                              surv_series = 1:4,
                              maturity_code = 5,
                              maturity_convention_code = 4,
                              start_year = 1996,
                              end_year = 2019,
                              sex = "both"){

  j <- surv %>%
    #filter(major_stat_area_code %in% areas) %>%
    filter(survey_series_id %in% surv_series) %>%
    filter(year %in% start_year:end_year) %>%
    filter(!is.na(age),
           !is.na(maturity_code),
           sex %in% 1:2) %>%
    filter(maturity_convention_code %in% !!maturity_convention_code) %>%
    select(age, sex, maturity_code)

  k <- age_prop_mature <- map(1:2, ~{
    j %>%
      filter(sex == .x) %>%
      group_by(age) %>%
      mutate(is_mature = ifelse(maturity_code < !!maturity_code, FALSE, TRUE)) %>%
      summarize(prop_mature = sum(is_mature) / n()) %>%
      ungroup
  })

  mat_model <- function(par, age, prop_mature){
    prop_m <- 1 / (1 + exp(- ((age - par[1]) / par[2])))
    sum((prop_m - prop_mature) ^ 2)
  }
  kk <- map2(k, 1:2, ~{
    conv <- optim(par = c(5, 3),
                  fn = mat_model,
                  method = "L-BFGS-B",
                  lower = 0,
                  upper = Inf,
                  age = k[[.y]]$age,
                  prop_mature = k[[.y]]$prop_mature)
    if(conv$convergence != 0){
      stop("Age-at-50% model failed to converge for ",
           ifelse(.y == 1, "males", "females"),
           ". Message from optim was: ", conv$message,
           call. = FALSE)
    }
    conv
  })
  list(kk[[1]]$par, kk[[2]]$par) %>%
    `names<-`(c("male", "female"))
}