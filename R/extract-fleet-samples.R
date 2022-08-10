#' Extract commercial samples by fleet, where fleet is defined by a vector of vessel ids
#'
#' @param comm Output from [gfdata::get_commercial_samples()]
#' @param vessel_ids A vector of vessel IDs to include in the commercial samples if `include` is `TRUE`,
#' or exclude if `include` is `FALSE`
#' @param include include or exclude the `vessel_ids` from the returned commercial samples data set
#'
#' @return A filtered commercial samples data frame
#' @export
extract_fleet_samples <- function(comm,
                                  vessel_ids = c(568,   # VIKING ENTERPRISE
                                                 592,   # NORTHERN ALLIANCE
                                                 569,   # OSPREY NO. 1
                                                 595,   # RAW SPIRIT
                                                 608,   # PACIFIC LEGACY NO. 1
                                                 609,   # SUNDEROEY
                                                 1727), # VIKING ALLIANCE
                                  include = TRUE){

  if(include){
    comm %>% filter(vessel_id %in% vessel_ids)
  }else{
    comm %>% filter(!vessel_id %in% vessel_ids)
  }
}
