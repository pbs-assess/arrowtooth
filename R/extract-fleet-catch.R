#' Extract catch by fleet, where fleet is defined by a vector of vessel names
#'
#' @param catch Output from [gfdata::get_catch()]
#' @param vessel_names A vector of vessel names to include in the catch if
#' `include` is `TRUE`,
#' or exclude if `include` is `FALSE`
#' @param include include or exclude the `vessel_names` from the returned
#' catch data set
#'
#' @return A filtered catch data frame
#' @export
extract_fleet_catch <- function(catch,
                                vessel_names = c("VIKING ENTERPRISE",
                                                 "NORTHERN ALLIANCE",
                                                 "OSPREY NO 1",
                                                 "RAW SPIRIT",
                                                 "PACIFIC LEGACY NO. 1",
                                                 "SUNDEROEY",
                                                 "VIKING ALLIANCE"),
                                include = TRUE){

  if(include){
    catch %>% filter(vessel_name %in% vessel_names)
  }else{
    catch %>% filter(!vessel_name %in% vessel_names)
  }
}
