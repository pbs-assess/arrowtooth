#' Create a map of commercial trawl footprint overlaid by synoptic survey
#' grids
#'
#' @param tf_rda_fn The name of the trawl footprint RDA file. Get it from here:
#' https://github.com/pbs-software/pbs-data/tree/master/PBSdata
#' @param crs_num The projection to use  See https://epsg.io/
#' @param x_lim The limits for the x-axis of the map in the units defined by
#' `crs_num`
#' @param y_lim The limits for the y-axis of the map in the units defined by
#' `crs_num`
#' @param leg_pos A vector of the x/y position of the legend inside the panel.
#' Values are between 0 and 1
#' @param trawl_foot_fill The color to use to fill the trawl footprint polygons
#' @param trawl_foot_alpha The opacity to use for the trawl footprint polygons
#' @param survey_alpha The opacity to use for the survey grid lines
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_trawl_footprint <- function(
    tf_rda_fn = "/home/grandin/github/main/pbs-data/PBSdata/data/trawlfoot.rda",
    crs_num = 4326,
    x_lim = c(-135, -122),
    y_lim = c(48, 55),
    leg_pos = c(0.87, 0.9),
    trawl_foot_fill = "black",
    trawl_foot_alpha = 1,
    survey_alpha = 0.5){

  # Function `load_and_convert_polysets()` loads a premade RDA file which
  # contains an object with the name `obj_nm` which is a data frame
  # representing a PolySet. A PolySet has 5 columns, PID (PolySet ID),
  # SID (Polygon or Shape ID), POS (Vertex number for unique PID, SID), X
  # (longitude) and Y (latitude). This data frame is converted into an `sf`
  # class data frame which has a `geometry` column, and the projection is set
  # to `crs_num` (4326 is lat/long or World Geodetic System 1984 WGS84).
  load_and_convert_polysets <- \(fn, obj_nm){

    load(fn)
    obj <- get(obj_nm)

    if(!"SID" %in% names(obj)){
      obj <- obj |>
        rename(SID = PID) |>
        mutate(PID = 0)
    }

    obj |>
      as_tibble() |>
      select(-PID) |>
      rename(poly = SID,
             vertex = POS,
             long = X,
             lat = Y) |>
      st_as_sf(coords = c("long", "lat")) |>
      group_by(poly) |>
      summarize(geometry = st_combine(geometry)) |>
      st_cast("POLYGON") |>
      st_set_crs(crs_num)
  }

  tf <- load_and_convert_polysets(tf_rda_fn, "trawlfoot")

  surv <- gfdata::survey_blocks |>
    as_tibble() |>
    # Remove HBLL surveys for this plot
    filter(grepl("^SYN", survey_abbrev)) |>
    rename(Survey = survey_abbrev) |>
    st_as_sf() |>
    st_transform(crs = crs_num)

  # Start with the hake map which incorporates bathymetry raster and
  # looks nice
  hake::plot_map(x_lim = x_lim,
                 y_lim = y_lim) +
    geom_sf(data = tf,
            fill = trawl_foot_fill,
            alpha = trawl_foot_alpha) +
    geom_sf(data = surv,
            aes(color = Survey,
                geometry = geometry),
            fill = "transparent",
            alpha = survey_alpha) +
    scale_colour_brewer(palette = "Dark2") +
    coord_sf(datum = st_crs(crs_num),
             xlim = x_lim,
             ylim = y_lim,
             expand = FALSE) +
    guides(colour = guide_legend(override.aes = list(alpha = 1))) +
    theme(legend.position = leg_pos)
}
