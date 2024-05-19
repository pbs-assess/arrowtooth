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
#' @param french Logical. If `TRUE`, use french names and labels in the plot
#' as necessary
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_trawl_footprint <- function(
    tf_rda_fn = "/home/grandin/github/main/pbs-data/PBSdata/data/trawlfoot.rda",
    crs_num = 3156, # Zone 9 NAD83
    utm_zone = 9, # For boundary labels only
    bath = c(100, 200, 500),
    x_lim = c(122, 890),
    y_lim = c(5373, 6027),
    rotation_angle = 0,
    rotation_center = c(500, 5700),
    show_majorbound = TRUE,
    major_labels = gfplot:::boundary_labels(utm_zone, xmin = x_lim[1]),
    leg_pos = c(0.86, 0.85),
    trawl_foot_fill = "black",
    trawl_foot_alpha = 1,
    survey_alpha = 0.5){

  crs_ll <- 4326
  # Function `load_and_convert_polysets()` loads a premade RDA file which
  # contains an object with the name `obj_nm` which is a data frame
  # representing a PolySet. A PolySet has 5 columns, PID (PolySet ID),
  # SID (Polygon or Shape ID), POS (Vertex number for unique PID, SID), X
  # (longitude) and Y (latitude). This data frame is converted into an `sf`
  # class data frame which has a `geometry` column, and the projection is set
  # to `crs_num` (crs_ll is lat/long or World Geodetic System 1984 WGS84).
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
      st_set_crs(crs_ll) |>
      st_transform(crs_num) |>
      group_by(poly) |>
      summarize(geometry = st_combine(geometry)) |>
      st_cast("POLYGON")
  }

  tf <- load_and_convert_polysets(tf_rda_fn, "trawlfoot")

  surv <- gfdata::survey_blocks |>
    as_tibble() |>
    # Remove HBLL surveys for this plot
    filter(grepl("^SYN", survey_abbrev))

  # Translate gear names
  surv <- surv |>
    mutate(survey_abbrev = tr(survey_abbrev))

  nm <- tr("Survey")
  nm_sym <- sym(nm)
  surv <- surv |>
    rename(!!nm_sym := survey_abbrev) |>
    st_as_sf() |>
    st_transform(crs = crs_num)

  ll_range <- gfplot:::utm2ll(cbind(X = x_lim, Y = y_lim))

  data("nepacLLhigh",
       package = "PBSmapping",
       envir = environment())

  buffer <- 2
  coastline_utm <- clipPolys(nepacLLhigh,
                             xlim = ll_range$X + c(-buffer, buffer),
                             ylim = ll_range$Y + c(-buffer, buffer)) |>
    as_tibble() |>
    select(-c(POS, oldPOS)) |>
    rename(poly = PID) |>
    rotate_df(rotation_angle, rotation_center) |>
    st_as_sf(coords = c("X", "Y")) |>
    st_set_crs(crs_ll) |>
    st_transform(crs_num) |>
    group_by(poly) |>
    summarize(geometry = st_combine(geometry)) |>
    st_cast("POLYGON")

  data("isobath",
       package = "PBSdata",
       envir = environment())
  isobath <- isobath |>
    filter(PID %in% bath)
  x_buf <- -3
  y_buf <- 3
  isobath_utm <- clipPolys(isobath,
                           xlim =  ll_range$X + c(x_buf, y_buf),
                           ylim =  ll_range$Y + c(x_buf, y_buf)) |>
    as_tibble() |>
    select(-c(POS, oldPOS)) |>
    rename(poly = PID,
           secpoly = SID) |>
    rotate_df(rotation_angle, rotation_center) |>
    st_as_sf(coords = c("X", "Y")) |>
    st_set_crs(crs_ll) |>
    st_transform(crs_num) |>
    group_by(poly, secpoly) |>
    summarize(geometry = st_combine(geometry)) |>
    st_cast("LINESTRING")

  g <- ggplot(isobath_utm,
              aes(geometry = geometry / 1000)) +
    geom_sf(lwd = 0.4,
            col = "grey70",
            alpha = 0.7)

  if(show_majorbound){
    # add major management region boundaries
    data("major",
         package = "PBSdata",
         envir = environment())

    majorbounds <- major |>
      as_tibble() |>
      rename(poly = PID) |>
      st_as_sf(coords = c("X", "Y")) |>
      st_set_crs(crs_ll) |>
      st_transform(crs_num) |>
      group_by(poly) |>
      summarize(geometry = st_combine(geometry)) |>
      st_cast("POLYGON")

    major_labels <- attributes(major)$PolyData |>
      as_tibble()

    major_labels[major_labels$label == "4B",]$X <- 885
    major_labels[major_labels$label == "4B",]$Y <- 5475
    major_labels[major_labels$label == "3C",]$X <- 200
    major_labels[major_labels$label == "3C",]$Y <- 5400
    major_labels[major_labels$label == "3D",]$X <- 200
    major_labels[major_labels$label == "3D",]$Y <- 5500
    major_labels[major_labels$label == "5A",]$X <- 200
    major_labels[major_labels$label == "5A",]$Y <- 5650
    major_labels[major_labels$label == "5B",]$X <- 200
    major_labels[major_labels$label == "5B",]$Y <- 5725
    major_labels[major_labels$label == "5C",]$X <- 445
    major_labels[major_labels$label == "5C",]$Y <- 5840
    major_labels[major_labels$label == "5D",]$X <- 340
    major_labels[major_labels$label == "5D",]$Y <- 5970
    major_labels[major_labels$label == "5E",]$X <- 200
    major_labels[major_labels$label == "5E",]$Y <- 5850

    major_labels <- major_labels |>
      st_as_sf(coords = c("X", "Y")) |>
      st_set_crs(crs_num)

    g <- g +
      geom_sf(data = majorbounds,
              aes(geometry = geometry / 1000),
              colour = "grey50",
              alpha = 0.1,
              lty = 1) +
      geom_sf_text(data = major_labels,
                   aes(geometry = geometry,
                       label = label),
                   color = "grey50")
  }

  g <- g +
    geom_sf(data = coastline_utm,
            aes(geometry = geometry / 1000),
            lwd = 0.4,
            col = "grey70",
            alpha = 0.7,
            fill = "grey70") +
    coord_sf(datum = st_crs(crs_num),
             xlim = x_lim,
             ylim = y_lim,
             expand = FALSE)

  g <- g +
    geom_sf(data = tf,
            fill = trawl_foot_fill,
            alpha = trawl_foot_alpha) +
    geom_sf(data = surv,
            aes(fill = !!nm_sym,
                geometry = geometry / 1000),
            color = "transparent",
            alpha = survey_alpha) +
    scale_colour_brewer(palette = "Dark2") +
    coord_equal(xlim = x_lim, ylim = y_lim) +
    coord_sf(datum = st_crs(crs_num),
             xlim = x_lim,
             ylim = y_lim,
             expand = FALSE) +
    scale_x_continuous(labels = comma) +
    scale_y_continuous(labels = comma) +
    guides(colour = guide_legend(override.aes = list(alpha = 1))) +
    theme(legend.position = leg_pos) +
    xlab(tr("Easting")) +
    ylab(tr("Northing"))

  g
}
