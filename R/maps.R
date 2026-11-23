# all_colony <- dplyr::tbl(con, dbplyr::in_schema("metadata", "colony"))
# kittiwake_sessions <- seatrackR::get_session_info(species = "Black-legged kittiwake", as_tibble = FALSE) %>%
#     left_join(dplyr::select(all_colony, colony_int_name, country), by = dplyr::join_by(colony == colony_int_name)) %>%
#     filter(country %in% c(
#         "Denmark",
#         "Norway",
#         "Netherlands",
#         "Great Britain",
#         "Faroe Islands",
#         "Greenland",
#         "Iceland",
#         "Sweden", "Finland",
#         "Ireland"
#     ))
# colony_counts <- dplyr::group_by(kittiwake_sessions, colony) %>%
#     dplyr::summarise(n = n()) %>%
#     collect()
# colony_counts <- colony_counts[order(colony_counts$colony), ]


# base_maps <- make_base_map(ortho = TRUE, graticules = FALSE, ortho_lat = 50, ortho_lon = 0)

# get_colony_map(
#     colony_names = colony_counts$colony,
#     symbol_size = colony_counts$n,
#     base_map = base_maps$base_map,
#     symbol_border_lwd = 0.001,
#     bbox = c(
#         -40, 49.238, 33.650, 82.085
#     ),
#     size_legend_title = "Deployments"
# )

get_colony_map <- function(
    colony_names = NULL,
    base_map = NULL,
    symbol_size = 2,
    size_legend_title = "colony_symbol_size",
    symbol_col = "#EE1D25",
    symbol_border_lwd = 2,
    ortho = TRUE,
    bbox = NULL,
    bbox_buffer = 2.5) {
    if (!is.null(base_map)) {
        # Force Ortho if passed an ortho base map
        crs_str <- as.character(sf::st_crs(base_map[[1]][[1]])$wkt)
        ortho <- any(grepl("Orthographic", crs_str))
        ortho_lat_str <- sub(".*Latitude of natural origin[^0-9+-]*([-+]?[0-9]*\\.?[0-9]+).*", "\\1", crs_str)
        ortho_lon_str <- sub(".*Longitude of natural origin[^0-9+-]*([-+]?[0-9]*\\.?[0-9]+).*", "\\1", crs_str)
        ortho_lat <- as.numeric(ortho_lat_str)
        ortho_lon <- as.numeric(ortho_lon_str)
    } else {
        ortho_lat <- 30
        ortho_lon <- -10
    }

    all_colony <- seatrackR::getColonies(loadGeometries = TRUE)
    all_colony <- all_colony[order(all_colony$colony_int_name), ]
    if (!is.null(colony_names)) {
        all_colony <- all_colony[all_colony$colony_int_name %in% colony_names | all_colony$colony_nat_name %in% colony_names, ]
    }
    all_colony$colony_symbol_size <- symbol_size
    all_colony$colony_symbol_col <- symbol_col
    if (is.na(sf::st_crs(all_colony))) {
        sf::st_crs(all_colony) <- 4326
    }
    if (!ortho) {
        if (is.null(bbox)) {
            bbox <- sf::st_bbox(all_colony) + c(-bbox_buffer, -bbox_buffer, +bbox_buffer, +bbox_buffer)
        } else {
            if (!class(bbox) == "bbox") {
                names(bbox) <- c("xmin", "ymin", "xmax", "ymax")
                bbox <- sf::st_bbox(bbox, crs = 4326)
            }
        }
    }


    if (ortho) {
        all_colony <- project_to_ortho(all_colony, lat = ortho_lat, lon = ortho_lon)
    }

    if (is.null(base_map)) {
        base_maps <- make_base_map(
            ortho = ortho,
            bbox = bbox
        )
        base_map <- base_maps$base_map
    }
    if (!ortho) {
        map <- tmap::tm_shape(sf::st_as_sfc(bbox), bbox = bbox) +
            base_map
    } else {
        map <- base_map
    }
    map <- map +
        tmap::tm_shape(all_colony) +
        tmap::tm_symbols(
            size = ifelse(length(symbol_size) == 1, symbol_size, "colony_symbol_size"),
            fill = "colony_symbol_col",
            lwd = symbol_border_lwd,
            col = grey(0),
            size.legend = tmap::tm_legend(title = size_legend_title, frame = FALSE)
        ) +
        tmap::tm_layout(
            frame = TRUE,
            frame.color = "black",
            frame.lwd = 2,
            frame.double_line = FALSE
        )
    return(map)
}

project_to_ortho <- function(object, lat = 30, lon = -10) {
    # Define the orthographic projection
    # Choose lat_0 with -90 <= lat_0 <= 90 and lon_0 with -180 <= lon_0 <= 180
    ortho <- paste0("+proj=ortho +lat_0=", lat, " +lon_0=", lon, " +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs")

    # Define the polygon that will help you finding the "blade"
    # to split what lies within and without your projection
    circle <- sf::st_point(x = c(0, 0)) %>%
        sf::st_buffer(dist = 6371000) %>%
        sf::st_sfc(crs = ortho)

    # Project this polygon in lat-lon
    circle_longlat <- circle %>% sf::st_transform(crs = 4326)

    # circle_longlat cannot be used as it is
    # You must decompose it into a string with ordered longitudes
    # Then complete the polygon definition to cover the hemisphere
    if (lat != 0) {
        circle_longlat <- sf::st_boundary(circle_longlat)

        circle_coords <- sf::st_coordinates(circle_longlat)[, c(1, 2)]
        circle_coords <- circle_coords[order(circle_coords[, 1]), ]
        circle_coords <- circle_coords[!duplicated(circle_coords), ]

        # Rebuild line
        circle_longlat <- sf::st_linestring(circle_coords) %>% sf::st_sfc(crs = 4326)

        if (lat > 0) {
            rectangle <- list(rbind(
                circle_coords,
                c(X = 180, circle_coords[nrow(circle_coords), "Y"]),
                c(X = 180, Y = 90),
                c(X = -180, Y = 90),
                c(X = -180, circle_coords[1, "Y"]),
                circle_coords[1, c("X", "Y")]
            )) %>%
                sf::st_polygon() %>%
                sf::st_sfc(crs = 4326)
        } else {
            rectangle <- list(rbind(
                circle_coords,
                c(X = 180, circle_coords[nrow(circle_coords), "Y"]),
                c(X = 180, Y = -90),
                c(X = -180, Y = -90),
                c(X = -180, circle_coords[1, "Y"]),
                circle_coords[1, c("X", "Y")]
            )) %>%
                st_polygon() %>%
                st_sfc(crs = 4326)
        }

        circle_longlat <- sf::st_union(sf::st_make_valid(circle_longlat), sf::st_make_valid(rectangle))
    }

    # This visualization shows the visible emisphere in red
    # ggplot() +
    #   geom_sf(data = object) +
    #   geom_sf(data = circle_longlat, color = 'red', fill = 'red', alpha = 0.3)

    # A small negative buffer is necessary to avoid polygons still disappearing in a few pathological cases
    # I should not change the shapes too much
    visible <- sf::st_intersection(sf::st_make_valid(object), sf::st_buffer(circle_longlat, -0.09)) %>%
        sf::st_transform(crs = ortho)

    # DISCLAIMER: This section is the outcome of trial-and-error and I don't claim it is the best approach
    # Resulting polygons are often broken and they need to be fixed
    # Get reason why they're broken
    broken_reason <- sf::st_is_valid(visible, reason = TRUE)

    # First fix NA's by decomposing them
    # Remove them from visible for now
    na_visible <- visible[is.na(broken_reason), ]
    visible <- visible[!is.na(broken_reason), ]




    if (nrow(na_visible) > 0) {
        # Open and close polygons
        na_visible <- sf::st_cast(na_visible, "MULTILINESTRING") %>%
            sf::st_cast("LINESTRING", do_split = TRUE)
        coords <- sf::st_coordinates(na_visible)
        na_visible$npts <- tabulate(coords[, ncol(coords)], nbins = nrow(na_visible))

        # Exclude polygons with less than 4 points
        na_visible <- na_visible %>%
            dplyr::filter(npts >= 4) %>%
            dplyr::select(-npts) %>%
            sf::st_cast("POLYGON")
    }
    # Fix other broken polygons
    broken <- which(!sf::st_is_valid(visible))
    for (land in broken) {
        result <- tryCatch(
            {
                # visible[land,] <- st_buffer(visible[land,], 0) # Sometimes useful sometimes not
                visible[land, ] <- sf::st_make_valid(visible[land, ]) %>%
                    st_collection_extract()
            },
            error = function(e) {
                visible[land, ] <<- sf::st_buffer(visible[land, ], 0)
            }
        )
    }

    # Bind together the two tables
    visible <- rbind(visible, na_visible)
    return(visible)
}

#' Get SEATRACK basemap
#'
#' This function creates a base map for SEATRACK visualizations, including bathymetry, land, rivers, lakes, glaciers, and optional graticules. The map can be projected in orthographic view or standard projection.
#'
#' @param ortho Logical, whether to use orthographic projection (default TRUE).
#' @param ortho_lat Latitude for orthographic projection center (default 30).
#' @param ortho_long Longitude for orthographic projection center (default -10).
#' @param graticules Logical, whether to include graticules (default TRUE).
#' @param graticule_lwd Line width for graticules (default 3).
#' @param colour_bath_1 Color for bathymetry level 0 (default "#9EBBD7").
#' @param colour_bath_2 Color for bathymetry level 1000 (default "#8FADCC").
#' @param colour_land Color for land areas (default "#E1E1E1").
#' @param colour_rivers Color for rivers (default "#A4BED8").
#' @param colour_glacier Color for glaciers (default "white").
#' @param bg_colour Background color for the map (default grey(0.5)).
#'
#' @return A list containing the base map, base map with bathymetry, and base map with land features.
#' @export
#' @concept maps
make_base_map <- function(
    ortho = TRUE,
    ortho_lat = 30,
    ortho_lon = -10,
    graticules = TRUE,
    graticule_lwd = 3,
    colour_bath_1 = "#9EBBD7",
    colour_bath_2 = "#8FADCC",
    colour_land = "#E1E1E1",
    colour_rivers = "#A4BED8",
    colour_glacier = "white",
    lwd_rivers = tmap::tm_const(),
    bg_colour = grey(0.5),
    bbox = NULL) {
    map_data_path <- file.path(the$sea_track_folder, "Admin", "Maps", "map data")

    proj.ortho <- glue::glue("+proj=ortho +lat_0={ortho_lat}  +lon_0={ortho_lon}  +units=km")

    if (ortho) {
        circle <- sf::st_point(x = c(0, 0)) %>%
            sf::st_buffer(dist = 6387.5) %>%
            sf::st_sfc(crs = proj.ortho)
        circle <- sf::st_sfc(sf::st_cast(circle, "LINESTRING"))
    }

    NPland <- sf::st_read(file.path(map_data_path, "Coastline_GSHHS_NPI.shp"))
    NPland <- NPland[NPland$SOURCE == "NPI 1MILL", ]
    if (ortho) {
        NPland <- sf::st_transform(NPland, proj.ortho)
    }

    NPland <- sf::st_cast(NPland, "POLYGON")

    bath0 <- sf::st_read(file.path(map_data_path, "ne_10m_bathymetry_all", "ne_10m_bathymetry_L_0.shp"))
    bath1000 <- sf::st_read(file.path(map_data_path, "ne_10m_bathymetry_all", "ne_10m_bathymetry_J_1000.shp"))
    if (ortho) {
        bath0 <- project_to_ortho(bath0, lat = ortho_lat, lon = ortho_lon)
        bath1000 <- project_to_ortho(bath1000, lat = ortho_lat, lon = ortho_lon)
    }

    rvrs <- sf::st_read(file.path(map_data_path, "ne_10m_rivers_lake_centerlines.shp"))
    rvrs <- sf::st_cast(rvrs, "MULTILINESTRING") %>%
        sf::st_cast("LINESTRING", do_split = TRUE)

    if (ortho) {
        rvrs <- sf::st_transform(rvrs, crs = proj.ortho)
    }

    coords <- sf::st_coordinates(rvrs)
    rvrs$npts <- tabulate(coords[, ncol(coords)], nbins = nrow(rvrs))
    rvrs <- rvrs[rvrs$npts > 3, ]

    lks <- sf::st_read(file.path(map_data_path, "ne_50m_lakes.shp"))
    lks <- sf::st_cast(lks, "MULTILINESTRING") %>%
        sf::st_cast("LINESTRING", do_split = TRUE)
    if (ortho) {
        lks <- sf::st_transform(lks, crs = proj.ortho)
    }

    coords <- sf::st_coordinates(lks)
    lks$npts <- tabulate(coords[, ncol(coords)], nbins = nrow(lks))
    lks <- lks[lks$npts > 3, ]

    lks <- sf::st_cast(lks, "POLYGON")

    glac <- sf::st_read(file.path(map_data_path, "ne_50m_glaciated_areas.shp"))
    glac <- sf::st_cast(glac, "MULTILINESTRING") %>%
        sf::st_cast("LINESTRING", do_split = TRUE)

    if (ortho) {
        glac <- sf::st_transform(glac, crs = proj.ortho)
    }

    coords <- sf::st_coordinates(glac)
    glac$npts <- tabulate(coords[, ncol(coords)], nbins = nrow(glac))
    glac <- glac[glac$npts > 3, ]

    glac <- sf::st_cast(glac, "POLYGON")

    if (graticules) {
        grat10 <- sf::st_read(file.path(map_data_path, "ne_50m_graticules_10.shp"))
        # grat10 <- grat10[grat10$direction %in% c("N"),]
        grat10 <- grat10[grat10$degrees %in% seq(-80, 80, 20) & grat10$direction %in% c("N", "S", NA), ]
        # if (ortho) {
        #     grat10 <- sf::st_crop(grat10, ymin = -85, ymax = 85, xmin = -180, xmax = 360)
        # }
        grat30 <- sf::st_read(file.path(map_data_path, "ne_50m_graticules_30.shp"))
        grat30 <- grat30[grat30$direction %in% c("E", "W", NA), ]
        # if (ortho) {
        #     grat30 <- sf::st_crop(grat30, ymin = -85, ymax = 85, xmin = -180, xmax = 360)
        # }
    }

    land <- rnaturalearth::ne_countries(scale = 50)
    if (ortho) {
        land <- project_to_ortho(land, lat = ortho_lat, lon = ortho_lon)
    }
    land$sovereignt[land$sovereignt == "Denmark"] <- land$subunit[land$sovereignt == "Denmark"]


    if (ortho) {
        # boundary box - seems overly complex?
        x1 <- -4125
        y1 <- 1775
        x2 <- 2233
        y2 <- 6350 # 5529.879
        bbox <- matrix(c(
            x1, x2, x2, x1, x1,
            y1, y1, y2, y2, y1
        ), 5, byrow = F)
        bbox_poly <- sf::st_sfc(sf::st_polygon(list(bbox)))

        sf::st_crs(bbox_poly) <- proj.ortho
    } else {
        if (is.null(bbox)) {
            bbox_poly <- sf::st_as_sfc(sf::st_bbox(land))
        } else {
            if (!class(bbox) == "bbox") {
                names(bbox) <- c("xmin", "ymin", "xmax", "ymax")
                bbox <- sf::st_bbox(bbox, crs = 4326)
            }
            bbox_poly <- sf::st_as_sfc(bbox)
        }
    }



    base_map_bath <-
        tmap::tm_shape(bbox_poly) +
        tmap::tm_polygons(
            fill = bg_colour, fill_alpha = 0, fill.legend = tmap::tm_legend_hide(),
            col = "white", col_alpha = 0
        ) +
        tmap::tm_shape(bath0) +
        tmap::tm_polygons(
            fill = colour_bath_1, fill.legend = tmap::tm_legend_hide(),
            col = colour_bath_1, lwd = 2
        ) +
        tmap::tm_shape(bath1000) +
        tmap::tm_polygons(
            fill = colour_bath_2, fill.legend = tmap::tm_legend_hide(),
            col = colour_bath_2, lwd = 1
        )

    base_map_land <-
        tmap::tm_shape(bbox_poly) +
        tmap::tm_polygons(
            fill = bg_colour, fill_alpha = 0, fill.legend = tmap::tm_legend_hide(),
            col = "white", col_alpha = 0
        ) +
        tmap::tm_shape(NPland) +
        tmap::tm_polygons(
            fill = colour_land, fill.legend = tmap::tm_legend_hide(),
            col = "white", col_alpha = 0
        ) +
        tmap::tm_shape(land) +
        tmap::tm_polygons(
            fill = colour_land, fill.legend = tmap::tm_legend_hide(),
            col = "white", col_alpha = 0
        ) +
        tmap::tm_shape(rvrs) +
        tmap::tm_lines(col = colour_rivers, lwd = lwd_rivers) +
        tmap::tm_shape(lks) +
        tmap::tm_polygons(
            fill = colour_rivers, fill.legend = tmap::tm_legend_hide(),
            col = "white", col_alpha = 0
        ) +
        tmap::tm_shape(glac) +
        tmap::tm_polygons(
            fill = colour_glacier, fill.legend = tmap::tm_legend_hide(),
            col = "white", col_alpha = 0
        ) +
        tmap::tm_layout(
            frame = FALSE,
            outer.margins = 0,
            bg.color = "transparent"
        )
    if (ortho) {
        base_map_land <- base_map_land +
            tmap::tm_shape(circle) +
            tmap::tm_lines(lwd = 4)
    }

    base_map <- base_map_bath + base_map_land
    if (graticules) {
        base_map_graticules <-
            tmap::tm_shape(bbox_poly) +
            tmap::tm_polygons(
                fill = bg_colour, fill_alpha = 0, fill.legend = tmap::tm_legend_hide(),
                col = "white", col_alpha = 0
            ) +
            tmap::tm_shape(grat10) +
            tmap::tm_lines(col = grey(1), lwd = graticule_lwd) +
            tmap::tm_shape(grat30) +
            tmap::tm_lines(col = grey(1), lwd = graticule_lwd)
        if (ortho) {
            base_map_graticules <- base_map_graticules +
                tmap::tm_shape(circle) +
                tmap::tm_lines(lwd = 4)
        }
        base_map <- base_map + base_map_graticules
    }

    return(list(base_map = base_map, base_map_bath = base_map_bath, base_map_land = base_map_land))
}
