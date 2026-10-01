seapop_paths <- list(
    colonies = file.path(the$sea_track_folder, "Data", "Data products", "Kernel_data", "SEAPOP kernel daten 20260923", "all_colonies_seapop.geojson"),
    kde = file.path(the$sea_track_folder, "Data", "Data products", "Kernel_data", "SEAPOP kernel daten 20260923", "all_kde_seapop.geojson")
)

get_web_kernel_data <- function(seapop_paths = NULL) {
    if (!is.null(seapop_paths)) {
        # Load existing SEAPOP kernels/colonies
        seapop_kernels <- sf::st_read(seapop_paths$kde)
        seapop_colonies <- sf::st_read(seapop_paths$colonies)
        seapop_data <- list(colonies = seapop_colonies, kde = seapop_kernels)

        seapop_table <- dplyr::select(seapop_colonies, species, colony) %>%
            dplyr::filter(!colony == "All_colonies") %>%
            distinct()
        selected_species <- unique(seapop_table$species)

        db_seapop_table <- dplyr::tbl(con, dbplyr::in_schema("metadata", "seapop_species_colony")) %>% collect()

        colonies <- unique(db_seapop_table$seatrack_colony)
    } else {
        colonies <- seatrackR::getColonies(loadGeometries = TRUE)
        colonies <- colonies[!sf::st_is_empty(colonies), , drop = FALSE]$colony_int_name
        selected_species <- seatrackR::getSpecies() %>% dplyr::filter(seatrack_species)$species_name_eng
        seapop_data <- NULL
    }
    ind_info <- seatrackR::getIndividInfo(colony = colonies, species = selected_species, age_at_deployment = "A")
    logger_info <- seatrackR::getLoggerInfo(colony = colonies, species = selected_species)

    all_models <- seatrackR::getLoggerModels()
    logger_info <- dplyr::left_join(logger_info, dplyr::select(all_models, model, logger_type), by = dplyr::join_by(logger_model == model), multiple = "first")

    # Add logger type, but nothing is currently done with this info.
    ind_info <- dplyr::left_join(ind_info, dplyr::select(logger_info, logger_serial_no, logger_type), by = dplyr::join_by(logger_id == logger_serial_no), multiple = "first")

    # run for each species ----------------------------------------------------

    for (species_name in selected_species) {
        species_session_ids <- ind_info$session_id[ind_info$species == species_name]
        species_kernel_result <- get_kernel(session_ids = species_session_ids, species_name = species_name, data_type = "GLS", seapop_data = seapop_data)
    }
}



get_kernel <- function(
    session_ids,
    export = FALSE,
    species_name = NULL,
    output_folder = file.path(the$sea_track_folder, "Data", "Data products", "Kernel_data_new"),
    data_type = "GLS",
    seapop_data = NULL) {

    # load postion data
    log_info("Getting position data for ", species_name, " (", data_type, ")")
    dat <- seatrackR::getPositions(species = species_name, datatype = data_type, sessionId = session_ids)
    # switch location out for colony
    locations <- seatrackR::getColonies(allLocations = TRUE)
    colonies <- seatrackR::getColonies(allLocations = FALSE, loadGeometries = TRUE)
    dat$location <- dat$colony
    dat <- dplyr::left_join(dat, dplyr::select(locations, location_name, colony_int_name), by = dplyr::join_by(colony == location_name))
    dat$colony <- dat$colony_int_name
    dat$colony_int_name <- NULL

    # if seapop, switch location out for seapop colony
    if (!is.null(seapop_data)) {
        db_seapop_table <- dplyr::tbl(con, dbplyr::in_schema("metadata", "seapop_species_colony")) %>% collect()

        # Load existing SEAPOP kernels/colonies
        seapop_kernels <- seapop_data$kde
        seapop_colonies <- seapop_data$colonies

        seapop_table <- dplyr::select(seapop_colonies, species, colony) %>%
            dplyr::filter(!colony == "All_colonies") %>%
            distinct()

        seapop_table <- dplyr::left_join(
            seapop_table,
            dplyr::select(db_seapop_table, seapop_colony, seatrack_colony, seatrack_species),
            by = dplyr::join_by(species == seatrack_species, colony == seapop_colony)
        )

        colonies <- dplyr::filter(seapop_table, species %in% dat$species) %>% dplyr::select(colony_int_name = colony)

        seapop_colony_match_idx <- match(paste(dat$colony, dat$species), paste(seapop_table$seatrack_colony, seapop_table$species))
        original_colony <- dat$colony
        dat$colony <- seapop_table$colony[seapop_colony_match_idx]
        dat$colony[is.na(dat$colony)] <- original_colony[is.na(dat$colony)]
        # dat <- dat[!is.na(dat$colony), ]

        # unique(paste(dat$colony, dat$species)[is.na(match(paste(dat$colony, dat$species), paste(seapop_table$seatrack_colony, seapop_table$species)))])
    }

    dat$type <- data_type

    # remove equinox positions following automated processing
    dat <- dat[dat$eqfilter == TRUE, ]
    # remove duplicate rows
    dat <- dat[!duplicated(paste(dat$tfirst, dat$tsecond, dat$type, dat$species, dat$colony, dat$id)), ]

    dat$date <- as.Date(dat$date_time)
    dat$month <- as.numeric(format(dat$date, "%m"))
    dat$year <- as.numeric(format(dat$date, "%Y"))
    dat$doy <- as.numeric(format(dat$date, "%j"))

    # define seasons
    dat$season <- "summer"
    dat$season[dat$month %in% c(8:10)] <- "autumn"
    dat$season[dat$month %in% c(11, 12, 1)] <- "winter"
    dat$season[dat$month %in% c(2:4)] <- "spring"

    # define periods

    # For SEAPOP this should be year

    if (is.null(seapop_data)) {
        dat$period <- NA
        dat$period[dat$date < "2010-08-01"] <- "2006 to 2010"
        dat$period[dat$date >= "2010-08-01" & dat$date < "2014-08-01"] <- "2010 to 2014"
        dat$period[dat$date >= "2014-08-01" & dat$date < "2018-08-01"] <- "2014 to 2018"
        dat$period[dat$date >= "2018-08-01" & dat$date < "2022-08-01"] <- "2018 to 2022"
        dat$period[dat$date >= "2022-08-01" & dat$date < "2026-08-01"] <- "2022 to 2026"

        # define period info
        dat$period_info <- NA
        dat$period_info[dat$date < "2010-08-01"] <- paste0(min(dat$year), " to ", max(dat$year[dat$date < "2010-08-01"]))
        dat$period_info[dat$date >= "2010-08-01" & dat$date < "2014-08-01"] <- paste0(min(dat$year[dat$date >= "2010-08-01"]), " to ", max(dat$year[dat$date < "2014-08-01"]))
        dat$period_info[dat$date >= "2014-08-01" & dat$date < "2018-08-01"] <- paste0(min(dat$year[dat$date >= "2014-08-01"]), " to ", max(dat$year[dat$date < "2018-08-01"]))
        dat$period_info[dat$date >= "2018-08-01" & dat$date < "2022-08-01"] <- paste0(min(dat$year[dat$date >= "2018-08-01"]), " to ", max(dat$year[dat$date < "2022-08-01"]))
        dat$period_info[dat$date >= "2022-08-01" & dat$date < "2026-08-01"] <- paste0(min(dat$year[dat$date >= "2022-08-01"]), " to ", max(dat$year))
    } else {
        dat$period <- dat$year
        dat$period_info <- dat$year
    }
    # remove colonies and periods represented by less than 30 rows of data
    dat$kernel_id <- paste(dat$species, dat$season, dat$colony, dat$period, sep = "_")

    kernel_counts <- dplyr::count(dat, kernel_id) %>% dplyr::filter(n >= 30)
    dat <- dat[dat$kernel_id %in% kernel_counts$kernel_id, ]

    # transform to sp object
    sp_dat <- dat
    sp_dat <- sp_dat[!is.na(sp_dat$lon) & !is.na(sp_dat$lat), ]
    sp_dat <- sf::st_as_sf(sp_dat, coords = c("lon", "lat"), crs = 4326)

    # run kernel UD analysis for each kernel_id
    kids <- unique(sp_dat$kernel_id)

    colony_season_period_kernels <- data.frame() # 3 kernels (at different kd) for each species, colony, period, season combo

    for (k in seq_along(kids)) {
        log_info("Callculating kernel ", k, " of ", length(kids))
        kid <- kids[k]

        sp_dat_sub <- sp_dat[sp_dat$kernel_id == kid, ]

        # create projection centred on mid point of location distribution
        proj.aezd <- paste0(
            "+proj=aeqd  +lat_0=",
            mean(range(sf::st_coordinates(sp_dat_sub)[, 2])),
            "  +lon_0=",
            mean(range(sf::st_coordinates(sp_dat_sub)[, 1])),
            " +units=km"
        )

        sp_dat_sub <- sf::st_transform(sp_dat_sub, proj.aezd)

        bbox <- sf::st_bbox(sp_dat_sub)
        buff <- 500

        # Consider moving this to a more modern package at some point
        test.grid <- sp::GridTopology(
            cellcentre.offset = c(
                floor(bbox[1]) - buff,
                floor(bbox[2]) - buff
            ),
            cellsize = c(20, 20),
            cells.dim = c(
                ceiling((bbox[3] - bbox[1] + buff * 2) / 20),
                ceiling((bbox[4] - bbox[2] + buff * 2) / 20)
            )
        )
        test.point <- sp::SpatialPoints(cbind(c(0), c(0)))
        test.pixel <- sp::SpatialPixels(test.point, proj4string = sp::CRS(proj.aezd), round = NULL, grid = test.grid)

        kud <- adehabitatHR::kernelUD(sf::as_Spatial(sp_dat_sub[, "kernel_id"]), h = 44, grid = test.pixel)

        c75 <- adehabitatHR::getverticeshr(kud, 75)
        c50 <- adehabitatHR::getverticeshr(kud, 50)
        c25 <- adehabitatHR::getverticeshr(kud, 25)

        c75$kernel_density <- 75
        c50$kernel_density <- 50
        c25$kernel_density <- 25

        cc_sub <- rbind(c75, c50)
        cc_sub <- rbind(cc_sub, c25)
        cc_sub <- sf::st_as_sf(cc_sub)
        cc_sub <- sf::st_transform(cc_sub, 4326)

        cc_sub$id <- kids[k]

        colony_season_period_kernels <- rbind(colony_season_period_kernels, cc_sub)
    }
    colony_season_period_kernels <- sf::st_wrap_dateline(colony_season_period_kernels, options = c("WRAPDATELINE=YES"))

    data_in_string <- as.data.frame(stringr::str_split_fixed(as.character(colony_season_period_kernels$id), "_", 4))
    names(data_in_string) <- c("species", "season", "colony", "period")
    colony_season_period_kernels <- cbind(colony_season_period_kernels, data_in_string)



    colony_season_period_kernels <- merge(colony_season_period_kernels, dat[!duplicated(dat$kernel_id), c("kernel_id", "period_info")],
        by.x = "id", by.y = "kernel_id", all.x = TRUE
    )

    period_details <- dplyr::group_by(sf::st_drop_geometry(sp_dat), species, colony, season, period) %>% dplyr::summarise(
        locations = dplyr::n(),
        individuals = dplyr::n_distinct(individ_id),
        colonies = dplyr::n_distinct(colony),
        days = n_distinct(doy),
        months = n_distinct(month),
        years = n_distinct(year),
    )
    period_details$period <- as.character(period_details$period)
    colony_season_period_kernels <- dplyr::left_join(colony_season_period_kernels, period_details, by = join_by("species", "colony", "season", "period"))
    colony_season_period_kernels$project <- "SEATRACK"
    colony_season_period_kernels$technology <- tolower(data_type)

    if (!is.null(seapop_data)) {
        # replace summer season with seapop kernel
        colony_season_period_kernels <- colony_season_period_kernels[colony_season_period_kernels$season != "summer", ]
        kernels_to_add <- seapop_kernels[seapop_kernels$species %in% colony_season_period_kernels$species &
            seapop_kernels$colony %in% colony_season_period_kernels$colony, ]
        kernels_to_add$id <- paste(kernels_to_add$species, "summer", kernels_to_add$colony, kernels_to_add$period, sep = "_")

        kernels_to_add <- sf::st_transform(kernels_to_add, sf::st_crs(colony_season_period_kernels))
        kernels_to_add <- kernels_to_add[, names(colony_season_period_kernels)]
        colony_season_period_kernels <- dplyr::bind_rows(colony_season_period_kernels, kernels_to_add)

        # dplyr::select(sf::st_drop_geometry(seapop_kernels), species, colony) %>% distinct() %>% dplyr::filter(species %in% colony_season_period_kernels$species)
    }

    colony_season_period_kernels$area <- NULL
    ## all colonies ---------
    log_info("Calculating kernels for all colonies")

    all_colony_kernels <- merge_kernels(sp_dat, c("species", "period", "season"), colony_season_period_kernels)

    all_colony_kernels$colony <- "All_colonies"
    all_colony_kernels$id <- paste(
        all_colony_kernels$species,
        all_colony_kernels$season,
        all_colony_kernels$colony,
        all_colony_kernels$period,
        all_colony_kernels$kernel_density,
        sep = " "
    )

    all_kernels <- dplyr::bind_rows(colony_season_period_kernels, all_colony_kernels)

    ## all seasons
    log_info("Calculating kernels for all seasons") # DON'T DO THIS FOR SEAPOP.
    all_season_details <- get_details(sp_dat, c("species", "period", "colony"))
    all_colony_season_details <- get_details(sp_dat, c("species", "period"))
    all_colony_season_details$colony <- "All_colonies"
    all_season_details <- rbind(all_season_details, all_colony_season_details)

    all_season_kernels <- merge_kernels(sp_dat, c("species", "period", "colony"), all_kernels, all_season_details)

    all_season_kernels$season <- "All_seasons"
    all_season_kernels$id <- paste(
        all_season_kernels$species,
        all_season_kernels$season,
        all_season_kernels$colony,
        all_season_kernels$period,
        all_season_kernels$kernel_density,
        sep = " "
    )

    all_kernels <- dplyr::bind_rows(colony_season_period_kernels, all_season_kernels)

    ## all periods -----
    log_info("Calculating kernels for all periods")
    all_period_details <- get_details(sp_dat, c("species", "season", "colony"))
    all_colony_period_details <- get_details(sp_dat, c("species", "season"))
    all_colony_period_details$colony <- "All_colonies"

    # DON'T DO THIS FOR SEAPOP
    all_season_period_details <- get_details(sp_dat, c("species", "colony"))
    all_season_period_details$season <- "All_seasons"

    all_season_period_colony_detail <- get_details(sp_dat, c("species"))
    all_season_period_colony_detail$colony <- "All_colonies"
    all_season_period_colony_detail$season <- "All_seasons"
    ###

    all_period_details <- rbind(all_period_details, all_colony_period_details, all_season_period_details, all_season_period_colony_detail)
    all_period_kernels <- merge_kernels(sp_dat, c("species", "season", "colony"), all_kernels, all_period_details)

    all_period_kernels$period <- "All_periods"
    all_period_kernels$id <- paste(
        all_period_kernels$species,
        all_period_kernels$season,
        all_period_kernels$colony,
        all_period_kernels$period,
        all_period_kernels$kernel_density,
        sep = " "
    )

    all_kernels <- dplyr::bind_rows(colony_season_period_kernels, all_period_kernels)


    # Replace kernels with colony locations
    all_colonies <- sf::st_drop_geometry(all_kernels)
    all_colonies <- dplyr::left_join(all_colonies, select(colonies, colony_int_name, geometry), by = dplyr::join_by(colony == colony_int_name))
    all_colonies <- sf::st_as_sf(all_colonies)

    all_colony_selection <- all_colonies[all_colonies$colony == "All_colonies", ]
    for (i in seq_len(nrow(all_colony_selection))) {
        missing_colony_row <- all_colony_selection[i, ]
        current_colony_points <- dplyr::semi_join(all_colonies[!all_colonies$colony == "All_colonies", ], sf::st_drop_geometry(missing_colony_row), by = join_by("kernel_density", "species", "season", "period")) %>% dplyr::distinct()

        sf::st_geometry(all_colonies)[all_colonies$kernel_density == missing_colony_row$kernel_density &
            all_colonies$species == missing_colony_row$species &
            all_colonies$season == missing_colony_row$season &
            all_colonies$period == missing_colony_row$period &
            all_colonies$colony == "All_colonies"] <-
            sf::st_union(current_colony_points$geometry)
    }

    # If seapop, throw out summer and replace with seapop data.

    all_kernels <- sf::st_transform(all_kernels, 3857)

    sf::st_crs(all_colonies) <- 3857

    if (!dir.exists(file.path(output_folder))) {
        dir.create(output_folder)
    }

    overview <- dplyr::select(dat, species, colony, season, period, period_info) %>% dplyr::distinct()

    for (species in unique(all_kernels$species)) {
        write.table(overview[overview$species == species, ],
            file = file.path(output_folder, paste("content_WebApp_", species, ".txt", sep = "")),
            sep = "\t", row.names = FALSE, fileEncoding = "UTF-8"
        )

        sf::st_write(all_kernels[all_kernels$species == species, ],
            file.path(output_folder, paste("KDE_", species_name, ".geojson", sep = "")),
            delete_layer = TRUE, # overwrite existing layer
            driver = "GeoJSON"
        )

        sf::st_write(all_colonies[all_colonies$species == species, ],
            file.path(output_folder, paste("COLONIES_", species_name, ".geojson", sep = "")),
            delete_layer = TRUE, # overwrite existing layer
            driver = "GeoJSON"
        )
    }

    return(list(all_kernels = all_kernels, all_colonies = all_colonies))
}

get_details <- function(sp_dat, grouping_vars) {
    details <- dplyr::summarise(
        sf::st_drop_geometry(sp_dat),
        locations = dplyr::n(),
        individuals = dplyr::n_distinct(individ_id),
        colonies = dplyr::n_distinct(colony),
        days = n_distinct(doy),
        months = n_distinct(month),
        years = n_distinct(year),
        .by = all_of(grouping_vars)
    )
    return(details)
}

merge_kernels <- function(sp_dat, grouping_vars, kernel_dataframe, details = NULL) {
    if (is.null(details)) {
        details <- get_details(sp_dat, grouping_vars)
    }
    result <- data.frame() # One row per species, season, period
    for (i in seq_len(nrow(details))) {
        current_details_row <- details[i, ]
        for (kd in unique(kernel_dataframe$kernel_density)) {
            # For each row, merge the kernels

            current_kernels <- dplyr::semi_join(kernel_dataframe, current_details_row, by = grouping_vars) %>% dplyr::filter(kernel_density == kd)
            current_result <- current_details_row
            current_result$kernel_density <- kd
            sf::st_geometry(current_result) <- sf::st_union(sf::st_make_valid(sf::st_geometry(current_kernels)))
            result <- rbind(result, current_result)
        }
    }

    return(result)
}
