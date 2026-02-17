# Black-legged kittiwake, Kongsfjorden

get_nofly <- function(nfArea = "None") {
    mask <- tryCatch({
        readRDS(system.file(glue::glue("shapefiles/World_buff03_nofly{nfArea}_res-i.rds"), package = "seatrackRtools"))
    }, ERROR = {
        readRDS(system.file(glue::glue("shapefiles/World_buff03_noflyNone_res-i.rds"), package = "seatrackRtools"))
    })
    return(mask)
}

#' Add country name to colony locations based on nearest NaturalEarth feature
#'
#' @param coldf
addCountryName <- function(coldf) {
    sf::sf_use_s2(FALSE)
    # download world map from NaturalEarth
    rnaturalearth::ne_download(scale = 10, returnclass = "sf") %>%
        # sf::st_make_valid() %>%
        sf::st_crop(xmin = -150, xmax = 150, ymin = 0, ymax = 90) ->
    worldmap

    coldf %>%
        dplyr::mutate(lon = col_lon, lat = col_lat) %>%
        sf::st_as_sf(coords = c("lon", "lat"), crs = sf::st_crs(4326)) %>%
        sf::st_nearest_feature(., worldmap) %>%
        dplyr::slice(worldmap, .) %>%
        cbind(coldf) %>%
        as.data.frame() %>%
        dplyr::rename(sovereignty = SOVEREIGNT, country = GEOUNIT) %>%
        dplyr::select(colony, sovereignty, country, col_lon, col_lat) %>%
        # manual adjustments
        dplyr::mutate(
            sovereignty = case_when(
                colony == "Machias Seal Island" ~ "Canada", # Island is disputed territory between US/Canada, but lies in US waters)
                TRUE ~ sovereignty
            ),
            country = case_when(
                colony == "Machias Seal Island" ~ "Canada", # Island is disputed territory between US/Canada, but lies in US waters)
                TRUE ~ country
            )
        ) %>%
        dplyr::arrange(country, colony) ->
    colonies_country

    return(colonies_country)
}

get_data_for_irma <- function(species_filter = NULL, colony_filter = NULL, release = format(Sys.Date(), "%Y%m%d"), export_dir = "db_data_for_irma") {
    pos_table <- tbl(con, dbplyr::in_schema("positions", "postable"))
    if (!is.null(colony_filter)) {
        pos_table <- dplyr::filter(pos_table, colony %in% colony_filter)
    }
    if (!is.null(species_filter)) {
        pos_table <- dplyr::filter(pos_table, species %in% species_filter)
    }
    # dplyr::filter(session_id %in% SEATRACK_sessionIDs) |>
    dplyr::select(pos_table, c(
        species, date_time, country_code, ring_number, individ_id, session_id, colony,
        lon, lat, eqfilter, tfirst, tsecond, twl_type, sun, light_threshold
    )) |>
        dplyr::left_join(DBI::dbReadTable(con, DBI::Id(schema = "metadata", table = "species"))[, c(2:3)], join_by(species == species_name_eng), copy = TRUE) ->
    res

    indSummary <- NULL

    # Generate table with colony locations
    DBI::dbReadTable(con, DBI::Id(schema = "metadata", table = "colony")) |>
        dplyr::select(colony_int_name, lat, lon) |>
        dplyr::filter(!colony_int_name %in% c(NA_character_, "Test Islands")) |>
        dplyr::filter(colony_int_name %in% unique(dplyr::pull(res, colony))) |>
        dplyr::rename(colony = colony_int_name, col_lat = lat, col_lon = lon) |>
        addCountryName() ->
    colSummary

    # Retrieve list of species present in database
    res |>
        dplyr::distinct(species, .keep_all = TRUE) |>
        dplyr::collect() |>
        dplyr::filter(!is.na(species_name_latin)) |>
        dplyr::mutate(species_name_latin = gsub(" ", "_", species_name_latin)) |>
        dplyr::mutate(species_short = toupper(paste0(substr(species_name_latin, 1, 2), purrr::map(species_name_latin, ~ substr(strsplit(.x, "_")[[1]][2], 1, 3) |> toupper()) |> unlist()))) |>
        dplyr::select(species, species_name_latin, species_short) |>
        dplyr::arrange(species_name_latin) ->
    SEATRACK.species


    # Retrieve bird age class linked to each session_ID
    seatrackR::getIndividInfo() |>
        dplyr::filter(!is.na(session_id)) |>
        dplyr::mutate(age = case_when(is.na(status_age) ~ "adult", TRUE ~ status_age)) |>
        dplyr::filter(eventType == "Deployment") |>
        dplyr::mutate(ring = paste0(country_code, "-", ring_number)) |>
        dplyr::mutate(age = case_when(
            age %in% c("pullus", "chick") ~ "pul",
            age %in% c("juvenile", "1", "2", "3", "subadult") ~ "juv",
            is.na(age) ~ "adu",
            TRUE ~ "adu"
        )) |>
        dplyr::distinct(session_id, age, status_date, .keep_all = TRUE) |>
        dplyr::select(session_id, ring, age) ->
    status.age

    dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)
    all_species_indsummary <- tibble()
    for (myspecies in SEATRACK.species$species_name_latin) {
        mysp <- gsub("_", " ", myspecies)

        # Collecting data from query
        res |>
            dplyr::filter(species_name_latin == mysp & !is.na(species_name_latin)) |>
            collect() ->
        species_data
        species_table_result <- get_species_table(species_data, status.age)
        # Save species data
        all_species_indsummary <- rbind(all_species_indsummary, species_table_result$indSummary)
        saveRDS(species_table_result$species_data, file = file.path(export_dir, glue::glue("SEATRACK_export_{release}_{myspecies}_raw.rds")))
    }
    saveRDS(all_species_indsummary, file.path(export_dir, glue::glue("SEATRACK_export_{release}_indinfo.rds")))
    saveRDS(colSummary, file.path(export_dir, glue::glue("SEATRACK_export_{release}_colinfo.rds")))
}

get_species_table <- function(species_data, status.age) {
    nrow.ori <- nrow(species_data)

    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Correcting database errors manually ----
    # remove obvious mistakes (missing spp, duplicates)
    # length(which(is.na(species_data$species)))
    # length(which(duplicated(species_data[, c("date_time", "session_id")])))

    species_data |>
        dplyr::distinct(date_time, session_id, .keep_all = TRUE) |>
        dplyr::filter(!session_id %in% c("13023_2009-06-30", "13025_2009-06-30")) |> # Remove individuals with temporally overlapping ring/logger_yeartracked values
        droplevels() ->
    species_data

    # Creating new ring variable - Dealing with NAs in $ring_number
    species_data |>
        dplyr::mutate(ring = paste0(country_code, "-", gsub(" ", "", as.character(ring_number)))) |>
        dplyr::mutate(ring = case_when(is.na(ring_number) ~ as.character(paste0(country_code, "-", session_id)), .default = ring)) |>
        dplyr::mutate(ring = gsub("_", "-", ring)) |>
        dplyr::select(-ring_number, -country_code) ->
    species_data

    species_data |>
        dplyr::left_join(status.age, by = join_by(session_id, ring)) ->
    species_data

    # total number of rows removed
    nrow.ori - nrow(species_data)
    log_info(glue::glue("Total number of records removed for {unique(species_data$species)}: {nrow.ori - nrow(species_data)}"))
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Capitalizing all ring numbers
    species_data$ring <- toupper(species_data$ring)

    # Testing whether $individ_id and $ring match perfectly
    species_data$individ_id <- toupper(gsub("_", "-", species_data$individ_id))
    # if(length(which(species_data$individ_id != species_data$ring)) > 0) stop("Inconsistencies between ring and individ_id") # species_data$individ_id <- NULL

    ## CHECK: are all inconsistencies mistakes?
    # unique(species_data$ring[which(species_data$individ_id != species_data$ring)]) -> sel.rings

    # species_data |>
    #   dplyr::filter(ring %in% sel.rings) |>
    #   dplyr::group_by(ring, individ_id) |>
    #   dplyr::summarise(count = n())

    species_data |>
        dplyr::mutate(
            individ_id = case_when(individ_id != ring ~ ring,
                is.na(individ_id) ~ ring,
                .default = individ_id
            ),
            timestamp = as.POSIXct(date_time, tz = "UTC"),
            species = as.factor(gsub(" ", "_", species_name_latin))
        ) |>
        dplyr::select(-date_time, -species_name_latin) ->
    species_data

    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Basic checks ----
    # Different capitalization whitin same factor?
    # unique(species_data$logger_yeartracked)

    # Detecting false duplicates (same ring or logger attributed to different individuals)
    # test.tmp  <- as.data.frame(table(species_data$ring, species_data$logger_yeartracked)) # Same ring number attributed to different species/colonies?

    # same logger_year_tracked attributed to different rings
    # test.tmp2 <- test.tmp[test.tmp$Freq > 0,]
    # test.tmp3 <- as.data.frame(table(test.tmp2$Var2)); test.tmp3 <- test.tmp3[test.tmp3$Freq > 1,]; unique(droplevels(test.tmp3[,1]))

    # rm(test.tmp, test.tmp2, test.tmp3); gc()
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Summarizing and splitting data into separate tables
    species_data |>
        dplyr::group_by(ring, individ_id, species, colony) |>
        dplyr::summarize(timestamp_min = min(timestamp), timestamp_max = max(timestamp), n_total = n()) |>
        ungroup() |>
        dplyr::left_join(colSummary, join_by(colony)) |>
        dplyr::select(species, ring, individ_id, colony, col_lon, col_lat, timestamp_min, timestamp_max, n_total) |>
        dplyr::distinct(species, ring, individ_id, colony, .keep_all = TRUE) ->
    indSummary

    if (length(grep("Ã", indSummary$colony)) > 0) {
        indSummary$colony <- iconv(indSummary$colony, from = "UTF-8", to = "Windows-1252")
    }

    # Adding information on equinox period (used later for identification
    # of models according to each specific individual/period)

    ## NB: takes into account that first tracking position for
    ## each bird/session_id has been forced near colony at
    ## deployment date, **even if during equinox period**

    species_data |>
        dplyr::mutate(eqfilter = case_when(is.na(eqfilter) ~ 1, .default = eqfilter)) %>% # Temporary fix for eqfilter3 errors in database
        dplyr::mutate(
            equinox = case_when(eqfilter == 0 & as.numeric(format(timestamp, "%j")) < 180 ~ paste0(format(timestamp, "%Y"), "_equin_spring"),
                eqfilter == 0 & as.numeric(format(timestamp, "%j")) >= 230 ~ paste0(format(timestamp, "%Y"), "_equin_fall"),
                .default = "other"
            ),
            lat = case_when(equinox != "other" & !is.na(tfirst) & !is.na(tsecond) ~ NA,
                .default = lat
            )
        ) ->
    species_data


    ######## START - TEMPORARY CORRECTION ##

    species_data |>
        # adjust eqfilter values for locations at deployment that were manually created (to ensure track starting at colony)
        dplyr::mutate(eqfilter = case_when(eqfilter == 0 & is.na(tfirst) & is.na(tsecond) ~ 1,
            .default = eqfilter
        )) |>
        # adjust lon/lat values to ensure they are not exactly the same
        # adjust lon/lat values to ensure they are not exactly the same
        dplyr::mutate(
            lon = case_when(is.na(tfirst) & is.na(tsecond) ~ jitter(lon, amount = 0.002),
                .default = lon
            ),
            lat = case_when(is.na(tfirst) & is.na(tsecond) ~ jitter(lat, amount = 0.002),
                .default = lat
            )
        ) ->
    species_data

    species_data <- dplyr::select(species_data, -colony, -species)

    indSummary <- dplyr::select(indSummary, individ_id, species, colony)

    ######## END - TEMPORARY CORRECTION ##

    # timestamp, session_id, individ_id, lon, lat, eqfilter

    return(list(species_data = species_data, indSummary = indSummary))
}

#     # Spatialize and identify locations above landmasses
#     NA.rows <- is.na(species_data$lon) | is.na(species_data$lat)
#     species_data$onLand <- FALSE

#     if (nfArea != "none") {
#         sf::st_as_sf(x = species_data[!NA.rows, ], coords = c("lon", "lat"), crs = "OGC:CRS84") %>%
#             sf::st_intersects(., landmask, sparse = FALSE) %>%
#             as.vector() ->
#         onLand

#         species_data$onLand[!NA.rows] <- onLand
#     }

#     # Saving raw data
#     species_data <- species_data[order(species_data$individ_id, species_data$timestamp), ]
#     saveRDS(species_data, file = paste0("data/SEATRACK_export_", release, "_", myspecies, "_raw.rds"))
#     # release <- "2024xxxx"
#     # species_data <- readRDS(paste0('data/SEATRACK_export_', release, '_', myspecies, '_raw.rds'))

#     # Removing individuals with too few locations (less than 100 locs)
#     rownames(species_data) <- NULL
#     # hist(table(species_data$individ_id), breaks = 100, xlab = "total number of relocations for an individual"); abline(v=100, col = "red")
#     rem.rings <- names(table(species_data$individ_id))[which(table(species_data$individ_id) < 100)]
#     species_data <- droplevels(species_data[!species_data$individ_id %in% rem.rings, ])
#     species_data <- species_data[order(species_data$individ_id, species_data$timestamp), ]

#     ## 2a - Modelling longitude and identifying & extracting equinox periods ----

#     # Modeling longitudes during equinoxes for further use
#     folder.out <- "data/Outputs_Models/Longitude_Equinoxes/"

#     # METHOD 1 - Modelling (GAM-based)
#     # for(spp in levels(species_data$species)){
#     #     model.lon.equinox(folder.out = folder.out, pics.out = "F:/OUTPUT_R_temporary/", mydf = species_data, species = spp)
#     # }

#     # METHOD 2 - Extracting the actual longitude data (which means there will likely remain many temporal gaps in the longitude data)
#     for (spp in levels(species_data$species)) {
#         extract.lon.equinox(folder.out = folder.out, mydf = species_data, species = spp)
#     }

#     # Extracting data from equinox period (based on the conservative filter, eqfilter3)
#     # data.equin <- droplevels(species_data[species_data$eqfilter3 == 0,])
#     # species_data$eqfilter9 <- ifelse(species_data$equinox == "other", 1, 0)

#     # Keeping only locations which were deemed reliable (filter3 == 1) and which provide both a longitude and a latitude
#     data.clean <- droplevels(species_data[species_data$eqfilter == 1 | (species_data$eqfilter == 0 & is.na(species_data$tfirst) & is.na(species_data$tsecond)), ])

#     # Saving the clean vs. equinox periods as separate datasets
#     # save(list = c("data.clean", "data.equin"), file = paste("data/Seatrack_export_", release, "_posdata.r", sep = ""), compress = TRUE, compression_level = 9)
#     # load(paste("data/Seatrack_export_", release, "_posdata.r", sep = ""))
#     # load(paste("data/Seatrack_export_", release, "_summaryTables.r", sep = ""))

#     ## 2b - Saving one file per species for easier handling ----

#     spp_mask <- as.logical(species.param[species.param$species_latin == myspecies, "mask_land"])
#     myfile <- paste0("data/SEATRACK_export_", release, "_", gsub(" ", "_", myspecies), "_clean.rds")

#     data.clean |>
#         dplyr::select(timestamp, ring, individ_id, session_id, age, lon, lat, eqfilter, tfirst, tsecond, twl_type, sun, light_threshold, equinox, onLand) ->
#     data.clean

#     rownames(data.clean) <- NULL

#     saveRDS(object = data.clean, file = myfile)

#     data.clean.frm <- Seatrack_GLS_Format_Dataset(mydf = data.clean, species = myspecies, mask = spp_mask, ind.summary = indSummary)

#     saveRDS(object = data.clean.frm, file = gsub(".rds", "_formatted.rds", myfile))
#     cat(mysp, "\n")
# }
