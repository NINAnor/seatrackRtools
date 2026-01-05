#' Calibrate all species/colony combinations found in seatrack logger data import folder
#'
#' Uses hard coded file paths to call gls_calibrate_species_colony for all species/colony combinations found in seatrack database for loggers in import folder.
#' @param no_pos_only Logical indicating whether to include only loggers without position data in the database. Default is TRUE.
#' @param rerun_existing Logical indicating whether to rerun calibration for loggers that already have calibration data. Default is TRUE.
#' @param include_existing Logical indicating whether to include existing calibration data in the final output. Default is TRUE.
#' @param filter_plots Logical indicating whether to export filter plots. Default is FALSE.
#' @return None. The function saves the prepared calibration data to the specified output directory.
#' @concept gls_helper
#' @export
gls_calibrate_all <- function(no_pos_only = TRUE, rerun_existing = TRUE, include_existing = TRUE, filter_plots = FALSE) {
    progressr::handlers(global = FALSE)
    progressr::handlers("progress")
    future::plan(future::multisession)
    seatrackR::connectSeatrack()

    import_directory <- file.path(the$sea_track_folder, "Database\\Imports_Logger data\\Raw logger data\\ALL")
    log_info("Get all species/colony combinations from ", import_directory)
    all_metadata <- gls_metadata(import_directory, no_pos_only = no_pos_only)
    species_colony <- all_metadata[, c("species", "colony")]
    species_colony <- species_colony[stats::complete.cases(species_colony), ]
    species_colony <- dplyr::distinct(species_colony)
    p <- progressr::progressor(along = seq_len(nrow(species_colony)))
    foreach::foreach(i = seq_len(nrow(species_colony))) %dofuture% {
        seatrackR::connectSeatrack()
        species <- species_colony$species[i]
        colony <- species_colony$colony[i]
        gls_calibrate_species_colony(import_directory, species, colony, no_pos_only, rerun_existing, include_existing, filter_plots)
        p(sprintf("i=%g", i))
    }
}

#' Prepare a seatrack species/colony combination for calibration
#'
#' Uses hard coded file paths to call gls_prepare_calibration. Filters metadata passed to gls_prepare_calibration by species/colony.
#' @param import_directory Path to the directory containing GLS files.
#' @param species Species name to filter metadata.
#' @param colony Colony name to filter metadata.
#' @param no_pos_only Logical indicating whether to include only loggers without position data in the database. Default is TRUE.
#' @param rerun_existing Logical indicating whether to rerun calibration for loggers that already have calibration data. Default is TRUE.
#' @param include_existing Logical indicating whether to include existing calibration data in the final output. Default is TRUE.
#' @param filter_plots Logical indicating whether to export filter plots. Default is FALSE.
#' @return None. The function saves the prepared calibration data to the specified output directory.
#' @export
#' @concept gls_helper
gls_calibrate_species_colony <- function(
    import_directory = file.path(the$sea_track_folder, "Database\\Imports_Logger data\\Raw logger data\\ALL"),
    species, colony, no_pos_only = TRUE, rerun_existing = TRUE, include_existing = TRUE, filter_plots = FALSE) {
    log_info("Preparing calibration for species '", species, "' and colony '", colony, "'")
    output_directory <- file.path(the$sea_track_folder, "Database", "Imports_Logger data", "Callibration_GLSpositions", species, colony)
    existing_calibration_dir <- file.path(the$sea_track_folder, "Locations")

    gls_prepare_calibration(import_directory, output_directory, species, colony, no_pos_only, existing_calibration_dir, rerun_existing, include_existing, filter_plots)
}



#' Prepare GLS calibration data using seatrack database
#'
#' Function to prepare GLS calibration data for use with seatrackRgls, based on GLS files in the import directory and metadata from the Sea Track database.
#' @param import_directory Path to the directory containing GLS files.
#' @param output_directory Path to the directory where the prepared calibration data will be saved.
#' @param species Species name to filter metadata. Default is NULL (no filtering).
#' @param colony Colony name to filter metadata. Default is NULL (no filtering).
#' @param no_pos_only Logical indicating whether to include only loggers without position data in the database. Default is TRUE.
#' @param existing_calibration_dir Directory containing existing calibration data. Default is NULL. If provided, calibration data from this directory will be merged.
#' @param rerun_existing Logical indicating whether to rerun calibration for loggers that already have calibration data. Default is TRUE.
#' @param include_existing Logical indicating whether to include existing calibration data in the final output. Default is TRUE.
#' @param filter_plots Logical indicating whether to export filter plots. Default is FALSE.
#' @return None. The function saves the prepared calibration data to the specified output directory.
#' @export
#' @concept gls_helper
gls_prepare_calibration <- function(import_directory, output_directory, species = NULL, colony = NULL, no_pos_only = TRUE, existing_calibration_dir = NULL, rerun_existing = TRUE, include_existing = TRUE, filter_plots = FALSE, rerun_existing_plots = FALSE) {
    metadata <- gls_metadata(import_directory, colony, species, time_windows = TRUE, no_pos_only = no_pos_only)
    calibration_output_dir <- file.path(output_directory)
    calibration_filename <- paste(species, colony, "calibration.xlsx", sep = "_")
    dir.create(calibration_output_dir, showWarnings = FALSE, recursive = TRUE)
    settings_path <- file.path(calibration_output_dir, "filter_settings.xlsx")
    if (!file.exists(settings_path)) {
        seatrackRgls::create_filter_settings_file(settings_path, species)
    }
    # Load to get year splits
    filter_setings_list <- seatrackRgls::read_filter_file(settings_path)
    species_filter_settings <- filter_setings_list$get_settings_from_list(species = species)

    existing_calibration_data <- data.frame(logger_id = character(), total_years_tracked = character())
    if (!is.null(existing_calibration_dir)) {
        # Load existing calibration data if available and merge
        existing_calibration_file <- gls_get_existing_calibration(existing_calibration_dir, species_filter_settings$split_years)
        existing_calibration_data <- existing_calibration_file$calibration_data
        if (!is.null(species)) {
            existing_calibration_data <- existing_calibration_data[existing_calibration_data$species %in% species, ]
        }
        if (!is.null(colony)) {
            existing_calibration_data <- existing_calibration_data[existing_calibration_data$colony %in% colony, ]
        }
    }



    # check for calibration data in export dir and merge it
    if (file.exists(file.path(calibration_output_dir, calibration_filename))) {
        existing_export_data <- openxlsx2::read_xlsx(file.path(calibration_output_dir, calibration_filename))
        # if any columns are incompatible, coerce to character
        common_cols <- intersect(names(existing_calibration_data), names(existing_export_data))
        for (col in common_cols) {
            if (any(class(existing_calibration_data[[col]]) != class(existing_export_data[[col]]))) {
                existing_calibration_data[[col]] <- as.character(existing_calibration_data[[col]])
                existing_export_data[[col]] <- as.character(existing_export_data[[col]])
            }
        }

        existing_calibration_data <- dplyr::bind_rows(existing_calibration_data, existing_export_data)
    }

    if (nrow(existing_calibration_data) > 0) {
        metadata_id_year <- paste(metadata$logger_id, metadata$total_years_tracked)
        existing_id_year <- paste(existing_calibration_data$logger_id, existing_calibration_data$total_years_tracked)
        existing_calibration_data <- existing_calibration_data[!duplicated(existing_id_year), ]

        # Remove existing calibration rows from metadata
        metadata <- metadata[!metadata_id_year %in% existing_id_year, ]
        if (!rerun_existing && nrow(metadata) == 0) {
            log_info("All files already have calibration data.")
            return()
        }
    }

    existing_plot_data <- data.frame(logger_id = character(), year_tracked = character())
    # check if these already have plots
    if (!rerun_existing_plots && file.exists(file.path(calibration_output_dir, "sun_calib"))) {
        existing_plots_files <- list.files(file.path(calibration_output_dir, "sun_calib"), pattern = "*.tiff")
        if (length(existing_plots_files) > 0) {
            existing_plots_ids <- data.frame(t(sapply(strsplit(existing_plots_files, "_"), function(x) x[1:3])))
            names(existing_plots_ids) <- c("logger_id", "year1", "year2")
            existing_plots_ids <- unique(paste(existing_plots_ids[, "logger_id"], paste(existing_plots_ids[, "year1"], existing_plots_ids[, "year2"], sep = "_"), sep = "_"))
            existing_plot_data <- existing_calibration_data[paste(existing_calibration_data$logger_id, existing_calibration_data$year_tracked, sep = "_") %in% existing_plots_ids, ]
            # Remove existing plot rows from existing calibration data
            existing_calibration_data <- existing_calibration_data[!paste(existing_calibration_data$logger_id, existing_calibration_data$year_tracked, sep = "_") %in% existing_plots_ids, ]
        }
    }

    if (rerun_existing && nrow(existing_calibration_data) > 0) {
        common_cols <- intersect(names(existing_calibration_data), names(metadata))
        for (col in common_cols) {
            if (any(class(existing_calibration_data[[col]]) != class(metadata[[col]]))) {
                existing_calibration_data[[col]] <- as.character(existing_calibration_data[[col]])
                metadata[[col]] <- as.character(metadata[[col]])
            }
        }
        metadata <- dplyr::bind_rows(existing_calibration_data, metadata)
    }

    if (nrow(metadata) == 0 && nrow(existing_calibration_data) == 0 && nrow(existing_plot_data) == 0) {
        log_warn("No metadata or previous calibration data found for", species, colony)
        return()
    }

    if (nrow(metadata) > 0) {
        all_colony_info <- gls_seatrack_colony_info()
        calibration_template <- seatrackRgls::prepare_calibration(
            import_directory,
            data.frame(metadata),
            all_colony_info,
            output_directory,
            export_calibration_template = FALSE,
            show_filter_plots = filter_plots,
            filter_setting_list = settings_path
        )
    } else {
        log_info("No new metadata to process, only existing calibration data.")
        calibration_template <- data.frame(logger_id = character(), year_tracked = character())
    }

    calibration_template_id_year <- paste(calibration_template$logger_id, calibration_template$year_tracked)
    existing_id_year <- paste(existing_calibration_data$logger_id, existing_calibration_data$year_tracked)


    if (include_existing) {
        existing_id_year <- paste(existing_calibration_data$logger_id, existing_calibration_data$year_tracked)
        match_idx <- match(existing_id_year, calibration_template_id_year)
        calibration_template[match_idx[!is.na(match_idx)], c("sun_angle_start", "sun_angle_end", "light_threshold")] <- existing_calibration_data[!is.na(match_idx), c("sun_angle_start", "sun_angle_end", "light_threshold")]
    } else {
        calibration_template <- calibration_template[!calibration_template_id_year %in% existing_id_year, ]
    }

    # reattach rows skipped due to existing plots
    if (include_existing && !rerun_existing_plots) {
        calibration_template <- dplyr::bind_rows(existing_plot_data, calibration_template)
    }

    seatrackRgls::calibration_to_wb(
        calibration_template,
        calibration_output_dir,
        calibration_filename = calibration_filename
    )
}

#' Process GLS position data using seatrackRgls
#'
#' Function to process GLS position data from a specified directory using seatrackRgls.
#' @param species Species name for colony information.
#' @param colony Colony name for colony information.
#' @param import_directory Path to the directory containing GLS files.
#' @param output_directory Path to the directory where the processed position data will be saved.
#' @return None. The function saves the processed position data to the specified output directory.
#' @export
#' @concept gls_helper
gls_process_positions <- function(species, colony, import_directory, output_directory) {
    progressr::handlers(global = FALSE)
    progressr::handlers("progress")
    future::plan(future::multisession)
    seatrackR::connectSeatrack()
    all_colony_info <- gls_seatrack_colony_info()

    import_directory <- file.path(the$sea_track_folder, "Database\\Imports_Logger data\\Raw logger data\\ALL")
    output_directory <- file.path(the$sea_track_folder, "Database\\Imports_Logger data\\Output_GLSpositions")
    log_info("Get all species/colony combinations from ", import_directory)
    all_metadata <- gls_metadata(import_directory, no_pos_only = no_pos_only)
    species_colony <- all_metadata[, c("species", "colony")]
    species_colony <- species_colony[stats::complete.cases(species_colony), ]
    species_colony <- dplyr::distinct(species_colony)

    p <- progressr::progressor(along = seq_len(nrow(species_colony)))
    foreach::foreach(i = seq_len(nrow(species_colony))) %dofuture% {
        species <- species_colony$species[i]
        colony <- species_colony$colony[i]

        calibration_dir <- file.path(the$sea_track_folder, "Database", "Imports_Logger data", "Callibration_GLSpositions", species, colony)


        process_positions(
            import_directory,
            calibration_data,
            all_colony_info,
            output_directory
        )
    }
}

#' Prepare GLS position data for database upload
#'
#' Function to prepare GLS position data from a specified directory for upload to the Sea Track database.
#' @param gls_directory_path Path to the directory containing GLS position data files.
#' @return A dataframe containing the prepared GLS position data ready for database upload.
#' @export
#' @concept gls_helper
gls_prepare_folder_upload <- function(gls_directory_path) {
    all_files <- list.files(gls_directory_path, pattern = "*.csv")
    all_file_paths <- file.path(gls_directory_path, all_files)
    pos_list <- lapply(all_file_paths, gls_prepare_file_upload)
    all_pos <- do.call(rbind, pos_list)
    return(all_pos)
}

#' Prepare a single GLS position data file for database upload
#'
#' Function to prepare a single GLS position data file for upload to the Sea Track database.
#' @param gls_file_path Path to the GLS position data file.
#' @return A dataframe containing the prepared GLS position data ready for database upload.
#' @export
#' @concept gls_helper
gls_prepare_file_upload <- function(gls_file_path) {
    filename <- basename(gls_file_path)
    filename_only <- tools::file_path_sans_ext(filename)
    id_year <- strsplit(filename_only, "_-_")[[1]][2]
    gls_pos_data <- read.csv(gls_file_path)
    db_info <- seatrackR::getSessionInfo(posdata_filename = id_year)
    gls_pos_data$session_id <- db_info$session_id
    gls_pos_data$data_version <- 3
    db_template <- dplyr::select(
        gls_pos_data,
        date_time,
        year_tracked,
        session_id,
        lon_raw,
        lat_raw,
        lon,
        lat,
        eqfilter,
        tfirst = tFirst,
        tsecond = tSecond,
        twl_type = type,
        sun = sun_angle,
        light_threshold,
        analyzer,
        data_version
    )
    return(db_template)
}
