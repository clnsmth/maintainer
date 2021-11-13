#' Download and source L0 to L1 conversion script
#'
#' @param eml (xml_document xml_node) EML of newest L1 child. The L0-to-L1 conversion script listed in \code{eml} will be used to create the child of the newest L0.
#' @param path (character) Directory to which create_ecocomDP.R will be written. Should be the same directory tables and metadata will be written to.
#' 
#' @details The script is downloaded \code{path} and then parsed to identify R libraries used by the script which are installed if not already.
#' 
#' @export
#'
download_and_source_conversion_script <- function(eml, path) {
  
  other_entities <- xml2::xml_text(
    xml2::xml_find_all(eml, ".//otherEntity/physical/objectName"))
  
  script <- stringr::str_detect(
    tolower(other_entities), "^create_ecocomdp.r$")
  
  urls <- xml2::xml_text(
    xml2::xml_find_all(eml, ".//otherEntity/physical/distribution/online/url"))
  
  r <- httr::GET(
    url = urls[script], 
    httr::write_disk(paste0(path, "/create_ecocomDP.R"), overwrite = TRUE), 
    httr::user_agent("ecocomDP"))
  
  install_missing_libraries(
    conversion.script = paste0(path, "/create_ecocomDP.R"))
  
  suppressMessages(source(paste0(path, "/create_ecocomDP.R")))
  
}








#' Install missing libraries
#' 
#' @description Identify if a create_ecocomDP.R script uses uninstalled libraries and install them for use in \code{update_L1()}.
#' 
#' @param conversion.script (character) Full path to create_ecocomDP.R
#' 
#' @details Libraries used by create_ecocomDP.R are identified by reading the full file to memory and using regular expressions to extract the name of the library from within \code{library()} (i.e. \code{"(?<=library\\().*(?=\\))"}). All libraries found in this way are installed.
#'
#' @export
#'
install_missing_libraries <- function(conversion.script) {
  
  lines <- readLines(conversion.script)
  
  libraries <- unlist(
    stringr::str_extract_all(lines, "(?<=library\\().*(?=\\))"))
  
  installed_libraries <- row.names(installed.packages())
  
  if (!(all(libraries %in% installed_libraries))) {
    missing_libraries <- libraries[which(!(libraries %in% installed_libraries))]
    message(
      "Installing missing libraries: ", 
      paste(missing_libraries, collapse = ", "))
    install.packages(missing_libraries)
  }
  
}








#' Run L0 to L1 conversion script
#' 
#' @description Repository specific methods for handling script arguments is enabled by this function.
#'
#' @param path (character) Directory to which L1 tables, scripts, and metadata will be written.
#' @param id.L0.newest (character) Identifier of newest L0 data package.
#' @param id.L1.next (character) Identifier of new L1 being created by this script
#' @param url (character) Publicly accessible URL to \code{path} for download by a data repository.
#'
#' @return
#' @export
#'
run_conversion_script <- function(path, 
                                  id.L0.newest, 
                                  id.L1.next, 
                                  url) {
  
  # Load Global Environment config --------------------------------------------
  
  if (exists("config.environment", envir = .GlobalEnv)) {
    environment <- get("config.environment", envir = .GlobalEnv)
  } else {
    environment <- "production"
  }
  
  # Run script ----------------------------------------------------------------
  
  message(paste0("Creating new L1 (", id.L1.next, ") from newest L0 (", 
                 id.L0.newest, ")."))
  
  r <- create_ecocomDP(
    path = path,
    package.id.L0 = id.L0.newest, 
    package.id.L1 = id.L1.next, 
    environment = environment,
    url = url)
  
}








#' Update L1 from updated L0
#'
#' @description Updates an L1 data package when it's L0 parent data package has been updated. This function is a wrapper to several subroutines.
#'
#' @param id.L0.newest (character) Identifier of newest L0 data package.
#' @param id.L1.newest (character) Identifier of L0's newest L1 child to be created by this function. The L0-to-L1 conversion script of \code{id.L1.newest} will be used to create the child of \code{id.L0.newest}.
#' @param path (character) Directory to which L1 tables, scripts, and metadata will be written.
#' @param url (character) Publicly accessible URL to \code{path} for download by a data repository.
#' @param user.id (character) User identifier within a specified \code{repository}. This controls editing access in some \code{repository}.
#' @param user.pass (character) Password associated with \code{user.id} for repository upload.
#'
#' @details The updated L1 data package published in the data repository system specified by \code{repository} and in the sub-system specified by \code{environment}. Messaging from this routine is written to log_master.txt. Dependencies specified in conversion scripts are added to dependencies_update_L1.txt and installed in servers R distribution via \code{update_supported_libraries()}.
#' 
#' \code{user.id}, \code{repository}, and \code{user.pass} form a set of credentials allowing edits to all data packages within a repository system, otherwise evaluate/upload will fail if the creator is someone else.
#' 
#' Requires an L0 and L1 already exist in the repository, because the L1 creation and upload process may have errors that need resolution by a human.
#' 
#' @export
#'
update_L1 <- function(id.L0.newest, 
                      id.L1.newest,
                      path, 
                      url, 
                      user.id, 
                      user.pass) {
  
  message("----- Converting L0 (", id.L0.newest, ") to L1 (", 
          increment_package_version(id.L1.newest), ")")
  
  
  # Download and source conversion script -------------------------------------
  
  message("----- Downloading and sourcing L0-to-L1 conversion script")
  
  eml_L1_newest <- EDIutils::read_metadata(id.L1.newest, tier = config.environment)
  download_and_source_conversion_script(eml_L1_newest, path)
  
  # Create L1 -----------------------------------------------------------------
  
  message("----- Running L0-to-L1 conversion script")
  # TODO temporarily set config.environment to "staging"
  r <- run_conversion_script(
    path = path,
    id.L0.newest = id.L0.newest,
    id.L1.next = increment_package_version(id.L1.newest),
    url = url)
  
  # Upload to repository ------------------------------------------------------
  
  message("----- Uploading L1 (", increment_package_version(id.L1.newest), 
          ") to ", "EDI")
  
  r <- upload_to_repository(
    path = config.path,
    package.id = increment_package_version(id.L1.newest),
    user.id = config.user.id,
    user.pass = config.user.pass)
  
}