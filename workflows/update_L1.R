#' Update an L1 ecocomDP data package from it's updated L0 source
#'
#' @description Updates an L1 data package when it's L0 source data package has 
#' been updated. This function is a wrapper to several subroutines.
#'
#' @param id.L0.newest (character) Identifier of newest L0 data package
#' @param path (character) Directory to which L1 tables, scripts, and metadata 
#' will be written
#' @param url (character) Publicly accessible URL to \code{path} for download 
#' by a data repository. Is specified by the \code{config.www} global variable.
#' @param user.id (character) User identifier for the EDI data repository
#' @param user.pass (character) Password for \code{user.id}
#'
#' @details The updated L1 data package published in EDI and in the sub-system
#' environment specified by the global variable \code{config.environment}. 
#' 
#' Dependencies specified in conversion scripts are installed via 
#' \code{install_missing_libraries()}.
#' 
#' Requires an L0 and L1 already exist in the EDI repository, because 
#' conversion script is archived with the L1 data package.
#'
update_L1 <- function(id.L0.newest, 
                      path, 
                      url, 
                      user.id, 
                      user.pass) {
  
  # Look up identifier of derived data package
  derived <- get_derived(id.L0.newest)
  message("----- Converting L0 (", id.L0.newest, ") to L1 (", 
          increment_package_version(derived), ")")
  
  # Download and source conversion script from the previous L1 data package
  message("----- Downloading and sourcing L0-to-L1 conversion script")
  eml_L1_newest <- EDIutils::api_read_metadata(
    package.id = derived, 
    environment = config.environment)
  download_and_source_conversion_script(eml_L1_newest, path)
  
  # Create L1
  message("----- Running L0-to-L1 conversion script")
  r <- run_conversion_script(
    path = path,
    id.L0.newest = id.L0.newest,
    id.L1.next = increment_package_version(derived),
    url = url)
  
  # Upload to EDI
  message("----- Uploading L1 (", increment_package_version(derived), 
          ") to ", "EDI")
  r <- upload_to_repository(
    path = config.path,
    package.id = increment_package_version(derived),
    user.id = user.id,
    user.pass = user.pass)
  
  # Clear workspace
  message("----- Clearing workspace")
  file.remove(list.files(config.path, full.names = T))
  
}








#' Download and source L0 to L1 conversion script
#'
#' @param eml (xml_document xml_node) EML of newest derived L1. The L0-to-L1 
#' conversion script listed in \code{eml} will be used to create the derived
#' data package of the newest L0.
#' @param path (character) Directory to which create_ecocomDP.R will be written. 
#' Should be the same directory tables and metadata will be written to. Is 
#' controlled by the \code{config.path} global variable.
#' 
#' @details The script is downloaded to \code{path} and then parsed to identify 
#' R libraries used by the script which are installed if not already.
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
#' @description This is a wrapper to create_ecocomDP() functions, which have a 
#' standardized set of input arguments.
#'
#' @param path (character) Directory to which L1 tables, scripts, and metadata 
#' will be written.
#' @param id.L0.newest (character) Identifier of newest L0 data package.
#' @param id.L1.next (character) Identifier of new L1 being created by this 
#' script
#' @param url (character) Publicly accessible URL to \code{path} for download 
#' by the ED data repository. Is set by the \code{config.www} global variable.
#'
run_conversion_script <- function(path, 
                                  id.L0.newest, 
                                  id.L1.next, 
                                  url) {
  # Load Global Environment config
  if (exists("config.environment", envir = .GlobalEnv)) {
    environment <- get("config.environment", envir = .GlobalEnv)
  } else {
    environment <- "production"
  }
  # Run script
  message(paste0("Creating new L1 (", id.L1.next, ") from newest L0 (", 
                 id.L0.newest, ")."))
  r <- create_ecocomDP(
    path = path,
    source_id = id.L0.newest, 
    derived_id = id.L1.next, 
    url = url)
}