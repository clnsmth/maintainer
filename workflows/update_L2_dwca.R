#' Update an L2 DwC-A data package from an updated L1 ecocomDP data package
#' 
#' @description Updates an L2 Darwin Core Archive data package when it’s L1 
#' source data package has been updated. This function is a wrapper to several 
#' subroutines.
#'
#' @param id.L1.newest (character) Identifier of newest L1 data package
#' @param core.name (character) The Darwin Core central table of the package. 
#' Can be: "event" (event core).
#' @param path (character) Directory to which L2 tables, meta.xml, and metadata 
#' will be written.
#' @param url (character) Publicly accessible URL to \code{path} for download 
#' by a data repository.
#' @param user.id (character) User identifier for EDI data repository
#' @param user.pass (character) \code{user.id} password
#' 
update_L2_dwca <- function(id.L1.newest,
                           core.name,
                           path,
                           url,
                           user.id,
                           user.pass) {
  
  # Lookup the derived data package and set next version
  derived <- get_derived(id.L1.newest)
  derived <- increment_package_version(derived)
  
  # Load Global Environment config
  if (exists("config.environment", envir = .GlobalEnv)) {
    environment <- get("config.environment", envir = .GlobalEnv)
  } else {
    environment <- "production"
  }
  
  # Create L2
  message("----- Converting L1 (", id.L1.newest, ") to L2 DwC-A ",
          stringr::str_to_title(core.name), " Core (", derived, ")")
  ecocomDP::convert_to_dwca(
    path = path, 
    core_name = "event", 
    source_id = id.L1.newest, 
    derived_id = derived, 
    user_id = user.id, 
    user_domain = "EDI")
  
  # Upload to repository
  message("----- Uploading L2 (", derived, ") to EDI")
  r <- upload_to_repository(
    path = config.path,
    package.id = derived,
    user.id = user.id,
    user.pass = user.pass)
  
  # Clear workspace
  message("----- Clearing workspace")
  files <- list.files(config.path, full.names = TRUE)
  i <- grep("^(?!README).*$", list.files(config.path), perl = TRUE)
  file.remove(files[i])
  
}