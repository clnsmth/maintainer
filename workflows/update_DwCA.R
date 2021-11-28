#' Update a DwC-A data package from its ecocomDP source
#' 
#' @description Updates an L2 Darwin Core Archive data package when its L1 
#' ecocomDP source data package has been updated. This function is a wrapper to 
#' several subroutines.
#'
#' @param source_id (character) Identifier of newest L1 data package
#' @param path (character) Directory to which the DwC-A tables, meta.xml, and 
#' EML will be written
#' @param url (character) Publicly accessible URL to \code{path} for download 
#' by a data repository
#' @param user_id (character) User identifier for EDI data repository
#' @param user_pass (character) \code{user_id} password
#' @param environment (character) Environment of the EDI data repository from 
#' which \code{source_id} originates and the derived data package will be 
#' written to
#' 
update_DwCA <- function(source_id,
                        path,
                        url,
                        user_id,
                        user_pass,
                        environment) {
  
  # Lookup the derived data package and set next version
  derived_id <- get_derived_id(source_id)
  derived_id <- increment_package_version(derived_id)
  
  # Create the DwC-A data package
  message("Converting L1 (", source_id, ") to L2 DwC-A (", derived_id, ")")
  ecocomDP::convert_to_dwca(
    path = path, 
    url = url,
    core_name = "event", 
    source_id = source_id, 
    derived_id = derived_id, 
    user_id = user_id, 
    user_domain = "EDI")
  
  # Upload to EDI
  message("Uploading L2 (", derived_id, ") to EDI")
  EDIutils::api_update_data_package(
    path = path, 
    package.id = derived_id, 
    user.id = user_id, 
    user.pass = user_pass,
    environment = environment,
    affiliation = "EDI")
  
  # Clear workspace
  message("Clearing workspace")
  files <- list.files(path, full.names = TRUE)
  file.remove(files)
  
}