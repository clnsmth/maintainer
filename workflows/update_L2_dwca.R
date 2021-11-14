#' Update L2 DwC-A from updated L1
#' 
#' @description Updates an L2 DwC-A data package when it’s L1 source data package has been updated. This function is a wrapper to several subroutines.
#'
#' @param id.L1.newest (character) Identifier of newest L1 data package
#' @param core.name (character) The Darwin Core central table of the package. Can be: "event" (event core).
#' @param path (character) Directory to which L2 tables, meta.xml, and metadata will be written.
#' @param url (character) Publicly accessible URL to \code{path} for download by a data repository.
#' @param user.id (character) User identifier within a specified \code{repository}. This controls editing access in some \code{repository}.
#' @param user.pass (character) Password associated with \code{user.id} for repository upload.
#'
#' @note \code{user.id} and \code{user.pass} should be a set of master credentials within \code{repository}, otherwise issues at the evaluation/upload step may arise. Requires an L2 already exists in the \code{repository}. A pre-existing L2 must exist because the human has to decide if the L0 data already exists in GBIF or should not be uploaded to GBIF for other reasons.
#' 
#' @details No comparisons of L1 newest and previous are required since they are both in a standardized format.
#'
#' @export
#'
#' @examples
#' 
update_L2_dwca <- function(id.L1.newest,
                           core.name,
                           path,
                           url,
                           user.id,
                           user.pass) {
  
  # Lookup the derived data package
  derived <- get_derived(id.L1.newest)
  derived = increment_package_version(derived)
  
  # Load Global Environment config --------------------------------------------
  
  if (exists("config.environment", envir = .GlobalEnv)) {
    environment <- get("config.environment", envir = .GlobalEnv)
  } else {
    environment <- "production"
  }
  
  # Create L2 -----------------------------------------------------------------
  
  message("----- Converting L1 (", id.L1.newest, ") to L2 DwC-A ",
          stringr::str_to_title(core.name), " Core (", derived, ")")
  
  L1_to_L2_DwCA(
    path = config.path, 
    core.name = core.name, 
    source.package.id = id.L1.newest, 
    derived.package.id = derived, 
    data.table.url = config.www, 
    user.id = config.user.id,
    user.domain = "EDI")
  
  # Upload to repository ------------------------------------------------------
  
  message("----- Uploading L2 (", derived, ") to EDI")
  
  r <- upload_to_repository(
    path = config.path,
    package.id = derived,
    user.id = config.user.id,
    user.pass = config.user.pass)
  
  # Clear workspace -----------------------------------------------------------
  
  file.remove(list.files(config.path, full.names = T))
  
}