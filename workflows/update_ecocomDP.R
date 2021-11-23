#' Update an ecocomDP data package
#'
#' @description This function updates an L1 ecocomDP data package from its 
#' source data package
#'
#' @param source_id (character) Identifier of newest L0 data package
#' @param path (character) Directory to which the derived L1 ecocomDP tables, 
#' scripts, and metadata will be written
#' @param url (character) Publicly accessible URL to \code{path} for download 
#' by a data repository
#' @param user_id (character) User identifier for the EDI data repository
#' @param user_pass (character) Password for \code{user_id}
#' @param environment (character) Environment of the EDI data repository from 
#' which \code{source_id} originates and the derived data package will be 
#' written to
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
update_ecocomDP <- function(source_id, 
                            path, 
                            url, 
                            user_id, 
                            user_pass,
                            environment) {
  
  # Look up identifier of derived data package
  derived_id <- get_derived_id(source_id)
  derived_id_next <- increment_package_version(derived_id)
  message("Converting L0 (", source_id, ") to L1 (", derived_id_next, ")")
  
  # Download and source conversion script from the previous L1 data package
  message("Downloading and sourcing L0-to-L1 conversion script")
  eml_L1_newest <- EDIutils::api_read_metadata(
    package.id = derived_id, 
    environment = environment)
  dwnld_and_src_script(eml_L1_newest, path)
  
  # Create L1
  message("Running create_ecocomDP()")
  r <- create_ecocomDP(
    path = path,
    source_id = source_id, 
    derived_id = derived_id_next, 
    url = url)
  
  # Create plots for the project website
  message("Updating plots")
  flat = ecocomDP::flatten_data(ecocomDP::read_data(from = path))
  plots <- list(
    diversity = ecocomDP::plot_taxa_diversity(flat, time_window_size = "month"),
    shared = ecocomDP::plot_taxa_shared_sites(flat),
    occurrence = ecocomDP::plot_taxa_occur_freq(flat))

  # Save plots to the website ./docs/assets directory
  r <- lapply(
    seq_along(plots),
    function(i) {
      ggplot2::ggsave(
        filename = paste0("./docs/assets/", names(plots)[i], ".png"),
        plot = plots[[i]],
        width = 5,
        height = 3,
        units = "in",
        bg = "white")
    })

  # Commit plots and push to GitHub
  repo <- git2r::repository()
  changes <- paste0("./docs/assets/", names(plots), ".png")
  git2r::add(path = changes)
  try(git2r::commit(message = "Update plots"))
  cred <- git2r::cred_user_pass(
    username = config.github.user,
    password = config.github.pass)
  with_timeout(
    expr = git2r::push(repo, "origin", refspec = "refs/heads/main", credentials = cred),
    limit = 10)

  
  # Upload to EDI
  message("Uploading L1 (", derived_id_next, ") to ", "EDI")
  EDIutils::api_update_data_package(
    path = path, 
    package.id = derived_id_next, 
    user.id = user_id, 
    user.pass = user_pass,
    environment = environment,
    affiliation = "EDI")
  
  
  # Clear workspace
  message("Clearing workspace")
  files <- list.files(config.path, full.names = TRUE)
  file.remove(files)

}








#' Download and source the L0 to L1 conversion script
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
dwnld_and_src_script <- function(eml, path) {
  message("Downloading and sourcing the conversion script")
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
  suppressMessages(source(paste0(path, "/create_ecocomDP.R")))
}
