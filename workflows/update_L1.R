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
  derived_next <- increment_package_version(derived)
  message("Converting L0 (", id.L0.newest, ") to L1 (", derived_next, ")")
  
  # Download and source conversion script from the previous L1 data package
  message("Downloading and sourcing L0-to-L1 conversion script")
  eml_L1_newest <- EDIutils::api_read_metadata(
    package.id = derived, 
    environment = config.environment)
  download_and_source_conversion_script(eml_L1_newest, path)
  
  # Create L1
  message("Running L0-to-L1 conversion script")
  r <- run_conversion_script(
    path = path,
    id.L0.newest = id.L0.newest,
    id.L1.next = derived_next,
    url = url)
  
  # # Create plots for the project website
  # message("Updating plots")
  # flat = ecocomDP::flatten_data(ecocomDP::read_data(from = path))
  # plots <- list(
  #   sampling = ecocomDP::plot_sample_space_time(flat),
  #   accumulation = ecocomDP::plot_taxa_accum_time(flat),
  #   diversity = ecocomDP::plot_taxa_diversity(flat, time_window_size = "month"),
  #   shared = ecocomDP::plot_taxa_shared_sites(flat),
  #   occurrence = ecocomDP::plot_taxa_occur_freq(flat))
  # 
  # # Save plots to the website ./docs/assets directory
  # r <- lapply(
  #   seq_along(plots),
  #   function(i) {
  #     ggplot2::ggsave(
  #       filename = paste0("./docs/assets/", names(plots)[i], ".png"),
  #       plot = plots[[i]],
  #       width = 5,
  #       height = 3,
  #       units = "in",
  #       bg = "white")
  #   })
  # 
  # # Commit plots and push to GitHub
  # repo <- git2r::repository()
  # changes <- paste0("./docs/assets/", names(plots), ".png")
  # git2r::add(path = changes)
  # try(git2r::commit(message = "Update plots"))
  # cred <- git2r::cred_user_pass(
  #   username = config.github.user, 
  #   password = config.github.pass)
  # git2r::push(repo, "origin", refspec = "refs/heads/main", credentials = cred)

  
  # Upload to EDI
  message("Uploading L1 (", derived_next, 
          ") to ", "EDI")
  r <- upload_to_repository(
    path = config.path,
    package.id = derived_next,
    user.id = user.id,
    user.pass = user.pass)
  
  # Clear workspace
  message("Clearing workspace")
  files <- list.files(config.path, full.names = TRUE)
  i <- grep("^(?!README).*$", list.files(config.path), perl = TRUE)
  file.remove(files[i])

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
                 id.L0.newest, ")"))
  r <- create_ecocomDP(
    path = path,
    source_id = id.L0.newest, 
    derived_id = id.L1.next, 
    url = url)
}
