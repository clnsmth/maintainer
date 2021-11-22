# This script is called by the listener whenever an update to a subscribed data
# package occurs



# Source the workflow manager -------------------------------------------------

#' A top level function for managing workflows
#' 
#' @description This function:
#' \itemize{
#'   \item Stops if another workflow is in progress - Running multiple 
#'   workflows at the same time can cause issues.
#'   \item Logs messages/warnings/errors to file - Provides a basis for trouble 
#'   shooting.
#'   \item Identifies the updated source data package - Updates are stored in
#'   a queue and are processed in order.
#'   \item Stops if an earlier version has not been processed - It's possible 
#'   (though unlikely) for the queue to be out of order.
#'   \item Looks for meaningful differences with the previous version - Data 
#'   packages can change and modifications to data structure may break a 
#'   workflow and changes to semantic meaning may produce misleading results.
#'   \item Identifies the workflow(s) to call - An explicit mapping is used to 
#'   process source data packages into their derived form(s).
#'   \item Runs the workflow(s) - Loads the workflow then runs it.
#'   \item Emails the run log to project maintainers - Tells project 
#'   maintainers that a workflow ran and if any issues occurred.
#' }
#' 
#' 
#' @details 
#' Fragile processes within this function are wrapped in \code{on.exit()} to 
#' ensure they are executed. Examples include: Logging, emailing, default 
#' settings.
#'
workflow_manager <- function() {
  
  # Initialize logging --------------------------------------------------------
  
  # Return warnings in a form that can be logged to file
  default <- getOption("warn")
  on.exit(options(warn = default), add = TRUE)
  options(warn = 1)
  
  # Create the log file and start logging
  log_file <- paste0("log_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
  log <- file(paste0("./logs/", log_file), open = "wt")
  sink(log, type = "message")
  msg("Starting")
  on.exit(msg("Copying ", log_file, " to ./logs"), add = TRUE)
  on.exit(msg("Emailing log file"), add = TRUE)
  on.exit(
    send_email(
      from = config.email.address,
      to = config.email.address,
      attachment = paste0("./logs/", log_file),
      smtp.relay = "smtp.gmail.com",
      relay.user = config.email.address,
      relay.user.pass = config.email.pass,
      subject = log_file,
      msg = "Log file from workflow_manager\\(\\) is attached"),
    add = TRUE)
  on.exit(msg("Exiting"), add = TRUE)
  on.exit(sink(type = "message"), add = TRUE)
  on.exit(close(log), add = TRUE)
  
  # Lock processing -----------------------------------------------------------
  
  # Stop if another workflow is already running
  lockfile <- "./webapp/lock.txt"
  if (file.exists(lockfile)) {
    msg("The workflow manager is locked")
    message("The workflow manager may already be running. If you feel you've ",
            "reached this message in error, then manually remove the lock ",
            "file (/maintainer/webapp/lock.txt) and try again.")
    return(NULL)
  }
  
  # Stop other workflows from running while this process is underway
  invisible(file.create(lockfile))
  on.exit(file.remove(lockfile), add = TRUE)
  
  # Iterate through updates ---------------------------------------------------
  # Run while the queue has unprocessed items. It's possible for the queue to 
  # gain additional updates while the workflow_manager() is running, and 
  # because the listener only calls upon receiving an update, these updates
  # wouldn't get processed.
  while (!queue_is_empty()) {
    
    # Identify the update -----------------------------------------------------
    
    # Query the queue for an updated data package and stop if there is none
    msg("Checking for updates")
    new_pkg <- get_from_queue()
    if (is.null(new_pkg)) {
      message("No update found")
      return(NULL)
    }
    message("Found an update (", new_pkg$id, ")")
    
    # Check series integrity --------------------------------------------------
    
    # Stop if an earlier version hasn't been processed
    msg("Checking series integrity")
    if (has_unprocessed_versions(new_pkg$id)) {
      return(NULL)
    }
    
    # Compare versions --------------------------------------------------------
    
    # Look for meaningful differences in the metadata of the newest and 
    # previous versions
    previous_version <- get_previous_version(new_pkg$id)
    if (!is.null(previous_version)) {
      msg("Looking for metadata changes")
      message("Comparing ", new_pkg$id, " to ", previous_version)
      eml_newest <- EDIutils::api_read_metadata(
        package.id = new_pkg$id, 
        environment = config.environment)
      eml_previous <- EDIutils::api_read_metadata(
        package.id = previous_version, 
        environment = config.environment)
      message(
        capture.output(
          compare_eml(eml_newest, eml_previous, return.all = FALSE)))
    }
    
    # Identify workflow -------------------------------------------------------
    
    # Get name of the workflow to run from ./maintainer/webapp/workflow_map.csv
    msg("Identifying workflow(s)")
    workflows <- get_workflows(new_pkg$id)
    if (is.null(workflows)) {
      message("Could not find workflow(s) for ", new_pkg$id)
      return(NULL)
    }
    message("Found workflow(s) for ", new_pkg$id)
    
    # Run workflow(s) ---------------------------------------------------------
    
    for (workflow in workflows) {
      msg(paste0("Running workflow: ", workflow))
      run_workflow(workflow, new_pkg_id = new_pkg$id)
    }
    msg("All workflow(s) have completed")
    
    # Remove the update from the queue ----------------------------------------
    msg("Removing ", new_pkg$id, " from the queue")
    r <- delete_from_queue(new_pkg$index, new_pkg$id)

  }
  
  return(NULL)
  
}







# Source configuration variables and functions --------------------------------
scripts <- list.files("./workflows", full.names = TRUE)
invisible(sapply(scripts, source))

# Run workflow manager --------------------------------------------------------
# Call the top level function that manages all workflows
workflow_manager()
