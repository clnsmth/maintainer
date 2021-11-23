# This script is called by the listener whenever an update to a subscribed data
# package occurs

# Source the workflow manager -------------------------------------------------

#' A top level function for managing workflows user defined workflows
#' 
#' @description This function:
#' \itemize{
#'   \item Stops if another workflow is in progress. Running multiple 
#'   workflows at the same time can cause issues.
#'   \item Logs messages/warnings/errors to file. Providing a basis for trouble 
#'   shooting.
#'   \item Identifies the updated source data package. Updates are stored in
#'   a queue and are processed in order.
#'   \item Stops if an earlier version has not been processed. It's possible 
#'   (though unlikely) for the queue to be out of order.
#'   \item Looks for meaningful differences with the previous version. Data 
#'   packages can change and modifications to data structure may break a 
#'   workflow and changes to semantic meaning may produce misleading results.
#'   \item Identifies the workflow(s) to call. An explicit mapping is used to 
#'   process source data packages into their derived form(s).
#'   \item Runs the workflow(s). Loads the workflow then runs it.
#'   \item Emails the run log to project maintainers. Tells project 
#'   maintainers that a workflow ran and if any issues occurred.
#' }
#' 
#' 
#' @details 
#' Fragile processes within this function are wrapped in \code{on.exit()} to 
#' ensure they are executed if an error is encountered. Examples include: 
#' Logging, emailing, default settings.
#' 
#' User defined workflows are in /maintainer/workflows/run_workflow.R
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
  msg("Starting workflow manager")
  on.exit(msg("Writing log"), add = TRUE)
  on.exit(message("/maintainer/logs/", log_file), add = TRUE)
  on.exit(msg("Emailing log"), add = TRUE)
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
  on.exit(msg("Exiting workflow manager"), add = TRUE)
  on.exit(sink(type = "message"), add = TRUE)
  on.exit(close(log), add = TRUE)
  
  # Lock processing -----------------------------------------------------------
  
  # Stop if another instance of the workflow manager is already running
  lockfile <- "./webapp/lock.txt"
  if (file.exists(lockfile)) {
    msg("The workflow manager is locked")
    message("The workflow manager may already be running. If you feel you've ",
            "reached this message in error, then manually remove the lock ",
            "file (/maintainer/webapp/lock.txt) and try again.")
    return(NULL)
  }
  
  # Stop other instances of the workflow manager from running while this one is
  invisible(file.create(lockfile))
  on.exit(file.remove(lockfile), add = TRUE)
  
  # Check for updates ---------------------------------------------------------
  
  # Check the queue for updates and stop if there are none
  msg("Checking for updates")
  if (queue_is_empty()) {
    message("No updates found")
    return(NULL)
  }
  
  # Iterate through updates ---------------------------------------------------
  
  # Run while the queue has unprocessed items. It's possible for the queue to 
  # gain additional updates while the workflow manager is running, and 
  # because the listener only calls upon receiving an update, these updates
  # wouldn't otherwise be processed.
  
  while (!queue_is_empty()) {
    
    # Identify the update -----------------------------------------------------
    
    new_pkg <- pop_queue()
    message("Found an update (", new_pkg$id, ")")
    
    # Check series integrity --------------------------------------------------
    
    # Stop if an earlier version hasn't been processed
    msg("Checking series integrity")
    if (check_series_integrity(new_pkg$id)) {
      return(NULL)
    }
    message("No issues with series integrity")
    
    # Compare versions --------------------------------------------------------
    
    # Look for meaningful differences in the metadata of the newest and 
    # previous versions of the updated data package
    previous_version <- get_previous_version(new_pkg$id)
    if (!is.null(previous_version)) {
      msg("Comparing EML metadata")
      message("Comparing ", new_pkg$id, " to ", previous_version)
      eml_newest <- EDIutils::api_read_metadata(
        package.id = new_pkg$id, 
        environment = config.environment)
      eml_previous <- EDIutils::api_read_metadata(
        package.id = previous_version, 
        environment = config.environment)
      res <- compare_eml(eml_newest, eml_previous, return.all = FALSE)
      if (is.null(res)) {
        message("No meaningful differences found")
      } else {
        message("Found potentially meaningful differences at xpaths:\n")
        message(paste(res, collapse = "\n"))
      }
    }
    
    # Identify workflow -------------------------------------------------------
    
    # Get the name of the workflow to run from the workflow map located at:
    # /maintainer/webapp/workflow_map.csv
    msg("Identifying workflow(s)")
    workflows <- get_workflows(new_pkg$id)
    if (is.null(workflows)) {
      message("Could not find workflow(s) for ", new_pkg$id)
      return(NULL)
    }
    message("Found workflow(s) for ", new_pkg$id)
    
    # Run workflow(s) ---------------------------------------------------------
    
    # Iterate across workflows
    for (workflow in workflows) {
      msg(paste0("Running workflow: ", workflow))
      run_workflow(workflow, source_id = new_pkg$id)
    }
    msg("All workflow(s) have completed")
    
    # Remove the update from the queue ----------------------------------------
    
    msg("Removing ", new_pkg$id, " from the queue")
    r <- remove_from_queue(new_pkg$index)

  }
  
  return(NULL)
  
}

# Source the workflow configuration and functions -----------------------------

scripts <- list.files("./workflows", full.names = TRUE)
invisible(sapply(scripts, source))

# Run the workflow manager ----------------------------------------------------

# Call the top level function that manages all workflows
workflow_manager()
