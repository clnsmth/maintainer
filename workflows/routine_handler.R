#' A top level function for handling sub-routines
#' 
#' @description This function performs a number of steps standard to any 
#' automated workflow including:
#' \itemize{
#'   \item Routine locking - To prevent multiple routines from running at the same time
#'   \item File logging - For diagnosing issues if they arise
#'   \item Emailing - For notification of a run and for shipping the log file
#'   \item Workflow mapping - For correctly identifying the source and derived data packages and the workflow to call
#'   \item Series integrity checks - As a fail safe against asynchronous processing
#'   \item Source differencing - To identify changes in the source data package that may break a workflow
#'   \item Clean up - Removing temporary files after a run
#' }
#'
routine_handler <- function() {
  
  # Check lock ----------------------------------------------------------------
  
  # Stop if a routine is in progress
  lockfile <- "./ecocomDP-maintainer/temp/lock.txt"
  if (file.exists(lockfile)) {
    return(NULL)
  }
  
  # Lock ----------------------------------------------------------------------
  
  # Keep another routine from running while this one is
  invisible(file.create(lockfile))
  on.exit(file.remove(lockfile), add = TRUE)
  
  # Initialize logging --------------------------------------------------------
  
  # Return warnings in a form that can be logged
  default <- getOption("warn")
  on.exit(options(warn = default), add = TRUE)
  options(warn = 1)
  
  # Create file
  log_file <- paste0("log_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
  log <- file(paste0("./ecocomDP-maintainer/logs/", log_file), open = "wt")
  sink(log, type = "message")
  message("----- Starting routine_handler() at ", Sys.time())
  on.exit(message("----- Copying ", log_file, " to ./logs"), add = TRUE)
  on.exit(
    file.copy(
      from = paste0(config.path, "/", log_file),
      to = paste0(dirname(config.path), "/logs/", log_file)),
    add = TRUE)
  on.exit(message("----- Exiting routine_handler() at ", Sys.time()), add = TRUE)
  on.exit(sink(type = "message"), add = TRUE)
  on.exit(close(log), add = TRUE)
  
  # Email
  on.exit(
    send_email(
      from = config.email.address,
      to = config.email.address,
      attachment = paste0("./ecocomDP-maintainer/", log_file),
      smtp.relay = "smtp.gmail.com",
      relay.user = config.email.address,
      relay.user.pass = config.email.pass,
      subject = log_file,
      msg = "Log file from routine_handler\\(\\) is attached"),
    add = TRUE)
  
  # Get next item from queue --------------------------------------------------
  
  # Get the new data package from the queue. Stop if there is none.
  new_pkg <- get_from_queue()
  if (is.null(new_pkg)) {
    return(NULL)
  }
  message("----- Processing ", new_pkg$id)
  
  # Identify derived ------------------------------------------------------------
  
  # Identify the derived data package to be create from this one
  derived <- get_child(new_pkg$id)
  
  # Identify routine ----------------------------------------------------------
  
  # Look up the routine/workflow to call for this data package. The info is
  # stored in ./ecocomDP-maintainer/webapp/map.csv
  workflow <- get_workflow(new_pkg$id)

  # Check series integrity ----------------------------------------------------
  
  # Stop if any earlier revisions of this data package have not been processed
  message("----- Checking series integrity")
  if (has_unprocessed_versions(new_pkg$id)) {
    return(NULL)
  }
  
  # Compare source versions ---------------------------------------------------
  
  previous_version <- get_previous_version(new_pkg$id)
  
  # Compare EML
  message("----- Comparing EML (Looking for meaningful changes between ",
          "newest and previous versions)")
  eml_newest <- EDIutils::read_metadata(new_pkg$id, "staging") # TODO control tier with env vars
  eml_previous <- EDIutils::read_metadata(previous_version, "staging") # TODO control tier with env vars
  message(
    capture.output(
      compare_eml(eml_newest, eml_previous, return.all = F)))
  
  # Compare tables
  message("----- Comparing data tables (Looking for meaningful changes ",
          "between newest and previous versions)")
  tables_L0_newest <- read_tables(eml_newest)
  tables_L0_previous <- read_tables(eml_previous)
  message(
    capture.output(
      compare_tables(tables_L0_newest, tables_L0_previous)))
  
   # Run workflow --------------------------------------------------------------
  
  if (workflow == "update_L1") {
    
    message("----- ", new_pkg$id, " is an L0")
    
    update_L1(
      id.L0.newest = new_pkg$id,
      id.L1.newest = derived,
      path = config.path, 
      url = config.path, 
      user.id = config.user.id,
      user.pass = config.user.pass)
    
  } else if (workflow == "update_L2") {
    
    message("----- ", new_pkg$id, " is an L1")
    
    update_L2_dwca(
      id.L1.newest = new_pkg$id,
      id.L2.next = increment_package_version(names(new_pkg$parent_of_dwcae)),
      core.name = "event",
      path = config.path,
      url = config.www,
      user.id = config.user.id,
      user.pass = config.user.pass)
    
  }
  
  # Delete from queue ---------------------------------------------------------
  
  message("----- Deleting from queue")
  r <- delete_from_queue(new_pkg$index, new_pkg$id)
  
  # Clear workspace -----------------------------------------------------------
  
  on.exit(file.remove(list.files(config.path, full.names = T)), add = TRUE)
  
  return(NULL)
  
}