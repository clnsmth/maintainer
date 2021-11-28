# This script is called by the listener whenever an update to a subscribed data
# package occurs








# Source the workflow manager (and helpers) -----------------------------------

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
  if (queue_is_empty(url = config.url.http)) {
    message("No updates found")
    return(NULL)
  }
  
  # Iterate through updates ---------------------------------------------------
  
  # Run while the queue has unprocessed items. It's possible for the queue to 
  # gain additional updates while the workflow manager is running, and 
  # because the listener only calls upon receiving an update, these updates
  # wouldn't otherwise be processed.
  
  while (!queue_is_empty(url = config.url.http)) {
    
    # Identify the update -----------------------------------------------------
    
    new_pkg <- queue_get_update(url = config.url.http)
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
    r <- queue_remove_update(url = config.url.http, index = new_pkg$index)

  }
  
  return(NULL)
  
}








#' Check if earlier unprocessed versions of a data package are in the queue
#'
#' The presence of such items may indicate the integrity of the series is 
#' compromised and processing should be halted until the issue is addressed.
#'
#' @param packageId (character) Package ID with the form 
#' "scope.identifier.revision"
#'
#' @return (logical) TRUE if earlier versions of \code{packageId} are found,
#' otherwise FALSE
#'
check_series_integrity <- function(packageId) {
  id <- stringr::str_remove(packageId, "\\.[:digit:]*$")
  rev <- stringr::str_extract(packageId, "(?<=\\.)[:digit:]*$")
  queue <- queue_get_update(
    url = "https://regan.edirepository.org/maintainer", 
    filter = "unprocessed")$id
  if (is.null(queue)) {
    return(FALSE)
  }
  ids <- stringr::str_remove(queue, "\\.[:digit:]*$")
  revs <- stringr::str_extract(queue, "(?<=\\.)[:digit:]*$")
  i <- rev > revs[ids %in% id]
  if (any(i)) {
    message("Unprocessed earlier versions of ", id, " found in the queue. ",
            "These earlier versions (", queue[i], ") need to ",
            "be processed before ", packageId, ".")
    return(TRUE)
  } else {
    return(FALSE)
  }
}








#' Identify potentially meaningful differences between EML documents
#' 
#' @description For discovering changes within a dataset that may affect 
#' downstream processes relying on consistent structure and meaning. This is 
#' useful in workflow automation where reporting such changes can expedite 
#' trouble shooting and manual intervention.
#'
#' @param newest (xml_document, xml_node) EML of the newest version of a data 
#' package
#' @param previous (xml_document, xml_node) EML of the previous version of a 
#' data package
#' @param return.all (logical) Return all differences? Default is FALSE, i.e. 
#' only potentially meaningful differences are returned. Meaningful differences 
#' do not include elements expected to change between versions (e.g. number of 
#' rows, file size, temporal coverage).
#'
#' @return (character) XPaths of nodes that differ between versions
#' 
#' @details 
#' XPaths of checked nodes (and whether "meaningful"):
#' \itemize{
#'   \item{.//dataset/abstract (TRUE)}
#'   \item{.//dataset/coverage/geographicCoverage (FALSE)}
#'   \item{.//dataset/coverage/temporalCoverage (FALSE)}
#'   \item{.//dataset/coverage/taxonomicCoverage (FALSE)}
#'   \item{.//dataset/keywordSet (FALSE)}
#'   \item{.//dataTable/physical/objectName (TRUE)}
#'   \item{.//dataTable/physical/size (FALSE)}
#'   \item{.//dataTable/physical/authentication (FALSE)}
#'   \item{.//dataTable/physical/dataFormat/textFormat/numHeaderLines (TRUE)}
#'   \item{.//dataTable/physical/dataFormat/textFormat/recordDelimiter (FALSE)}
#'   \item{.//dataTable/physical/dataFormat/textFormat/attributeOrientation (TRUE)}
#'   \item{.//dataTable/physical/dataFormat/textFormat/simpleDelimited/fieldDelimiter (TRUE)}
#'   \item{.//dataTable/attributeList (TRUE)}
#'   \item{.//dataTable/numberOfRecords (FALSE)}
#'   \item{.//otherEntity/physical/objectName (TRUE)}
#'   \item{.//otherEntity/physical/size (FALSE)}
#'   \item{.//otherEntity/physical/authentication (FALSE)}
#'   \item{.//otherEntity/physical/dataFormat/textFormat/numHeaderLines (TRUE)}
#'   \item{.//otherEntity/physical/dataFormat/textFormat/recordDelimiter (TRUE)}
#'   \item{.//otherEntity/physical/dataFormat/textFormat/attributeOrientation (TRUE)}
#'   \item{.//otherEntity/physical/dataFormat/textFormat/simpleDelimited/fieldDelimiter (TRUE)}
#'   \item{.//otherEntity/attributeList (TRUE)}
#' }
#' 
compare_eml <- function(newest, 
                        previous,
                        return.all = FALSE) {
  
  # Collapse EML node to string then compare
  #'
  #' @param newest (xml_document, xml_node) Newest version of an EML document
  #' @param previous (xml_document, xml_node) Previous version of an EML document
  #' @param xpath (character) xpath of node to compare
  #'
  #' @return (character) xpath of node if \code{newest} and \code{previous} 
  #' differ, otherwise NULL
  #' 
  compare_node_as_string <- function(newest, previous, xpath) {
    # Collapse to string
    newest <- xml2::xml_text(xml2::xml_find_all(newest, xpath))
    previous <- xml2::xml_text(xml2::xml_find_all(previous, xpath))
    # Only return dissimilar nodes. Add node number to xpath for exact reference.
    if (!all(newest == previous)) {
      nodes <- which(!(newest == previous))
      if (stringr::str_detect(xpath, "dataTable")) {
        parts <- stringr::str_split(xpath, "(?<=dataTable)")
        res <- paste0(parts[[1]][1], "[", nodes, "]", parts[[1]][2])
        return(res)
      } else if (stringr::str_detect(xpath, "otherEntity")) {
        parts <- stringr::str_split(xpath, "(?<=otherEntity)")
        res <- paste0(parts[[1]][1], "[", nodes, "]", parts[[1]][2])
        return(res)
      } else {
        res <- xpath
        return(res)
      }
    }
  }
  
  # Nodes to test and whether "meaningful"
  nodes <- c(
    `.//dataset/abstract` = TRUE,
    `.//dataset/coverage/geographicCoverage` = FALSE,
    `.//dataset/coverage/temporalCoverage` = FALSE,
    `.//dataset/coverage/taxonomicCoverage` = FALSE,
    `.//dataset/keywordSet` = FALSE,
    `.//dataTable/physical/objectName` = TRUE,
    `.//dataTable/physical/size` = FALSE,
    `.//dataTable/physical/authentication` = FALSE,
    `.//dataTable/physical/dataFormat/textFormat/numHeaderLines` = TRUE,
    `.//dataTable/physical/dataFormat/textFormat/recordDelimiter` = FALSE,
    `.//dataTable/physical/dataFormat/textFormat/attributeOrientation` = TRUE,
    `.//dataTable/physical/dataFormat/textFormat/simpleDelimited/fieldDelimiter` = TRUE,
    `.//dataTable/attributeList` = TRUE,
    `.//dataTable/numberOfRecords` = FALSE,
    `.//otherEntity/physical/objectName` = TRUE,
    `.//otherEntity/physical/size` = FALSE,
    `.//otherEntity/physical/authentication` = FALSE,
    `.//otherEntity/physical/dataFormat/textFormat/numHeaderLines` = TRUE,
    `.//otherEntity/physical/dataFormat/textFormat/recordDelimiter` = TRUE,
    `.//otherEntity/physical/dataFormat/textFormat/attributeOrientation` = TRUE,
    `.//otherEntity/physical/dataFormat/textFormat/simpleDelimited/fieldDelimiter` = TRUE,
    `.//otherEntity/attributeList` = TRUE)
  
  # Filter
  if (return.all) {
    nodes <- names(nodes)
  } else {
    nodes <- names(nodes[nodes])
  }
  
  # Compare
  res <- sapply(
    nodes,
    function(x) {
      compare_node_as_string(newest, previous, x)
    }, 
    USE.NAMES = FALSE)
  
  return(unlist(res))
  
}








#' Get identifier of previous data package version
#'
#' @param packageId (character) Data package identifier of the form 
#' "scope.identifier.revision"
#'
#' @return (character) Previous data package version. Returns NULL if 
#' \code{packageId} is the first version.
#' 
get_previous_version <- function(packageId) {
  if (exists("config.environment", envir = .GlobalEnv)) {
    environment <- get("config.environment", envir = .GlobalEnv)
  } else {
    environment <- "production"
  }
  parts <- unlist(stringr::str_split(packageId, "\\."))
  vers <- suppressMessages(
    EDIutils::api_list_data_package_revisions(
      scope = parts[1],
      identifier = parts[2],
      environment = environment))
  vers <- as.numeric(vers)
  if (length(vers) == 1) {
    return(NULL)
  }
  if (which(vers == as.numeric(parts[3])) == 1) {
    return(NULL)
  }
  parts[3] <- vers[which(vers == as.numeric(parts[3])) - 1]
  res <- paste(parts, collapse = ".")
  return(res)
}








#' Get name of workflow(s) from workflow_map.csv
#'
#' @param packageId (character) Package ID with the form 
#' "scope.identifier.revision"
#'
#' @return (character) Name(s) of workflow(s). Returns NULL if no matches are 
#' found.
#' 
#' @details This function references the global "config.environment" variable
#' during workflow look up to facilitate different workflows for different
#' repository environments.
#'
get_workflows <- function(packageId) {
  map <- read.csv("./webapp/workflow_map.csv", na.strings = c("", "NA"))
  packageId <- paste(unlist(strsplit(packageId, "\\."))[1:2], collapse = ".")
  i <- (map$environment == config.environment) & (map$source_id == packageId)
  if (!any(i)) {
    return(NULL)
  }
  res <- map$workflow[i]
  return(res)
}








#' Add a high-level workflow message to the log file
#' 
#' @description Because all messages/warnings/errors are logged to file, 
#' subsequent reading can be a challenge. This function creates a recognizable
#' message format to delineate high level processes from lower level ones.
#'
#' @param x (character) Message
#'  
#' @return (message) Message with the form:
#' \code{----- MMM DD HH:MM:SS |maintainer|:}
#' 
msg <- function(...) {
  x <- list(...)
  x <- paste(unlist(x), collapse = "")
  x <- paste0("\n----- ", format(Sys.time(), "%b %d %H:%M:%S"), 
              " [maintainer]: ", x, "\n")
  message(x)
}








#' Get the next update from the queue (maintainer.sqlite)
#' 
#' @param url (character) The address of the web service endpoint listening for 
#' event notifications.
#' @param filter (character) If "unprocessed", all unprocessed updates are 
#' returned.
#' 
#' @details The queue is an SQLite data base located at 
#' \code{/maintainer/webapp/maintainer.sqlite} and is accessible with HTTP 
#' request methods.
#' 
#' There are separate queues for "staging" and "production" environments of EDI
#'
#' @return A tibble with columns:
#' \item{index}{(integer) Index of item in queue. This is used for removing the 
#' item from the queue.}
#' \item{id}{(character) Data package identifier in the form 
#' "scope.identifier.revision"}
#' 
queue_get_update <- function(url, filter = NULL) {
  if (config.environment == "staging") {
    url <- paste0(url, "/package-s.lternet.edu")
  } else if (config.environment == "production") {
    url <- paste0(url, "//package.lternet.edu")
  }
  if (!is.null(filter)) {
    url <- paste0(url, "?filter=", filter)
  }
  resp <- httr::GET(url)
  if (httr::status_code(resp) == 200) {
    res <- readr::read_csv(
      I(httr::content(resp, as = "text")), 
      col_names = c("index", "id"), 
      show_col_types = FALSE, )
    return(res)
  }
}








#' Check if there are any unprocessed items in the queue (maintainer.sqlite)
#'
#' @param url (character) The address of the web service endpoint listening for 
#' event notifications.
#' @return (logical) TRUE if empty, otherwise FALSE
#'
queue_is_empty <- function(url) {
  res <- is.null(queue_get_update(url, filter = "unprocessed"))
  return(res)
}








#' Remove an update from the queue (maintainer.sqlite)
#'
#' @param url (character) The address of the web service endpoint listening for 
#' event notifications.
#' @param index (integer) Index of item to remove. References the "index" field.
#'
#' @return (logical) Indicates whether the item was successfully removed (i.e.
#' the value in the "processed" field changed from "0" to "1")
#' 
#' @details This function marks the corresponding data package as processed
#' 
queue_remove_update <- function(url, index) {
  # Only the index number is needed to delete an item from the "production" 
  # and "staging" queues (it's the same queue).
  r <- httr::DELETE(
    paste0(url, index))
  if (httr::status_code(r) == 200) {
    return(TRUE)
  } else {
    message("Could not remove item ", index, " from the queue. Devine ",
            "intervention is needed.")
    return(FALSE)
  }
}









#' Send email to a Gmail account
#'
#' @param from (character) Email address
#' @param to (character) Email address
#' @param attachment (character) Attachment file name with full path and file 
#' extension
#' @param smtp.relay (character) SMTP relay
#' @param relay.user (character) Relay user
#' @param relay.user.pass (character) Relay user password
#' @param subject (character) Subject line
#' @param msg (character) Message
#' 
#' @note Currently, only works for Gmail recipients.
#' 
send_email <- function(from, 
                       to, 
                       attachment, 
                       smtp.relay, 
                       relay.user, 
                       relay.user.pass, 
                       subject, 
                       msg) {
  cmd <- paste("/usr/bin/sendemail -f", from, "-t", to, "-a", attachment, "-s", 
               smtp.relay, "-xu", relay.user, "-xp", relay.user.pass, "-u", 
               subject, "-m", msg)
  system(cmd)
}








# Source the workflow configuration variables ---------------------------------

scripts <- list.files("./workflows", full.names = TRUE)
readme <- grepl(".README.md$", scripts)
invisible(sapply(scripts[!readme], source))

# Run the workflow manager ----------------------------------------------------

# Call the top level function that manages all workflows
workflow_manager()
