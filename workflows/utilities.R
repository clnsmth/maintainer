#' Get derived data package identifier from workflow_map.csv
#'
#' @param packageId (character) Package ID with the form 
#' "scope.identifier.revision"
#'
#' @return (character) \code{package.id} of derived data package(s)
#'
get_derived_id <- function(packageId) {
  map <- read.csv("./webapp/workflow_map.csv", na.strings = c("", "NA"))
  packageId <- paste(unlist(strsplit(packageId, "\\."))[1:2], collapse = ".")
  i <- (map$environment == config.environment) & (map$source_id == packageId)
  scope <- unlist(strsplit(map$derived_id[i], "\\."))[1]
  identifier <- unlist(strsplit(map$derived_id[i], "\\."))[2]
  revision <- suppressMessages(
    EDIutils::api_list_data_package_revisions(
      scope = scope,
      identifier = identifier,
      filter = "newest", 
      environment = config.environment))
  res <- paste(c(scope, identifier, revision), collapse = ".")
  if (!any(i)) {
    return(NULL)
  }
  return(res)
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








#' Increment data package version number
#'
#' @param packageId (character) Package ID with the form 
#' "scope.identifier.revision"
#'
#' @return (character) Package identifier with version number incremented by 1.
#' 
increment_package_version <- function(packageId) {
  parts <- unlist(stringr::str_split(packageId, "\\."))
  parts[3] <- as.character(as.numeric(parts[3]) + 1)
  parts <- paste(parts, collapse = ".")
  return(parts)
}








#' Delete record from queue (maintainer.sqlite)
#'
#' @param path (character) Path to the maintainer.sqlite
#' @param index (numeric) Index of record to which \code{value} will be applied
#'
#' @return (numeric) A value of 1, if successful
#' 
queue_delete <- function(path = "./webapp/maintainer.sqlite",
                         index) {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), path)
  statement <- paste0("DELETE from events WHERE `index`=", index)
  res <- RSQLite::dbExecute(con, statement)
  RSQLite::dbDisconnect(con)
  return(res)
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








#' Manually insert a record into queue (maintainer.sqlite)
#'
#' @param path (character) Path to the maintainer.sqlite
#' @param pid (character) Package identifier with form 
#' "scope.identifier.revision"
#' @param env (character) Repository environment from which \code{pid} 
#' originates
#' @param dt (character) Date time of insert of the format 
#' "YYYY-MM-DD hh:mm:ss.ssssss"
#' @param processed (numeric) Indication of whether the record has been 
#' processed (1 = TRUE, 0 = FALSE)
#'
#' @return (numeric) A value of 1, if successful
#'
queue_insert <- function(path = "./webapp/maintainer.sqlite", 
                         pid, 
                         env, 
                         dt = paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), ".000000"), 
                         processed) {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), path)
  next_record <- as.numeric(
    RSQLite::dbGetQuery(con, "SELECT max(`index`) FROM events")) + 1
  statement <- paste0("INSERT INTO events (`index`, pid, env, dt, processed) VALUES(",
                      next_record, ", '", pid, "', '", env, "', ", 
                      "'", dt, "', ", processed, ")")
  res <- RSQLite::dbExecute(con, statement)
  RSQLite::dbDisconnect(con)
  return(res)
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








#' Return all fields and records in the queue (maintainer.sqlite)
#'
#' @param path (character) Path to the maintainer.sqlite
#'
#' @return (data.frame) The queue
#' 
queue_select_all <- function(path = "./webapp/maintainer.sqlite") {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), path)
  res <- RSQLite::dbGetQuery(con, 'SELECT * FROM events')
  RSQLite::dbDisconnect(con)
  return(res)
}







#' Update a value in the queue (maintainer.sqlite)
#'
#' @param path (character) Path to the maintainer.sqlite
#' @param index (numeric) Index of record to which \code{value} will be applied
#' @param field (character) Name of field to which \code{value} will be applied
#' @param value (character or numeric) Value to set
#'
#' @return (numeric) A value of 1, if successful
#'
queue_update <- function(path = "./webapp/maintainer.sqlite", 
                         index, 
                         field, 
                         value) {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), path)
  statement <- paste0("UPDATE events SET ", field, "=", value, " WHERE ",
                      "`index`=", index)
  res <- RSQLite::dbExecute(con, statement)
  RSQLite::dbDisconnect(con)
  return(res)
}








#' Imitate an event notification sent from the EDI Data Repository
#' 
#' @param url (character) The address of the web service endpoint listening for 
#' event notifications.
#'
#' @description This function is good for testing a remotely deployed 
#' maintainer or restarting one that has stalled out after an error.  It 
#' simulates an event notification sent from the EDI Data Repository, which is
#' accepted into the queue (maintainer.sqlite), and which runs the workflow 
#' manager.
#' 
#' @details This function randomly selects a data package identifier from the 
#' repository environment specified by the global variable 
#' \code{config.environment} and submits an HTTP POST request to the listener. 
#' This random package identifier is added to the queue, with the \code{env} 
#' field of the maintainer.sqlite database set to "localhost", which is 
#' recognized as being a "test" POST and not an actual update, thus having not 
#' effect on the list of items to process other than cluttering the database 
#' with some extraneous items. These can be manually removed with the 
#' \code{queue_*()} helper functions.
#'
send_mock_notification <- function(url) {
  idle <- TRUE
  i <- 1
  while (idle) {
    identifiers <- suppressMessages(EDIutils::api_list_data_package_identifiers("edi", config.environment))
    identifier <- sample(identifiers, 1)
    revisions <- suppressMessages(EDIutils::api_list_data_package_revisions("edi", identifier, environment = config.environment))
    revision <- sample(revisions, 1)
    packageId <- paste(c("edi", identifier, revision), collapse = ".")
    resp <- httr::POST(
      url = url, 
      body = packageId)
    idle <- httr::status_code(resp) != 200
    i <- i + 1
    if (i == 3) {
      idle <- FALSE
      message("Failed on 3 consequtive attempts. Taking a break.")
    }
  }
}








#' Time out a function that's taking too long ...
#'
#' @param expr An expression to be executed
#' @param limit Seconds to wait before timing out
#'
#' @return Return from \code{expr}, or an error if \code{limit} is exceeded
#' 
with_timeout <- function(expr, limit) {
  expr_txt <- deparse(substitute(expr))
  setTimeLimit(cpu = limit, elapsed = limit, transient = TRUE)
  on.exit(setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE))
  tryCatch({
    pf <- parent.frame()
    eval(parse(text = expr_txt), envir = pf)
  }, error = function(e) {
    stop(e$message)
  })
}

