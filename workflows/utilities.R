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
  queue <- queue_get_update(filter = "unprocessed")$id
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
queue_get_update <- function(filter = NULL) {
  if (config.environment == "staging") {
    url <- "https://regan.edirepository.org/maintainer/package-s.lternet.edu"
  } else if (config.environment == "production") {
    url <- "https://regan.edirepository.org/maintainer/package.lternet.edu"
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
#' @return (logical) TRUE if empty, otherwise FALSE
#'
queue_is_empty <- function() {
  res <- is.null(queue_get_update(filter = "unprocessed"))
  return(res)
}








#' Remove an update from the queue (maintainer.sqlite)
#'
#' @param index (integer) Index of item to remove. References the "index" field.
#'
#' @return (logical) Indicates whether the item was successfully removed (i.e.
#' the value in the "processed" field changed from "0" to "1")
#' 
#' @details This function marks the corresponding data package as processed
#' 
queue_remove_update <- function(index) {
  # Only the index number is needed to delete an item from the "production" 
  # and "staging" queues (it's the same queue).
  r <- httr::DELETE(
    paste0("https://regan.edirepository.org/maintainer/", index))
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








#' Run the maintainer
#' 
#' @description Run a remotely deployed maintainer with its current 
#' state/configuration. This function is helpful when a manual reboot of the 
#' maintainer is needed to resume processing, possibly after a workflow error 
#' and subsequent fix.
#' 
#' @details This function randomly selects a data package identifier from the 
#' repository environment specified by the global variable 
#' \code{config.environment} and submits an HTTP POST request to the listener. 
#' This random package identifier is added to the queue, with the \code{env} 
#' field of the maintainer.sqlite database set to "localhost", which is 
#' recognized as being a "test" post and not an actual update, thus having not 
#' effect on the list of items to process other than cluttering the database 
#' with some extraneous items.
#'
run_maintainer <- function() {
  idle <- TRUE
  i <- 1
  while (idle) {
    identifiers <- suppressMessages(EDIutils::api_list_data_package_identifiers("edi", config.environment))
    identifier <- sample(identifiers, 1)
    revisions <- suppressMessages(EDIutils::api_list_data_package_revisions("edi", identifier, environment = config.environment))
    revision <- sample(revisions, 1)
    packageId <- paste(c("edi", identifier, revision), collapse = ".")
    resp <- httr::POST(
      url = "https://regan.edirepository.org/maintainer", 
      body = packageId)
    idle <- httr::status_code(resp) != 200
    i <- i + 1
    if (i == 3) {
      idle <- FALSE
      message("Failed on 3 consequtive attempts. Taking a break.")
    }
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

