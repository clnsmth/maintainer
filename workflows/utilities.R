#' Are there meaningful differences between EML documents?
#' 
#' @description For discovering changes within a dataset that may have affect downstream processes relying on consistent dataset structure and meaning. This is useful in workflow automation where reporting such changes can expedite trouble shooting and manual intervention.
#'
#' @param newest (xml_document, xml_node) EML of the newest version of a data package, where inputs are returned from \code{api_read_metadata()}.
#' @param previous (xml_document, xml_node) EML of the previous version of a data package, where inputs are returned from \code{api_read_metadata()}.
#' @param return.all (logical) Return all differences? Default is FALSE, i.e. only return meaningful differences. Meaningful differences do not include elements expected to change between versions (e.g. number of rows, file size, temporal coverage).
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
#' @export
#'
#' @examples
#' 
#' # Return only "meaningful" differences (default behavior)
#' compare_eml(
#'   newest = api_read_metadata("knb-lter-hfr.118.32"),
#'   previous = api_read_metadata("knb-lter-hfr.118.31"))
#'   
#' # Return all differences
#' compare_eml(
#'   newest = api_read_metadata("knb-lter-hfr.118.32"),
#'   previous = api_read_metadata("knb-lter-hfr.118.31"),
#'   return.all = TRUE)
#' 
compare_eml <- function(newest, 
                        previous,
                        return.all = FALSE) {
  
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








#' Collapse EML node to string then compare
#'
#' @param newest (xml_document, xml_node) Newest version of an EML document
#' @param previous (xml_document, xml_node) Previous version of an EML document
#' @param xpath (character) xpath of node to compare
#'
#' @return (character) xpath of node if \code{newest} and \code{previous} differ, otherwise NULL
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








#' Convert missing value codes to NA
#'
#' @param v Vector of values
#' @param code (character) Missing value code
#' @param type (character) Type (class) \code{v} should be. Supported types are: "character", "numeric", "datetime"
#'
#' @return Vector of values with \code{code} replaced by NA in the class of \code{type}
#'
convert_missing_value <- function(v, code, type) {
  if (type == "character") {
    res <- stringr::str_replace_all(as.character(v), paste(code, collapse = "|"), NA_character_)
  } else if (type == "numeric") {
    res <- stringr::str_replace_all(as.character(v), paste(code, collapse = "|"), NA_character_)
    res <- as.numeric(res)
  } else if (type == "datetime") {
    # TODO: Parse datetime according to date time format specifier
    res <- v
  }
  return(res)
}








#' Delete item from ecocomDP-maintainer queue
#'
#' @param index (integer) Index of item to remove
#' @param id (character) Data package identifier, corresponding with \code{index}, to remove
#'
#' @return (logical) Indicates whether the item was successfully removed
#' 
delete_from_queue <- function(index, id) {
  
  # Only the index number is needed to delete an item from the "production" 
  # and "staging" queues (it's the same queue).
  
  r <- httr::DELETE(
    paste0("https://regan.edirepository.org/ecocom-listener/", index))
  
  if (httr::status_code(r) == 200) {
    message(id, " has been deleted from the queue")
    return(TRUE)
  } else {
    message(id, " could not be deleted from the queue. Devine intervention ",
            "is required.")
    return(FALSE)
  }
  
}








#' Get next item from the processing queue
#' 
#' @param filter (character) If "unprocessed" a full list of unprocessed items 
#' are returned.
#' 
#' @details The queue is an SQLite data base located at 
#' \code{/maintainer/webapp/maintainer.sqlite} and is accessible with HTTP 
#' request methods.
#' 
#' There are separate queues for "staging" and "production" tiers of EDI
#'
#' @return A tibble with columns:
#' \item{index}{(integer) Index of item in queue. This is used for removing the 
#' item from the queue.}
#' \item{id}{(character) Data package identifier in the form 
#' "scope.identifier.revision"}
#' 
get_from_queue <- function(filter = NULL) {
  if (config.environment == "staging") {
    url <- "https://regan.edirepository.org/ecocom-listener/package-s.lternet.edu"
  } else if (config.environment == "production") {
    url <- "https://regan.edirepository.org/ecocom-listener/package.lternet.edu"
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








#' Get name of derived from map.csv
#'
#' @param packageId (character) Package ID with the form "scope.identifier.revision"
#'
#' @return (character) \code{packageId} of derived
#'
get_derived <- function(id) {
  map <- read.csv("./webapp/map.csv", na.strings = c("", "NA"))
  id <- paste(unlist(strsplit(id, "\\."))[1:2], collapse = ".")
  i <- (map$tier == config.environment) & (map$source == id)
  scope <- unlist(strsplit(map$derived[i], "\\."))[1]
  identifier <- unlist(strsplit(map$derived[i], "\\."))[2]
  revision <- EDIutils::list_data_package_revisions(
    scope,
    identifier,
    filter = "newest", 
    tier = config.environment)
  res <- paste(c(scope, identifier, revision), collapse = ".")
  return(res)
}








#' Get previous data package version
#'
#' @param package.id (character) Data package identifier
#'
#' @return (character) Previous data package version
#' 
#' @details Supports repository specific methods.
#'
#' @examples
#' \dontrun{
#' #' get_previous_data_package_version("edi.100.2")
#' }
#' 
get_previous_version <- function(package.id) {
  
  # Load Global Environment config --------------------------------------------
  
  if (exists("config.environment", envir = .GlobalEnv)) {
    environment <- get("config.environment", envir = .GlobalEnv)
  } else {
    environment <- "production"
  }
  
  # Repository specific methods -----------------------------------------------
  
  parts <- unlist(stringr::str_split(package.id, "\\."))
  
  vers <- suppressMessages(
    EDIutils::list_data_package_revisions(
      scope = parts[1],
      identifier = parts[2],
      tier = environment))
  
  if (length(vers) == 1) {
    return("0")
  }
  
  parts[3] <- vers[which(vers == as.numeric(parts[3])) - 1]
  res <- paste(parts, collapse = ".")
  return(res)
  
}








#' Get name of workflow from map.csv
#'
#' @param packageId (character) Package ID with the form "scope.identifier.revision"
#'
#' @return (character) Name of workflow
#'
get_workflow <- function(id) {
  map <- read.csv("./webapp/map.csv", na.strings = c("", "NA"))
  id <- paste(unlist(strsplit(id, "\\."))[1:2], collapse = ".")
  i <- (map$tier == config.environment) & (map$source == id)
  res <- map$workflow[i]
  return(res)
}







#' Check if earlier unprocessed versions of a data package are in the ecocomDP-maintainer queue
#'
#' The presence of such items may indicate the integrity of a data package series is compromised and processing should be halted until the issue is fixed.
#'
#' @param package.id 
#'
#' @return (logical) TRUE if earlier versions of a data package are found
#' @export
#'
#' @examples
has_unprocessed_versions <- function(package.id) {
  # Identifier and revision of package.id
  id <- stringr::str_remove(package.id, "\\.[:digit:]*$")
  rev <- stringr::str_extract(package.id, "(?<=\\.)[:digit:]*$")
  
  # Identifiers and revisions of all queued items
  queue <- get_from_queue(filter = "unprocessed")$id
  if (is.null(queue)) {
    return(FALSE)
  }
  ids <- stringr::str_remove(queue, "\\.[:digit:]*$")
  revs <- stringr::str_extract(queue, "(?<=\\.)[:digit:]*$")
  
  # Any earlier versions that are unprocessed?
  i <- rev > revs[ids %in% id]
  if (any(i)) {
    message("Unprocessed earlier versions of ", id, " found in the queue. ",
            "These earlier versions (", queue[i], ") may need to ",
            "be processed before ", package.id, ". Devine intervention is ",
            "required.")
    return(TRUE)
  } else {
    return(FALSE)
  }
  
}








#' Increment data package version number
#'
#' @param package.id (character) Data package identifier
#'
#' @return (character) Package identifier with version number incremented by 1.
#' 
#' @details Supports repository specific methods.
#'
#' @examples
#' increment_package_version("edi.100.1")
#' 
increment_package_version <- function(package.id) {
  parts <- unlist(stringr::str_split(package.id, "\\."))
  parts[3] <- as.character(as.numeric(parts[3]) + 1)
  parts <- paste(parts, collapse = ".")
  return(parts)
}








#' Is empty nodeset?
#'
#' @param nodeset (xml_nodeset) Any nodeset returned by the xml2 library
#' 
#' @return (logical) TRUE if nodeset length = 0
#' 
is_empty_nodeset <- function(nodeset) {
  res <- length(nodeset) == 0
  return(res)
}








#' Check if the processing queue is empty
#'
#' @return (logical) TRUE if empty, otherwise FALSE
#'
queue_is_empty <- function() {
  res <- is.null(get_from_queue(filter = "unprocessed"))
  return(res)
}








#' Send email
#'
#' @param from (character) Email address
#' @param to (character) Email address
#' @param attachment (character) Attachment file name with full path and file extension
#' @param smtp.relay (character) SMTP relay
#' @param relay.user (character) Relay user
#' @param relay.user.pass (character) Relay user password
#' @param subject (character) Subject line
#' @param msg (character) Message
#'
#' @details Works for Linux. May not work for other OS.
#' 
#' @export
#' 
send_email <- function(from, to, attachment, smtp.relay, relay.user, 
                       relay.user.pass, subject, msg) {
  
  cmd <- paste("/usr/bin/sendemail -f", from, "-t", to, "-a", attachment, "-s", 
               smtp.relay, "-xu", relay.user, "-xp", relay.user.pass, "-u", 
               subject, "-m", msg)
  
  system(cmd)
}








#' Upload data package to a repository
#'
#' @description A wrapper function to repository specific upload methods.
#'
#' @param path (character) Publicly accessible server directory from which L1 tables, scripts, and metadata can be downloaded
#' @param package.id (character) Identifier of data package to be uploaded
#' @param user.id (character) User identifier within a specified \code{repository}. This controls editing access in some \code{repository}.
#' @param user.pass (character) Password associated with \code{user.id} for repository upload.
#'
#' @return (character) Evaluation/upload summary
#' @export
#'
#' @examples
#' 
upload_to_repository <- function(path,
                                 package.id,
                                 user.id,
                                 user.pass) {
  
  # Load Global Environment config --------------------------------------------
  
  if (exists("config.environment", envir = .GlobalEnv)) {
    environment <- get("config.environment", envir = .GlobalEnv)
  } else {
    environment <- "production"
  }
  
  # Upload --------------------------------------------------------------------
  
  EDIutils::api_update_data_package(
    path = path, 
    package.id = package.id, 
    environment = environment, 
    user.id = user.id, 
    user.pass = user.pass,
    affiliation = repository)
  
  # TODO Add a create method here for new data packages
  
}