#file.create("/home/pasta/maintainer/webapp/trigger_old.txt")
#wd <- getwd()
#libs <- rownames(installed.packages())
#res <- c(wd, libs)
pop_queue <- function(filter = NULL) {
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
config.environment <- "staging"
niq <- pop_queue()
res <- niq$id
writeLines(text = res, con = "/home/pasta/maintainer/webapp/test_result.txt")
