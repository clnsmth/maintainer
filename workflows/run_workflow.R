#' Logic controlling workflow execution
#'
#' @param workflow (character) Name of workflow to execute for a given source
#' data package. The \code{workflow} value is listed in the "workflow" column
#' of the workflow map located at: /maintainer/webapp/workflow_map.csv
#' @param source_id (character) Name of the input data source to 
#' \code{workflow}
#' 
#' @details This function is called by the workflow_manager() located at:
#' /maintainer/webapp/workflow_manager.R
#'
run_workflow <- function(workflow, source_id) {
  
  if (workflow == "update_ecocomDP") {
    
    update_ecocomDP(
      source_id = source_id,
      path = "./temp", 
      url = config.url.dwnld, 
      user_id = config.edi.user,
      user_pass = config.edi.pass,
      environment = config.environment)
    
  } else if (workflow == "update_DwCA") {
    
    update_DwCA(
      source_id = source_id,
      path = "./temp",
      url = config.url.dwnld,
      user_id = config.edi.user,
      user_pass = config.edi.pass,
      environment = config.environment)
    
  }
  
}
