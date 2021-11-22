# This function is called by the workflow_manager() and is expecting the 
# workflow parameter. Any contents within the function can be customized.

run_workflow <- function(workflow, new_pkg_id) {
  
  if (workflow == "update_L1") {

    update_L1(
      id.L0.newest = new_pkg_id,
      path = config.path, 
      url = config.www, 
      user.id = config.user.id,
      user.pass = config.user.pass)
    
  } else if (workflow == "update_L2_dwca") {
    
    update_L2_dwca(
      id.L1.newest = new_pkg_id,
      core.name = "event",
      path = config.path,
      url = config.www,
      user.id = config.user.id,
      user.pass = config.user.pass)
    
  }
  
}
