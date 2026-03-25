calculate_vif <- function(env) {
  # Now we will remove those variables that have a high VIF
  env.vif <- usdm::vifstep(env)
  env.rem <- env.vif@excluded
  # env.rem <- setdiff(env.vif@excluded, "chl")
  # env.rem <- union(env.rem, "nppv")
  env <- env[[!(names(env) %in% env.rem)]]
  
  return(list(env = env, vif = env.vif, rem = env.rem))
}