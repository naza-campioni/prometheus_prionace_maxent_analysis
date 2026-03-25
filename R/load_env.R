load_env <- function(path) {
  # load env covariates
  env.dir <- file.path(path)
  
  chl <- rast(file.path(env.dir,"chl.asc"))
  cur <- rast(file.path(env.dir,"cur.asc"))
  depth <- rast(file.path(env.dir,"depth.asc"))
  dox <- rast(file.path(env.dir,"dox.asc"))
  nppv <- rast(file.path(env.dir,"nppv.asc"))
  ph <- rast(file.path(env.dir,"ph.asc"))
  so <- rast(file.path(env.dir,"so.asc"))
  sst <- rast(file.path(env.dir,"sst.asc"))
  
  # stack
  env <- c(chl,cur,depth,dox,nppv,ph,so,sst)
  names(env) <- c(
    "chl","cur","depth","dox",
    "nppv","ph","so","sst"
  )
  
  return(env)
  
}
  
