# Function wrappers for saving and restoring state to/from disk when running
# Shiny locally.
#
# These functions provide a directory to the callback function.
#
# @param id A session ID to save.
# @param callback A callback function that saves state to or restores state from
#   a directory. It must take one argument, \code{stateDir}, which is a
#   directory to which it writes/reads.
redisInterface <- function(id, callback) {
  redisConfig <- getShinyOption("redis")
  if(is.null(redisConfig)){
    stop("No redis config.")
  }else{
    tryCatch({
      do.call(redisConnect, redisConfig)
    },
    error = function(e) {
      stop("Error connecting redis server.")
    }
    )
  }

  callback(id)
  redisClose()
}
