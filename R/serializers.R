# For most types of values, simply return the value unchanged.
serializerDefault <- function(value, stateDir, id) {
  value
}

readBinaryFile<-function(filepath){
  size = file.info(filepath)$size
  readBin(filepath, raw(), size)
}

serializerFileInput <- function(value, stateDir = NULL, id = NULL) {
  # File inputs can be serialized only if there's a stateDir
  store <- getShinyOption("bookmarkStore", default = "")
  if (is.null(stateDir) && store != 'redis') {
    return(serializerUnserializable())
  }

  # value is a data frame. When persisting files, we need to copy the file to
  # the persistent dir and then strip the original path before saving.
  serializerLocal<-function(stateDir, value){
    newpaths <- file.path(stateDir, basename(value$datapath))
    file.copy(value$datapath, newpaths, overwrite = TRUE)
    value$datapath <- basename(newpaths)

    value
  }

  serializerRedis<-function(id, value){
    saveRedisFile <- function(datapath){
      filename <- basename(datapath)
      content <- readBinaryFile(datapath)
      redisHSet(id, filename, content)
    }
    filename <- basename(value$datapath)
    redisProgress(value$datapath, saveRedisFile, "Save files to redis.")
    value$datapath <- filename

    value
  }

  storeFileInRedis <- getShinyOption("storeFileInRedis", default = TRUE)
  if (store == 'redis' && storeFileInRedis == TRUE) {
    #redisInterface(id, serializerRedis, value=value)
    serializerRedis(id, value)
  } else {
    serializerLocal(stateDir, value)
  }
}


# Return a sentinel value that represents "unserializable". This is applied to
# for example, passwords and actionButtons.
serializerUnserializable <- function(value, stateDir) {
  structure(
    list(),
    serializable = FALSE
  )
}

# Is this an "unserializable" sentinel value?
isUnserializable <- function(x) {
  identical(
    attr(x, "serializable", exact = TRUE),
    FALSE
  )
}


# Given a reactiveValues object and optional directory for saving state, apply
# serializer function to each of the values, and return a list of the returned
# values. This function passes stateDir to the serializer functions, so if
# stateDir is non-NULL, it can have a side effect of writing values to disk (in
# stateDir).
serializeReactiveValues <- function(values, exclude, stateDir = NULL, id = NULL) {
  impl <- .subset2(values, "impl")

  # Get named list where keys and values are the names of inputs; we'll retrieve
  # actual values later.
  vals <- isolate(impl$names())
  vals <- setdiff(vals, exclude)
  names(vals) <- vals

  # Get values and apply serializer functions
  vals <- lapply(vals, function(name) {
    val <- impl$get(name)

    # Get the serializer function for this input value. If none specified, use
    # the default.
    serializer <- impl$getMeta(name, "shiny.serializer")
    if (is.null(serializer))
      serializer <- serializerDefault

    # Apply serializer function.
    serializer(val, stateDir, id)
  })

  # Filter out any values that were marked as unserializable.
  vals <- Filter(Negate(isUnserializable), vals)
  vals
}
