setClass('bird',
         slots = c(
           name = 'character',
           lat = 'numeric',
           lon = 'numeric',
           alt = 'numeric',
           speed = 'numeric',
           time = 'POSIXct')
)

readBird <- function(name, path)
{
  csvFile <- file.path(path, paste0(name, ".csv"))

  vals <- read.csv(csvFile)

  new('bird',
      name = name,
      lat = vals$latitude,
      lon = vals$longitude,
      alt = vals$altitude,
      speed = vals$speed_2d,
      time = as.POSIXct(vals$date_time)
  )
}


valida <- function (object) {
  if (any(sapply(object@members,
                 function(x) !is(x, "bird"))))
    stop("only bird objects are accepted.")
  return(TRUE)
}

setClass("flock",
         slots = c(
           name = "character",
           members = "list"),
         validity = valida
)

newFlock <- function(name, ...){
  birds <- list(...)
  new("flock",
      name = name,
      members = birds)
}
