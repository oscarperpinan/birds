
setMethod('show',
          signature = "bird",
          definition = function(object)
          {
            cat("Name: ", object@name, "\n")
            cat("Latitude: ", summary(object@lat), "\n")
            cat("Longitude: ", summary(object@lon), "\n")
            cat("Speed: ", summary(object@speed), "\n")
          })


setMethod('show',
          signature = "flock",
          definition = function(object)
          {
            cat("Flock Name: ", object@name, "\n")
            N <- length(object@members)
            lapply(seq_len(N), function(i)
            {
              cat("Bird #", i, "\n")
              print(object@members[[i]])
            })
          })


setGeneric("as.data.frame")


setMethod("as.data.frame",
          signature = "bird",
          definition = function(x, ...)
          {
            data.frame(
              name = x@name,
              lat = x@lat,
              lon = x@lon,
              alt = x@alt,
              speed = x@speed,
              time = x@time)
          })

setMethod("as.data.frame",
          signature = "flock",
          definition = function(x, ...)
          {
            dfs <- lapply(x@members, as.data.frame)
            dfs <- do.call(rbind, dfs)
            dfs$flock_name <- x@name
            dfs
          })

setGeneric("xyplot")

setMethod('xyplot',
          signature = "bird",
          definition = function(x, data = NULL,
                                mode = "latlon", ...)
          {
            df <- as.data.frame(x)
            switch(mode,
                   lontime = xyplot(lon ~ time, data = df, ...),
                   lattime = xyplot(lat ~ time, data = df, ...),
                   latlon = xyplot(lat ~ lon, data = df, ...),
                   speed = xyplot(speed ~ time, data = df, ...)
            )
          })    


setMethod('xyplot',
          signature = "flock",
          definition = function(x, data = NULL, ...)
          {
            df <- as.data.frame(x)
            xyplot(lon ~ lat,
                   group = name,
                   data = df,
                   auto.key = list(space = "right"))
          })
