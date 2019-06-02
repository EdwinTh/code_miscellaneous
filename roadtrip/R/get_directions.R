#' Import roadtrip data
#'
#' Import the data for plotting the roadtrip. Checks some simple assumptions.
#' @param path Path to the file.
#' @param delim The delimeter used.
#' @return A tbl of the imported data with two columns. The query to be
#' executed and a boolean subdirection. Indicating it was not a stop but just
#' a subdirection included so Google Maps does not take the shortes path.
#' @export
import_roadtrip <- function(path,
                            delim = ';') {
  road_trip <- readr::read_delim(path, delim = delim)
  stopifnot(colnames(road_trip) == c("query", "subdirection"))
  stopifnot(is.logical(road_trip$subdirection))
  road_trip
}


#' Get the directions
#'
#' Obtain the full road trip from the Google API using `ggmap::route()`.
#' @param road_trip A tbl as outputted by  `import_roadtrip`.
#' @param slack The number of seconds to wait between each API call.
#' @return A tbl with all the coordinates for plotting the route using `ggmap`.
#' The column `stay` is added, which is `TRUE` for all records in
#' `road_trip` for which subdirection is not `TRUE`.
#' @export
get_directions <- function(road_trip,
                           slack = 10) {
  road_trip_from_to <- make_from_to(road_trip)
  coordinates <- vector("list", nrow(road_trip_from_to))

  if (any(purrr::map_lgl(coordinates, is.null)) {
    coordinates <- get_coordinates(road_trip_from_to, coordinates)
  }

    for(i in 1:nr_of_calls) {
      if (!is.null(coordinates[[i]])) next
      result <- tryCatch(
        call_google_api(road_trip_from_to[i, ],i == nr_of_calls),
        error = function(e) NULL
      )
      if (is.null(return)){
        break
      } else {
        coordinates[[i]] <- result
      }
      print(paste0(i," out of ", nr_of_calls, " calls made."))
      Sys.sleep(slack)
    }
  }
  coordinates
}



# Make a from to for each part of the trip, from the single directions.
make_from_to <- function(road_trip) {
  query <- road_trip$query
  data.frame(
    from = query[1:(length(query)-1)],
    to   = query[2:(length(query))],
    stay = is.na(road_trip$subdirection)[1:(length(query)-1)],
    stringsAsFactors = FALSE
  )
}

# Call the google API for a part of the route
call_google_api <- function(x,
                            last_call) {
  call_return <- ggmap::route(x$from,
                              x$to,
                              structure = "route")
  call_return$stay <- NA
  call_return$stay[1] <- x$stay
  if (last_call) {
    call_return$stay[nrow(call_return)] <- TRUE
  } else {
    # remove the last line, this will be the first line of the next call
    call_return <- call_return[-nrow(call_return), ]
  }
  call_return
}

aus_list <- get_directions(road_trip, slack = 5)
