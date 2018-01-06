#' read FARS files
#'
#' read FARS data (US National Highway Traffic Safety Administration's Fatality Analysis Reporting System)from file
#'
#' @param filename , name of file to be read
#'
#' @return a data.frame with the data of file
#'
#' @details
#' it is necessary to download the fars files before, this functions read the files from the working directory, if not an error will be thrown
#' Throws an error if wrong file name is provided
#'
#'
#' @examples
#' library(dplyr)
#' library(readr)
#' library(tidyr)
#' f13path<-system.file("inst", extdata", "accident_2013.csv.bz2")
#' file.copy(from=c(f13path),to=getwd())
#' fars_read('accident_2013.csv')
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' make the file name for the FARs file
#'
#' using the year complete the name for the fars file to be read
#'
#' @param year , year
#'
#' @return a string with the name of file
#'
#' @details
#' there is not directory, only name of file, so the default directory is the working directory
#' Throws an error if non numeric value is provided as Year
#'
#' @examples
#' make_filename(2013)
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#'read FARS files
#'
#' read FARS data from file
#'
#' @param years , a list with all years to be displayed
#'
#' @return a data.frame with the month and the total of accidents by year
#'
#' @details
#' it is necessary to download the fars files before, this functions read the files from the working directory, if not a warning
#' Throws a warning of "invalid year:"
#'    if a numeric other than what is available in data is provided as year.
#'
#'
#' @examples
#' library(dplyr)
#' library(readr)
#' library(tidyr)
#' fars_read_years(c(2013))
#'
#' @export
#'
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' total FARS accidents by month and year
#'
#' read FARS data from file and count the number of accidents group_by year and month
#'
#' @param years , a list with all years to be displayed
#'
#' @return a data.frame with the month and the total of accidents by year
#'
#' @details
#' it is necessary to download the fars files before, this functions read the files from the working directory, if not a warning will be thrown
#' Throws a warning of "invalid year:" if a numeric other than
#'   what is available is provided as years
#'
#'
#' @examples
#' library(dplyr)
#' library(readr)
#' library(tidyr)
#' fars_summarize_years(c(2013))
#'
#' @export
#'
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' map FARS data by state and year
#'
#' map accidents by state and year with US National Highway Traffic Safety Administration's Fatality Analysis Reporting System (FARS)
#'
#' @param state.num, state from which the data will be displayed (number 1- 56)
#' @param year , year from which the data will be displayed
#'
#' @return This function returns a graphical object.
#'
#' @details
#' it is necessary to download the fars files before, this functions read the files from the working directory, if not an error will be thrown
#' Throws an error of "invalid State Number:" for invalid state number.
#'
#' @examples
#' library(dplyr)
#' library(readr)
#' library(tidyr)
#' library(maps)
#' library(graphics)
#' f13path<-system.file("extdata", "accident_2013.csv.bz2", package = "farsfuncs")
#' file.copy(from=c(f13path),to=getwd())
#' fars_map_state(1, 2013)
#'
#' @importFrom graphics points
#'
#' @export
#'
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
