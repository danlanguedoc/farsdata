#' @title fars_read
#'
#' @description Function that reads in the fars data
#'
#' @param filename A character string giving the path to the file to be read
#'
#' @return if the file exists at the path, it returns a data frame
#'     with the parsed data using the column types inferred
#'     from the first 1000 rows of data. If the file name does not exist
#'     the function stops with a message
#'
#' @details
#' The function uses read_csv to import the data and then converts it to a dataframe
#'     using tbl_df.  read_csv is silent through the use of suppressMessages
#'
#'     stops processing if the file does not exist at the path
#'
#'
#' @examples
#' \dontrun{
#' fars_read("accident_2013.csv.bz2")
#' }
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @export
fars_read <- function(filename) {
##  if(!file.exists(filename))
##    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
##    readr::read_csv(filename, progress = FALSE)
      readr::read_csv(system.file("extdata", filename, package="farsdata"), progress = FALSE)

      })
  dplyr::tbl_df(data)
}

#' @title make_filename
#'
#' @description create a data file name given an input year
#'
#' @param year param with the year information
#'     will be coerced to a integer
#'
#' @return string of the form "accident_yyyy.csv.bz2" where yyyy repersents the value
#'     passed in the "year" param
#'
#' @details
#' The function ettempts to explicitly convert the year param to
#'     an integer.  This may return NA resulting in "accident_NA.csv.bz2"
#'     as the returned file name as well as a warning message
#'
#'    #'
#' @examples
#' \dontrun{
#' make_filename("2013")
#' make_filename("2013a")
#' }
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' @title fars_read_years
#'
#' @description generates the year and month data for the years in question
#'
#' @param years list of years to return the data
#'
#' @return returns a list of dataframes that contain year and month information.
#'     The length of the list is the same as the number of items in the
#'     input parameter years.  Invalid years or years with no data are returned
#'     as NULL with an error message
#'
#' @details
#' The function loops over all the elements of the "years" and extracts a data frame
#'     consisting of the "MONTH" column from the original data file and
#'     a computed "year" column based on the current element of the "years" param
#'
#' @examples
#' \dontrun{
#' fars_read_years(c("2013","2014","2015"))
#' fars_read_years(c(2013,2014))
#' }
#'
#' @importFrom dplyr mutate select
#' @importFrom  magrittr %>%
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate_(dat, year = ~year) %>%
        dplyr::select_("MONTH", "year")
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' @title fars_summarize_years
#'
#' @description generates 12-by-n monthly summary data for the n-years in question
#'
#' @param years list of years to return the data
#'
#' @return a dataframe that contain up to 12 rows of data with one column
#'     of summary data per input year
#'
#' @details
#' The function binds all the rows returned in the yearly dataframes by
#'     fars_read_years and computes the monthly summaries and
#'     converts the summary into a 12 row monthly dataframe with
#'     one column per row
#'
#'     the dataframe may have less than 12 rows if the summary data doesn't
#'     exist for any particular row
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(c("2013","2014","2015"))
#' fars_summarize_years(c(2013,2014))
#' }
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by_("year", "MONTH") %>%
    dplyr::summarize_(n = ~n()) %>%
    tidyr::spread_("year", "n")
}

#' @title fars_map_state
#'
#' @description returns a plot of the fatalities in a particular state in
#'     particular year
#'
#' @param state.num state number that corresponds to a state in the FARS dataset
#' @param year the year in question
#'
#' @return a plot of the fatalities in a particular state in
#'     particular year
#'
#' @details
#' The function plots a data point for each fatality in the dataset for the particular state
#'
#'     The LONGITUDE and LATITUDE values are cleaned up before being passed to the map routine
#'     with invalid entires being marked as NA.  The map routine then removes NA values
#'
#'     The function will return an error if the state.num is not in the dataset
#'     It will also return a message if there are no value
#'
#'
#' @examples
#' \dontrun{
#' fars_map_state(1,2013)
#' fars_map_state(5,2014)
#' }
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter_(data, ~STATE == state.num)
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
