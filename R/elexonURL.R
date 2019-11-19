#' @title elexonURL
#'
#'@name elexonURL
#'
#' @description This function gives either a single URL or many URLs that can be used to download csv files manually.
#'    Please note: it does not matter if BMRS requires the dates to be in a different format to "yyyy-mm-dd".
#'    The Relexon package will take care of this. Just enter the dates in the usual format!
#'
#'
#'
#' @param dataset The dataset you are pulling from BMRS/Elexon.
#' @param key Your personal scripting key from elexon. Find out more at https://www.elexonportal.co.uk
#' @param from This is the start date/datetime of the dataset
#' @param to This is the end date/datetime of the dataset
#' @param test This is set to FALSE by default. Set this argument to TRUE if you want to use the test version of the API.
#'
#' @examples
#' \dontrun{
#' elexonURL(
#' "HHFUEL",
#' key = "948ghmgpe",
#' from  = "2018-01-01",
#' to = "2018-01-05",
#' test = TRUE
#' )
#' }
#'
#'
#' @export
#'

elexonURL <- function(dataset = "ROLSYSDEM",
                      key,
                      from = Sys.Date() - 2,
                      to = Sys.Date() - 1,
                      test = FALSE) {

  dataset <- as.character(dataset)
  params <- elexonList()
  if (is.na(match(x = dataset, table = params$FullName))){
    rn <- match(x = dataset, table = params$Component)
    } else {
      rn <- match(x = dataset, table = params$FullName)
    }

  key <- as.character(key)

  from <- as.POSIXct(x = from, tz = "")

  to <- as.POSIXct(x = to, tz = "")

  users_dates <- seq.POSIXt(
    from = as.POSIXct(x = from),
    to = to,
    by = "1 day"
  )

  if(grepl(pattern = " ", x = dataset)){
    dataset <- as.character(params[match(dataset, params$FullName), 1])
  }

  if(grepl("DateRange", params$DateStyle[rn]) |
     (!grepl("Time", params$FromStyle[rn]) &
      grepl("Date", params$FromStyle[rn]))) {
    users_dates <- format.Date(users_dates, "%Y-%m-%d")
    } else if (grepl("Year", params$DateStyle[rn])){
      users_dates <- format.Date(users_dates,  "%Y")
      to <-   ""
      } else {
        from <- format.Date(from, "%Y-%m-%d %H:%M:%S")
        to <- format.Date(to, "%Y-%m-%d %H:%M:%S")
        }
  if (grepl("Unique", params$DateStyle[rn])){
    to <- ""
    from <- ""
  }
  if (grepl("Single", params$DateStyle[rn])) {
    to <- ""
  }

  if(test == TRUE){
    base <- "https://testapi.bmreports.com/BMRS/"
  } else {
      base <- "https://api.bmreports.com/BMRS/"
  }

  c(
    lapply(
      1:(length(users_dates) - 1),
      function(i) paste0(
          base,
          dataset,
          "/v1?APIKey=",
          key,
          if (!grepl("Unique", params$DateStyle[rn])) {
            paste0(
              params$FromStyle[rn],
              users_dates[i]
              )
          },
          if (
            !grepl("Unique", params$DateStyle[rn]) &
              !is.na(params$ToStyle[rn])
              ) {
            paste0(
              params$ToStyle[rn],
              users_dates[i + 1]
            )
          },
          if(!is.na(params$Periodic[rn])) {
            params$Periodic[rn]
            }, if(!is.na(params$Periodic[rn])){
            (seq_len(50))
            },
            "&ServiceType=csv"
        )
      )
    )



}
