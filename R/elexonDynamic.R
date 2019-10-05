#' @title elexonDynamic
#'
#'
#'
#'
#' @name elexonDynamic
#' @description This function pulls dynamic data from Elexon/BMRS, given a user's key, start and end dates.
#'     The data returned are given in a list due to their varying column lengths, separated by each item.
#'
#' @param key Your personal scripting key from elexon. Find out more at https://www.elexonportal.co.uk
#' @param from This is the start date/datetime of the dataset
#' @param to This is the end date/datetime of the dataset
#' @param test This is set to TRUE by default. Set this argument to FALSE if you want to use the live version of the API.
#'
#'
#' @examples
#' \dontrun{
#' elexonDynamic(
#' key = "948ghmgpe",
#' from = "2018-01-01",
#' to = "2019-01-01",
#' test = TRUE)
#' }
#'
#'
#' @export


elexonDynamic <- function(key,
                          from = Sys.Date() - 60 ,
                          to = Sys.Date() - 59,
                          test = TRUE) {

  links <- unlist(elexonURL("DYNBMDATA", key = key, test = test))

  message("Loading dynamic data . . . This could take a while . . .")

  elexonDynamicPB <- utils::txtProgressBar(0, length(links), style = 3)

  df <-  tryCatch(
    {
      suppressWarnings(
        do.call(
          rbind,
          lapply(
            1:length(links), function(i) {
              df <- readr::read_csv(links[i],
                             col_names = paste0("V", seq_len(8)),
                             skip = 1,
                             col_types = readr::cols())
              utils::setTxtProgressBar(elexonDynamicPB, i)
              return(df)
              }
            )
          )
      )
      }
    )
  rm("elexonDynamicPB")
  df <- df[df$V1 != "FTR",]

  df$V3 <- as.POSIXct(
    x = as.character(df$V3),
    tz = "GMT",
    format = "%Y%m%d%H%M%S",
    origin = "1970-01-01"
    )
  closeAllConnections()
  unique(df)

  }


