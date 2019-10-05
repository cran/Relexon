#' @title elexonPhysical
#'
#'
#'
#'
#' @name elexonPhysical
#' @description This function pulls physical data from Elexon/BMRS, given a user's key, start and end dates.
#'     All half-hourly periods are used, accounting for daylight savings as well. The test parameter is set to TRUE by default, as datasets ranging more than a mere week will result in pullinh very large datasets.
#'
#' @param key Your personal scripting key from elexon. Find out more at https://www.elexonportal.co.uk
#' @param from This is the start date/datetime of the dataset
#' @param to This is the end date/datetime of the dataset
#' @param test This is set to TRUE by default. Set this argument to FALSE if you want to use the live version of the API.
#'
#'
#'
#' @examples
#' \dontrun{
#' elexonPhysical(
#' key = "948ghmgpe",
#' from = "2018-01-01",
#' to = "2019-01-01",
#' test = TRUE)
#' }
#'
#'
#' @export


elexonPhysical <- function(key,
                           from = Sys.Date() - 20 ,
                           to = Sys.Date() - 19,
                           test = TRUE) {

  message("Getting physical data . . . This could take a while . . . ")

  key <- as.character(key)

  from <- as.POSIXct(x = from, tz = "GMT")

  to <- as.POSIXct(x = to, tz = "GMT")


  if(test == TRUE){
    base <- "https://testapi.bmreports.com/BMRS/PHYBMDATA/v1?APIKey="
  } else {
    base <- "https://api.bmreports.com/BMRS/PHYBMDATA/v1?APIKey="
    }


links <- unlist(elexonURL("PHYBMDATA", key = key, test = test))

elexonPhysicalPB <- utils::txtProgressBar(0, length(links), style = 3)

df <-  tryCatch(
  {
    suppressWarnings(
      do.call(
        rbind,
        lapply(
          1:length(links), function(i) {
            df <- readr::read_csv(links[i],
                           col_names = paste0("V", seq_len(11)),
                           skip = 1,
                           col_types = readr::cols())
            utils::setTxtProgressBar(elexonPhysicalPB, i)
            return(df)
          }
        )
      )
    )
  }
)

rm("elexonPhysicalPB")

df <- df[df$V1 != "FTR",]


bo <- df[df$V1 == "BOALF", c(1:3, 8:11)]

df <- df[ , 1:7]

colnames(df) <- c("Item", "BM_Unit", "RefNo", "From", "From_Level", "To", "To_Level")

colnames(bo) <- colnames(df)

df <- rbind(df, bo)

df$To <- as.POSIXct(x = df$To,
  tz = "GMT",
  format = "%Y%m%d%H%M%S",
  origin = "1970-01-01")

df$From <- as.POSIXct(x = df$From,
  tz = "GMT",
  format = "%Y%m%d%H%M%S",
  origin = "1970-01-01")

closeAllConnections()
unique(df)

}

