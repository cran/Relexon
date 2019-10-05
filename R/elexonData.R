#' @title elexonData
#'
#'
#'
#'
#' @name elexonData
#' @description This function pulls data from Elexon/BMRS, given a user's key, start and end dates and the dataset in question.
#'     Please note: it does not matter if BMRS requires the dates to be in a different format to "yyyy-mm-dd".
#'     The Relexon package will take care of this. Just enter the dates in the usual format!
#' @param dataset The dataset you are pulling from BMRS/Elexon.
#' @param key Your personal scripting key from elexon. Find out more at https://www.elexonportal.co.uk
#' @param from This is the start date/datetime of the dataset.
#' @param to This is the end date/datetime of the dataset.
#' @param test This is set to FALSE by default. Set this argument to TRUE if you want to use the test version of the API. It comes in handy if you are pulling the same datasets repeatedly without getting warnings from Elexon.
#'
#'
#'
#' @examples
#' \dontrun{
#' elexonData(
#' "ROLSYSDEM",
#' key = "948ghmgpe",
#' from = "2018-01-01",
#' test = TRUE
#' )
#' }
#'
#'
#' @export

elexonData <- function(dataset = "ROLSYSDEM",
                       key,
                       from = Sys.Date() - 2,
                       to = Sys.Date() - 1,
                       test = FALSE){

dataset <- as.character(dataset)

if (is.na(match(x = dataset, table = listAPI$FullName))) {
  rn <- match(x = dataset, table = listAPI$Component)
} else {
  rn <- match(x = dataset, table = listAPI$FullName)
}

if(listAPI$wnmsg[rn] != "Available") {
  stop("Unavailable in this package version")
}


if (listAPI$cn[rn] != "0" & listAPI$cn[rn] != 0){
  df_cn <- as.character(
    as.vector(
      unlist(strsplit(listAPI$cn[rn], ",")),
      mode = "list"
    )
  )
} else {
  df_cn <- as.logical(listAPI$ColNames[rn])
  }

links <- unlist(
  elexonURL(dataset, key, from, to, test = test)
  )

df <-   tryCatch(
    suppressMessages(
      suppressWarnings(
        do.call(
          rbind,
          lapply(
            links,
            readr::read_csv,
            col_names =  df_cn,
            skip = listAPI$SKP[rn])
          )
        )
      )
    )
df <- df[df$V1 != "FTR",]
closeAllConnections()
unique(df)
}




