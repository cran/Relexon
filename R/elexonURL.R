#' @title elexonURL
#'
#'@name elexonURL
#'
#' @description This function a URL that can be used to download csv files manually
#'    Please note: it does not matter if BMRS requires the dates to be in a different format to "yyyy-mm-dd".
#'    The Relexon package will take care of this. Just enter the dates in the usual format!
#'
#'
#'
#' @param dataset The dataset you are pulling from BMRS/Elexon.
#' @param key Your personal scripting key from elexon. Find out more at https://www.elexonportal.co.uk
#' @param from This is the start date/datetime of the dataset
#' @param to This is the end date/datetime of the dataset
#'
#' @examples
#' \dontrun{
#' elexonURL(dataset = "HHFUEL", key = "948ghmgpe",  from  = "2018-01-01", to = "2018-01-05")
#' }
#'
#'
#' @export
#'

elexonURL <- function(dataset, key, from, to){


  dataset <- as.character(dataset)

  if (is.na(match(x = dataset, table = listAPI$FullName)) == TRUE){
    rn <- match(x = dataset, table = listAPI$Component)
  } else {
    rn <- match(x = dataset, table = listAPI$FullName)
  }

  key <- as.character(key)


  from <- as.POSIXct(x = from, tz = "GMT")

  to <- as.POSIXct(x = to, tz = "GMT")


  if(grepl(pattern = " ", x = dataset) == TRUE){

    dataset <- as.character(listAPI[match(dataset, listAPI$FullName),1])

  }


  if(grepl("Single", listAPI$DateStyle[rn]) == TRUE |
     grepl("DateRange", listAPI$DateStyle[rn]) == TRUE ){

    from <- format(from, "%Y-%m-%d")

    to <- format(to, "%Y-%m-%d")

  } else if (grepl("Year", listAPI$DateStyle[rn]) == TRUE){

    from <-  format(from, "%Y")

    to <-   format(to, "%Y")

  } else {

    from <- format(from, "%Y-%m-%d %H:%M:%S")

    to <- format(to, "%Y-%m-%d %H:%M:%S")

  }


  if (grepl("Single", listAPI$DateStyle[rn]) == TRUE |
      grepl("Year", listAPI$DateStyle[rn]) == TRUE){

    to <- ""

  }

  if (grepl("Unique", listAPI$DateStyle[rn]) == TRUE){
    to <- ""
    from <- ""
  }


  ### ~~~ URL Designer ~~~ ###

  url_df <-  paste0("https://api.bmreports.com/BMRS/",

                    dataset,

                    "/v1?APIKey=",

                    key,

                    if (grepl("Unique", listAPI$DateStyle[rn]) != TRUE){
                      listAPI$FromStyle[rn]
                    },

                    from,

                    if (grepl("Single", listAPI$DateStyle[rn]) != TRUE &
                        grepl("Year", listAPI$DateStyle[rn]) != TRUE &
                        grepl("Unique", listAPI$DateStyle[rn]) != TRUE){

                      listAPI$ToStyle[rn]

                    },

                    to,

                    if (grepl("Single", listAPI$DateStyle[rn]) == TRUE){

                      listAPI$Periodic[rn]

                    },

                    "&ServiceType=csv")
  data_message <- "[[Please note that if your from- and to-dates are too far apart, this URL will not work in your browser!]]"
  outputs <- paste(url_df, cat("\n"), cat("\n"), data_message, sep = "  ")
  return(outputs)


}


  ### ~~~ End of URL Designer ~~~ ###
