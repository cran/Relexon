#' @title elexonData
#'
#'
#'
#'
#' @name elexonData
#' @description This function pulls data from Elexon/BMRS, given a user's key, start and end dates and the dataset in question
#'     Please note: it does not matter if BMRS requires the dates to be in a different format to "yyyy-mm-dd".
#'     The Relexon package will take care of this. Just enter the dates in the usual format!
#' @param dataset The dataset you are pulling from BMRS/Elexon.
#' @param key Your personal scripting key from elexon. Find out more at https://www.elexonportal.co.uk
#' @param from This is the start date/datetime of the dataset
#' @param to This is the end date/datetime of the dataset
#'
#'
#'
#' @examples
#' \dontrun{
#' elexonData(dataset = "ROLSYSDEM", key = "bne89n4f", from = "2018-01-01", to = "2019-01-01")
#' }
#'
#'
#' @export


elexonData <- function(dataset, key, from, to){

dataset <- as.character(dataset)

if (is.na(match(x = dataset, table = listAPI$FullName)) == TRUE){
rn <- match(x = dataset, table = listAPI$Component)
} else {
rn <- match(x = dataset, table = listAPI$FullName)
}

key <- as.character(key)

from <- as.POSIXct(x = from, tz = "GMT")

to <- as.POSIXct(x = to, tz = "GMT")

TD <- as.numeric(to - from)

if (TD > 1){
users_dates <- seq.POSIXt(from = as.POSIXct(x = from), to = as.POSIXct(x = to), by = "1 day")
}

### ~~~ Start of Giant If Statement ~~~ ###
if (TD == 1){
### ~~~ Start of Giant If Statement ~~~ ###

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

### ~~~ End of URL Designer ~~~ ###

df <- readr::read_csv(as.character(url_df),
col_names = as.logical(listAPI$ColNames[rn]),
skip = as.numeric(listAPI$SKP[rn]))

if (listAPI$cn[rn] != "0" & listAPI$cn[rn] != 0){
colnames(df) <- as.vector(unlist(strsplit(listAPI$cn[rn],",")),mode="list")
}

df <- df[df[,1] != "FTR",]

return(df)

#as.POSIXct(x = as.character(df$DateTime), "%Y%m%d%H%M%S", tz="GMT")





### ~~~ End of Giant If Statement ~~~ ###
}
### ~~~ End of Giant If Statement ~~~ ###

### ~~~ Start of Giant Else Statement ~~~ ###
else {
### ~~~ Start of Giant Else Statement ~~~ ###

### ~~~ Start of Giant For Loop ~~~ ###
for (i in 1:(TD + 1)){
### ~~~ Start of Giant For Loop ~~~ ###

from <- users_dates[i]
to <- users_dates[i]

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

  ### ~~~ End of URL Designer ~~~ ###


if (i == 1){

df <- readr::read_csv(as.character(url_df),
col_names = as.logical(listAPI$ColNames[rn]),
skip = as.numeric(listAPI$SKP[rn]))

if (listAPI$cn[rn] != "0" & listAPI$cn[rn] != 0){
colnames(df) <- as.vector(unlist(strsplit(listAPI$cn[rn],",")),mode="list")
}

df <- df[df[,1] != "FTR",]

} else {

suppressWarnings(suppressMessages(
df2 <- readr::read_csv(as.character(url_df),
col_names = as.logical(listAPI$ColNames[rn]),
skip = as.numeric(listAPI$SKP[rn]))
))

if (listAPI$cn[rn] != "0" & listAPI$cn[rn] != 0){
colnames(df2) <- as.vector(unlist(strsplit(listAPI$cn[rn],",")),mode="list")
}

df2 <- df2[df2[,1] != "FTR",]

df <- rbind(df, df2)
}

### ~~~ End of Giant For Loop ~~~ ###
}
### ~~~ End of Giant For Loop ~~~ ###


return(df)



### ~~~ End of Giant Else Statement ~~~ ###
}
### ~~~ End of Giant Else Statement ~~~ ###

### ~~~ End of Function ~~~ ###
}
### ~~~ End of Function ~~~ ###





