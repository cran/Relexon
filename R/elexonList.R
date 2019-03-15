#' @title elexonList
#'
#'
#' @name elexonList
#' @description This simply gives a list of all the datasets you can pull from Elexon/BMRS
#'
#'
#' @examples
#' \dontrun{
#' elexonList()}
#' \dontrun{
#' View(elexonList())
#' }
#'
#'
#'
#' @export
elexonList <- function(){
  LA <- listAPI[,c(1:2,10)]
  return(LA)
  }






