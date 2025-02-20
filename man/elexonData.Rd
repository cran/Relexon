% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/elexonData.R
\name{elexonData}
\alias{elexonData}
\title{elexonData}
\usage{
elexonData(dataset = "ROLSYSDEM", key, from = Sys.Date() - 2,
  to = Sys.Date() - 1, test = FALSE)
}
\arguments{
\item{dataset}{The dataset you are pulling from BMRS/Elexon.}

\item{key}{Your personal scripting key from elexon. Find out more at https://www.elexonportal.co.uk}

\item{from}{This is the start date/datetime of the dataset.}

\item{to}{This is the end date/datetime of the dataset.}

\item{test}{This is set to FALSE by default. Set this argument to TRUE if you want to use the test version of the API. It comes in handy if you are pulling the same datasets repeatedly without getting warnings from Elexon.}
}
\description{
This function pulls data from Elexon/BMRS, given a user's key, start and end dates and the dataset in question.
    Please note: it does not matter if BMRS requires the dates to be in a different format to "yyyy-mm-dd".
    The Relexon package will take care of this. Just enter the dates in the usual format!
}
\examples{
\dontrun{
elexonData(
"ROLSYSDEM",
key = "948ghmgpe",
from = "2018-01-01",
test = TRUE
)
}


}
