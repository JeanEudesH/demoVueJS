% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getDF.R
\name{getDF}
\alias{getDF}
\title{getDF}
\usage{
getDF(varURI, smoothing = FALSE, sensor = NULL, endDate = NULL,
  startDate = NULL, wsUrl = "www.opensilex.org/openSilexAPI/rest/")
}
\arguments{
\item{varURI}{uri of the variable to plot, from the \code{\link{variableList}} function or the web service directly}

\item{smoothing}{logical, smoothing of the data,  returns the incertitudes from the smoothing}

\item{sensor}{character, uri of a sensor to filter with}

\item{endDate}{date, date from which to filter data, format "\%Y-\%m-\%dT\%H:\%M:\%S"}

\item{startDate}{date, date to which filter the data, format "\%Y-\%m-\%dT\%H:\%M:\%S"}

\item{wsUrl}{url of the webservice}
}
\value{
data.frame
}
\description{
get the dataset of incertitudes for the data selected
}
\examples{
\donttest{
connectToPHISWS(apiID="ws_private", url = "www.opensilex.org/openSilexAPI/rest/", "guest@opensilex.org","guest")
vars <- variableList(wsUrl = "www.opensilex.org/openSilexAPI/rest/")
vars
getDF(varURI = list("http://www.opensilex.org/demo/id/variables/v004",
                           "http://www.opensilex.org/demo/id/variables/v007"))
}
}
