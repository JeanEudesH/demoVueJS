#-------------------------------------------------------------------------------
# Program: variableList.R
# Objective: retrieve data in suitable form for graph labelling
# Authors: Chourrout Elise
# Creation: 15/02/2019
# Update:
#-------------------------------------------------------------------------------

#' @title Get Variable's Names from WS2 and formate them
#'
#' @importFrom phisWSClientR getEnvironmentData
#' @importFrom phisWSClientR connectToPHISWS
#'
#' @param wsUrl url of the webservice
#' @return WSResponse
#' @export
#'
#' @examples
#' \donttest{
#' connectToPHISWS(apiID="ws_private",
#' url = "www.opensilex.org/openSilexAPI/rest/",
#' "guest@opensilex.org",
#' "guest")
#'  variableList()
#' }
variableList <- function(wsUrl = "www.opensilex.org/openSilexAPI/rest/"){
  phisWSClientR::connectToPHISWS(apiID="ws_private", url = wsUrl, username = 'guest@opensilex.org', password = 'guest')


  # Recuperation of variables information
  rawVar <- phisWSClientR::getVariables2()

  # Extraction of the information of interest
  label <- rawVar$data$label
  acronyms <- rawVar$data$trait.label
  unitVar <- rawVar$data$unit.comment
  uriVar <- rawVar$data$uri

  # Creation of the dataTable with information of interest
  variableList <- data.frame(acronym = acronyms, unity = unitVar, uri = uriVar, label = label)
  variableList <- data.frame(lapply(variableList, as.character), stringsAsFactors=FALSE)

  return(variableList)
}

