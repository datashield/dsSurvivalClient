#' @title Creates a server-side datadist object for use with rms package functions
#' @description Creates a server-side datadist object that contains statistical summaries
#' of variables for use in plotting regression model fits.
#' @details This function creates a server-side datadist object which contains statistical
#' summaries of variables that are needed for plotting effects in regression models.
#' The summaries include quantiles, ranges, and other distribution statistics.
#' 
#' Server function called: \code{datadistDS}
#' 
#' @param data character string specifying the name of a data frame on the server
#' @param adjust_to named list where names are variable names and values are either "min", "max", or "mean"
#'        to set the "Adjust to" values for specific variables. For example: 
#'        list(age="min", weight="max", height="mean") will set age's "Adjust to" to its minimum,
#'        weight's to its maximum, and height's to its mean.
#' @param objectname character string specifying name of the output object to create on the server
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login.
#' If the \code{datasources} argument is not specified the default set of connections will be used:
#' see \code{\link{datashield.connections_default}}.
#' @return Assigns a datadist object on the server side containing distribution summaries
#' @author Xavier Escribà Montagut, 2025
#' @examples
#' \dontrun{
#'   ## Version 1.0.0
#'   
#'   # connecting to the Opal servers
#'   builder <- DSI::newDSLoginBuilder()
#'   builder$append(server = "study1", 
#'                 url = "http://192.168.56.100:8080/", 
#'                 user = "administrator", password = "datashield_test&", 
#'                 table = "SURVIVAL.EXPAND_NO_MISSING1", driver = "OpalDriver")
#'   logindata <- builder$build()
#'   
#'   connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D")
#'   
#'   # Create datadist object for the entire data frame
#'   # and set "Adjust to" values for specific variables
#'   ds.datadist(data = "D", 
#'               adjust_to = list(age="min", weight="max", height="mean"),
#'               objectname = "dist1")
#'               
#'   # Clear the Datashield R sessions and logout
#'   datashield.logout(connections)
#' }
#'
#' @export
ds.datadist <- function(data = NULL,
                       adjust_to = NULL,
                       objectname = NULL,
                       datasources = NULL) {

  if(is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }

  if(is.null(data)) {
    stop("Please provide the name of a data frame in the 'data' parameter", call.=FALSE)
  }

  if(is.null(objectname)) {
    objectname <- paste0("datadist_", data)
    warning("No objectname provided, using default: ", objectname, call.=FALSE)
  }

  if (!is.null(adjust_to)) {
    if (!is.list(adjust_to)) {
      stop("adjust_to must be a named list", call.=FALSE)
    }
    if (is.null(names(adjust_to)) || any(names(adjust_to) == "")) {
      stop("all elements in adjust_to must be named", call.=FALSE)
    }
    if (!all(adjust_to %in% c("min", "max", "mean"))) {
      stop("adjust_to values must be either 'min', 'max', or 'mean'", call.=FALSE)
    }
  }

  calltext <- call("datadistDS", data, adjust_to)
  output <- DSI::datashield.assign(
    conns = datasources,
    value = calltext, 
    symbol = objectname
  )

  return(output)
} 
