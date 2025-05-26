#' @title Sets a server-side datadist object as the global option
#' @description Sets a previously created datadist object as the global option for rms plotting functions
#' @details This function sets a datadist object previously created with ds.datadist
#' as the global option for use with rms plotting functions.
#' 
#' Server function called: \code{useDatadistDS}
#' 
#' @param datadist character string specifying the name of a datadist object on the server
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login.
#' If the \code{datasources} argument is not specified the default set of connections will be used:
#' see \code{\link{datashield.connections_default}}.
#' @return A message indicating the success or failure of the operation
#' @author Xavier Escribà Montagut, 2025
#' @examples
#' \dontrun{
#'   ## Version 1.0.0
#'   
#'   # connecting to the Opal servers
#'   builder <- DSI::newDSLoginBuilder()
#'   builder$append(server = "study1",
#'                  url = "http://192.168.56.100:8080/",
#'                  user = "administrator",
#'                  password = "datashield_test&",
#'                  table = "SURVIVAL.EXPAND_WITH_MISSING1",
#'                  driver = "OpalDriver")
#'   logindata <- builder$build()
#'   connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D")
#'   ds.mice(data = 'D', m = 5, method = 'rf', newobj_df = 'D2', seed = 'fixed', newobj_mids = "imputed_mids")
#'
#'   ds.datadist(data = 'D2.1', adjust_to = list(age.60 = 'min'))
#'
#'   # Set the datadist object as the global option
#'   ds.useDatadist(datadist = "datadist_D2.1")
#'
#'   # When finished, clear the session and logout
#'   datashield.logout(connections)
#' }
#'
#' @export
ds.useDatadist <- function(datadist = NULL,
                          datasources = NULL) {

  if(is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }

  if(is.null(datadist)) {
    stop("Please provide the name of a datadist object in the 'datadist' parameter", call.=FALSE)
  }

  cally <- call("useDatadistDS", datadist)
  result <- DSI::datashield.aggregate(datasources, cally)

  all_success <- all(unlist(result))

  if (all_success) {
    message("Datadist object successfully set as global option on all servers")
  } else {
    warning("Failed to set datadist object as global option on some servers", call.=FALSE)
  }
} 