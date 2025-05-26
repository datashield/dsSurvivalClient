#' @title Creates restricted cubic splines using rms package
#' @description Creates server-side restricted cubic splines using rms::rcs.
#' @details This function creates restricted cubic spline transformations on the server side
#'     using the rms package. It is specifically designed for creating non-linear terms
#'     in survival analysis models.
#' 
#' Server function called: \code{rcsDS}
#' 
#' @param x character string specifying the name of the server-side variable to be transformed
#' @param knots integer specifying the number of knots for restricted cubic splines. Default is 5
#' @param objectname character string specifying the name for the created server-side object
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login.
#'        If the \code{datasources} argument is not specified
#'        the default set of connections will be used: see \code{\link{datashield.connections_default}}
#' @return Server-side object containing the restricted cubic spline transformation
#' @author Xavier Escribà Montagut, 2025
#' @examples
#' \dontrun{
#'   ## Version 1.0.0
#'   
#'   # connecting to the Opal servers
#'   require('DSI')
#'   require('DSOpal')
#'   require('dsBaseClient')
#'   
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
#'   dsSurvivalClient::ds.rcs(x = "D2.1$age",
#'                              knots = 5,
#'                              objectname = "age_rcs")
#'   
#'   # When finished, clear the session and logout
#'   datashield.logout(connections)
#' }
#'
#' @export
ds.rcs <- function(x = NULL,
                knots = 5,
                objectname = NULL,
                datasources = NULL) {
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }

  if (is.null(x)) {
    stop("Please provide a valid variable name to transform", call. = FALSE)
  }

  if (is.null(objectname)) {
    warning("No objectname provided - using default name 'rcs_output'")
    objectname <- "rcs_output"
  }

  calltext <- call(
    "rcsDS",
    x,
    paste(knots, collapse=",")
  )

  output <- DSI::datashield.assign(
    conns = datasources,
    value = calltext,
    symbol = objectname
  )
  return(output)
} 