#' @title Creates restricted cubic splines and other transformations using rms package
#' @description Creates server-side transformed variables using rms::rms.trans, primarily for restricted cubic splines.
#' @details This function creates transformed variables on the server side using the rms package,
#'     with a primary focus on restricted cubic splines (rcs). It supports various transformations
#'     available in the rms package and is particularly useful for creating non-linear terms
#'     in survival analysis models.
#' 
#' Server function called: \code{rmsTransDS}
#' 
#' @param x character string specifying the name of the server-side variable to be transformed
#' @param transformation character string specifying the type of transformation. Default is "rcs".
#'        Possible values are:
#'        \itemize{
#'          \item "rcs" - restricted cubic splines
#'          \item "asis" - linear (untransformed)
#'          \item "pol" - polynomial
#'          \item "lsp" - linear spline
#'          \item "catg" - categorical
#'          \item "scored" - ordinal scores
#'          \item "strat" - stratification
#'          \item "gTrans" - custom transformation function
#'        }
#' @param parms parameters specific to the transformation type:
#'        \itemize{
#'          \item "rcs" - numeric vector specifying knot locations
#'          \item "asis" - NULL (no parameters needed)
#'          \item "pol" - integer specifying polynomial degree
#'          \item "lsp" - numeric vector specifying knot locations for linear spline
#'          \item "catg" - numeric vector specifying cut points for categories
#'          \item "scored" - numeric vector specifying scores for ordinal levels
#'          \item "strat" - numeric vector specifying cut points for stratification
#'          \item "gTrans" - function to transform variable using custom function
#'        }
#' @param objectname character string specifying the name for the created server-side object
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login.
#'        If the \code{datasources} argument is not specified
#'        the default set of connections will be used: see \code{\link{datashield.connections_default}}
#' @return \code{rmsTransDS} returns to the client-side a transformed variable for use in
#'         survival analysis models
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
#'                 url = "http://192.168.56.100:8080/",
#'                 user = "administrator", password = "datashield_test&",
#'                 table = "SURVIVAL.EXPAND_WITH_MISSING1", driver = "OpalDriver")
#'   logindata <- builder$build()
#'
#'   connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D")
#'
#'   # Examples of different transformations:
#'
#'   ds.mice(data = 'D', m = 5, method = 'rf', newobj_df = 'D2', seed = 'fixed', newobj_mids = "imputed_mids")
#'   ds.rmsTrans(x = "D2.1$age.60",
#'               transformation = "rcs",
#'               parms = c(20, 40, 60, 80),
#'               objectname = "age_rcs")
#'   ds.rmsTrans(x = "D2.1$bmi",
#'                  transformation = "pol",
#'                  parms = 2,
#'               objectname = "bmi_poly")
#'   # Clear the Datashield R sessions and logout
#'   datashield.logout(connections)
#' }
#'
#' @export
ds.rmsTrans <- function(x = NULL,
                       transformation = "rcs",
                       parms = NULL,
                       objectname = NULL,
                       datasources = NULL) {
    
    # Look for DS connections
    if (is.null(datasources)) {
        datasources <- DSI::datashield.connections_find()
    }
    
    # Verify that 'x' was set
    if (is.null(x)) {
        stop("Please provide a valid variable name to transform", call.=FALSE)
    }
    
    # Verify that 'objectname' was set
    if (is.null(objectname)) {
        stop("Please provide a valid objectname to store the transformed variable", call.=FALSE)
    }
    
    # Verify transformation parameter
    valid_transformations <- c("rcs", "asis", "pol", "lsp", "catg", "scored", "strat", "gTrans")
    if (!(transformation %in% valid_transformations)) {
        stop(paste("Invalid transformation. Must be one of:", 
                  paste(valid_transformations, collapse=", ")), call.=FALSE)
    }
    
    # Call the server side function
    calltext <- call("rmsTransDS", 
                    x,
                    transformation,
                    parms)
    
    # Call assign function
    output <- DSI::datashield.assign(conns = datasources,
                                   value = calltext,
                                   symbol = objectname)
    
    return(output)
} 