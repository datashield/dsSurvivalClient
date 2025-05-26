#' @title Creates predictions from regression models fitted with rms on the server side
#' @description This is a wrapper function for rms::Predict that works in the DataSHIELD infrastructure
#' @details This function allows users to generate predictions from regression models fitted with rms
#' on the server side. It supports various types of predictions and confidence intervals.
#' 
#' Server function called: \code{predictDS}
#' 
#' @param fit character string specifying the name of the fitted model object on the server
#' @param ... character strings specifying names of predictors to vary
#' @param fun character string specifying a transformation for the predictions
#' @param type character string specifying type of predictions
#' @param conf.int numeric confidence level (0-1)
#' @param conf.type character string specifying type of confidence interval
#' @param ref.zero logical; if TRUE, predictions are centered at a reference value
#' @param objectname character string specifying name for the resulting prediction object
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login
#' 
#' @return assigns the prediction object to the server side with the specified objectname
#' @author Xavier Escribà Montagut, 2025
#' @examples
#' \dontrun{
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
#'   ds.asNumeric(x.name = "D2.1$cens",
#'             newobj = "EVENT",
#'             datasources = connections)
#'   ds.asNumeric(x.name = "D2.1$survtime",
#'             newobj = "SURVTIME",
#'             datasources = connections)
#'   ds.asNumeric(x.name = "D2.1$starttime",
#'             newobj = "STARTTIME",
#'             datasources = connections)
#'   ds.asNumeric(x.name = "D2.1$endtime",
#'             newobj = "ENDTIME",
#'             datasources = connections)
#'
#'   ds.datadist(data = 'D2.1', adjust_to = list(age.60 = 'min'))
#'
#'   ds.useDatadist(datadist = "datadist_D2.1")
#'
#'   dsSurvivalClient::ds.Surv(time='STARTTIME', time2='ENDTIME',
#'         event = 'EVENT', objectname='surv_object')
#'
#'   ds.coxphSLMAassign(formula = 'surv_object ~ age.60',
#'                         dataName = 'D2.1', objectname = 'cph1', use.rms = TRUE)
#'
#'   ds.Predict(fit = 'cph1', objectname = 'predictions', fun = "exp", ref.zero = TRUE)
#'
#'   # When finished, clear the session and logout
#'   datashield.logout(connections)
#' }
#' @export
ds.Predict <- function(fit = NULL,
                      ...,
                      fun = NULL,
                      type = c("predictions", "model.frame", "x"),
                      conf.int = 0.95,
                      conf.type = c("mean", "individual", "simultaneous"),
                      ref.zero = FALSE,
                      objectname = NULL,
                      datasources = NULL) {

  if(is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }

  if(is.null(fit)) {
    stop("Please provide a valid fitted model object name", call.=FALSE)
  }

  if(is.null(objectname)) {
    objectname <- "predictions"
    warning("No objectname provided, using default 'predictions'", call.=FALSE)
  }

  dots <- list(...)
  pred_vars <- names(dots)
  pred_values <- dots

  calltext <- call(
    "predictDS",
    fit = fit,
    pred_vars = pred_vars,
    pred_values = pred_values,
    fun = fun,
    type = type,
    conf.int = conf.int,
    conf.type = conf.type,
    ref.zero = ref.zero
  )
  output <- DSI::datashield.assign(
    conns = datasources,
    value = calltext,
    symbol = objectname
  )
  return(output)
} 
