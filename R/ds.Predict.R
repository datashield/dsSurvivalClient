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
#'   # Assuming you have a fitted model 'fit' on the server
#'   ds.Predict(fit="fit", age=30:70, sex="both", 
#'             conf.int=0.95, ref.zero=TRUE, objectname="predictions")
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
