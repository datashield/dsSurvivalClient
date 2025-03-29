#' @title Client-side function to generate ACM plots in DataSHIELD
#' @description This function creates a ggplot visualization for ACM (All-Cause Mortality) analysis
#' by calling the server-side acmPlotDS function.
#'
#' @details This function takes a prediction object created by ds.Predict and generates
#' a customizable ggplot visualization. The function allows customization of colors,
#' line sizes, axis labels, and more.
#'
#' @param pred_obj character string specifying the name of prediction object on the server-side
#' created using ds.Predict()
#' @param line_color color for the main line (default: "blue")
#' @param line_size size of the main line (default: 2)
#' @param ref_line_color color for the reference line (default: "brown")
#' @param ref_line_size size of the reference line (default: 1.5)
#' @param x_breaks numeric vector for x-axis breaks (optional)
#' @param x_label label for x-axis (default: "Primary exposure")
#' @param y_label label for y-axis (default: "Hazard ratio")
#' @param title plot title (optional)
#' @param event_n number of events (optional)
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login
#' @return a list of ggplot objects from each study
#' @author Xavier Escribà Montagut, 2025
#' @examples
#' \dontrun{
#'   # After setting up DataSHIELD connections and creating prediction object
#'   ds.acmPlot(pred_obj = "pred_obj",
#'              line_color = "darkblue",
#'              x_label = "BMI",
#'              event_n = 1000)
#' }
#' @export
ds.acmPlot <- function(pred_obj = NULL,
                      line_color = "blue",
                      line_size = 2,
                      ref_line_color = "brown",
                      ref_line_size = 1.5,
                      x_breaks = NULL,
                      x_label = "Primary exposure",
                      y_label = "Hazard ratio",
                      title = NULL,
                      event_n = NULL,
                      datasources = NULL) {

  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }

  if (is.null(pred_obj)) {
    stop("Please provide a valid prediction object name!", call.=FALSE)
  }

  call <- call("acmPlotDS",
                pred_obj = pred_obj,
                line_color = line_color,
                line_size = line_size,
                ref_line_color = ref_line_color,
                ref_line_size = ref_line_size,
                x_breaks = x_breaks,
                x_label = x_label,
                y_label = y_label,
                title = title,
                event_n = event_n)
  output <- DSI::datashield.aggregate(datasources, call)
  return(output)
}