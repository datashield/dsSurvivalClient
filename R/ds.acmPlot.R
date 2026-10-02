#' @title Client-side function to generate hazard ratio plots in DataSHIELD
#' @description Builds a ggplot2 hazard ratio curve for each connected study, using prediction data fetched from the DataSHIELD server. Plot appearance (colors, line sizes, axis labels) can be customized.
#' @details This function sends a request to the server to extract prediction data from an object previously created with ds.Predict, then builds the plot locally with ggplot2 (using rms for underlying plot support). Server function called: `acmPlotDS`. If x_breaks is not supplied, five evenly spaced breaks are computed from the range of the first column of each study's returned prediction data.
#' @param pred_obj character string specifying the name of prediction object on the server-side
#' created using ds.Predict()
#' @param outcome_name character string specifying the outcome name (e.g., "ACM", "CVD") (default: "ACM")
#' @param line_color color for the main line (default: "blue")
#' @param line_size size of the main line (default: 2)
#' @param ref_line_color color for the reference line (default: "brown")
#' @param ref_line_size size of the reference line (default: 1.5)
#' @param x_breaks Optional numeric vector of x-axis tick positions; if NULL, five evenly spaced breaks are computed from each study's data range.
#' @param x_label label for x-axis (default: "Primary exposure")
#' @param y_label label for y-axis (default: "Hazard ratio")
#' @param event_n Optional numeric value giving the number of events; if supplied, it is appended to the plot title as "(n = event_n)".
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login
#' @return Returns a list of ggplot objects, one per study/connection in datasources, each built from the prediction summary data returned by the server (no individual-level data leaves the server).
#' @author Xavier Escribà Montagut, 2025
#' @examples
#' \dontrun{
#'   # After setting up DataSHIELD connections and creating prediction object
#'   ds.acmPlot(pred_obj = "pred_obj",
#'              outcome_name = "ACM",
#'              line_color = "darkblue",
#'              x_label = "BMI",
#'              event_n = 1000)
#'   
#'   # For CVD analysis
#'   ds.acmPlot(pred_obj = "pred_obj",
#'              outcome_name = "CVD",
#'              line_color = "darkblue",
#'              x_label = "BMI",
#'              event_n = 800)
#' }
#' @export
ds.acmPlot <- function(pred_obj = NULL,
                      outcome_name = "ACM",
                      line_color = "blue",
                      line_size = 2,
                      ref_line_color = "brown",
                      ref_line_size = 1.5,
                      x_breaks = NULL,
                      x_label = "Primary exposure",
                      y_label = "Hazard ratio",
                      event_n = NULL,
                      datasources = NULL) {

  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }

  if (is.null(pred_obj)) {
    stop("Please provide a valid prediction object name!", call.=FALSE)
  }

  call <- call("acmPlotDS", pred_obj = pred_obj)
  pred_data <- DSI::datashield.aggregate(datasources, call)

  plots <- lapply(pred_data, function(study_data) {
    # Create title if event number is provided
    if (!is.null(event_n)) {
      plot_title <- paste0(outcome_name, " (n = ", event_n, ")")
    } else {
      plot_title <- outcome_name
    }

    if (is.null(x_breaks)) {
      x_min <- min(study_data[,1])
      x_max <- max(study_data[,1])
      x_breaks <- seq(x_min, x_max, length.out = 5)
    }
    # Needed for plot
    require(rms)

    p <- ggplot2::ggplot(study_data, colfill = "Darkblue") +
      ggplot2::coord_trans(y = "log10", ylim = c(0.1, 6)) +
      ggplot2::scale_x_continuous(breaks = x_breaks) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(face = "bold", color = "black", size = 15, angle = 0),
        axis.text.y = ggplot2::element_text(face = "bold", color = "black", size = 15, angle = 0),
        axis.line = ggplot2::element_line(colour = "darkblue", size = 1, linetype = "solid"),
        plot.title = ggplot2::element_text(color = "Black", size = 18, face = "bold"),
        axis.title.x = ggplot2::element_text(color = "grey20", size = 20),
        axis.title.y = ggplot2::element_text(color = "grey20", size = 20),
        legend.title = ggplot2::element_text(size = 20),
        legend.text = ggplot2::element_text(size = 17)
      ) +
      ggplot2::xlab(x_label) +
      ggplot2::ylab(y_label) +
      ggplot2::geom_line(color = line_color, size = line_size) +
      ggplot2::geom_hline(yintercept = 1, linetype = "dashed",
                         color = ref_line_color, size = ref_line_size)

    if (!is.null(plot_title)) {
      p <- p + ggplot2::ggtitle(plot_title)
    }
    return(p)
  })
  return(plots)
}
