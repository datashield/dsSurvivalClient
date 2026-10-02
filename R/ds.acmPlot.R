#' @title Client-side function to generate hazard ratio plots in DataSHIELD
#' @description Builds a customizable ggplot2 hazard ratio curve for each connected study, using prediction data retrieved from the server-side acmPlotDS function.
#' @details This function sends pred_obj (the name of an object previously created with ds.Predict on the server) to the server via DSI::datashield.aggregate, which calls the server-side function acmPlotDS to retrieve prediction data for each study. It then builds one ggplot2 plot per study locally, using ggplot2::geom_line, ggplot2::geom_hline and related layers, and requires the rms package to be installed (loaded via require(rms)) for the plotting to succeed. If x_breaks is not supplied, it is computed per study as 5 equally spaced values spanning the range of the first column of the returned prediction data.
#' Server function called: `acmPlotDS`.
#' @param pred_obj Character string giving the name of a prediction object already created on the server with ds.Predict(); this is a server-side object name, not a local R object. Required, no default.
#' @param outcome_name Character string used as (the start of) the plot title, e.g. "ACM" or "CVD" (default: "ACM").
#' @param line_color Color (as accepted by ggplot2) used for the main hazard ratio line (default: "blue").
#' @param line_size Numeric line width for the main hazard ratio line, passed to ggplot2::geom_line (default: 2).
#' @param ref_line_color Color for the horizontal reference line at hazard ratio = 1 (default: "brown").
#' @param ref_line_size Numeric line width for the horizontal reference line, passed to ggplot2::geom_hline (default: 1.5).
#' @param x_breaks Optional numeric vector of x-axis tick positions; if NULL, 5 equally spaced breaks are computed per study from the range of the prediction data's first column.
#' @param x_label Character string used as the x-axis label (default: "Primary exposure").
#' @param y_label Character string used as the y-axis label (default: "Hazard ratio").
#' @param event_n Optional number of events; if supplied, it is appended to the plot title as "outcome_name (n = event_n)".
#' @param datasources A list of DSConnection-class objects as returned by DSI::datashield.login; if NULL, the function uses DSI::datashield.connections_find() to find existing connections.
#' @return Returns a plain R list of ggplot objects, one per study/data source, in the same order as the connections in datasources; no data values are returned, only the plot objects built from the already-aggregated prediction summaries, so no additional disclosure control is applied in this function.
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
