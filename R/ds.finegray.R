#' @title Creates data for a Fine-Gray model
#' @description The function creates a dataset for fitting a Fine-Gray model using a competing risks approach
#' @details This function is a wrapper for the survival::finegray function. It creates a dataset 
#' for fitting a Fine-Gray model on the server side. The Fine-Gray model handles competing risks
#' by creating a special dataset and then fitting a weighted Cox model to the result.
#' 
#' Server function called: \code{finegrayDS}
#' 
#' @param formula a survival formula with survival object on the left and covariates on the right 
#' @param data character string specifying the name of the data frame that contains the variables in the model
#' @param weights optional character string specifying a variable providing weights for the observations
#' @param na.action character string specifying how to handle missing values; default is "na.pass"
#' @param etype the event type for which a data set will be generated (character or numeric)
#' @param prefix prefix for the new variables that will be added to the dataset; default is "fg"
#' @param count optional character string specifying a name for a count variable
#' @param id optional character string specifying an ID variable
#' @param timefix logical indicating whether to apply correction for tied survival times; default is TRUE
#' @param newobj character string specifying the name of the new dataset to be created on the server
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login.
#' If the \code{datasources} argument is not specified the default set of connections will be used:
#' see \code{\link{datashield.connections_default}}.
#' @return \code{ds.finegray} returns a message indicating the success of the operation.
#' @author Xavier Escribà Montagut, 2025
#' @examples
#' \dontrun{
#'   ## Version 1.0.0
#'
#'   # connecting to the Opal servers
#'   builder <- DSI::newDSLoginBuilder()
#'   builder$append(server = "study1",
#'                  url = "http://192.168.56.100:8080/",
#'                  user = "administrator", password = "datashield_test&",
#'                  table = "SURVIVAL.EXPAND_NO_MISSING1", driver = "OpalDriver")
#'   logindata <- builder$build()
#'
#'   connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D")
#'
#'   # Create a Fine-Gray dataset for competing risks analysis
#'   ds.finegray(formula = "Surv(time, status) ~ age + sex",
#'               data = "D",
#'               etype = 1,
#'               newobj = "fg_data")
#'
#'   # Now use the created dataset for a weighted Cox model
#'   ds.coxph.SLMAassign("Surv(fgstart, fgstop, fgstatus) ~ age + sex",
#'                       weights = "fgwt",
#'                       data = "fg_data",
#'                       newobj = "coxph_model")
#'
#'   # Clear the Datashield R sessions and logout
#'   datashield.logout(connections)
#' }
#'
#' @export
ds.finegray <- function(formula = NULL,
                        data = NULL,
                        weights = NULL,
                        na.action = "na.pass",
                        etype = NULL,
                        prefix = "fg",
                        count = NULL,
                        id = NULL,
                        timefix = TRUE,
                        newobj = NULL,
                        datasources = NULL) {
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }

  if (is.null(formula)) {
    stop("Please provide a formula", call.=FALSE)
  }

  if (is.null(data)) {
    stop("Please provide the name of a data.frame in the 'data' parameter", call.=FALSE)
  }

  if (is.null(newobj)) {
    newobj <- "finegray_data"
    warning("No name provided for the output object. Using default name: ", newobj)
  }

  if (!is.character(formula)) {
    formula <- stats::as.formula(formula)
    formula <- Reduce(paste, deparse(formula))
  }

  formula <- gsub("survival::Surv(", "sssss", formula, fixed = TRUE)
  formula <- gsub("Surv(", "sssss", formula, fixed = TRUE)
  formula <- gsub("rms::rcs(", "ggggg", formula, fixed = TRUE)
  formula <- gsub("rms::asis(", "aaaaa", formula, fixed = TRUE)
  formula <- gsub("rms::matrx(", "mmmmm", formula, fixed = TRUE)
  formula <- gsub("rms::pol(", "ooooo", formula, fixed = TRUE)
  formula <- gsub("rms::lsp(", "hhhhh", formula, fixed = TRUE)
  formula <- gsub("rms::catg(", "ccccc", formula, fixed = TRUE)
  formula <- gsub("rms::scored(", "ddddd", formula, fixed = TRUE)
  formula <- gsub("rms::strat(", "nnnnn", formula, fixed = TRUE)
  formula <- gsub("rms::gTrans(", "ttttt", formula, fixed = TRUE)
  formula <- gsub(".", "jjj", formula, fixed = TRUE)
  formula <- gsub("=", "lll", formula, fixed = TRUE)
  formula <- gsub("|", "xxx", formula, fixed = TRUE)
  formula <- gsub("(", "yyy", formula, fixed = TRUE)
  formula <- gsub(")", "zzz", formula, fixed = TRUE)
  formula <- gsub("/", "ppp", formula, fixed = TRUE)
  formula <- gsub(":", "qqq", formula, fixed = TRUE)
  formula <- gsub(",", "rrr", formula, fixed = TRUE)
  formula <- gsub(" ", "", formula, fixed = TRUE)
  formula <- stats::as.formula(formula)

  calltext <- call("finegrayDS", 
                  formula = formula,
                  data = data,
                  weights = weights,
                  na.action = na.action,
                  etype = etype,
                  prefix = prefix,
                  count = count,
                  id = id,
                  timefix = timefix)

  datashield.assign(conns = datasources, 
                                value = calltext,
                                symbol = newobj)
}