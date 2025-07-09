#' @title Performs plotting of privacy preserving survival curves in DataSHIELD. 
#' @description Performs plotting of privacy preserving survival curves. 
#' @details This is a client-side function that plots privacy preserving survival curves in DataSHIELD.
#' 
#' The survival function of multiple servers are aggregated using one of two methods (method_aggregation). The smoothed survival function is weighted by the smoothed counts of persons at risk ('smoothed-weighted') or the survival function is calculated using the smoothed counts of persons at risk and events ('smoothed-counts').
#' 
#' Server function called: \code{plotsurvfitDS}. 
#' 
#' @param formula character string  
#' 	specifying the name of survfit object on the server-side created using ds.survfit().
#' For more information see \strong{Details}. 
#' @param dataName character string of name of data frame
#' @param fun optional parameter to have an argument. For example, you can pass 'cloglog' for a log-log survival plot.
#' @param type an optional character string that represents the type of analysis to carry out. This can be set as 'combined', 'split', or 'both' (default).
#' @param method_aggregation an optional character string that represents the method to combine the separate servers into one plot. Either 'smoothed-weighted' (default) or 'smoothed-counts' (see Details)
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link{datashield.connections_default}}.
#' @param xlab X-axis label, a character string. Default value is ''.
#' @param ylab Y-axis label, a character string. Default value is ''.
#' @return privacy preserving survival curve from the server side environment.
#' @author Soumya Banerjee, Demetris Avraam, Paul Burton, Xavier Escriba-Montagut, Juan Gonzalez and Tom RP Bishop (2022), updated by Nadja Lendle (2025).
#' @examples
#' \dontrun{
#'
#'   ## Version 2.0
#'   
#'   # connecting to the Opal servers
#' 
#'   require('DSI')
#'   require('DSOpal')
#'   require('dsBaseClient')
#'   library(dsSurvivalClient)
#'
#'   builder <- DSI::newDSLoginBuilder()
#'   builder$append(server = "study1", 
#'                  url = "http://192.168.56.100:8080/", 
#'                  user = "administrator", password = "datashield_test&", 
#'                  table = "SURVIVAL.EXPAND_NO_MISSING1", driver = "OpalDriver")
#'   builder$append(server = "study2", 
#'                  url = "http://192.168.56.100:8080/", 
#'                  user = "administrator", password = "datashield_test&", 
#'                  table = "SURVIVAL.EXPAND_NO_MISSING2", driver = "OpalDriver")
#'   builder$append(server = "study3",
#'                  url = "http://192.168.56.100:8080/", 
#'                  user = "administrator", password = "datashield_test&", 
#'                  table = "SURVIVAL.EXPAND_NO_MISSING3", driver = "OpalDriver")
#'   logindata <- builder$build()
#'   
#'   connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D") 
#'   
#'   # make sure that the outcome is numeric 
#'   ds.asNumeric(x.name = "D$cens",
#'             newobj = "EVENT",
#'             datasources = connections)
#'
#'   ds.asNumeric(x.name = "D$survtime",
#'             newobj = "SURVTIME",
#'             datasources = connections)
#'
#'   dsSurvivalClient::ds.Surv(time='SURVTIME', event='EVENT', objectname='surv_object')
#'
#'   dsSurvivalClient::ds.coxph.SLMA(formula = 'surv_object ~  D$female', 
#'             dataName = 'D', datasources = connections)
#'
#'   dsSurvivalClient::ds.survfit(formula='surv_object~1', objectname='survfit_object')
#'
#'   dsSurvivalClient::ds.plotsurvfit(formula = 'survfit_object')
#'
#'   dsSurvivalClient::ds.plotsurvfit(formula = 'survfit_object', fun = 'cloglog')
#'   
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(connections)
#' }
#'
#' @export
ds.plotsurvfit <- function(formula = NULL,
                           dataName = NULL,
                           fun = NULL,
                           type = 'both',
                           method_aggregation = 'smoothed-weighted',
                           datasources = NULL,
                           # method_anonymization = 2,
                           # noise = 0.03,
                           # knn = 20,
			   xlab = '',
			   ylab = '',
			   ggplot = FALSE
			  )
{
  
  # look for DS connections
  # if one not provided then get current
  if(is.null(datasources))
  {
    datasources <- DSI::datashield.connections_find()
  }
  
  # if the argument 'dataName' is set, check that the data frame is defined (i.e. exists) on the server site
  if(!(is.null(dataName)))
  {
    defined <- dsBaseClient::ds.exists(dataName, datasources)
  }
  
  # verify that 'formula' was set
  if(is.null(formula))
  {
    stop(" Please provide a valid survival formula!", call.=FALSE)
  }
  # unify and verify 'type'
  if (type == "combine" | type == "combined" | type == "combines" | 
      type == "c") 
    type <- "combined"
  if (type == "split" | type == "splits" | type == "s") 
    type <- "split"
  if (type == "both" | type == "b") 
    type <- "both"
  if (type != "combined" & type != "split" & type != "both") {
    stop("Function argument \"type\" has to be either \"both\", \"combine\" or \"split\"", 
         call. = FALSE)
  }
  if (method_aggregation != "smoothed-weighted" & method_aggregation != "smoothed-counts"){
    stop("Function argument \"method_aggregation\" has to be either \"smoothed-weighted\" or \"smoothed-counts\"", 
         call. = FALSE)
  }

  # call the server side function
  calltext <- call("plotsurvfitDS", formula=formula, dataName) #, method_anonymization, noise, knn)
  
  # call aggregate function
  output <- DSI::datashield.aggregate(datasources, calltext)

  if((type == 'both' | type == 'combined') & length(output)==1){
    message('\n Type is either \"both\" or \"combined\" but there is only one server to aggregate. Type is set to \"split\".')
    type <- 'split'
  }  
  
  # function to combine outputs of individual servers to one survfit object
  comb_outputs <- function(output_obj){
    # copy first server and remove non-used variables
    output_comb <- output_obj[[1]]
    output_comb$conf.type <- 'none'
    output_comb$conf.int <- FALSE
    for(i in c('std.err', 'cumhaz', 'std.chaz', 'logse', 'na.action')){
      is.na(output_comb[[i]]) <- TRUE
    }
    # start manipulating used variables
    # add sample sizes 'n' and merge time points of individual servers (combine all times, remove duplicates, sort)
    output_comb$n <- 0
    times <- c()
    for(i in 1:length(output_obj)){
      times <- c(times, output_obj[[i]]$time)
      output_comb$n <- output_comb$n + output_obj[[i]]$n
    }
    output_comb$time <- sort(unique(times))
    rm(times)
    
    # set upper and lower confidence intervals to NA
    output_comb$lower <- rep(NA, length(output_comb$time))
    output_comb$upper <- rep(NA, length(output_comb$time))
    
    # add the counts 'n.risk', 'n.event', and 'n.censor' and calculate new 'surv'
      ## first create vector with time to merge on
    n.risk <- data.frame('time' = output_comb$time)
    n.event <- data.frame('time' = output_comb$time)
    n.censor <- data.frame('time' = output_comb$time)
    surv <- data.frame('time' = output_comb$time)
    
      ## for every server: merge data from server to overall dataframe
    for(i in 1:length(output_obj)){
      output_i <- output_obj[[i]]
      n.risk <- merge(n.risk, cbind('time'=output_i$time, output_i$n.risk), by = 'time', all = TRUE, suffixes = c('', i))
      n.event <- merge(n.event, cbind('time'=output_i$time, output_i$n.event), by = 'time', all = TRUE, suffixes = c('', i))
      n.censor <- merge(n.censor, cbind('time'=output_i$time, output_i$n.censor), by = 'time', all = TRUE, suffixes = c('', i))
      if (method_aggregation == 'smoothed-weighted'){
        surv <- merge(surv, cbind('time'=output_i$time, output_i$surv * output_i$n.risk), by = 'time', all = TRUE, suffixes = c('', i))
      }
    }
    
      ## calculate rowsums (without time column).
      ## n.risk: all times not observed at the individual servers are filled with the prior value
      ## and missing values in the beginning (prior to first observation at individual server) are filled by first non-missing values
    output_comb$n.risk <- rowSums(zoo::na.locf(zoo::na.locf(n.risk, na.rm = FALSE), na.rm = FALSE, fromLast = TRUE)[, -1])
      ## for n.event and n.censor missing values are treated as 0 - no events/censoring happened at the individual server.
    output_comb$n.event <- rowSums(n.event[, -1], na.rm = TRUE)
    output_comb$n.censor <- rowSums(n.censor[, -1], na.rm = TRUE)
    if (method_aggregation == 'smoothed-weighted'){
      output_comb$surv <- rowSums(zoo::na.locf(zoo::na.locf(surv, na.rm = FALSE), na.rm = FALSE, fromLast = TRUE)[, -1]) / output_comb$n.risk
    } else if (method_aggregation == 'smoothed-counts'){
      output_comb$surv = cumprod(1-(output_comb$n.event/output_comb$n.risk))
    }
    return(output_comb)
  }
  # for type both add combined output to the list of all outputs
  if(type == 'both'){
    # add one output for all servers combined
    output[[length(output)+1]] <- comb_outputs(output)
    names(output)[length(output)] <- 'all servers'
  } else if ( type == 'combined'){
    # for type combined remove all outputs for single servers and add one output for all servers combined 
    output <- list(comb_outputs(output))
    names(output) <- 'all servers'
  }
  # for type split do not change output at all
  
  # Get the required grid according to the number of servers
  nrows_plot <- ceiling(length(output) / 2)
  if(nrows_plot != 1){graphics::par(mfrow=c(nrows_plot, 2))}
  # Plot for each server
  Map(function(x, n) {
    funct <- eval("fun")
    if (is.null(fun)){	
      funct <- rlang::missing_arg()
    }
    if(ggplot){
      survminer::ggsurvplot(survminer::surv_summary(x, data = 1)) +
        ggplot2::ggtitle(paste0('Survival curve of anonymized data \n [', n, ']'))
    } else {
      if(n=='all servers'){ # differ between combined output vs individual server output to turn off the confidence interval
        graphics::plot(x, 
                       main = paste0("Survival curve of anonymized data \n [", n, "]"), 
                       fun = funct, 
                       conf.type = 'none', 
                       xlab = xlab, 
                       ylab = ylab,
                       ylim = c(0, 1))
      } else {
        graphics::plot(x, 
                       main = paste0("Survival curve of anonymized data \n [", n, "]"), 
                       fun = funct, 
                       xlab = xlab,
                       ylab = ylab,
                       ylim = c(0, 1))
      }
    }
  }, output, names(output)) -> res
	
  # Reset graphic options to not interfere other plots
  graphics::par(mfrow=c(1,1))
  
  # return this privacy preserving plot	
  if(ggplot){
    return(res)
  } else {
    return(output)
  }
  
}
#ds.plotsurvfit
