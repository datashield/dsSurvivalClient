#'
#' @title Distributed Cox Proportional Hazards Model
#' @description This function runs a distributed Cox Proportional Hazards (PH) Model 
#' based on the WebDISCO algorithm 
#' @details For additional details of the WebDISCO algorithm see the paper: 
#' (\url{https://pmc.ncbi.nlm.nih.gov/articles/PMC5009917/}). The R code was adopted 
#' from vantage6 and converted to a DataSHIELD function.
#' @param df the name of the data frame that contains the variables to be used 
#' in the Cox PH model.
#' @param expl_vars list of the explanatory variables (covariates) that are
#' included as columns in \code{df}
#' @param time_col name of the column in \code{df} that contains the event/censor times
#' @param censor_col name of the column in \code{df} that explains whether an event 
#' occurred or the patient was censored
#' @param datasources  a list of \code{\link{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link{datashield.connections_default}}.
#' @return a dataframe with the regression results. These include the estimated coefficients (beta),
#' their exponents, their standard errors, the upper and lower limits of the 95% confidence intervals,
#' and the z and p values.
#' @author Demetris Avraam
#' @export
#'
ds.coxph <- function(df, expl_vars, time_col, censor_col, datasources=NULL){
  
  # Compute the primary derivative (vector of length beta)
  compute.derivatives <- function(z_hat, D_all, aggregates) {
    
    # Sum the aggregate statistics for each site
    for (k in 1:length(aggregates)) {
      if (k == 1) {
        summed_agg1 <- aggregates[[k]]$agg1
        summed_agg2 <- aggregates[[k]]$agg2
        summed_agg3 <- aggregates[[k]]$agg3
      } else {
        summed_agg1 <- summed_agg1 + aggregates[[k]]$agg1
        summed_agg2 <- summed_agg2 + aggregates[[k]]$agg2
        summed_agg3 <- summed_agg3 + aggregates[[k]]$agg3
      }
    }
    
    # summed_agg1: vector with scalar for each time point
    # summed_agg2: vector of length m for each time point
    # summed_agg3: matrix of size (m x m) for each time point
    for (i in 1:length(D_all)) {
      # primary
      s1 <- D_all[[i]] * (summed_agg2[i, ] / summed_agg1[i])
      
      # secondary
      first_part <- (summed_agg3[i, , ] / summed_agg1[i])
      
      # the numerator is the outer product of agg2
      numerator <- summed_agg2[i, ] %*% t(summed_agg2[i, ])
      denominator <- summed_agg1[i] * summed_agg1[i]
      second_part <- numerator / denominator
      
      s2 <- D_all[[i]] * (first_part - second_part)
      
      if (i == 1) {
        total_p1 <- s1
        total_p2 <- s2
        
      } else {
        total_p1 <- total_p1 + s1
        total_p2 <- total_p2 + s2
      }
    }
    
    primary_derivative <- z_hat - total_p1
    secondary_derivative <- -total_p2
    
    return(list(
      primary <- primary_derivative,
      secondary <- secondary_derivative
    ))
  }
  
  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }
  
  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }
  
  # number of covariates
  m <- length(expl_vars)
  
  # Call all nodes to return their unique event times with counts
  calltext <- call("coxphDS1",  df, time_col, censor_col)
  out1 <- DSI::datashield.aggregate(datasources, calltext)
  
  # convert each matrix from list "out1" to a dataframe and call the new list as "Ds"
  Ds <- lapply(out1, as.data.frame)
  
  # Compute combined ties
  
  # Merge the list of event times & counts from all studies into a single data frame
  for (k in 1:length(Ds)) {
    # This only works if the joined columns have different names
    site_name <- sprintf("site_%i", k)
    colnames(Ds[[k]]) <- c("time", site_name)
    
    if (k == 1) {
      D <- Ds[[k]]
    } else {
      D <- merge(D, Ds[[k]], by="time", all=T)
    }
  }
  
  # convert the "time" column to numeric to enable proper sorting
  D[, "time"] <- as.numeric(D$time)
  D <- D[order(D$time), ]
  
  # Set the "time" column as the index
  rownames(D) <- D$time
  
  # Drop the "time" column; drop=F ensures that the data frame is not
  # coerced to a vector in case of a single column.
  D <- D[, -1, drop=F]
  
  # The merge/join will have introduced NAs: set these to 0
  D[is.na(D)] <- 0
  
  # Sum the columns to get the total nr of ties for each time
  D_all <- rowSums(D)
  
  unique_event_times <- as.numeric(names(D_all))
  unique_event_times <- paste0(as.character(unique_event_times), collapse=",")
  
  expl_vars.tr <- paste0(as.character(expl_vars), collapse=",")
  
  # Call all nodes to compute the summed Z statistic
  calltext <- call("coxphDS2",  df, expl_vars.tr, time_col, censor_col)
  summed_zs <- DSI::datashield.aggregate(datasources, calltext)
  
  # z_hat: vector of same length m
  z_hat <- t(sapply(summed_zs, c))
  z_hat <- apply(z_hat, 2, as.numeric)
  z_hat <- matrix(z_hat, ncol=m, dimnames=list(NULL, expl_vars))
  z_hat <- colSums(z_hat)
  
  # Initialize the betas to 0 and start iterating
  beta <- beta_old <- rep(0, m)
  delta <- 0
  
  i = 1
  while (i <= 30) {

    beta.tr <- paste0(as.character(as.numeric(beta)), collapse=",")
    
    calltext <- call("coxphDS3",  df, expl_vars.tr, time_col, censor_col, beta.tr, unique_event_times)
    aggregates <- DSI::datashield.aggregate(datasources, calltext)
    
    # Compute the primary and secondary derivatives
    derivatives <- compute.derivatives(z_hat, D_all, aggregates)

    # Update the betas
    beta_old <- beta
    beta <- beta_old - (solve(derivatives$secondary) %*% derivatives$primary)
    
    delta <- abs(sum(beta - beta_old))
    
    if (is.na(delta)) {
      break
    }
    
    if (delta <= 10^-10) {
      break
    }
    
    i <- i + 1
    
  }
  
  # Computing the standard errors
  SErrors <- NULL
  fisher <- solve(-derivatives$secondary)
  
  # Standard errors are the squared root of the diagonal
  for(k in 1:dim(fisher)[1]){
    se_k <- sqrt(fisher[k,k])
    SErrors <- c(SErrors, se_k)
  }
  
  # Calculate the z-values
  zvalues <- beta/SErrors
  pvalues <- 2*stats::pnorm(-abs(zvalues))
  pvalues <- format.pval(pvalues, digits = 3)
  
  # Calculate the 95% CI = beta +- 1.96 * SE
  results <- data.frame("coef"=round(beta,5), "exp(coef)"=round(exp(beta), 5), "se(coef)"=round(SErrors,5))
  results <- dplyr::mutate(results, lower_ci=round(exp(beta - 1.96 * SErrors), 5))
  results <- dplyr::mutate(results, upper_ci=round(exp(beta + 1.96 * SErrors), 5))
  results <- dplyr::mutate(results, "z"=round(zvalues, 3), "p"=pvalues)
  row.names(results) <- rownames(beta)
  
  # return the results
  return(results)
  
}
