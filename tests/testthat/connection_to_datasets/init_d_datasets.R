init.studies.dataset.d <- function(variables)
{
    if (ds.test_env$secure_login_details)
    {
      if (ds.test_env$driver == "OpalDriver")
      {
        builder <- DSI::newDSLoginBuilder(.silent = TRUE)
        builder$append(server = "study1", url = ds.test_env$ip_address_1, user = ds.test_env$user_1, password = ds.test_env$password_1, table = "D.D1", options=ds.test_env$options_1)
        ds.test_env$login.data <- builder$build()
      }
      else 
      {
        builder <- DSI::newDSLoginBuilder(.silent = TRUE)
        builder$append(server = "study1", url = ds.test_env$ip_address_1, user = ds.test_env$user_1, password = ds.test_env$password_1, table = "D.D1", driver = ds.test_env$driver, options=ds.test_env$options_1)
        ds.test_env$login.data <- builder$build()
        ds.test_env$login.data <- DSLite::setupDSLiteServer(packages = c(), datasets = c("D1"), logindata = "logindata.dslite.d", pkgs = "dsSurvivalClient", dslite.server = "dslite.server", env = ds.test_env)
      }
      ds.test_env$stats.var <- variables
    }
}

connect.studies.dataset.d <- function(variables)
{
    log.out.data.server()
    source("connection_to_datasets/login_details.R")
    init.studies.dataset.d(variables)
    log.in.data.server()
}

disconnect.studies.dataset.d <- function()
{
    log.out.data.server()
}
