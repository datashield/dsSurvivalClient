#-------------------------------------------------------------------------------
# Copyright (c) 2019-2020 University of Newcastle upon Tyne. All rights reserved.
# Copyright (c) 2025 ProPASS Consortium. All rights reserved.
#
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#
# Set up
#

context("ds.vcov.coxph::arg::setup")

# load survival expand no missing data
connect.studies.dataset.survival_nomissing(list("cens","survtime","time.id","female","age.60","starttime","endtime"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#connect.studies.dataset.survival(list("D"))
##init.studies.dataset.survival(list("D"))
#init.studies.dataset.survival_nomissing(list("D"))
##connect.studies.dataset.dasim(c("SURVTIME"))

######################################
# add server side survival variables
######################################
# add survival related server side variables like SURVTIME, etc.
#   need to convert these to numeric and create server side
#   variables
# enure that objects have been added
test_that("variable presence checks", {
    ls_object <- add_server_side_var_survival()

    ds_expect_variables(c("AGE", "D", "ENDTIME", "EVENT", "STARTTIME", "SURVTIME"))
})

#
# Tests
#

context("ds.vcov.coxph::arg::test errors")
    
test_that("vcovcoxph_errors", {
        
        expect_error(dsSurvivalClient::ds.vcov.coxph(), " Please provide a valid name for a server-side Cox proportional hazards model that has been fitted to data !", fixed=TRUE)
        
        expect_error(dsSurvivalClient::ds.vcov.coxph("D"), "There are some DataSHIELD errors, list them with datashield.errors()")
        }
    )

#
# Done
#

context("ds.vcov.coxph::arg::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("AGE", "D", "ENDTIME", "EVENT", "STARTTIME", "SURVTIME"))
})

disconnect.studies.dataset.survival()

context("ds.coxphSummary::arg::done")
