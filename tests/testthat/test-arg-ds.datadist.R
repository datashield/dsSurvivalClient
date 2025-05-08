#-------------------------------------------------------------------------------
# Copyright (c) 2025 XXXX. All rights reserved.
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

context("ds.datadist::arg::setup")

# load "d" test data set
# connect.studies.dataset.d(list('ID', 'age', 'sex', 'smoke', 'fruit', 'veg', 'edu', 'eth', 'job', 'slf_hlth', 'alc', 'mobility', 'fasting', 'med_lipid', 'med_bp', 'med_glucose', 'prev_cvd', 'prev_ht', 'prev_bronchitis', 'body_fat_percent'))
# load "survival" test data set
connect.studies.dataset.survival(list('id', 'study.id', 'time.id', 'starttime', 'endtime', 'survtime', 'cens', 'age.60', 'female', 'noise.56', 'pm10.16', 'bmi.26'))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.datadist::arg empty arguments")
test_that("empty arguments",  {
 
    expect_error(dsSurvivalClient::ds.datadist(), "Please provide the name of a data frame in the 'data' parameter")
})

context("ds.datadist::arg objectname is NULL, adjust_to not list")
test_that("objectname is NULL, adjust_to not list",  {
 
    expect_error(expect_warning(dsSurvivalClient::ds.datadist(data = "D", adjust_to = 1.0, objectname = NULL), "No objectname provided, using default: datadist_D"), "adjust_to must be a named list")
})

context("ds.datadist::arg objectname is not NULL, adjust_to not list")
test_that("objectname is not NULL, adjust_to not list",  {
 
    expect_error(dsSurvivalClient::ds.datadist(data = "D", adjust_to = 1.0, objectname = "test"), "adjust_to must be a named list")
})

context("ds.datadist::arg objectname is not NULL, adjust_to not valid 1")
test_that("objectname is not NULL, adjust_to not valid 1",  {

    expect_error(dsSurvivalClient::ds.datadist(data = "D", adjust_to = list("a"), objectname = "test"), "all elements in adjust_to must be named")
})

context("ds.datadist::arg objectname is not NULL, adjust_to not valid 2")
test_that("objectname is not NULL, adjust_to not valid 2",  {

    expect_error(dsSurvivalClient::ds.datadist(data = "D", adjust_to = list(a="a", "b"), objectname = "test"), "all elements in adjust_to must be named")
})

context("ds.datadist::arg objectname is not NULL, adjust_to not valid 3")
test_that("objectname is not NULL, adjust_to not valid 3",  {
 
    expect_error(dsSurvivalClient::ds.datadist(data = "D", adjust_to = list(a="a"), objectname = "test"), "adjust_to values must be either 'min', 'max'. or 'mean'")
})

#
# Done
#

context("ds.datadist::arg::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

# disconnect.studies.dataset.d()
disconnect.studies.dataset.survival()

context("ds.datadist::arg::done")
