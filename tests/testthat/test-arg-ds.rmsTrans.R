#-------------------------------------------------------------------------------
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

context("ds.rmsTrans::arg::setup")

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

context("ds.rmsTrans::arg x is NULL")
test_that("x is NULL",  {
 
    expect_error(dsSurvivalClient::ds.rmsTrans(x = NULL), "Please provide a valid variable name to transform")
})

context("ds.rmsTrans::arg x is not NULL, objectname is NULL")
test_that("x is not NULL, objectname is NULL",  {
 
    expect_error(dsSurvivalClient::ds.rmsTrans(x = "", objectname = NULL), "Please provide a valid objectname to store the transformed variable")
})

context("ds.rmsTrans::arg invalid transformation")
test_that("invalid transformation",  {
    x_arg              <- "D$age.60"
    transformation_arg <- "unknown"
    parm_arg           <- NULL
    objectname_arg     <- "rms_obj"

    expect_error(dsSurvivalClient::ds.rmsTrans(x = x_arg, transformation = transformation_arg, parm = parm_arg, objectname = objectname_arg))
})

#
# Done
#

context("ds.rmsTrans::arg::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

# disconnect.studies.dataset.d()
disconnect.studies.dataset.survival()

context("ds.rmsTrans::arg::done")
