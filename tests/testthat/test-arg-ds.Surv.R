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

context("ds.Surv::arg::setup")

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

context("ds.Surv::arg time is NULL")
test_that("time is NULL",  {
 
    expect_error(dsSurvivalClient::ds.Surv(time = NULL), " Please provide a valid survival start time or follow-up time parameter")
})

context("ds.Surv::arg event is NULL")
test_that("event is NULL",  {
 
    expect_error(dsSurvivalClient::ds.Surv(time = 0, event = NULL), " Please provide a valid survival event parameter")
})

context("ds.Surv::arg objectname is NULL")
test_that("objectname is NULL",  {
 
    expect_error(dsSurvivalClient::ds.Surv(time = 0, event = 0, objectname = NULL), " Please provide a valid objectname to store the server-side survival::Surv() object", fixed = TRUE)
})

#
# Done
#

context("ds.Surv::arg::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

# disconnect.studies.dataset.d()
disconnect.studies.dataset.survival()

context("ds.Surv::arg::done")
