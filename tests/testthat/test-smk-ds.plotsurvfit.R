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

context("ds.plotsurvfit::smk::setup")

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

context("ds.plotsurvfit::smk simple example")
test_that("simple example",  {

    ds.asNumeric(x.name = "D$cens", newobj = "EVENT")

    ds.asNumeric(x.name = "D$survtime", newobj = "SURVTIME")

    dsSurvivalClient::ds.Surv(time='SURVTIME', event='EVENT', objectname='surv_object')

    dsSurvivalClient::ds.coxph.SLMA(formula = 'surv_object ~  D$female', dataName = 'D')

    dsSurvivalClient::ds.survfit(formula = 'surv_object~1', objectname = 'survfit_object')

    res <- dsSurvivalClient::ds.plotsurvfit(formula = 'survfit_object')

    expect_length(res, 3)
})

#
# Done
#

context("ds.plotsurvfit::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "EVENT", "SURVTIME", "surv_object", "survfit_object"))
})

# disconnect.studies.dataset.d()
disconnect.studies.dataset.survival()

context("ds.plotsurvfit::smk::done")
