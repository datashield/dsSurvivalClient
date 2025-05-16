#-------------------------------------------------------------------------------
# Copyright (c) 2025, XXXX
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

context("ds.Predict::smk::setup")

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

context("ds.Predict::smk objectname is NULL")
test_that("fit is NULL",  {

    expect_error(dsSurvivalClient::ds.Predict(fit = NULL), "Please provide a valid fitted model object name")
})

context("ds.Predict::smk simple usecase")
test_that("simple usecase", {
    ds.Surv(time = "D$starttime", time2 = "D$endtime", event = "D$cens", objectname = "surv_object", type = "counting")

    res.surv.classes <- ds.class("surv_object")

    expect_length(res.surv.classes, 3)
    expect_equal(res.surv.classes$survival1, 'Surv')
    expect_equal(res.surv.classes$survival2, 'Surv')
    expect_equal(res.surv.classes$survival3, 'Surv')

    ds.survfit(formula = "surv_object~1", objectname = "fit_surv_object")

    res.fix.surv.classes <- ds.class("fit_surv_object")

    expect_length(res.fix.surv.classes, 3)
    expect_true(all(res.fix.surv.classes$survival1 %in% c('survfitms', 'survfit')))
    expect_true(all(res.fix.surv.classes$survival2 %in% c('survfitms', 'survfit')))
    expect_true(all(res.fix.surv.classes$survival3 %in% c('survfitms', 'survfit')))

    # TODO: correct
    res <- expect_error(expect_warning(ds.Predict(fit = "fit_surv_object", age = 30:70, sex = "both", conf.int = 0.95, ref.zero = TRUE, objectname = NULL)))

    print(datashield.errors())

    expect_null(res)
})

context("ds.Predict::smk simple usecase")
test_that("simple usecase", {
    ds.Surv(time = "D$starttime", time2 = "D$endtime", event = "D$cens", objectname = "surv_object", type = "counting")

    res.surv.classes <- ds.class("surv_object")

    expect_length(res.surv.classes, 3)
    expect_equal(res.surv.classes$survival1, 'Surv')
    expect_equal(res.surv.classes$survival2, 'Surv')
    expect_equal(res.surv.classes$survival3, 'Surv')

    ds.survfit(formula = "surv_object~1", objectname = "fit_surv_object")

    res.fix.surv.classes <- ds.class("fit_surv_object")

    expect_length(res.fix.surv.classes, 3)
    expect_true(all(res.fix.surv.classes$survival1 %in% c('survfitms', 'survfit')))
    expect_true(all(res.fix.surv.classes$survival2 %in% c('survfitms', 'survfit')))
    expect_true(all(res.fix.surv.classes$survival3 %in% c('survfitms', 'survfit')))

    # TODO: correct
    res <- expect_error(ds.Predict(fit = "fit_surv_object", age = 30:70, sex = "both", conf.int = 0.95, ref.zero = TRUE, objectname = "predictions"))

    print(datashield.errors())

    expect_null(res)
})

#
# Done
#

context("ds.Predict::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "surv_object", "fit_surv_object"))
})

# disconnect.studies.dataset.d()
disconnect.studies.dataset.survival()

context("ds.Predict::smk::done")
