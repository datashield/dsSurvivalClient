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

context("ds.rmsTrans::smk::setup")

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

context("ds.rmsTrans::smk simple example, rcs")
test_that("simple example, rcs",  {
    x_arg              <- "D$age.60"
    transformation_arg <- "rcs"
    parm_arg           <- NULL
    objectname_arg     <- "rms_obj"

    dsSurvivalClient::ds.rmsTrans(x = x_arg, transformation = transformation_arg, parm = parm_arg, objectname = objectname_arg)

    res_classes <- ds.class("rms_obj")
    expect_length(res_classes, 3)
    expect_length(res_classes$survival1, 1)
    expect_true(all(c("rms") %in% res_classes$survival1))
    expect_length(res_classes$survival2, 1)
    expect_true(all(c("rms") %in% res_classes$survival2))
    expect_length(res_classes$survival3, 1)
    expect_true(all(c("rms") %in% res_classes$survival3))
})

context("ds.rmsTrans::smk simple example, asis")
test_that("simple example, asis",  {
    x_arg              <- "D$age.60"
    transformation_arg <- "asis"
    parm_arg           <- NULL
    objectname_arg     <- "rms_obj"

    dsSurvivalClient::ds.rmsTrans(x = x_arg, transformation = transformation_arg, parm = parm_arg, objectname = objectname_arg)

    res_classes <- ds.class("rms_obj")
    expect_length(res_classes, 3)
    expect_length(res_classes$survival1, 1)
    expect_true(all(c("rms") %in% res_classes$survival1))
    expect_length(res_classes$survival2, 1)
    expect_true(all(c("rms") %in% res_classes$survival2))
    expect_length(res_classes$survival3, 1)
    expect_true(all(c("rms") %in% res_classes$survival3))
})

context("ds.rmsTrans::smk simple example, pol")
test_that("simple example, pol",  {
    x_arg              <- "D$age.60"
    transformation_arg <- "pol"
    parm_arg           <- NULL
    objectname_arg     <- "rms_obj"

    dsSurvivalClient::ds.rmsTrans(x = x_arg, transformation = transformation_arg, parm = parm_arg, objectname = objectname_arg)

    res_classes <- ds.class("rms_obj")
    expect_length(res_classes, 3)
    expect_length(res_classes$survival1, 1)
    expect_true(all(c("rms") %in% res_classes$survival1))
    expect_length(res_classes$survival2, 1)
    expect_true(all(c("rms") %in% res_classes$survival2))
    expect_length(res_classes$survival3, 1)
    expect_true(all(c("rms") %in% res_classes$survival3))
})

context("ds.rmsTrans::smk simple example, lsp")
test_that("simple example, lsp",  {
    x_arg              <- "D$age.60"
    transformation_arg <- "lsp"
    parm_arg           <- 6
    objectname_arg     <- "rms_obj"

    dsSurvivalClient::ds.rmsTrans(x = x_arg, transformation = transformation_arg, parm = parm_arg, objectname = objectname_arg)

    res_classes <- ds.class("rms_obj")
    expect_length(res_classes, 3)
    expect_length(res_classes$survival1, 1)
    expect_true(all(c("rms") %in% res_classes$survival1))
    expect_length(res_classes$survival2, 1)
    expect_true(all(c("rms") %in% res_classes$survival2))
    expect_length(res_classes$survival3, 1)
    expect_true(all(c("rms") %in% res_classes$survival3))
})

context("ds.rmsTrans::smk simple example, catg")
test_that("simple example, catg",  {
    x_arg              <- "D$age.60"
    transformation_arg <- "catg"
    parm_arg           <- NULL
    objectname_arg     <- "rms_obj"

    dsSurvivalClient::ds.rmsTrans(x = x_arg, transformation = transformation_arg, parm = parm_arg, objectname = objectname_arg)

    res_classes <- ds.class("rms_obj")
    expect_length(res_classes, 3)
    expect_length(res_classes$survival1, 2)
    expect_true(all(c("factor", "rms") %in% res_classes$survival1))
    expect_length(res_classes$survival2, 2)
    expect_true(all(c("factor", "rms") %in% res_classes$survival2))
    expect_length(res_classes$survival3, 2)
    expect_true(all(c("factor", "rms") %in% res_classes$survival3))
})

context("ds.rmsTrans::smk simple example, scored")
test_that("simple example, scored",  {
    x_arg              <- "D$age.60"
    transformation_arg <- "scored"
    parm_arg           <- NULL
    objectname_arg     <- "rms_obj"

    dsSurvivalClient::ds.rmsTrans(x = x_arg, transformation = transformation_arg, parm = parm_arg, objectname = objectname_arg)

    res_classes <- ds.class("rms_obj")
    expect_length(res_classes, 3)
    expect_length(res_classes$survival1, 3)
    expect_true(all(c("ordered", "factor", "rms") %in% res_classes$survival1))
    expect_length(res_classes$survival2, 3)
    expect_true(all(c("ordered", "factor", "rms") %in% res_classes$survival2))
    expect_length(res_classes$survival3, 3)
    expect_true(all(c("ordered", "factor", "rms") %in% res_classes$survival3))
})

context("ds.rmsTrans::smk simple example, strat")
test_that("simple example, strat",  {
    x_arg              <- "D$age.60"
    transformation_arg <- "strat"
    parm_arg           <- NULL
    objectname_arg     <- "rms_obj"

    dsSurvivalClient::ds.rmsTrans(x = x_arg, transformation = transformation_arg, parm = parm_arg, objectname = objectname_arg)

    res_classes <- ds.class("rms_obj")
    expect_length(res_classes, 3)
    expect_length(res_classes$survival1, 2)
    expect_true(all(c("factor", "rms") %in% res_classes$survival1))
    expect_length(res_classes$survival2, 2)
    expect_true(all(c("factor", "rms") %in% res_classes$survival2))
    expect_length(res_classes$survival3, 2)
    expect_true(all(c("factor", "rms") %in% res_classes$survival3))
})

context("ds.rmsTrans::smk simple example, gTrans")
test_that("simple example, gTrans",  {
    x_arg              <- "D$age.60"
    transformation_arg <- "gTrans"
    parm_arg           <- "exp"
    objectname_arg     <- "rms_obj"

    dsSurvivalClient::ds.rmsTrans(x = x_arg, transformation = transformation_arg, parm = parm_arg, objectname = objectname_arg)

    res_classes <- ds.class("rms_obj")
    expect_length(res_classes, 3)
    expect_length(res_classes$survival1, 1)
    expect_true(all(c("rms") %in% res_classes$survival1))
    expect_length(res_classes$survival2, 1)
    expect_true(all(c("rms") %in% res_classes$survival2))
    expect_length(res_classes$survival3, 1)
    expect_true(all(c("rms") %in% res_classes$survival3))
})

#
# Done
#

context("ds.rmsTrans::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "rms_obj"))
})

# disconnect.studies.dataset.d()
disconnect.studies.dataset.survival()

context("ds.rmsTrans::smk::done")
