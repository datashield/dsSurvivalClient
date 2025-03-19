#-------------------------------------------------------------------------------
# Copyright (c) 2025 XXXX. All rights reserved.
#-------------------------------------------------------------------------------

#
# Set up
#

context("D::datachk::setup")

connect.studies.dataset.d(list('ID', 'age', 'sex', 'smoke', 'fruit', 'veg', 'edu', 'eth', 'job', 'slf_hlth', 'alc', 'mobility', 'fasting', 'med_lipid', 'med_bp', 'med_glucose', 'prev_cvd', 'prev_ht', 'prev_bronchitis', 'body_fat_percent'))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("D::datachk")
test_that("Check D dataset", {
    res.class <- ds.class(x='D')
    expect_length(res.class, 1)
    expect_length(res.class$study1, 1)
    expect_equal(res.class$study1, "data.frame")

    res.length <- ds.length(x='D')
    expect_length(res.length, 2)
    expect_length(res.length$`length of D in study1`, 1)
    expect_equal(res.length$`length of D in study1`, 20)
    expect_equal(res.length$`total length of D in all studies combined`, 20)

    res.colnames <- ds.colnames(x='D')
    expect_length(res.colnames, 1)
    expect_length(res.colnames$study1, 20)
    expect_equal(res.colnames$study1, c('ID', 'age', 'sex', 'smoke', 'fruit', 'veg', 'edu', 'eth', 'job', 'slf_hlth', 'alc', 'mobility', 'fasting', 'med_lipid', 'med_bp', 'med_glucose', 'prev_cvd', 'prev_ht', 'prev_bronchitis', 'body_fat_percent'))

    res.class.id <- ds.class(x='D$ID')
    expect_length(res.class.id, 1)
    expect_length(res.class.id$study1, 1)
    expect_equal(res.class.id$study1, "numeric")

    res.length.id <- ds.length(x='D$ID')
    expect_length(res.length.id, 2)
    expect_length(res.length.id$`length of D$ID in study1`, 1)
    expect_equal(res.length.id$`length of D$ID in study1`, 537)
    expect_length(res.length.id$`total length of D$ID in all studies combined`, 1)
    expect_equal(res.length.id$`total length of D$ID in all studies combined`, 537)

    res.class.age <- ds.class(x='D$age')
    expect_length(res.class.age, 1)
    expect_length(res.class.age$study1, 1)
    expect_equal(res.class.age$study1, "numeric")

    res.length.age <- ds.length(x='D$age')
    expect_length(res.length.age, 2)
    expect_length(res.length.age$`length of D$age in study1`, 1)
    expect_equal(res.length.age$`length of D$age in study1`, 537)
    expect_length(res.length.age$`total length of D$age in all studies combined`, 1)
    expect_equal(res.length.age$`total length of D$age in all studies combined`, 537)

    res.class.sex <- ds.class(x='D$sex')
    expect_length(res.class.sex, 1)
    expect_length(res.class.sex$study1, 1)
    expect_equal(res.class.sex$study1, "factor")

    res.length.sex <- ds.length(x='D$sex')
    expect_length(res.length.sex, 2)
    expect_length(res.length.sex$`length of D$sex in study1`, 1)
    expect_equal(res.length.sex$`length of D$sex in study1`, 537)
    expect_length(res.length.sex$`total length of D$sex in all studies combined`, 1)
    expect_equal(res.length.sex$`total length of D$sex in all studies combined`, 537)

    res.class.smoke <- ds.class(x='D$smoke')
    expect_length(res.class.smoke, 1)
    expect_length(res.class.smoke$study1, 1)
    expect_equal(res.class.smoke$study1, "character")

    res.length.smoke <- ds.length(x='D$smoke')
    expect_length(res.length.smoke, 2)
    expect_length(res.length.smoke$`length of D$smoke in study1`, 1)
    expect_equal(res.length.smoke$`length of D$smoke in study1`, 537)
    expect_length(res.length.smoke$`total length of D$smoke in all studies combined`, 1)
    expect_equal(res.length.smoke$`total length of D$smoke in all studies combined`, 537)

    res.class.fruit <- ds.class(x='D$fruit')
    expect_length(res.class.fruit, 1)
    expect_length(res.class.fruit$study1, 1)
    expect_equal(res.class.fruit$study1, "character")

    res.length.fruit <- ds.length(x='D$fruit')
    expect_length(res.length.fruit, 2)
    expect_length(res.length.fruit$`length of D$fruit in study1`, 1)
    expect_equal(res.length.fruit$`length of D$fruit in study1`, 537)
    expect_length(res.length.fruit$`total length of D$fruit in all studies combined`, 1)
    expect_equal(res.length.fruit$`total length of D$fruit in all studies combined`, 537)

    res.class.veg <- ds.class(x='D$veg')
    expect_length(res.class.veg, 1)
    expect_length(res.class.veg$study1, 1)
    expect_equal(res.class.veg$study1, "character")

    res.length.veg <- ds.length(x='D$veg')
    expect_length(res.length.veg, 2)
    expect_length(res.length.veg$`length of D$veg in study1`, 1)
    expect_equal(res.length.veg$`length of D$veg in study1`, 537)
    expect_length(res.length.veg$`total length of D$veg in all studies combined`, 1)
    expect_equal(res.length.veg$`total length of D$veg in all studies combined`, 537)

    res.class.edu <- ds.class(x='D$edu')
    expect_length(res.class.edu, 1)
    expect_length(res.class.edu$study1, 1)
    expect_equal(res.class.edu$study1, "character")

    res.length.edu <- ds.length(x='D$edu')
    expect_length(res.length.edu, 2)
    expect_length(res.length.edu$`length of D$edu in study1`, 1)
    expect_equal(res.length.edu$`length of D$edu in study1`, 537)
    expect_length(res.length.edu$`total length of D$edu in all studies combined`, 1)
    expect_equal(res.length.edu$`total length of D$edu in all studies combined`, 537)

    res.class.eth <- ds.class(x='D$eth')
    expect_length(res.class.eth, 1)
    expect_length(res.class.eth$study1, 1)
    expect_equal(res.class.eth$study1, "character")

    res.length.eth <- ds.length(x='D$eth')
    expect_length(res.length.eth, 2)
    expect_length(res.length.eth$`length of D$eth in study1`, 1)
    expect_equal(res.length.eth$`length of D$eth in study1`, 537)
    expect_length(res.length.eth$`total length of D$eth in all studies combined`, 1)
    expect_equal(res.length.eth$`total length of D$eth in all studies combined`, 537)

    res.class.job <- ds.class(x='D$job')
    expect_length(res.class.job, 1)
    expect_length(res.class.job$study1, 1)
    expect_equal(res.class.job$study1, "character")

    res.length.job <- ds.length(x='D$job')
    expect_length(res.length.job, 2)
    expect_length(res.length.job$`length of D$job in study1`, 1)
    expect_equal(res.length.job$`length of D$job in study1`, 537)
    expect_length(res.length.job$`total length of D$job in all studies combined`, 1)
    expect_equal(res.length.job$`total length of D$job in all studies combined`, 537)

    res.class.slf_hlth <- ds.class(x='D$slf_hlth')
    expect_length(res.class.slf_hlth, 1)
    expect_length(res.class.slf_hlth$study1, 1)
    expect_equal(res.class.slf_hlth$study1, "factor")

    res.length.slf_hlth <- ds.length(x='D$slf_hlth')
    expect_length(res.length.slf_hlth, 2)
    expect_length(res.length.slf_hlth$`length of D$slf_hlth in study1`, 1)
    expect_equal(res.length.slf_hlth$`length of D$slf_hlth in study1`, 537)
    expect_length(res.length.slf_hlth$`total length of D$slf_hlth in all studies combined`, 1)
    expect_equal(res.length.slf_hlth$`total length of D$slf_hlth in all studies combined`, 537)

    res.class.alc <- ds.class(x='D$alc')
    expect_length(res.class.alc, 1)
    expect_length(res.class.alc$study1, 1)
    expect_equal(res.class.alc$study1, "character")

    res.length.alc <- ds.length(x='D$alc')
    expect_length(res.length.alc, 2)
    expect_length(res.length.alc$`length of D$alc in study1`, 1)
    expect_equal(res.length.alc$`length of D$alc in study1`, 537)
    expect_length(res.length.alc$`total length of D$alc in all studies combined`, 1)
    expect_equal(res.length.alc$`total length of D$alc in all studies combined`, 537)

    res.class.mobility <- ds.class(x='D$mobility')
    expect_length(res.class.mobility, 1)
    expect_length(res.class.mobility$study1, 1)
    expect_equal(res.class.mobility$study1, "numeric")

    res.length.mobility <- ds.length(x='D$mobility')
    expect_length(res.length.mobility, 2)
    expect_length(res.length.mobility$`length of D$mobility in study1`, 1)
    expect_equal(res.length.mobility$`length of D$mobility in study1`, 537)
    expect_length(res.length.mobility$`total length of D$mobility in all studies combined`, 1)
    expect_equal(res.length.mobility$`total length of D$mobility in all studies combined`, 537)

    res.class.fasting <- ds.class(x='D$fasting')
    expect_length(res.class.fasting, 1)
    expect_length(res.class.fasting$study1, 1)
    expect_equal(res.class.fasting$study1, "numeric")

    res.length.fasting <- ds.length(x='D$fasting')
    expect_length(res.length.fasting, 2)
    expect_length(res.length.fasting$`length of D$fasting in study1`, 1)
    expect_equal(res.length.fasting$`length of D$fasting in study1`, 537)
    expect_length(res.length.fasting$`total length of D$fasting in all studies combined`, 1)
    expect_equal(res.length.fasting$`total length of D$fasting in all studies combined`, 537)

    res.class.med_lipid <- ds.class(x='D$med_lipid')
    expect_length(res.class.med_lipid, 1)
    expect_length(res.class.med_lipid$study1, 1)
    expect_equal(res.class.med_lipid$study1, "numeric")

    res.length.med_lipid <- ds.length(x='D$med_lipid')
    expect_length(res.length.med_lipid, 2)
    expect_length(res.length.med_lipid$`length of D$med_lipid in study1`, 1)
    expect_equal(res.length.med_lipid$`length of D$med_lipid in study1`, 537)
    expect_length(res.length.med_lipid$`total length of D$med_lipid in all studies combined`, 1)
    expect_equal(res.length.med_lipid$`total length of D$med_lipid in all studies combined`, 537)

    res.class.med_bp <- ds.class(x='D$med_bp')
    expect_length(res.class.med_bp, 1)
    expect_length(res.class.med_bp$study1, 1)
    expect_equal(res.class.med_bp$study1, "numeric")

    res.length.med_bp <- ds.length(x='D$med_bp')
    expect_length(res.length.med_bp, 2)
    expect_length(res.length.med_bp$`length of D$med_bp in study1`, 1)
    expect_equal(res.length.med_bp$`length of D$med_bp in study1`, 537)
    expect_length(res.length.med_bp$`total length of D$med_bp in all studies combined`, 1)
    expect_equal(res.length.med_bp$`total length of D$med_bp in all studies combined`, 537)

    res.class.med_glucose <- ds.class(x='D$med_glucose')
    expect_length(res.class.med_glucose, 1)
    expect_length(res.class.med_glucose$study1, 1)
    expect_equal(res.class.med_glucose$study1, "numeric")

    res.length.med_glucose <- ds.length(x='D$med_glucose')
    expect_length(res.length.med_glucose, 2)
    expect_length(res.length.med_glucose$`length of D$med_glucose in study1`, 1)
    expect_equal(res.length.med_glucose$`length of D$med_glucose in study1`, 537)
    expect_length(res.length.med_glucose$`total length of D$med_glucose in all studies combined`, 1)
    expect_equal(res.length.med_glucose$`total length of D$med_glucose in all studies combined`, 537)

    res.class.prev_cvd <- ds.class(x='D$prev_cvd')
    expect_length(res.class.prev_cvd, 1)
    expect_length(res.class.prev_cvd$study1, 1)
    expect_equal(res.class.prev_cvd$study1, "factor")

    res.length.prev_cvd <- ds.length(x='D$prev_cvd')
    expect_length(res.length.prev_cvd, 2)
    expect_length(res.length.prev_cvd$`length of D$prev_cvd in study1`, 1)
    expect_equal(res.length.prev_cvd$`length of D$prev_cvd in study1`, 537)
    expect_length(res.length.prev_cvd$`total length of D$prev_cvd in all studies combined`, 1)
    expect_equal(res.length.prev_cvd$`total length of D$prev_cvd in all studies combined`, 537)

    res.class.prev_ht <- ds.class(x='D$prev_ht')
    expect_length(res.class.prev_ht, 1)
    expect_length(res.class.prev_ht$study1, 1)
    expect_equal(res.class.prev_ht$study1, "logical")

    res.length.prev_ht <- ds.length(x='D$prev_ht')
    expect_length(res.length.prev_ht, 2)
    expect_length(res.length.prev_ht$`length of D$prev_ht in study1`, 1)
    expect_equal(res.length.prev_ht$`length of D$prev_ht in study1`, 537)
    expect_length(res.length.prev_ht$`total length of D$prev_ht in all studies combined`, 1)
    expect_equal(res.length.prev_ht$`total length of D$prev_ht in all studies combined`, 537)

    res.class.prev_bronchitis <- ds.class(x='D$prev_bronchitis')
    expect_length(res.class.prev_bronchitis, 1)
    expect_length(res.class.prev_bronchitis$study1, 1)
    expect_equal(res.class.prev_bronchitis$study1, "logical")

    res.length.prev_bronchitis <- ds.length(x='D$prev_bronchitis')
    expect_length(res.length.prev_bronchitis, 2)
    expect_length(res.length.prev_bronchitis$`length of D$prev_bronchitis in study1`, 1)
    expect_equal(res.length.prev_bronchitis$`length of D$prev_bronchitis in study1`, 537)
    expect_length(res.length.prev_bronchitis$`total length of D$prev_bronchitis in all studies combined`, 1)
    expect_equal(res.length.prev_bronchitis$`total length of D$prev_bronchitis in all studies combined`, 537)

    res.class.body_fat_percent <- ds.class(x='D$body_fat_percent')
    expect_length(res.class.body_fat_percent, 1)
    expect_length(res.class.body_fat_percent$study1, 1)
    expect_equal(res.class.body_fat_percent$study1, "numeric")

    res.length.body_fat_percent <- ds.length(x='D$body_fat_percent')
    expect_length(res.length.body_fat_percent, 2)
    expect_length(res.length.body_fat_percent$`length of D$body_fat_percent in study1`, 1)
    expect_equal(res.length.body_fat_percent$`length of D$body_fat_percent in study1`, 537)
    expect_length(res.length.body_fat_percent$`total length of D$body_fat_percent in all studies combined`, 1)
    expect_equal(res.length.body_fat_percent$`total length of D$body_fat_percent in all studies combined`, 537)
})

#
# Tear down
#

context("D::datachk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.d()

#
# Done
#

context("D::datachk::done")
