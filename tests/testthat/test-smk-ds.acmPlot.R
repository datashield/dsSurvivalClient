#-------------------------------------------------------------------------------

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

context("ds.acmPlot::smk::setup")

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

context("ds.acmPlot::smk")
test_that("simple error,wrong formula", {
})

context("ds.acmPlot::smk")
test_that("create a Surv object with a parameter which is not of correct type (not numeric)",  {
 
    expect_error( as.character( dsSurvivalClient::ds.acmPlot(time='D$female', time2='ENDTIME', event='EVENT', objectname='surv_object', type='counting') ) )
    
})

#
# Done
#

context("ds.acmPlot::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

# disconnect.studies.dataset.d()
disconnect.studies.dataset.survival()

context("ds.acmPlot::smk::done")
