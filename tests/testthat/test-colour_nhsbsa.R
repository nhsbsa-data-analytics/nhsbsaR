library(NHSRtheme)

testthat::test_that("custom colour palette returns lengh > 1",{
  expect_equal(length(
    palette_nhsbsa(palette = NHSRtheme::get_nhs_colours(c("Blue","Yellow","Pink"))
    )),3)

  # custom colour should have more than one colour
  # If you want to use one colour then default NA will render
  expect_error(palette_nhsbsa(palette = NHSRtheme::get_nhs_colours("Pink")))

})
