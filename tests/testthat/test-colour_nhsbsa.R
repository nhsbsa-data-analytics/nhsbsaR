library(NHSRtheme)


# Color test helpers ------------------------------------------------------

testthat::test_that("is_color detects built-in R colors", {
  expect_equal(is_color(c("red", "redred")), c(TRUE, FALSE))
})

testthat::test_that("is_nhs_color detects NHS colors", {
  expect_equal(is_nhs_color(c("DarkBlue", "blue")), c(TRUE, FALSE))
})


# Palette -----------------------------------------------------------------

testthat::test_that("gender palette is correct", {
  expect_equal(palette_nhsbsa("gender"), c("#AE2573", "#41B6E6"))
})

testthat::test_that("gradient palette is correct", {
  expect_equal(palette_nhsbsa("gradient"), c("#FFFFFF", "#003087"))
})

testthat::test_that("highlight palette is correct", {
  expect_equal(palette_nhsbsa("highlight"), c("#768692", "#41B6E6"))
})

testthat::test_that("palette of one NHS colour name", {
  expect_equal(palette_nhsbsa("DarkBlue"), "#003087")
})

testthat::test_that("one named NHS colour", {
  expect_equal(palette_nhsbsa(get_nhs_colours(c("DarkBlue"))), "#003087")
  expect_null(palette_nhsbsa(get_nhs_colours(c("DarkBlue"))) %>% names())
})

testthat::test_that("one built-in R color name", {
  expect_equal(palette_nhsbsa("red"), "red")
})

testthat::test_that("one hex code", {
  expect_equal(palette_nhsbsa("#bada55"), "#bada55")
})

testthat::test_that("one hex code with alpha", {
  expect_equal(palette_nhsbsa("#bada55"), "#bada55c0")
})

testthat::test_that("invalid atomic vector throws error", {
  expect_error(palette_nhsbsa("rod"))
})

testthat::test_that("palette returns vector of same length as input", {
  palette = get_nhs_colours(c("Blue", "Yellow", "Pink"))

  expect_equal(length(palette_nhsbsa(palette)), length(palette))
})

testthat::test_that("vector of R color names", {
  palette = c("blue", "yellow", "pink")

  expect_equal(palette_nhsbsa(palette), palette)
  expect_equal(palette_nhsbsa(palette, reverse = TRUE), rev(palette))
})

testthat::test_that("vector of named NHS colors", {
  palette = get_nhs_colours(c("Blue", "Yellow", "Pink"))

  expect_equal(palette_nhsbsa(palette), c("#005EB8", "#FAE100", "#AE2573"))
  expect_equal(
    palette_nhsbsa(palette, reverse = TRUE),
    rev(c("#005EB8", "#FAE100", "#AE2573"))
  )
  expect_null(palette_nhsbsa(palette) %>% names())
})

testthat::test_that("mixed named vector of R color names and hex codes", {
  palette = c("blue", "yellow", "#4444ffcc", "#abcdef")
  names(palette) <-letters[1:4]

  expect_equal(palette_nhsbsa(palette), palette)
  expect_null(palette_nhsbsa(palette) %>% names())
})

testthat::test_that("mixed vector of R color names, hex codes and NHS colors", {
  palette = c("blue", "yellow", "Pink", "#abcdef")

  expect_error(palette_nhsbsa(palette))
})

testthat::test_that("no palette provided", {
  expect_equal(palette_nhsbsa(), unname(c(get_nhs_colours(), "White" = "#FFFFFF")))
  expect_equal(
    palette_nhsbsa(reverse = TRUE),
    rev(unname(c(get_nhs_colours(), "White" = "#FFFFFF")))
  )
})
