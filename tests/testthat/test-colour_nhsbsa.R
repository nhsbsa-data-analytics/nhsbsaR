library(NHSRtheme)


testthat::test_that("built-in R colour palette", {
  expect_equal(is_color(c("red","redred")), c(TRUE,FALSE))
})

testthat::test_that("NHSRtheme colour palette", {
  expect_true(is_nhs_color(c("DarkBlue","Blue")))
})


testthat::test_that("NHSRtheme colour palette", {
  expect_false(is_nhs_color(c("DarkBlue","blue")))
})





testthat::test_that("custom colour palette returns lengh > 1",{
  expect_equal(length(
    palette_nhsbsa(palette = NHSRtheme::get_nhs_colours(c("Blue","Yellow","Pink"))
    )),3)

  # Test custom color with one built-in R colour
  expect_equal(palette_nhsbsa(palette = "red"), "red")

  expect_equal(palette_nhsbsa(palette = "DarkBlue"), "DarkBlue")

  # Mix built-in & NHSRtheme colour
  expect_error(palette_nhsbsa(c("indigo", "Pink", "DarkBlue")))
  expect_error(palette_nhsbsa("gander"))

  expect_equal(palette_nhsbsa(c("blue", "red", "#bada55")), c("blue","red","#bada55"))

})

# # Works
# palette_nhsbsa("gender") # known word
# palette_nhsbsa("green") # single R color
# palette_nhsbsa(c("blue", "red", "#bada55")) # mixture of R colors and hexes
# palette_nhsbsa(c("Orange", "Pink")) # all NHS colour names
# palette_nhsbsa("#BADA55") # single hex
# palette_nhsbsa("#BADA55CC", "#BADDAD") # multiple hexes, one with alpha characters
#
# # Does not work
# palette_nhsbsa("gander") # unknown word
# palette_nhsbsa(c("indigo", "Pink", "DarkBlue")) # mixtures not allowed with NHS colours


