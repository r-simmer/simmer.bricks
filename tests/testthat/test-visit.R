context("visit")

test_that("visit generates the correct sequence of activities", {
  t <- trajectory() %>%
    visit("dummy", 5, 3)

  expect_equal(length(t), 3)
  expect_equal(get_n_activities(t), 3)
  expect_output(print(t[[1]]), "Seize.*dummy.*3")
  expect_output(print(t[[2]]), "Timeout.*5")
  expect_output(print(t[[3]]), "Release.*dummy.*3")

  t <- trajectory() %>%
    visit_selected(5, 3)

  expect_equal(length(t), 3)
  expect_equal(get_n_activities(t), 3)
  expect_output(print(t[[1]]), "Seize.*\\[\\].*3")
  expect_output(print(t[[2]]), "Timeout.*5")
  expect_output(print(t[[3]]), "Release.*\\[\\].*3")
})
