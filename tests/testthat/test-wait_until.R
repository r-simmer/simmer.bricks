context("wait_until")

test_that("wait_until generates the correct sequence of activities", {
  t <- trajectory() %>%
    wait_until("green")

  expect_equal(length(t), 3)
  expect_equal(get_n_activities(t), 3)
  expect_output(print(t[[1]]), "Trap.*green")
  expect_output(print(t[[2]]), "Wait")
  expect_output(print(t[[3]]), "UnTrap.*green")
})
