context("wait_n")

test_that("wait_n generates the correct sequence of activities", {
  expect_error(trajectory() %>% wait_n(-1))

  t <- trajectory() %>%
    wait_n(0)

  expect_equal(length(t), 0)
  expect_equal(get_n_activities(t), 0)

  t <- trajectory() %>%
    wait_n(3)

  expect_equal(length(t), 3)
  expect_equal(get_n_activities(t), 3)
  expect_output(print(t[[1]]), "Wait")
  expect_output(print(t[[2]]), "Wait")
  expect_output(print(t[[3]]), "Wait")
})

test_that("wait_until generates the correct sequence of activities", {
  expect_error(trajectory() %>% wait_until("asdf", 0))

  t <- trajectory() %>%
    wait_until("green")

  expect_equal(length(t), 3)
  expect_equal(get_n_activities(t), 3)
  expect_output(print(t[[1]]), "Trap.*green")
  expect_output(print(t[[2]]), "Wait")
  expect_output(print(t[[3]]), "UnTrap.*green")
})
