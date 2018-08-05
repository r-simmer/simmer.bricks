context("delayed_release")

test_that("delayed_release generates the correct sequence of activities", {
  t <- trajectory() %>%
    delayed_release("dummy", 5, 3)

  expect_equal(length(t), 2)
  expect_equal(get_n_activities(t), 6)
  expect_output(print(t[[1]]), paste0(
    "Clone.*2.*",
    "Fork 1.*SetCapacity.*dummy.*Release.*dummy.*3.*",
    "Fork 2.*Timeout.*5.*SetCapacity.*dummy"))
  expect_output(print(t[[2]]), "Synchronize.*0")

  t <- trajectory() %>%
    delayed_release_selected(5, 3)

  expect_equal(length(t), 2)
  expect_equal(get_n_activities(t), 6)
  expect_output(print(t[[1]]), paste0(
    "Clone.*2.*",
    "Fork 1.*SetCapacity.*Release.*3.*",
    "Fork 2.*Timeout.*5.*SetCapacity"))
  expect_output(print(t[[2]]), "Synchronize.*0")

  t <- trajectory() %>%
    delayed_release("dummy_preemptive", 5, 3, preemptive=TRUE)

  expect_equal(length(t), 2)
  expect_equal(get_n_activities(t), 7)
  expect_output(print(t[[1]]), paste0(
    "Clone.*2.*",
    "Fork 1.*Release.*dummy_preemptive.*3.*",
    "Fork 2.*SetPrior.*Seize.*dummy_preemptive.*3.*Timeout.*5.*Release.*dummy_preemptive.*3"))
  expect_output(print(t[[2]]), "Synchronize.*0")

  t <- trajectory() %>%
    delayed_release_selected(5, 3, preemptive=TRUE)

  expect_equal(length(t), 2)
  expect_equal(get_n_activities(t), 7)
  expect_output(print(t[[1]]), paste0(
    "Clone.*2.*",
    "Fork 1.*Release.*3.*",
    "Fork 2.*SetPrior.*Seize.*3.*Timeout.*5.*Release.*3"))
  expect_output(print(t[[2]]), "Synchronize.*0")
})
