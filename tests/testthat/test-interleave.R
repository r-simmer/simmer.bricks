context("interleave")

test_that("interleave generates the correct sequence of activities", {
  resources <- c("A", "B", "C")
  n_activities <- length(resources) * 3 + (length(resources)-1) * 2

  t <- trajectory() %>%
    interleave(resources, c(1, 2, 3), c(1, 2, 3))

  expect_equal(length(t), n_activities)
  expect_equal(get_n_activities(t), n_activities)
  expect_output(print(t), paste0(
    "Seize.*A.*1.*Timeout.*1.*Seize.*B_token.*2.*Release.*A.*1.*",
    "Seize.*B.*2.*Timeout.*2.*Seize.*C_token.*3.*Release.*B.*2.*Release.*B_token.*2.*",
    "Seize.*C.*3.*Timeout.*3.*Release.*C.*3.*Release.*C_token.*3"))
})
