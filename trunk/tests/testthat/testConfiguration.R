########################################################
# Unitary tests for configuration in J4R
# Author: Mathieu Fortin, Canadian Wood Fibre Centre
# Date: June 2020
########################################################

context("Configuration tests in J4R")

library(J4R)

out <- tryCatch(
  {
    j4r.config.setDefaultJVMMemorySize(25)
    "did not throw any exception"
  },
  error = function(cond) {
    return("threw an exception")
  }
)

test_that("Check if a default memory size smaller than 50 Mb raises an exception", {
  expect_equal(out, "threw an exception")
})

j4r.config.setDefaultJVMMemorySize(50)

test_that("Check if the default memory has been properly set", {
  expect_equal(get("defaultJVMMemory", envir = settingEnv), 50)
})

j4r.config.setDefaultJVMMemorySize(NULL)

test_that("Check if the default memory has been properly set", {
  expect_equal(exists("defaultJVMMemory", envir = settingEnv), F)
})
