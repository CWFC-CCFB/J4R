########################################################
# Unitary tests for configuration in J4R
# Author: Mathieu Fortin, Canadian Wood Fibre Centre
# Date: June 2020
########################################################

context("Configuration tests in J4R")

library(J4R)

initialNbEnvironments <- length(get("environments", envir = settingEnv))

myEnv <- new.env()

res <- j4r.config.registerEnvironment(myEnv)

test_that("A new environment has been added", {
  expect_equal(length(get("environments", envir = settingEnv)), initialNbEnvironments + 1)
  expect_equal(res, T)
})

res <- j4r.config.registerEnvironment(myEnv)

test_that("The new environment has not been added because it is already included", {
  expect_equal(length(get("environments", envir = settingEnv)), initialNbEnvironments + 1)
  expect_equal(res, F)
})

res <- j4r.config.registerEnvironment(globalenv())

test_that("The global environment has not been added", {
  expect_equal(length(get("environments", envir = settingEnv)), initialNbEnvironments + 1)
  expect_equal(res, F)
})

res <- j4r.config.registerEnvironment(NULL)

test_that("A null value has not been added", {
  expect_equal(length(get("environments", envir = settingEnv)), initialNbEnvironments + 1)
  expect_equal(res, F)
})

res <- j4r.config.registerEnvironment("carrot")

test_that("A character string has not been added", {
  expect_equal(length(get("environments", envir = settingEnv)), initialNbEnvironments + 1)
  expect_equal(res, F)
})


res <- j4r.config.removeEnvironment("carrot")

test_that("A character string has not been removed", {
  expect_equal(length(get("environments", envir = settingEnv)), initialNbEnvironments + 1)
  expect_equal(res, F)
})


res <- j4r.config.removeEnvironment(NULL)

test_that("A null has not been removed", {
  expect_equal(length(get("environments", envir = settingEnv)), initialNbEnvironments + 1)
  expect_equal(res, F)
})

res <- j4r.config.removeEnvironment(globalenv())

test_that("The global environment cannot be removed", {
  expect_equal(length(get("environments", envir = settingEnv)), initialNbEnvironments + 1)
  expect_equal(res, F)
})

res <- j4r.config.removeEnvironment(myEnv)

test_that("The new environment can be removed", {
  expect_equal(length(get("environments", envir = settingEnv)), initialNbEnvironments)
  expect_equal(res, T)
})

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
