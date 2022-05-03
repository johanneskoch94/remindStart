#skip("Skipping tests")

test_that("Clone remind", {
  skip_if(dir.exists(testRemind), "testRemind directory already exists.")
  gitCloneRemind(to = testRemind)
  expect_true(TRUE)
})

# Definde stubs for run and postProc functions
prepareStub <- function() file.create("input.gdx")
postProcStub <- function() {
  load("config.Rdata")
  file.create(paste0("REMIND_generic_", cfg$title, ".mif"),
              paste0("REMIND_generic_", cfg$title, "_withoutPlus.mif"),
              paste0("REMIND_LCOE_", cfg$title, ".mif"))
}
runStub <- function() {
  file.copy("input.gdx", "fulldata.gdx")
  file.create("full.log", "full.lst")
}

test_that("Start remind: default run", {
  mockr::local_mock(run = runStub)
  mockr::local_mock(postProc = postProcStub)
  suppressWarnings(quietly(start(testRemind)))
  expect_true(TRUE)
})

