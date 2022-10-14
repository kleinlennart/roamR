test_that("Error if no API Key", {
  expect_error(roam_q(
    query = "QUERY",
    graph = "TEST"
  ))
})

test_that("Error if numeric API Key", {
  expect_error(roam_q(
    query = "QUERY",
    graph = "TEST",
    key = 123
  ))
})


test_that("Error if wrong API Key", {
  expect_error(roam_q(
    query = "QUERY",
    graph = "TEST",
    key = "wrong-key"
  ))
})

# FIXME
# test_that("Default example works", {
#   expect_success(
#     roam_q(
#       query = "[:find ?p ?title :where [?p :node/title ?title]]",
#       graph = "roamr",
#       key = Sys.getenv("ROAMR"),
#       verbose = TRUE,
#       format = "tibble"
#     )
#   )
# })
