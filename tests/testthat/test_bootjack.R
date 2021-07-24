context("Testing bootjack")

test_that("bootjack correctly returns values", {
  flows <- flows_1030500
  results <- bootjack(flows, GOF_stat = "NSE", seed = 1)
  test1 <- (results$seJack[1] >= 0.0484) & (results$seJack[1] <= 0.0486)
  test2 <- (results$seBoot[1] >= 0.0471) & (results$seBoot[1] <= 0.0472)
  test3 <- (results$seJab[1] >= 0.0316) & (results$seJab[1] <= 0.0317)
  expect_true(test1 & test2 & test3)
})
