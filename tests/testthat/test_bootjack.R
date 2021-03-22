context("Testing bootjack")

test_that("bootjack correctly returns values", {
  flows <- flows_1030500
  test <- bootjack(flows)
  expect_true((test$seJack[1] >= 0.0484) & (test$seJack[1] <= 0.0486))
})
