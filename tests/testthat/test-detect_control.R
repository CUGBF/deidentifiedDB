test_that("multiplication works", {
  expect_false(detect_control(testkit_id = '117M192AF961B664HT',
                             result = 'Negative'))
  expect_true(detect_control(testkit_id = 'negative control',
                              result = NA))
  expect_true(detect_control(testkit_id = '117M192AF961B7D3O0',
                             result = "known positive"))
  expect_true(detect_control(testkit_id = 'known positive',
                             result = NA))
})
