test_that("works on data frame", {
  t1 <- tibble::tibble(dt = lubridate::ymd(20250201), dttm = lubridate::now(), intg = 5L, chr = "5",
               chr1 = "5L", chr2 = "L5")

  set_int(t1) %>% dplyr::pull(chr) %>% is.integer %>% expect_all_true()
})

