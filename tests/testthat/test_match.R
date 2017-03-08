context("Match for appending data")

# Inputs
xs <- data.frame(a = 4:6, b = letters[4:6], stringsAsFactors = FALSE)
xl <- rbind(data.frame(a = 14:24, b = letters[14:24], stringsAsFactors = FALSE), xs)

ref_template <- data.frame(a = 10:1, b = letters[10:1], c = rep(NA, 10), stringsAsFactors = FALSE)

test_that("Match", {
  #--- Correct use of match to append data
  ref <- ref_template
  expect_equal({
      id_x <- match(ref$a, xs$a, nomatch = 0)
      use_r <- id_x > 0
      ref$c[use_r] <- xs$b[id_x]
      ref$c[use_r]
    }, ref$b[use_r])
    
  ref <- ref_template
  expect_equal({
      id_x <- match(ref$a, xl$a, nomatch = 0)
      use_r <- id_x > 0
      ref$c[use_r] <- xl$b[id_x]
      ref$c[use_r]
    }, ref$b[use_r])

  #--- Incorrect use of match (first test works because nrow(xs) <= nrow(ref); second test fails)
  ref <- ref_template
  expect_equal({
      id_r <- match(xs$a, ref$a, nomatch = 0)
      ref$c[id_r] <- xs$b
      ref$c[id_r]
    }, ref$b[id_r])

  ref <- ref_template
  expect_warning({
      id_r <- match(xl$a, ref$a, nomatch = 0)
      ref$c[id_r] <- xl$b  # number of items to replace is not a multiple of replacement length
    })
})
