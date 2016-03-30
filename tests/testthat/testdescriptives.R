
x <- as.factor(rep(c("a", "b","c"), 4))
y <- c(4,4,3,4,8,0,9,8,8,6,0,3)
z <- c(NA,NaN,3,-1,-2,1,1,-2,2,-2,-3,3)

data <- data.frame(x=x, y=y, z=z)
desc <- silkyDescriptives(data, c("x", "y", "z"), median=TRUE, mode=TRUE, skew=TRUE, kurt=TRUE, quart=TRUE)
#print(desc)

# Test descriptives table numerical values
expect_output(desc$results()$get("descriptives"), "\n Descriptives                                     \n ──────────────────────────────────────────────── \n                      x       y           z       \n ──────────────────────────────────────────────── \n   Mean                         4.75       0.00   \n   Median                       4.00       0.00   \n   Mode                         4.00 ᵃ    -2.00   \n   Minimum                      0.00      -3.00   \n   Maximum                      9.00       3.00   \n   Skewness                   -0.127      0.104   \n   Kurtosis                     1.58       1.19   \n   25th percentile              3.00      -2.00   \n   50th percentile              4.00       0.00   \n   75th percentile              8.00       1.75   \n ──────────────────────────────────────────────── \n ᵃ More than one mode exists, only the first \n   is reported \n", fixed=TRUE)
expect_output(desc$results()$get("frequencies"), " Frequencies\n\n Frequencies of x                            \n ─────────────────────────────────────────── \n   Level    Counts    %       Cumulative %   \n ─────────────────────────────────────────── \n   a       4.00    33.3     33.3   \n   b       4.00    33.3     66.7   \n   c       4.00    33.3    100.0   \n ─────────────────────────────────────────── \n", fixed=TRUE)

# Test footnote appearance
expect_true(!length(desc$results()$get("descriptives")$getCell(1, "mode")$sups))
expect_false(!length(desc$results()$get("descriptives")$getCell(2, "mode")$sups))
expect_true(!length(desc$results()$get("descriptives")$getCell(3, "mode")$sups))

# Test frequency table numerical values
expect_equal(4, desc$results()$get("frequencies")$get("x")$getCell(1,"counts")$value)
expect_equal(100/3, desc$results()$get("frequencies")$get("x")$getCell(1,"percentage")$value)
expect_equal(200/3, desc$results()$get("frequencies")$get("x")$getCell(2,"cumpercentage")$value)

expect_false(is.numeric(desc$results()$get("descriptives")$getCell(1, "mean")$value))

expect_error(silkyDescriptives(data.frame(x=c(Inf,-Inf)),c("x")), "Error : Argument 'vars' specifies column 'x' which contains (and must not) infinite values\n", fixed=TRUE)


