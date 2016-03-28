
x <- as.factor(rep(c("a", "b","c"), 4))
y <- c(4,4,3,4,8,0,9,8,8,6,0,3)
z <- c(NA,NaN,3,-1,-2,1,1,-2,2,-2,-3,3)

data <- data.frame(x=x, y=y, z=z)
desc <- silkyDescriptives(data, c("x", "y", "z"), median=TRUE, mode=TRUE, skew=TRUE, kurt=TRUE, quart=TRUE)
#print(desc)

# Test descriptives table numerical values
expect_equal(4.75, desc$results()$get("descriptives")$getCell(2, "mean")$value)
expect_equal(4, desc$results()$get("descriptives")$getCell(2, "mode")$value)
expect_equal(57, desc$results()$get("descriptives")$getCell(2, "sum")$value)
expect_equal(sd(y), desc$results()$get("descriptives")$getCell(2, "sd")$value)
expect_equal(var(y), desc$results()$get("descriptives")$getCell(2, "variance")$value)
expect_equal(9, desc$results()$get("descriptives")$getCell(2, "range")$value)
expect_equal(0, desc$results()$get("descriptives")$getCell(2, "min")$value)
expect_equal(9, desc$results()$get("descriptives")$getCell(2, "max")$value)
expect_equal(sqrt(var(y)/12), desc$results()$get("descriptives")$getCell(2, "se")$value)

deviation <- y-mean(y)
expect_equal(sum(deviation^3)/(12*sd(y)^3), desc$results()$get("descriptives")$getCell(2, "skew")$value)
expect_equal(sum(deviation^4)/(12*var(y)^2), desc$results()$get("descriptives")$getCell(2, "kurt")$value)

expect_equivalent(3, desc$results()$get("descriptives")$getCell(2, "quart1")$value)
expect_equivalent(4, desc$results()$get("descriptives")$getCell(2, "quart2")$value)
expect_equivalent(8, desc$results()$get("descriptives")$getCell(2, "quart3")$value)
expect_equal(0, desc$results()$get("descriptives")$getCell(3, "mean")$value)

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


