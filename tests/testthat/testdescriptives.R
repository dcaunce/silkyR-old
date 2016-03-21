
x <- as.factor(rep(c("a", "b"), 5))
y <- runif(10)
data <- data.frame(x=x, y=y)
desc <- silkyDescriptives(data, c("x", "y"), median=TRUE)
#print(desc)

#desc$options()$set(vars=c("y", "x"))
#print(desc)

# desc$setDataset(data.frame(go=7, stop=4, total=9, eric=7))
# desc$options()$set(vars=c("go", "eric", "stop"))
# desc$options()$set("median"=TRUE)
# print(desc)
# 
