x <- as.factor(c(NA,rep(c("a", "b"), 6)))
y <- c(8,51,2,74,1,91,5,25,1,59,5,32,7) # breaks equality of variance
z <- as.factor(c("good",rep(c("good", "bad", "neutral"), 4)))

data <- data.frame(x=x,y=y,z=z)
anova <- silkyANOVA(data, "y", c("x","z"), list("x","z",c("x","z")))
#print(anova)

# data<-read.csv("/home/jonathon/Documents/Tooth Growth.csv")
# data$dose<-as.factor(data$dose)
# anova <- silkyANOVA(data, "len", c("supp","dose"), list("supp","dose",c("supp","dose")))
# print(anova)