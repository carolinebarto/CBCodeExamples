#install.packages("glmnet")
library(glmnet)

#loading in relevant variables and merging in v2x_polyarchy
load("merged_var_CB0329.rda")

x <- read.csv("V-Dem-CY-Full+Others-v11.1.csv")

categorized$v2x_polyarchy

x <- x[, c("country_id", "year", "v2x_polyarchy")]

forLasso <- merge(categorized, x, by = c("country_id", "year"))

#removing null values for glmnet

forLasso <- forLasso[-which(is.na(forLasso$v2x_polyarchy)),]

sum(is.na(forLasso$v2x_polyarchy))

#finding lambda without country_id
cvLasso <- cv.glmnet(data.matrix(forLasso[,-c(1, 139)]), forLasso[,139], alpha = 1)

plot(cvLasso)

#best lambda
cvLam2 <- cvLasso$lambda.min

#running model
lasso <- glmnet(forLasso[,-c(1, 139)], forLasso[,139], alpha = 1, lambda = cvLam2)

summary(lasso)

y <- coef(lasso)
y

list <- y[,1]

#irrelevant columns
z <- names(list[list == 0])
#helpful columns 
relevant <- names(list[list!=0])
