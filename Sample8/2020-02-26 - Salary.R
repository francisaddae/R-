library(data.table)
library(Metrics)
library(glmnet)

load("hitters.rdata")
hitters
colnames(hitters)
dim(hitters)
# There are 322 baseball hitters with their statistics
# Can we predict salary from the statistics?

# First, we need to remove the NA's from the Salary

#data.frame style
sum(is.na(hitters$Salary))
#data.table style
hitters[, sum(is.na(Salary))]

# remove all rows with NA in it
hitters = na.omit(hitters)

# Divide into testing and training sets:

set.seed(31415)
idx = sample(1:nrow(hitters), nrow(hitters) * .7)
train = hitters[idx]
test  = hitters[!idx]

fit.lm = lm(Salary ~ ., data = train)
train.err = rmse(train$Salary, fit.lm$fitted.values)
pred.lm = predict(fit.lm, test)
test.err = rmse(test$Salary, pred.lm)
errors = data.table(method = "lm", 
                    train.rmse = train.err,
                    test.rmse  = test.err,
                    coefs      = length(fit.lm$coefficients) - 1)

# Not all predictors were significant (overfitting)
# So let's remove them automatically

step.lm = step(fit.lm)
summary(step.lm)
train.err = rmse(train$Salary, step.lm$fitted.values)
pred.step = predict(step.lm, test)
test.err = rmse(test$Salary, pred.step)
errors = rbind(errors, 
               list("step", 
                    train.err, 
                    test.err,
                    length(step.lm$coefficients) - 1))
errors

# Let's see what happens for Ridge and Lasso
# Ridge and Lasso functions don't have y ~ . input
# so we need to separate

y.train = train$Salary
x.train = as.data.table(model.matrix(Salary ~ ., train))
x.train
x.train[, "(Intercept)" := NULL]
x.train
y.test = test$Salary
x.test = as.data.table(model.matrix(Salary ~ ., test))
x.test[, "(Intercept)" := NULL]


# Let's do ridge regression
grid = exp(seq(-3, 10, length = 131))
cv.ridge = cv.glmnet(as.matrix(x.train), y.train, alpha = 0, lambda = grid)
plot(cv.ridge)
bestlam.ridge = cv.ridge$lambda.min
log(bestlam.ridge)
fit.ridge = glmnet(as.matrix(x.train), y.train, alpha = 0, lambda = grid)
idx = which(fit.ridge$lambda == bestlam.ridge)
fit.ridge$a0[idx]
fit.ridge$beta[, idx]
ridge.train = predict(fit.ridge, s = bestlam.ridge, newx = as.matrix(x.train))
ridge.test  = predict(fit.ridge, s = bestlam.ridge, newx = as.matrix(x.test))
ridge.train.err = rmse(train$Salary, ridge.train)
ridge.test.err  = rmse(test$Salary, ridge.test)
errors = rbind(errors,
               list("ridge", 
                    ridge.train.err,
                    ridge.test.err,
                    sum(fit.ridge$beta[, idx] != 0)))
plot(fit.ridge)
errors


# Let's do lasso regression since that can set coefficients to 0

cv.lasso = cv.glmnet(as.matrix(x.train), y.train, alpha = 1, lambda = grid)
plot(cv.lasso)
bestlam.lasso = cv.lasso$lambda.min
log(bestlam.lasso)
fit.lasso = glmnet(as.matrix(x.train), y.train, alpha = 1, lambda = grid)
idx = which(fit.lasso$lambda == bestlam.lasso)
fit.lasso$a0[idx]
fit.lasso$beta[, idx]
lasso.train = predict(fit.lasso, s = bestlam.lasso, newx = as.matrix(x.train))
lasso.test  = predict(fit.lasso, s = bestlam.lasso, newx = as.matrix(x.test))
lasso.train.err = rmse(train$Salary, lasso.train)
lasso.test.err  = rmse(test$Salary, lasso.test)
errors = rbind(errors,
               list("lasso", 
                    lasso.train.err,
                    lasso.test.err,
                    sum(fit.lasso$beta[, idx] != 0)))
plot(fit.lasso)
print(errors)

