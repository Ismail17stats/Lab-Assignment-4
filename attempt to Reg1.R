# Define variable groups

linreg <- setRefClass("linreg",
                      contains = "Data",
                      methods = list(
                        
                        
                      )
                      
                      ze_code <- function(formula, data){
                        
                        X <- model.matrix(formula, data)
                        y <- as.matrix(data[])
                      }
)

#regression coefficient
?sys.call
head(iris)
data <- iris
?model.matrix()
?all.vars()
library(modelr)
model.matrix( Sepal.Length , iris)
reg1           # Coefficients only
summary(reg1)

anova(reg1)            # Coefficients w/inferential tests
coef(reg1)             # Coefficients (same as reg1)
confint(reg1)          # CI for coefficients
resid(reg1)            # Residuals case-by-case
hist(residuals(reg1))  # Histogram of residuals

