library(glmnet)
#courtesy https://www.r-bloggers.com/2020/05/simple-guide-to-ridge-regression-in-r/

#get variables
x_var <- data.matrix(modelData %>% 
  select(a_orya,
           a_opya,
           a_ofum,
           a_oepa,
           a_oqepa,
           a_ocpoe,
           a_ocmp,
           a_oatt,
           a_ocmpct,
           a_oyds,
           a_otd,
           a_oint,
           a_orat,
           a_oyatt,
           a_oaya,
           a_drya,
           a_dpya,
           a_dfum,
           a_depa,
           a_dqepa,
           a_dcpoe,
           h_orya,
           h_opya,
           h_ofum,
           h_oepa,
           h_oqepa,
           h_ocpoe,
           h_ocmp,
           h_oatt,
           h_ocmpct,
           h_oyds,
           h_otd,
           h_oint,
           h_orat,
           h_oyatt,
           h_oaya,
           h_drya,
           h_dpya,
           h_dfum,
           h_depa,
           h_dqepa,
           h_dcpoe
))

y_var <- modelData$total

#setting the range of lambda values
lambdas <- 10^seq(2, -2, by = -.1)
#using glmnet function to build the ridge regression in r
fit <- glmnet(x_var, y_var, alpha = 0, lambda  = lambdas)
#checking the model
summary(fit)

#cross validation
ridge_cv <- cv.glmnet(x_var, y_var, alpha = 0, lambda = lambdas)
best_lambda <- ridge_cv$lambda.min
best_fit <- ridge_cv$glmnet.fit

#rebuilding with best lambda
best_ridge <- glmnet(x_var, y_var, alpha = 0, lambda = best_lambda)
