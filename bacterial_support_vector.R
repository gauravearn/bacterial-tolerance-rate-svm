support_vector_machine_tolerance <- function(data){
    # applying support vector machine to predict and fine tune a 
    # model to predict the tolerance rates based on the timing, age
    # and exposure to a bacterial species.if you change the type to 
    # c-classification and uses the time formula then it accurately 
    # predicts the onset of the bacterial rates. 
    data_load <- data
    tolerance <- load("/Users/gauravsablok/Desktop/tolerance.RData")
    head(tolerance_tidy)
    table(tolerance_tidy)
    prop.table(table(tolerance_tidy))
    svm(tolerance ~ ., data = tolerance_tidy)
    # first optimization
    # Parameters:
    # SVM-Type:  eps-regression
    # SVM-Kernel:  radial
    #  cost:  1
    # gamma:  0.2
    # epsilon:  0.1
    # Number of Support Vectors:  68
    # model training start 
    x <- subset(tolerance_tidy, select = -tolerance)
    y <- tolerance_tidy$tolerance
    svm_tolerance_first <- svm(tolerance ~ .,
                                data = tolerance_tidy,
                                    type = "eps-regression", cost = 0.1, gamma = 0.2)
    svm_tolerance_predict <- predict(svm_tolerance_first, x)
    mse(svm_tolerance_first, tolerance_tidy)
    rmse(svm_tolerance_first, tolerance_tidy)
    mae(svm_tolerance_first, tolerance_tidy)
    qae(svm_tolerance_first, tolerance_tidy)
    rsquare(svm_tolerance_first, tolerance_tidy)
    #model optimization 
    tune(svm,
    train.x = x, train.y = y, kernel = "radial",
    ranges = list(cost = 2^(-1:1), gamma = 2^(-1:1)))
    svm_tune <- svm(tolerance ~ .,
                    data = tolerance_tidy,
                            type = "eps-regression", cost = 1, gamma = 2)
    svm_tune_predict <- predict(svm_tune, x)
    mse(svm_tune_predict, tolerance_tidy)
    rmse(svm_tune_predict, tolerance_tidy)
    mae(svm_tune_predict, tolerance_tidy)
    qae(svm_tune_predict, tolerance_tidy)
    rsquare(svm_tune_predict, tolerance_tidy)
}
