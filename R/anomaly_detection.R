#' MVAD: MultiVariate Anomaly Detection
#'
#' A package for computing Mahalanobis and conditional
#' probability scores of VAR(k) residuals in streaming real-time data to
#' be used in multivariate anomaly detection.
#'
#' The MVAD package provides four main important functions:
#' preprocess_data, train_model, get_residual, and get_anomaly_score.
#' This library was born from anomaly detection done in the power grid
#' at Los Alamos National Laboratory. Results from this approach and methods
#' for parameter selection can be found in a paper
#' \href{https://arxiv.org/abs/1911.06316}{Real-time Anomaly Detection and Classification in Streaming PMU Data}.
#'
#' @section preprocess_data functions:
#' The preprocess_data functions are used to format data to a unit-
#' independent representation. Inverse functions are present to express
#' model predicted values in original units.
#'
#' @section train_model function:
#' The train_model function is used to fit a VAR(p) model on the training
#' data using the parameters and data supplied to the anomaly_detection
#' data structure
#'
#' @section get_residual functions:
#' The get residual function uses the trained model to calculate residuals
#' of the testing data supplied.
#'
#' @section get_anomaly_score functions:
#' The anomaly score functions calculate the conditional probability of
#' each variables' residual sepraretly and the overall multivariate
#' residual distrance from the learned models error term.
#'
#' @section MVAD S3 object type:
#' This package revolves around a simple S3 object type. The MVAD object
#' contains the parameters of the anomaly detection, the preprocessing steps,
#' the detection model, the residuals, and the scores. It is a simple way to
#' release this package, while maintaining little tech debt. The following is
#' a complete MVAD object, i.e., after calling the previous 4 functions.
#'
#' \verb{
#' dt.ad:
#'   params:
#'     num_vars              (Integer)
#'     tau                   (Integer)
#'     data_resolution       (Integer)
#'     num_lag_terms         (Integer)
#'   raw_training_data       (data.table)
#'   raw_testing_data        (data.table)
#'   coarse_data             (data.table)
#'   linearized_data:
#'     linearized_data       (data.table)
#'     detrend_parameter_m   (Float)
#'     detrend_parameter_b   (Float)
#'   normalized_data:
#'     standardized_data     (data.table)
#'     standardized_stds     (list)
#'   model                   (VAR model*)
#'   detrended_eval_data     (data.table)
#'   predict_x_expected      (data.table)
#'   predict_x_observed      (data.table)
#'   covariance_matrices     (data.table)
#'   residuals               (data.table)
#'   scores                  (data.table)
#'}
#' The MVAD object is updatedand appended from the relevant functions.
#'
#'* \href{https://cran.r-project.org/web/packages/vars/index.html}{VAR model}
#' @docType package
#' @author Christopher Hannon
#' @name MVAD
NULL

#########################
##    Preprocessing    ##
#########################

#' Simple-Usage Script for the Vignette
#'
#'
#' @export
#'
test <- function() {
        ## Generate sample data
        dt.data <- get_test_data()
        ## get length
        dt.training_data <- dt.data[1:(1/1.1 * nrow(dt.data)),]
        dt.testing_data <- dt.data[(1/1.1 * nrow(dt.data) + 1): nrow(dt.data),]
        ##
        dt.ad <- setup_model()
        dt.ad <- add_data(dt.ad, training_data = dt.training_data, testing_data = dt.testing_data)
        dt.ad <- preprocess_data(dt.ad)
        dt.ad <- train_model(dt.ad)
        dt.ad <- get_residual(dt.ad)
        dt.ad <- get_anomaly_score(dt.ad)
}

#' Add training and testing data.
#'
#' \code{add_data} returns the added anomaly detection data structure
#' with the training and testing data provided as arguments.
#'
#' @param dt.ad An S3 data structure used in the package, see \code{\link{Summary}}.
#' @param training_data A data.table with \code{dt.ad$param$num_variables} number of columns.
#' @param testing_data A data.table with \code{dt.ad$param$num_variables} number of columns.
#'
#' @return An S3 object of mvad type.
#'
#' @examples
#' dt.ad <- add_data(setup_model(),
#'                   data.table("v" = 1:10,"i" = 11:20, "t" = 21:30, "f" = 31:40),
#'                   data.table("v" = 1:5,"i" = 6:10, "t" = 11:15, "f" = 16:20))
#' @export
#'
add_data <- function(dt.ad, training_data, testing_data) {
        dt.ad$raw_training_data = training_data
        dt.ad$raw_testing_data = testing_data
        return(dt.ad)
}

#' Set initial parameters for MVAD.
#'
#' \code{setup_model} returns the initial mvad object created be initializing parameters.
#'
#' @param num_variables Integer - defining ncol of training/testing data tables.
#' @param tau Integer - refering to number of data points to train model over.
#' i.e., 10 mins @ 30Hz  = 10 * 60 * 30.
#' @param data_resolution Integer - number of terms to be 'smoothed' for per-point by the model estimation.
#' @param num_lag_terms Integer - number of lag terms to include in the model. TODO expand from 1 to k
#'
#' @return An S3 object of MVAD type.
#'
#' @examples
#' dt.ad <- setup_model(4, 10 * 60 * 30, 15, 1)
#' dt.ad <- setup_model() ## defaults to above
#'
#' @export
#'
setup_model <- function(num_variables = 4,
                        tau = 10 * 60 * 30,
                        data_resolution = 15,
                        num_lag_terms = 1) {
        ## TODO: allow k lag terms
        num_lag_terms <- 1

        params <- list(
                        "num_variables" = num_variables,
                        "tau" = tau,
                        "data_resolution" = data_resolution,
                        "num_lag_terms" = num_lag_terms
                        )
        dt.ad <- list("params" = params)
        return(dt.ad)
}

#' Create testing data synthetically.
#'
#' \code{get_test_data} returns example multivartiate testing data with similar trend and standard deviations
#' as (a) real data set did
#'
#' @return A data.table with 4 column/variables and 1.1 * tau rows.
#'
#' @examples
#' dt.synthetic_data <- get_test_data()
#'
#' @export
#' @importFrom data.table data.table
#'
get_test_data <- function() {
        set.seed(123)
        tau <- 10*60*30 ## (10 mins of data at 30Hz)
        ## mx+b
        test.b <- c( 80000, 15, 0.43, 60)          ## similar to variables in our papers data
        xs <- 1:(1.1 * tau)
        test.m <- list("v1" = xs * -0.0036, "v2" = xs * 0.0000029, "v3" = xs * -0.00000054, "v4" = xs * -0.0000011)
        test.linear_trend <- list("v1" = test.m$v1 + test.b[1],
                                  "v2" = test.m$v2 + test.b[2],
                                  "v3" = test.m$v3 + test.b[3],
                                  "v4" = test.m$v4 + test.b[4])
        test.sd <- c(20, 0.17, 0.0084, 0.009)

        dt.test <- data.table("v1" = data.frame(Y = rnorm(1.1 * tau, mean = 0)) * test.sd[1] + test.linear_trend$v1,
                              "v2" = data.frame(Y = rnorm(1.1 * tau, mean = 0)) * test.sd[2] + test.linear_trend$v2,
                              "v3" = data.frame(Y = rnorm(1.1 * tau, mean = 0)) * test.sd[3] + test.linear_trend$v3,
                              "v4" = data.frame(Y = rnorm(1.1 * tau, mean = 0)) * test.sd[4] + test.linear_trend$v4
        )
        return(dt.test)
}

#' Wrapper to evaluate all steps of anomaly detection.
#'
#' \code{evaluate} runs preprocessing, model fitting, residual finding, and anomaly scoring.
#' Before calling, \code{\link{setup_model}}, and\code{\link{add_data}} should be called.
#'
#' @param dt.ad An S3 object of MVAD type (partially evaluated)
#'
#' @return An S3 object of MVAD type (fully evaluated).
#'
#' @export
#'
evaluate <- function(dt.ad) {
        dt.ad <- preprocess_data(dt.ad)
        dt.ad <- train_model(dt.ad)
        dt.ad <- get_residual(dt.ad)
        dt.ad <- get_anomaly_score(dt.ad)
        return(dt.ad)
}

#######################
## Primary Functions ##
#######################

#' Preprocess Data.
#'
#' \code{preprocess_data} returns the input data normalized and detrended (unitless).
#' Before calling, \code{\link{setup_model}}, and\code{\link{add_data}} should be called.
#'
#' @param dt.ad  - An S3 object of MVAD type (partially evaluated)
#'
#' @return An S3 object of MVAD type (partially evaluated).
#'
#' @export
#'
preprocess_data <- function(dt.ad) {
        ## This function takes in the raw data and performs the preprocessing
        ## The data is changed first by resolution, then by linear detrend
        ## Finally the data is devided by the standard deviation
        training_data <- dt.ad$raw_training_data
        tresting_data <- dt.ad$raw_testing_data
        # preprocess data
        coarse_data <- make_coarse(dt.ad, training_data)
        linearized_data <- linearize_data(dt.ad, coarse_data)
        normalized_data <- normalize_data(dt.ad, linearized_data$linearized_data)
        # add to data structure
        dt.ad$coarse_data = coarse_data
        dt.ad$linearized_data = linearized_data
        dt.ad$normalized_data = normalized_data
        return(dt.ad)
}

#' Train Model.
#'
#' \code{train_model} estimates a VAR(k) model fitted on the preprocessed training data.
#' Before calling, \code{\link{setup_model}}, \code{\link{add_data}}, and \code{\link{preprocess_data}} should be called.
#'
#' @param dt.ad  - An S3 object of MVAD type (partially evaluated)
#'
#' @return An S3 object of MVAD type (partially evaluated).
#'
#' @export
#' @importFrom vars VAR
#'
train_model <- function(dt.ad) {
        ## This function takes in the preprocessed data and returns a fitted VAR model
        dt.ad$model = VAR(dt.ad$normalized_data$standardized_data, p = dt.ad$params$num_lag_terms)
        return(dt.ad)
}

#' Gets data residuals using model and testing data.
#'
#' \code{get_residual} returns the residuals calculated by the models prediction and testing data.
#' Before calling, \code{\link{setup_model}}, \code{\link{add_data}}, \code{\link{preprocess_data}},
#' and \code{\link{train_model}} should be called.
#'
#' @param dt.ad  - An S3 object of MVAD type (partially evaluated)
#'
#' @return An S3 object of MVAD type (partially evaluated).
#'
#' @export
#'
get_residual <- function(dt.ad) {
        ## given a data table of eval data, compute the  residuals of the fitted model
        ##  and the predicted model
        ##
        dt.ad <- detrend_data(dt.ad)
        dt.ad <- predict_data(dt.ad)

        residuals <- data.table()
        for (r in 1:nrow(dt.ad$predict_x_oberved)) {
                residuals_tmp <- data.table()
                for (name in names(dt.ad$predict_x_oberved)) {
                        residuals_tmp <- cbind(residuals_tmp, data.table(abs(dt.ad$predict_x_oberved[r,][[name]]
                                                        - dt.ad$predict_x_expected[r,][[name]])))
                }
                residuals <- rbind(residuals, residuals_tmp)
        }
        colnames(residuals) <- names(dt.ad$predict_x_oberved)
        dt.ad$residuals  = residuals
        return(dt.ad)
}

#' Scores data residuals using model.
#'
#' \code{get_anomaly_detection} returns the scores of the residuals calculated by the models parameters.
#' Before calling, \code{\link{setup_model}}, \code{\link{add_data}}, \code{\link{preprocess_data}},
#' \code{\link{train_model}}, and \code{\link{get_residual}} should be called.
#'
#' @param dt.ad  - An S3 object of MVAD type (partially evaluated)
#'
#' @return An S3 object of MVAD type (fully evaluated).
#'
#' @export
#' @importFrom condMVNorm condMVN
#'
get_anomaly_score <- function(dt.ad) {
        ## calculate Mahalanobis distance, and all conditional probabilities
        conditionals <- data.table()
        mahalanobiss <- data.table()
        for (r in 1:nrow(dt.ad$residuals)) {
                sig <- dt.ad$covariance_matrices[[r]]
                res <- unlist(dt.ad$residuals[r,])
                mv <- mahalanobis(t(matrix(res)), center = FALSE, sig)
                #mv <- getMahalanobisDistance(matrix(res),  sig)
                mahalanobiss <- rbind(mahalanobiss, data.table(mv))
                indx <- 1
                conditionals_tmp <- data.table()
                for (name in names(dt.ad$residuals)) {
                        giv <- matrix(res[-indx])
                        cond <- condMVN(mean = rep(0, 4), sig, dependent.ind = indx,
                                        given.ind = c(1:dt.ad$params$num_variables)[-indx], X.given = giv, check.sigma = TRUE)
                        conditional <- abs(cond$condMean - res[indx]) / sqrt(cond$condVar)
                        conditionals_tmp <- cbind(conditionals_tmp, data.table(conditional))
                        indx <- indx + 1
                }
                conditionals <- rbind(conditionals, conditionals_tmp)
        }
        colnames(conditionals) <- names(dt.ad$residuals)
        colnames(mahalanobiss) <- c("Mahalanobis")
        scores <- cbind(mahalanobiss, conditionals)
        dt.ad$scores <- scores
        return(dt.ad)
}

###########################
##  Secondary Functions  ##
###########################

#' internal
#'
#' Not to be used for external
#' @param dt.ad MVAD obj
#' @param training_data data.frame
#'
make_coarse <- function(dt.ad, training_data) {
        # This function takes every data_resolution number of points and computes the average
        ## e.g. make_coarse(1:30) with data resolution of 15 returns a data table of 7.5 and 22.5
        ##   Input : data table of training data
        ##   Output : 'coarse_data' in data table
        #          nrow(coarse_data) = nrow(training_data)/dt.ad$params$data_resolution
        ##
        coarse_data <- data.table()
        if (nrow(training_data) < dt.ad$params$data_resolution) {
                stop("Error: not enough training data, aborting...")
        }
        if (nrow(training_data) %% dt.ad$params$data_resolution != 0) {
                warning("Training data length not multiple of data resolution: Truncating beginning of data to match")
                training_data <- training_data[(-nrow(training_data) %% dt.ad$params$data_resolution),]
        }
        for (v in names(training_data)) {
                tmp_data <- c()
                for (i in 1:(length(training_data[[v]])/dt.ad$params$data_resolution)) {
                        tmp_data[i] <- mean(training_data[[v]][(1 + (i-1) * dt.ad$params$data_resolution):(dt.ad$params$data_resolution * i)])
                }
                coarse_data <- cbind(coarse_data, v = tmp_data)
        }
        colnames(coarse_data) <- names(training_data)
        return(coarse_data)
}

#' internal
#'
#' Not to be used for external
#' @param dt.ad MVAD obj
#' @param coarse_data data.table
#'
linearize_data <- function(dt.ad, coarse_data) {
        ## This function takes in a data.table of data and removes a linear trend line.
        ##  This trend line is fit using least squares and the slope and y offset is
        ##  collected. The detrended data is per variable which are stored as the input
        ##  data's columns. Outputted are these parameters which are used to '
        ##
        ## Input: data.table
        ##
        ## Output: list containing: data.table and two collections
        ##
        linearized_data <- data.table()
        detrend_parameter_m <- c(rep(0, dt.ad$params$num_variables))
        detrend_parameter_b <- c(rep(0, dt.ad$params$num_variables))
        indx <- 1
        for (v in names(coarse_data)) {
                tmp_data <- detrend(coarse_data[[v]])
                linearTrend <- coarse_data[[v]] - tmp_data
                tmp_m <- (linearTrend[2]-linearTrend[1])/(2.0-1.0)
                tmp_b <- linearTrend[1] - tmp_m
                linearized_data <- cbind(linearized_data, tmp_data)
                detrend_parameter_m[indx] <- tmp_m
                detrend_parameter_b[indx] <- tmp_b
                indx <- indx + 1
        }
        colnames(linearized_data) <- names(coarse_data)
        return(list("linearized_data" = linearized_data,
                    "detrend_parameter_m" = detrend_parameter_m,
                    "detrend_parameter_b" = detrend_parameter_b))
}

#' internal
#'
#' Not to be used for external
#' @param dt.ad MVAD obj
#' @param linearized_data data.table
#'
normalize_data <- function(dt.ad, linearized_data) {
        ## This function 'standardizes' the data by dividing the data points by the standard deviation of each
        ##   column.
        ##
        ##  Input: data.table
        ##  Output: s3 object
        ##      $standardized_data : data.table
        ##      $standardized_stds : collection of length(dt.ad$params$num_variables)
        ##
        standardized_data <- data.table()
        standardized_stds <- c(rep(0, dt.ad$params$num_variables))
        indx <- 1
        for (v in names(linearized_data)) {
                standardized_stds[indx] <- sd(linearized_data[[v]])
                tmp_data <- (linearized_data[[v]] / standardized_stds[indx])
                standardized_data <- cbind(standardized_data, tmp_data)
                indx <- indx + 1
        }
        colnames(standardized_data) <- names(linearized_data)
        return( list("standardized_data" = standardized_data,
                     "standardized_stds" = standardized_stds))
}

#' internal
#'
#' Not to be used for external
#' @param data.preprocessed MVAD obj
#' @param data_to_use data.table
#'
detrend_data <- function(data.preprocessed, data_to_use = data.preprocessed$raw_testing_data) {
        ## This function removes the trend line (defined by m and b) and removes the stds from training data
        ##
        ms <- data.preprocessed$linearized_data$detrend_parameter_m
        bs <- data.preprocessed$linearized_data$detrend_parameter_b
        stds <- data.preprocessed$normalized_data$standardized_stds
        detrended_eval_data <- data.table()
        indx <- 1
        for (v in names(data_to_use)) {
                detrend_line <- (data.preprocessed$params$tau + 1):(data.preprocessed$params$tau + nrow(data_to_use)) * (1/data.preprocessed$params$data_resolution) * ms[indx] + bs[indx]
                tmp_data <- (data_to_use[[v]] - detrend_line) / stds[indx]
                detrended_eval_data <- cbind(detrended_eval_data, tmp_data)
                indx <- indx + 1
        }
        colnames(detrended_eval_data) <- names(data_to_use)
        if (identical(data_to_use, data.preprocessed$raw_testing_data)) {
                ## defualt
                data.preprocessed$detrended_eval_data = detrended_eval_data
                return(data.preprocessed)
        } else { ## if used for other stuff
                return(detrended_eval_data)
        }
}

#' internal
#'
#' Not to be used for external
#' @param dt.ad MVAD obj
#'
predict_data <- function(dt.ad) {
        ## This function is needed because the data resolution is different from the detection resolution
        ##  The detection resolution is every data point so if data resolution is greater than 1, we need to
        ## manually predict fuuture points.
        ##
        ##   Input: VAR model object, integer value of number of points to use.
        ##   Output: An s3 object
        ##          $predictions : data.table
        ##          $covs : data.table of covariances
        ##
        ##

        ##
        ##  This is kind of tricky: We need to train (length(dt.ad$params$data_resolution)) number of VARS and use dynamic
        ##    programming to fill in a recursive structure. The data structure needs to store the following:
        ##    tuple: (X_t , cov_t) the future


        ## Note: We should fill in  like: our A values should be in cache... same O tho
        ##                                     x . . . . . . . . . . .
        ##                                     x . . . x . . . . . . .
        ##                                     x . . . x . . . x . . .
        ##                                     x x . . x . . . x . . .
        ##                                     x x . . x x . . x . . .
        ##                                     x x . . x x . . x x . .
        ##                                     x x x . x x . . x x . .
        ##                                     x x x . x x x . x x . .
        ##                                     x x x . x x x . x x x .
        ##                                     x x x x x x x . x x x .
        ##                                     x x x x x x x x x x x .
        ##                                     x x x x x x x x x x x x
        ##

        ## For now we assume  P = 1
        ## In the future we should expand this to k lags (actually not that hard just a bit confusing to get probs)
        ##

        ## initialize values:
        ## given (dt.ad$params$data_resolution number of historic points) * 2

        ## get detrended training data (just enough to use)

        ##
        data_prev <- data.table()
        starting_point <- nrow(dt.ad$raw_training_data) - 2 * dt.ad$params$data_resolution + 1
        detrended_training_data <- detrend_data(dt.ad, dt.ad$raw_training_data[starting_point:nrow(dt.ad$raw_training_data),])
        starting_point <- 1
        # smooth data based on dt.ad$params$data_resolution (obvserved values i.e., X_t-1 )
        ############################################dt.ad$raw_training_data[nrows()]
        for (x in 1:(dt.ad$params$data_resolution)) {
                data_prev_tmp <- data.table()
                for (name in colnames(detrended_training_data)) {
                        data_prev_tmp <- cbind(data_prev_tmp,
                                               data.table(mean(detrended_training_data[starting_point:(starting_point + dt.ad$params$data_resolution )][[name]])))
                }
                ## populate historic X_prevs
                data_prev <- rbind(data_prev, data_prev_tmp)
                starting_point <- starting_point + 1
        }
        colnames(data_prev) <- names(dt.ad$raw_training_data)

        # average future observed values ( if running iteratively should only loop once )
        starting_point <- nrow(detrended_training_data) - dt.ad$params$data_resolution + 1
        data_obvs <- data.table()
        for (x in 1:nrow(dt.ad$detrended_eval_data)) {
                data_obvs_tmp <- data.table()
                for (name in colnames(dt.ad$detrended_eval_data)) {
                        if(x < dt.ad$params$data_resolution) {
                                data_obvs_tmp <- cbind(data_obvs_tmp, data.table(mean(c(detrended_training_data[starting_point:(starting_point + dt.ad$params$data_resolution - x),][[name]],
                                                                                         dt.ad$detrended_eval_data[1:x,][[name]]
                                        ))))
                        }
                        else {
                                data_obvs_tmp <- cbind(data_obvs_tmp,
                                                       data.table( mean(dt.ad$detrended_eval_data[(x - dt.ad$params$data_resolution + 1):x,] [[name]]
                                                                        )))
                        }
                }
                data_obvs <- rbind(data_obvs, data_obvs_tmp)
                starting_point <- starting_point + 1
        }
        colnames(data_obvs) <- names(dt.ad$raw_training_data)

        # compute expected values
        # For every future value compute the probability distribution of error
        A <- Acoef(dt.ad$model)[[1]] ## TODO expand for arbitrary
        obs <- nrow(dt.ad$model$datamat)
        init_covariance <- list(cov(residuals(dt.ad$model)) * (obs-1)/ (obs - (ncol(dt.ad$model$datamat) - dt.ad$model$K)))

        # Future covariance formula ~ A %*% covariances[x-1] %*% t(A) + init_covariances
        x_expt <- data.table()
        covs <- rep(init_covariance, nrow(data_obvs)) # store covariances (avoid recursion)
        for (x in 1:nrow(data_obvs)) {
                if(x <= dt.ad$params$data_resolution) {
                        x_expt <- rbind(x_expt, data.table(unlist(data_prev[(nrow(data_prev) - dt.ad$params$data_resolution + x), ]) %*% A))
                }
                else{
                        x_expt <- rbind(x_expt, data.table(unlist(x_expt[(x - dt.ad$params$data_resolution), ]) %*% A))
                        covs[[x]] <- covs[[x]] + A %*% covs[[x-dt.ad$params$data_resolution]] %*% t(A)
                }
        }
        colnames(x_expt) <- names(dt.ad$raw_training_data)
        dt.ad$predict_x_expected = x_expt
        dt.ad$predict_x_oberved = data_obvs
        dt.ad$covariance_matrices = covs
        return(dt.ad)
}



