#Classifier Training Functions
#https://github.com/ustunb/classification-pipeline
#Berk Ustun | www.berkustun.com

#methods with class weight support:
#lars_elasticnet, rounded_elasticnet, ahrs

#methods with case weight support:
#glmnet
#gbm
#ahrs

#methods without case weight support:
#e1071
#RWeka
#OneR
#randomForest
#Rpart
#C5.0

#Libraries
required_packages = c('MASS', 'methods', 'gtools', 'rpart', 'C50', 'OneR', #'RWeka',
                      'glmnet', 'gbm', 'randomForest', 'e1071', 'dplyr');
for (pkg in required_packages){
    suppressPackageStartupMessages(library(pkg, character.only = TRUE, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE));
}

#override conflicts
select=dplyr::select

#### logging #####
print.method.is.starting.message = function(method_name, extra_message = NULL) {

    print_message = sprintf("Starting %s on %s with weights w- = %1.3f w+ = %1.3f",
                            method_name,
                            data_name,
                            class_weights[1],
                            class_weights[2]);

    if (!is.null(extra_message)){
        print_message = sprintf("%s%s", print_message, extra_message)
    }

    print.to.console(print_message)
    return(print_message)
}

print.method.is.ending.message = function(method_name, extra_message = NULL) {

    print_message = sprintf("Finished %s on %s with weights w- = %1.3f w+ = %1.3f\n",
                            method_name,
                            data_name,
                            class_weights[1],
                            class_weights[2]);
    if (!is.null(extra_message)){
        print_message = sprintf("%s%s", print_message, extra_message)
    }

    print.to.console(print_message)
    return(print_message)
}

#### easy parameter tuning and results management #####

create.parameter.df = function(parameter_list, K){
    #given a named list of N parameters and a max number of folds (K)
    #returns a data.frame with columns
    #
    #[fold, parameter_1_name, parameter_1_value,... parameter_N_name, parmaeter_N_value]
    #
    #where each row is distinct (fold, parameter) instance that needs to be trained

    n_parameters = length(parameter_list)
    fold_list = list(fold = seq.int(0, K))

    if (n_parameters == 0){
        return(data.frame(fold_list, stringsAsFactors = FALSE));
    }

    #get parameter names from list
    parameter_names = names(parameter_list)
    parameter_names = parameter_names[order(parameter_names)]

    #create initial df with headers [fold_column_name, parameter_names (sorted alphabetically)]
    parameter_value_df = expand.grid(c(fold_list, parameter_list), stringsAsFactors = FALSE) %>%
        select(sort(current_vars())) %>%
        select(fold, everything())

    parameter_numbers = seq.int(1, n_parameters)
    names(parameter_value_df)[seq.int(2, n_parameters + 1)] = sprintf("parameter_%d_value", parameter_numbers)

    #convert df with headers [fold_column_name, parameter_1_name, parameter_1_value...]
    if (n_parameters == 1){
        parameter_df = parameter_value_df;
        parameter_df$parameter_1_name = parameter_names[1];
    } else {
        parameter_name_df = as.list(parameter_names)
        names(parameter_name_df) = sprintf("parameter_%d_name", parameter_numbers)
        parameter_name_df = data.frame(parameter_name_df, stringsAsFactors = FALSE);
        parameter_name_df = parameter_name_df[rep(1, each = nrow(parameter_value_df)), ]
        parameter_df = bind_cols(parameter_value_df, parameter_name_df)
    }

    #order columns, rows, and make sure that everything is distinct
    parameter_df = parameter_df %>%
        select(sort(current_vars())) %>%
        select(fold, everything()) %>%
        arrange(fold) %>%
        distinct()

    return(parameter_df);
}

create.results.df  = function(parameter_list, K){

    results_df = create.parameter.df(parameter_list, K);

    #accuracy metrics
    SET_NAMES = c("train", "valid", "test")
    STAT_NAMES = c("true_positives", "false_positives", "true_negatives", "false_negatives")

    metric_names = expand.grid(set_name = SET_NAMES,
                               stat_name = STAT_NAMES,
                               stringsAsFactors = FALSE) %>%
        mutate(metric_name = paste0(set_name,"_", stat_name)) %>%
        pull(metric_name)

    results_df[, metric_names] = NA

    #model_size, id, print_model_id, debug_model_id
    results_df = results_df %>%
        mutate(model_size = NA,
               id = row_number(),
               runtime = NA,
               print_model_id = sprintf("M%08d", id),
               debug_model_id = sprintf("M%08d", id))

    return(results_df);

}

convert.parameter.df.row.to.list = function(parameter_df_row){
    #given a 1-row data.frame() in the form:
    #
    #parameter_1_name, parameter_1_value ... parameter_N_name, parameter_N_value
    #"alpha", 1.0                       ...  "beta", TRUE
    #
    #will return a named list in the form:
    #list("alpha" = 1.0,...,"beta" = TRUE)
    stopifnot(nrow(parameter_df_row) == 1)

    cnames = colnames(parameter_df_row)
    name_cols = grep("parameter_[0-9]+_name", cnames, value = TRUE)
    value_cols = grep("parameter_[0-9]+_value", cnames, value = TRUE)

    #check rep
    n_param = length(name_cols)
    stopifnot(length(value_cols) == n_param)
    param_list = list()
    if (n_param > 0){
        for (n in 1:n_param){
            pname = parameter_df_row %>% pull(name_cols[n])
            pvalue = parameter_df_row %>% pull(value_cols[n])
            if (is.factor(pvalue)){
                pvalue = as.vector(pvalue)
            }
            param_list = c(param_list, setNames(list(pvalue), pname))
        }
    }
    return(param_list);
}

get.filter.string = function(filter_row_df, negate = FALSE){

    filter_string = ""
    fnames = colnames(filter_row_df)
    n_pairs = ncol(filter_row_df)
    for (n in 1:n_pairs){
        fname = fnames[n]
        fvalue = filter_row_df[[fname]]
        if (is.character(fvalue)){
            fvalue = paste0("'", fvalue, "'")
        }
        if (negate){
            if (n < n_pairs){
                filter_string = paste0(filter_string,fname,"!=", fvalue, "|")
            } else {
                filter_string = paste0(filter_string,fname,"!=", fvalue)
            }
        } else {
            if (n < n_pairs){
                filter_string = paste0(filter_string,fname,"==", fvalue, "&")
            } else {
                filter_string = paste0(filter_string,fname,"==", fvalue)
            }
        }
    }
    return(filter_string)
}

#### error computation #####
compute.error.metrics = function(error_type, y_hat, ind_pos_type, ind_neg_type, sample_weights_type = NULL){
    #computes class based error metrics for all methods
    error_output = list();
    if (is.null(sample_weights_type)){
        if (length(dim(y_hat)) > 1){
            error_output$true_positives = apply(y_hat[ind_pos_type,], 2, function(y) sum(y == 1));
            error_output$false_negatives = apply(y_hat[ind_pos_type,], 2, function(y) sum(y != 1));
            error_output$true_negatives = apply(y_hat[ind_neg_type,], 2, function(y) sum(y != 1));
            error_output$false_positives = apply(y_hat[ind_neg_type,], 2, function(y) sum(y == 1));
        } else {
            error_output$true_positives = sum(y_hat[ind_pos_type]==1);
            error_output$false_negatives = sum(y_hat[ind_pos_type]!=1);
            error_output$true_negatives = sum(y_hat[ind_neg_type]!=1);
            error_output$false_positives = sum(y_hat[ind_neg_type]==1);
        }
    } else {
        if (length(dim(y_hat)) > 1){
            error_output$true_positives = apply(y_hat[ind_pos_type,], 2, function(y) sum(sample_weights_type[ind_pos_type] * (y == 1)));
            error_output$false_negatives = apply(y_hat[ind_pos_type,], 2, function(y) sum(sample_weights_type[ind_pos_type] * (y != 1)));
            error_output$true_negatives = apply(y_hat[ind_neg_type,], 2, function(y) sum(sample_weights_type[ind_neg_type] * (y != 1)));
            error_output$false_positives = apply(y_hat[ind_neg_type,], 2, function(y) sum(sample_weights_type[ind_neg_type] * (y == 1)));
        } else {
            error_output$true_positives = sum(sample_weights_type[ind_pos_type] * (y_hat[ind_pos_type]==1));
            error_output$false_negatives = sum(sample_weights_type[ind_pos_type] * (y_hat[ind_pos_type]!=1));
            error_output$true_negatives = sum(sample_weights_type[ind_neg_type] * (y_hat[ind_neg_type]!=1));
            error_output$false_positives = sum(sample_weights_type[ind_neg_type] * (y_hat[ind_neg_type]==1));
        }
    }
    names(error_output) = paste0(error_type, "_", names(error_output));
    error_df = data.frame(error_output, row.names = NULL, check.rows = TRUE, check.names = TRUE, stringsAsFactors = FALSE);
    return(error_df);
}

#### ad hoc risk score helper methods
predict.logit = function(newx, coefs){
    if (length(coefs) == ncol(newx) + 1){
        scores = coefs[1] + (newx %*% coefs[-1]);
    } else {
        scores = newx %*% coefs;
    }
    probabilities = inv.logit(scores);
    predictions = round(probabilities);
    return(predictions);
}

get.histogram.risk = function(model, X_new){
    if (length(model$coefficients) == ncol(X_new) + 1){
        scores = model$coefficients[1] + X_new %*% model$coefficients[-1];
    } else {
        scores = X_new %*% model$coefficients;
    }
    idx = findInterval(x = scores, vec = model$score_bins);
    phat = as.vector(model$risk_df[idx,"risk_pdf"]);
    return(phat)
}

get.histogram.decision = function(model, X_new, threshold_risk = 0.5){
    if (length(model$coefficients) == ncol(X_new) + 1){
        scores = model$coefficients[1] + X_new %*% model$coefficients[-1];
    } else {
        scores = X_new %*% model$coefficients;
    }
    idx = findInterval(x = scores, vec = model$score_bins)
    yhat = as.vector(model$risk_df[idx,"risk_cdf"] >= threshold_risk);
    return(yhat)
}

get.auc.of.linear.model = function(coefs, X, Y, weights = NULL){

    pos_ind = Y == 1;

    if (is.null(weights)){
        N = nrow(X);
        N_pos = sum(pos_ind);
        N_neg = N - N_pos;
        weights = as.vector(rep(1,N))
    } else {
        weights = as.vector(weights);
        N = sum(weights);
        N_pos = sum(weights[pos_ind]);
        N_neg = N - N_pos;
    }

    #drop intercept from X
    if (INTERCEPT_NAME %in% colnames(X)){
        to_keep = !(INTERCEPT_NAME == colnames(X))
        X = X[, to_keep]
    }

    #drop intercept from coefs
    if (INTERCEPT_NAME %in% names(coefs)){
        to_keep = !(INTERCEPT_NAME == names(coefs))
        coefs = coefs[to_keep]
    }

    probabilities = inv.logit(X %*% coefs);
    idx = order(probabilities, decreasing = TRUE);
    Y = Y[idx];
    weights = weights[idx];
    TP = cumsum(weights*(Y==1));
    FP = cumsum(weights*(Y==0));
    keep_idx = !rev(duplicated(rev(probabilities[idx])));

    ## remove fp & tp for duplicated predictions
    #as duplicated keeps the first occurrence, but we want the last, two rev are used.
    TP = c(0, TP[keep_idx]);
    FP = c(0, FP[keep_idx]);

    ## compute auc
    auc = 0;
    for (k in 2:length(FP)) {
        auc <- auc + abs(FP[k] - FP[k-1]) * (TP[k] + TP[k-1]);
    }
    auc = auc / (2 * N_neg * N_pos);

    # validation using ROCR
    # require(ROCR)
    # pred_object = prediction(probabilities, labels = Y);
    # auc_rocr = performance(pred_object, measure = "auc")@y.values[[1]]
    # stopifnot(auc==auc_rocr)
    return(auc)
}

scale.then.round.coefficients = function(coefs, Lj_min, Lj_max){
    max_coefs = pmax(abs(Lj_min), abs(Lj_max));
    scale_factor = max(abs(coefs)/max_coefs);
    coefs = round(coefs/scale_factor);
    coefs = pmin(pmax(coefs, Lj_min), Lj_max);
    return(coefs);
}

cap.then.round.coefficients = function(coefs, Lj_min, Lj_max){
    coefs = pmin(pmax(round(coefs), Lj_min), Lj_max);
    return(coefs);
}

get.unit.coefficients = function(coefs){
    coefs[coefs > 0] = 1
    coefs[coefs < 0] = -1
    return(coefs);
}

get.risk.model = function(coefs, X, Y, n_bins = 10, platt_scaling = FALSE){

    if (length(coefs) == ncol(X) + 1){
        scores = coefs[1] + (X %*% coefs[2:length(coefs)])
    } else {
        scores = X %*% coefs
    }

    bins = suppressWarnings(cut2(x = scores, g = n_bins, oneval = FALSE, onlycuts = TRUE))
    nb = length(bins)

    if (nb > 1) {
        bins[1] = -Inf
        bins[nb] = Inf
        risk_df = data.frame(score.min = bins[1:nb-1], score.max = bins[2:nb])
        binned_scores = as.numeric(cut2(x = scores, cuts = bins, oneval = FALSE))
        for (b in (1:nb-1)){
            idx = b==binned_scores
            risk_df[b, "n_total"] = sum(idx)
            risk_df[b, "n_positive"] = sum(Y[idx]>0)
        }
        stopifnot(sum(risk_df$n_total) == nrow(X),
                  sum(risk_df$n_positive) == sum(Y>0))

        risk_df = risk_df %>%
            mutate(risk_cdf = cumsum(n_positive)/length(scores)) %>%
            rowwise() %>%
            mutate(risk_pdf = n_positive / n_total) %>%
            ungroup() %>%
            select(-n_positive, -n_total)

    } else if (nb == 1) {

        if (is.infinite(bins)) {

            risk_df = data.frame(score.min = -Inf,
                                 score.max = Inf,
                                 risk_pdf = mean(Y),
                                 risk_cdf = 1)
        } else {

            risk_df = data.frame(score.min = c(-Inf, bins),
                                 score.max = c(bins, Inf),
                                 risk_pdf = c(mean(Y), 0),
                                 risk_cdf = c(0, 1));

        }
    }

    stopifnot(any(risk_df$score.min==-Inf),
              any(risk_df$score.max==Inf),
              any(!is.na(risk_df$risk_pdf)),
              any(!is.na(risk_df$risk_cdf)))

    model = list(coefficients = coefs,
                 score_bins = bins,
                 risk_df = risk_df);

    return(model);
}

get.coefficients.from.model = function(m){
    if (is.list(m) && (!is.null(m[["coefficients"]]))){
        coefs = m[["coefficients"]];
    } else {
        coefs = m;
    }
    stopifnot(is.numeric(coefs));
    return(coefs);
}

check.ahrs.model = function(m){
    if (is.list(m) && (!is.null(m[["coefficients"]])) && (!is.null(m[["score_bins"]])) && (!is.null(m[["risk_df"]]))){
        stopifnot(all(c("score.min", "score.max", "risk_cdf", "risk_pdf") %in% colnames(m$risk_df)))
    } else if (is.numeric(m)){
        stopifnot(length(m) > 0)
    }
    return(TRUE)
}

check.coefficient.constraints = function(model, coefficient_set, rounding_type = "none"){

    coefs = get.coefficients.from.model(model);

    # check model size
    intercept_idx = names(coefs) == INTERCEPT_NAME
    model_size = sum(coefs[intercept_idx]!=0);
    stopifnot(model_size >= coefficient_set$L0_min,
              model_size <= coefficient_set$L0_max)

    # check coefficients
    if (rounding_type != "none"){

        stopifnot(all(coefs >= coefficient_set$Lj_min),
                  all(coefs <= coefficient_set$Lj_max))

        # check rounding method
        if (rounding_type == "unit") {
            stopifnot(abs(coefs) <= 1)
        } else if (rounding_type == "capped"){
            stopifnot(all(coefs >= coefficient_set$Lj_min),
                      all(coefs <= coefficient_set$Lj_max))
        } else if (rounding_type == "scaled"){
            stopifnot(all(coefs >= coefficient_set$Lj_min),
                      all(coefs <= coefficient_set$Lj_max),
                      any(coefs == coefficient_set$Lj_max | coefs == coefficient_set$Lj_min))
        }
    }

    return(TRUE);
}

#### ad hoc risk score training functions ####

run.elastic.net = function(X, Y, coefficient_set, alpha_value = 0.5, nlambda = 100, set_coef_limit = FALSE, set_size_limit = FALSE){
    #runs L1/L2 penalized logistic regression models using glmnet
    #returns K + 1 models at all effective L1/L2 penalties for fixed values of alpha and class_weights
    #X, Y, X_test, Y_test, folds class_weights, has_class_weights, sample_weights, has_sample_weights, has_test_data
    #set alpha = 0.0 for pure L1 penalty (lasso)
    #set alpha = 1.0 for pure L2 penalty (ridge)
    #set alpha in (0.0,1.0) for combined L1/L2 penalty (elasticnet)

    #default glmnet inputs
    call_inputs = list(x = X[, colnames(X) != INTERCEPT_NAME],
                       y = as.factor(Y),
                       family = "binomial",
                       alpha = alpha_value,
                       nlambda = nlambda,
                       maxit = 1000000)


    #limit size directly in glmnet
    if (set_size_limit && (coefficient_set$L0_max < ncol(X))){
        print.to.console('coefficient_set includes hard constraint on model_size');
        print.to.console(sprintf('Setting pmax = %d', coefficient_set$L0_max))
        call_inputs$pmax = coefficient_set$L0_max;
    }

    #limit coef directly in glmnet
    if (set_coef_limit && !all((coefficient_set$coef_ub == Inf) & (coefficient_set$coef_lb == -Inf))){
        print.to.console(sprintf('coefficient_set includes bounds on coefficients'))
        print.to.console(sprintf('Setting lower.limits = c(%s)', paste(coefficient_set$coef_lb, collapse = ",")))
        print.to.console(sprintf('Setting upper.limits = c(%s)', paste(coefficient_set$coef_ub, collapse = ",")))
        call_inputs$lower.limits = coefficient_set$coef_lb;
        call_inputs$upper.limits = coefficient_set$coef_ub;
    }

    if (has_sample_weights){
        print.to.console("Using sample weights");
        weights = as.vector(sample_weights)
    } else {
        weights = as.vector(rep(1,nrow(X)))
    }

    if (has_class_weights){
        print.to.console("Using class weights");
        weights[Y!=1] = class_weights[1] * weights[Y!=1];
        weights[Y==1] = class_weights[2] * weights[Y==1];
    }

    if (has_sample_weights || has_class_weights){
        call_inputs$weights = as.vector(weights)
    }

    #fit model using glmnet
    model = do.call(glmnet, call_inputs);

    #process coefficients
    coefficients = coef(model);
    coefficient_names = rownames(coefficients)
    variable_idx = coefficient_names != INTERCEPT_NAME

    #remove models that are too large
    model_sizes = apply(coefficients[variable_idx, ], 2, function(b) sum(b != 0.0));
    to_keep = model_sizes <= coefficient_set$L0_max;
    if (any(to_keep)){
        coefficients = as.matrix(coefficients[, to_keep]);
    } else {
        coefficients = as.matrix(coefficients[, 1]);
        coefficients[variable_idx,] = 0.0
    }

    #choose best model
    auc_values = apply(coefficients, 2, function(b) get.auc.of.linear.model(b, X, Y, weights));
    best_idx = which.max(auc_values);

    results = list(coefficients = coefficients[, best_idx],
                   lambda = model$lambda[best_idx],
                   auc = as.numeric(auc_values[best_idx]),
                   alpha = alpha_value)
    return(results)
}

run.penalized.logistic.regression = function(X, Y, coefficient_set, method_settings){

    subresults = vector("list", length(method_settings$alpha_values));
    for (i in 1:length(method_settings$alpha_values)){
        subresults[[i]] = run.elastic.net(X, Y, coefficient_set,
                                          alpha_value = method_settings$alpha_values[i],
                                          nlambda = method_settings$nlambda,
                                          set_coef_limit = method_settings$set_coef_limit,
                                          set_size_limit = method_settings$set_size_limit);
    }

    #return coefficients of best model
    auc_values = sapply(subresults, function(r) r$auc)
    best_idx = which.max(auc_values);
    best_results = subresults[[which.max(auc_values)]]
    return(best_results$coefficients)
}

run.stepwise.logistic.regression = function(X, Y, coefficient_set, method_settings){
    # http://rstudio-pubs-static.s3.amazonaws.com/2899_a9129debf6bd47d2a0501de9c0dc583d.html

    direction = method_settings$direction
    trace_flag = method_settings$trace_flag
    L0_max = coefficient_set$L0_max;

    #drop intercept and setup coefficient vector
    variable_names = colnames(X);
    if (any(variable_names == INTERCEPT_NAME)){
        coefficient_names = variable_names;
        to_keep = !(variable_names == INTERCEPT_NAME)
        X = X[,to_keep]
        variable_names = variable_names[to_keep]
    } else {
        coefficient_names = c(INTERCEPT_NAME, variable_names);
    }
    coefficients = setNames(rep(0, length(coefficient_names)), coefficient_names);
    n_variables = length(variable_names);

    #create train_data
    colnames(Y) = "y"
    train_data = as.data.frame(cbind(X, Y));

    # colnames(Y_train) = "Y"
    # variable_names = colnames(X_train);
    # to_keep = !(variable_names == INTERCEPT_NAME)
    # X_train = X_train[, to_keep]
    # variable_names[to_keep]
    # train_data = as.data.frame(cbind(X_train, Y_train));

    n_variables = length(variable_names);
    if (direction == "forward") {
        # adds variables to null model until either:
        # (i) AIC[n] > training AIC[n+1]
        # (ii) n == L0_max
        # where n = model_size
        # initial_formula = "Y ~ 1"
        # model = glm(initial_formula, family = binomial, data = train_data, x = FALSE, y = FALSE);
        # model_size = length(model$coefficients) - 1;
        # added_variables = rep(NA, 0)
        #
        # model_full = glm(formula = Y ~ ., family = binomial, data = train_data, x = FALSE, y = FALSE);
        # scope_formula = as.formula(model_full)
        #
        # #add variables until model size <= L0_max
        # while (model_size < L0_max){
        #     info = add1(model, scope = scope_formula, test = "LRT", trace = trace_flag);
        #     to_add = rownames(info)[which.max(info$LRT)]
        #     print.to.console(sprintf("adding %s", to_add));
        #     added_variables = c(added_variables, to_add);
        #     model_formula = paste0(initial_formula, paste0(sprintf("+%s", added_variables), collapse = " "));
        #     model = glm(model_formula, family = binomial, data = train_data, x = FALSE, y = FALSE);
        #     model_size = length(model$coefficients) - 1;
        # }

        train_data$y = as.numeric(train_data$y)
        model = glm(y ~ 1, family = binomial, data = train_data)
        model_size = length(model$coefficients) - 1;
        model_full = glm(formula = y ~ ., family = binomial, data = train_data)
        scope_formula = as.formula(model_full)
        model = stepAIC(model, scope =  scope_formula, direction = "forward", steps = min(n_variables, L0_max), trace = trace_flag)

    } else if (direction == "backward") {

        initial_model = glm(formula = "y ~ .", family = binomial, data = train_data);
        dropped_variables = names(initial_model$coefficients[is.na(initial_model$coefficients)]);
        model_formula = paste0("y ~ . ", paste0(sprintf("-%s", dropped_variables), collapse = " "));
        model = glm(formula = model_formula, family = binomial, data = train_data);
        model_size = length(model$coefficients) - (INTERCEPT_NAME %in% names(model$coefficients))

        #drop variables until model size <= L0_max
        while (model_size > L0_max){
            info = drop1(model, test = "LRT", trace = trace_flag);
            to_drop = rownames(info)[which.min(info$LRT)]
            model_formula = paste0(model_formula, " -", to_drop);
            model = glm(formula = model_formula, family = binomial, data = train_data);
            model_size = length(model$coefficients) -  (INTERCEPT_NAME %in% names(model$coefficients));
            dropped_variables = c(dropped_variables, to_drop);
        }

        #standard backward elimination
        model = step(model, direction = "backward", trace = trace_flag)

    } else if (direction == "both") {

        train_data$y = as.numeric(train_data$y)
        model = glm(y ~ 1, family = binomial, data = train_data)
        model_size = length(model$coefficients) - 1;
        model_full = glm(formula = y ~ ., family = binomial, data = train_data)
        scope_formula = as.formula(model_full)
        model = stepAIC(model, scope =  scope_formula, direction = "both", steps = min(n_variables, L0_max),  trace = trace_flag)
    }

    #return coefficients
    coefs = model$coefficients;
    coefs = coefs[!is.na(coefs)];
    coefficients[names(coefs)] = coefs;

    return(coefficients);
}

run.ahrs = function(method_settings){

    #unpack method_settings
    print.method.is.starting.message(method_settings$method_name);
    training_start_time = proc.time();
    variable_names = colnames(X);

    if (has_sample_weights){
        print.to.console("Using sample weights");
        weights = sample_weights
    } else {
        weights = rep(1, nrow(X));
    }

    if (has_class_weights) {
        print.to.console("Using class weights");
        weights[Y!=1] = class_weights[1] * weights[Y!=1];
        weights[Y==1] = class_weights[2] * weights[Y==1];
    }

    #hard constraint warnings
    if (coefficient_set$L0_max < (ncol(X) -1)){
        print.to.console('coefficient_set includes hard constraint on model_size');
        print.to.console(sprintf('setting pmax = %d', coefficient_set$L0_max))
    }

    # set rounding function
    print.to.console(sprintf("rounding_type is %s", method_settings$rounding_type));

    if (method_settings$rounding_type == "scaled" && all(is.infinite(coefficient_set$coef_lb) & is.infinite(coefficient_set$coef_ub))){
        print.to.console("warning: all bounds on coefficients are set as +/- Inf")
        print.to.console(sprintf('min(coefs) >= c(%s)', paste(coefficient_set$coef_lb, collapse = ",")))
        print.to.console(sprintf('max(coefs) <= c(%s)', paste(coefficient_set$coef_ub, collapse = ",")))
        print.to.console("switching from scaled rounding to capped rounding")
        method_settings$rounding_type = "scaled"
    }

    if (method_settings$rounding_type == "scaled") {
        print.to.console("will scale then round coefficients")
        print.to.console(sprintf('min(coefs) >= c(%s)', paste(coefficient_set$coef_lb, collapse = ",")))
        print.to.console(sprintf('max(coefs) <= c(%s)', paste(coefficient_set$coef_ub, collapse = ",")))
        rounding_function = function(x){scale.then.round.coefficients(x, coefficient_set$Lj_min, coefficient_set$Lj_max)};
    } else if (method_settings$rounding_type == "capped") {
        print.to.console("will cap then round coefficients")
        print.to.console(sprintf('min(coefs) >= c(%s)', paste(coefficient_set$coef_lb, collapse = ",")))
        print.to.console(sprintf('max(coefs) <= c(%s)', paste(coefficient_set$coef_ub, collapse = ",")))
        rounding_function = function(x){cap.then.round.coefficients(x, coefficient_set$Lj_min, coefficient_set$Lj_max)};
    } else if (method_settings$rounding_type == "unit") {
        print.to.console("will set coefficients to +1/-1")
        print.to.console(sprintf('min(coefs) >= -1'));
        print.to.console(sprintf('max(coefs) <= +1'));
        rounding_function = function(x){get.unit.coefficients(x)};
    } else {
        rounding_function = function(x){return(x)};
    }

    # set risk function
    print.to.console(sprintf("risk_model is %s", method_settings$risk_model_type))
    if (method_settings$risk_model_type == "logit") {
        estimate_risk_model = FALSE
        prediction_function = function(model, X){
            if (length(model) == ncol(X) + 1){
                scores = model[1] + X %*% model[-1];
            } else {
                scores = X %*% model;
            }
            probabilities = inv.logit(scores);
            predictions = round(probabilities);
            return(as.vector(predictions));
        }
    } else {
        estimate_risk_model = TRUE
        prediction_function = function(model, X){
                return(get.histogram.decision(model, X_new = X, threshold_risk = 0.5))
        }
    }

    #initialize results df (each row contains results from glmnet)
    results_param = list(feature_selection_type = method_settings$feature_selection_type,
                         rounding_type = method_settings$rounding_type,
                         risk_model_type = method_settings$risk_model_type,
                         max_offset = coefficient_set$max_offset,
                         max_coefficient = coefficient_set$max_coefficient,
                         max_L0 = ifelse(coefficient_set$max_L0 < ncol(X), coefficient_set$max_L0, Inf));

    results_df = create.results.df(results_param, K);
    n_instances = results_df %>% nrow();
    print_models = setNames(vector("list", n_instances), results_df$print_model_id);
    debug_models = setNames(vector("list", n_instances), results_df$debug_model_id);

    #create run_param_df (each row contains parameters for a separate call to glmnet)
    run_param = results_param;
    run_param_df = create.parameter.df(run_param, K)
    n_runs = run_param_df %>% nrow();

    for (ii in 1:n_runs) {

        run_param_row = run_param_df[ii,];
        run_param_list = convert.parameter.df.row.to.list(run_param_row);

        #initialize training dataset
        train_ind       = !(folds == run_param_row$fold);
        X_train         = as.matrix(X[train_ind,]);
        Y_train         = as.matrix(Y[train_ind]);
        ind_pos_train   = Y_train == 1;
        ind_neg_train   = !ind_pos_train;

        #initialize validation dataset
        has_validation_set = run_param_row$fold > 0;
        has_test_set    = nrow(X_test) > 0;
        valid_ind       = folds == run_param_row$fold;
        X_valid         = as.matrix(X[valid_ind,]);
        Y_valid    		= as.matrix(Y[valid_ind]);
        ind_pos_valid   = Y_valid == 1;
        ind_neg_valid   = !ind_pos_valid;

        call_start_time = proc.time();

        # train model
        if (method_settings$feature_selection_type %in% c("forward_stepwise", "backward_stepwise")) {
            coefficients = run.stepwise.logistic.regression(X = X_train,
                                                            Y = Y_train,
                                                            coefficient_set = coefficient_set,
                                                            method_settings = method_settings)
        } else if (method_settings$feature_selection_type == "elastic_net") {
            coefficients = run.penalized.logistic.regression(X = X_train,
                                                             Y = Y_train,
                                                             coefficient_set = coefficient_set,
                                                             method_settings = method_settings)
        }

        # round coefficients;
        coefficients = rounding_function(coefficients);

        # pair model with risk estimates
        if (estimate_risk_model){
            model = get.risk.model(coefs = coefficients,
                                   X = X_train,
                                   Y = Y_train,
                                   n_bins = method_settings$histogram_estimation_maxbins)
        } else {
            model = coefficients;
        }

        #get indices of results_df where results should be stored
        row_idx = results_df %>% filter_(get.filter.string(run_param_row)) %>% pull(id) %>% unlist(use.names = FALSE);

        #model size metriocs
        results_df[row_idx, "model_size"] = sum(coefficients[names(coefficients) != INTERCEPT_NAME] != 0)

        #accuracy metrics on training set
        y_hat = prediction_function(model, X_train);
        if (has_sample_weights){
            error_df = compute.error.metrics("train", y_hat, ind_pos_train, ind_neg_train, sample_weights[train_ind]);
        } else {
            error_df = compute.error.metrics("train", y_hat, ind_pos_train, ind_neg_train);
        }
        results_df[row_idx, names(error_df)] = error_df;

        #accuracy metrics on validation set
        if (has_validation_set){
            y_hat = prediction_function(model, X_valid);
            if (has_sample_weights){
                error_df = compute.error.metrics("valid", y_hat, ind_pos_valid, ind_neg_valid, sample_weights[valid_ind]);
            } else {
                error_df = compute.error.metrics("valid", y_hat, ind_pos_valid, ind_neg_valid);
            }
            results_df[row_idx, names(error_df)] = error_df;
        }

        #accuracy metrics on test set
        if (has_test_set){
            y_hat = prediction_function(model, X_test);
            if (has_sample_weights){
                error_df = compute.error.metrics("test", y_hat, ind_pos_test, ind_neg_test, sample_weights_test);
            } else {
                error_df = compute.error.metrics("test", y_hat, ind_pos_test, ind_neg_test);
            }
            results_df[row_idx, names(error_df)] = error_df;
        }

        #runtime
        runtime = (proc.time() - call_start_time)[["elapsed"]]/length(row_idx);
        results_df[row_idx, "runtime"] = ifelse(is.na(runtime), 0.0, runtime);

        #store coefficients to print in reports
        if (method_settings$save_print_models){
            model_id = results_df[row_idx,]$print_model_id;
            print_models[[model_id]] = model;
        }

        #full model objects (for debugging or other use)
        if (method_settings$save_debug_models){
            model_ids = results_df[row_idx,]$debug_model_id;
            debug_models[[model_id]] = model;
        }
    }

    #build output
    results = list();
    results$method_name = method_settings$method_name;
    results$method_settings = method_settings;
    results$results_df = results_df;
    results$print_models = print_models;
    results$debug_models = debug_models;

    #print completion message
    training_end_time = proc.time() - training_start_time;
    results$total_runtime = training_end_time[["elapsed"]];
    print.to.console(sprintf("Trained %d instances in %1.1f seconds", results$results_df %>% nrow(), results$total_runtime));
    print.method.is.ending.message(method_settings$method_name);
    return(results);
}

train.ahrs = function(all_settings, check_ahrs_flag = FALSE){

    AHRS_FEATURE_SELECTION_ID = c("forward_stepwise"="fwd",
                                  "backward_stepwise"="bwd",
                                  "elastic_net"="enet")

    AHRS_ROUNDING_ID = c("none"="none",
                         "unit"="unit",
                         "capped"="rnd",
                         "scaled"="srnd")

    AHRS_RISK_MODEL_ID = c("logit"="logit",
                           "histogram"="hist",
                           "binning"="bin",
                           "pbinning"="pbin",
                           "platt"="platt")

    #filter feature selection to implemented types
    drop_idx = all_settings$feature_selection_type %nin% names(AHRS_FEATURE_SELECTION_ID)
    all_settings$feature_selection_type[drop_idx] = NULL
    stopifnot(length(all_settings$feature_selection_type) > 0)

    #filter rounding to implemented types
    drop_idx = all_settings$rounding_type %in% names(AHRS_ROUNDING_ID)
    all_settings$rounding_type = all_settings$rounding_type[drop_idx];
    stopifnot(length(all_settings$rounding_type) > 0)

    #filter risk_model to implemented types
    drop_idx = all_settings$risk_model_type %in% names(AHRS_RISK_MODEL_ID)
    all_settings$risk_model_type = all_settings$risk_model_type[drop_idx];
    stopifnot(length(all_settings$risk_model_type) > 0)

    # check to make sure we are not scaling when coefficient bounds are +/- Inf
    if ("scaled" %in% all_settings$rounding_type){
        if (all(is.infinite(coefficient_set$coef_ub) & is.infinite(coefficient_set$coef_lb))){
            print.to.console("warning: read scaled rounding, but all coefficient bounds are set as +/- infinity")
            print.to.console("warning: removing scaled rounding from ahrs methods")
            to_keep = "scaled" != all_settings$rounding_type
            all_settings$rounding_type = all_settings$rounding_type[to_keep]
        }
    }

    #build table of ahrs combinations to train
    helper_df = expand.grid(feature_selection_type = all_settings$feature_selection_type,
                            rounding_type = all_settings$rounding_type,
                            risk_model_type = all_settings$risk_model_type,
                            stringsAsFactors = FALSE) %>%
        mutate(fsl = AHRS_FEATURE_SELECTION_ID[feature_selection_type],
               rnd = AHRS_ROUNDING_ID[rounding_type],
               rsk = AHRS_RISK_MODEL_ID[risk_model_type],
               method_name = sprintf("ahrs_%s_%s_%s", fsl, rnd, rsk),
               save_print_models = all_settings$save_print_models,
               save_debug_models = all_settings$save_debug_models) %>%
        ungroup() %>%
        select(-fsl, -rnd, -rsk) %>%
        select(method_name, everything()) %>%
        arrange(method_name, feature_selection_type, rounding_type, risk_model_type)

    all_results = list();

    for (i in 1:nrow(helper_df)){

        method_settings = helper_df %>% slice(i) %>% as.list();

        if (method_settings$risk_model_type %in% c("histogram", "binning", "pbinning")){
            method_settings$histogram_estimation_pct = all_settings$histogram_estimation_pct;
            method_settings$histogram_estimation_maxbins = all_settings$histogram_estimation_maxbins;
        }

        if (method_settings$feature_selection_type == "elastic_net"){
            method_settings$alpha_values = all_settings$elastic_net_alpha_values;
            method_settings$nlambda = all_settings$elastic_net_nlambda;
            method_settings$set_size_limit = all_settings$elastic_net_set_size_limit;
            method_settings$set_coef_limit = all_settings$elastic_net_set_coef_limit;
        } else if (method_settings$feature_selection_type == "forward_stepwise") {
            method_settings$direction = "forward"
            method_settings$trace_flag = all_settings$stepwise_trace_flag;
        } else if (method_settings$feature_selection_type == "backward_stepwise") {
            method_settings$direction = "backward"
            method_settings$trace_flag = all_settings$stepwise_trace_flag;
        }

        msg = c("-------------------------\n",
                "running ad hoc risk score",
                sprintf("method_name: \t\t%s", method_settings$method_name),
                sprintf("feature_selection: \t%s", method_settings$feature_selection_type),
                sprintf("rounding: \t\t%s", method_settings$rounding_type),
                sprintf("risk_model: \t\t%s\n\n", method_settings$risk_model))
        print.to.console(paste(msg,collapse = "\n"))

        results = run.ahrs(method_settings);
        if (check_ahrs_flag){
            model_chk = sapply(results$print_models, FUN = check.ahrs.model)
            coef_chk = sapply(results$print_models,
                              FUN = function(m){return(check.coefficient.constraints(model = m,
                                                                                     coefficient_set = coefficient_set,
                                                                                     rounding_type = method_settings$rounding_type))})
            stopifnot(all(model_chk), all(coef_chk));
        }
        all_results[[method_settings$method_name]] = results;
    }
    return(all_results);
}

#### general classification methods ####

train.lars = function(method_settings, alpha_value, rounding_type = "none"){
    #runs L1/L2 penalized logistic regression models using glmnet
    #returns K + 1 models at all effective L1/L2 penalties for fixed values of alpha and class_weights
    #X, Y, X_test, Y_test, folds class_weights, has_class_weights, sample_weights, has_sample_weights, has_test_data
    #set alpha = 1.0 for pure L1 penalty (lasso)
    #set alpha = 0.0 for pure L2 penalty (ridge)
    #set alpha in (0.0,1.0) for combined L1/L2 penalty (elasticnet)

    #method_name = "rounded_elasticnet"
    #method_settings = settings[[method_name]]
    #alpha_value = method_settings$alpha_values[1]

    print.method.is.starting.message(method_name);
    print.to.console(sprintf("alpha = %1.2f", alpha_value));

    training_start_time = proc.time();
    if (has_sample_weights){
        print.to.console("Using sample weights");
        weights = sample_weights
    } else {
        weights = rep(1, nrow(X));
    }

    if (has_class_weights) {
        print.to.console("Using class weights");
        weights[Y!=1] = class_weights[1] * weights[Y!=1];
        weights[Y==1] = class_weights[2] * weights[Y==1];
    }

    #y_hat = round(predict(model, newx = X_train, type="response"));
    #y_hat = apply(coefficients, 2, function(c) predict.logit(newx = X_valid, coefs=c));

    if (rounding_type == "scaled") {
        print.to.console(sprintf("rounding_type is %s. Will scale then round coefficients", rounding_type));
        rounding_function = function(x){scale.then.round.coefficients(x, coefficient_set$Lj_min, coefficient_set$Lj_max)};
        prediction_function = function(model, order_idx, coefficients, newx){apply(coefficients, 2, function(c) predict.logit(newx, coefs = c))};
    } else if (rounding_type == "capped") {
        print.to.console(sprintf("rounding_type is %s. Will cap then round coefficients", rounding_type));
        rounding_function = function(x){cap.then.round.coefficients(x, coefficient_set$Lj_min, coefficient_set$Lj_max)}
        prediction_function = function(model, order_idx, coefficients, newx){apply(coefficients, 2, function(c) predict.logit(newx, coefs = c))};
    } else {
        rounding_function = function(x){return(x)};
        prediction_function = function(model, order_idx, coefficients, newx){round(predict(model, newx, type="response"))[,order_idx]};
    }

    # run once to figure out lambda values
    method_settings$param$alpha = alpha_value;
    initial_param_df = create.parameter.df(method_settings$param, 0);
    initial_plist = convert.parameter.df.row.to.list(initial_param_df);

    #hard constraint warnings
    if (coefficient_set$L0_max < ncol(X)){
        print.to.console('coefficient_set includes hard constraint on model_size');
        print.to.console(sprintf('Setting pmax = %d', coefficient_set$L0_max))
    }

    if (!all((coefficient_set$coef_ub == Inf) & (coefficient_set$coef_lb == -Inf))){
        print.to.console(sprintf('coefficient_set includes bounds on coefficients'))
        print.to.console(sprintf('Setting lower.limits = c(%s)', paste(coefficient_set$coef_lb, collapse = ",")))
        print.to.console(sprintf('Setting upper.limits = c(%s)', paste(coefficient_set$coef_ub, collapse = ",")))
    }

    call_inputs = c(initial_plist,
                    list(x = X,
                         y = Y,
                         family = "binomial",
                         weights = weights,
                         foldid = folds,
                         pmax = coefficient_set$max_L0,
                         #lower.limits = coefficient_set$coef_lb,
                         #upper.limits = coefficient_set$coef_ub,
                         type.measure = "class"))

    initial_model = do.call(cv.glmnet, call_inputs);

    #glmnet output is parametrized by lambda decreasing order, we store results in by lambda in increasing order
    lambda = initial_model$lambda;
    lambda_increasing_idx = order(lambda)

    #initialize results df (each row contains results from glmnet)
    results_param = method_settings$param;
    results_param$nlambda = NULL
    results_param$max_offset = coefficient_set$max_offset;
    results_param$max_coefficient = coefficient_set$max_coefficient;
    results_param$max_L0 = ifelse(coefficient_set$max_L0 < ncol(X), coefficient_set$max_L0, Inf);
    results_param$lambda = lambda;
    results_param$alpha = alpha_value;
    results_param$rounding_type = rounding_type;

    results_df = create.results.df(results_param, K);
    n_instances = results_df %>% nrow();
    print_models = setNames(vector("list", n_instances), results_df$print_model_id);
    debug_models = setNames(vector("list", n_instances), results_df$debug_model_id);

    #create run_param_df (each row contains parameters for a separate call to glmnet)
    run_param = results_param;
    run_param_df = create.parameter.df(run_param, K)
    run_param_df = run_param_df %>%
        select(-parameter_2_name, -parameter_2_value,
               -parameter_3_name, -parameter_3_value,
               -parameter_4_name, -parameter_4_value,
               -parameter_4_name, -parameter_4_value,
               -parameter_5_name, -parameter_5_value,
               -parameter_6_name, -parameter_6_value) %>%
        distinct()

    n_runs = run_param_df %>% nrow();
    for (ii in 1:n_runs){

        run_param_row = run_param_df %>% slice(ii)
        run_param_list = convert.parameter.df.row.to.list(run_param_row);

        #initialize training dataset
        train_ind       = !(folds == run_param_row$fold);
        X_train         = as.matrix(X[train_ind,]);
        Y_train   	    = as.matrix(Y[train_ind]);
        ind_pos_train   = Y_train == 1;
        ind_neg_train   = !ind_pos_train;

        #initialize validation dataset
        has_validation_set = run_param_row$fold > 0;
        has_test_set    = nrow(X_test) > 0;
        valid_ind       = folds == run_param_row$fold;
        X_valid         = as.matrix(X[valid_ind,]);
        Y_valid    		= as.matrix(Y[valid_ind]);
        ind_pos_valid   = Y_valid == 1;
        ind_neg_valid   = !ind_pos_valid;

        #train model
        call_inputs = c(run_param_list,
                        list(x = X_train,
                             y = Y_train,
                             pmax = coefficient_set$L0_max,
                             lower.limits = coefficient_set$coef_lb,
                             upper.limits = coefficient_set$coef_ub,
                             family = "binomial",
                             weights = weights[train_ind],
                             lambda = lambda,
                             maxit = 1000000));

        call_start_time = proc.time();
        model = do.call(glmnet, call_inputs);

        #round the coefficients from the model
        coefficients = apply(coef(model), 2, rounding_function);
        coefficients = coefficients[, lambda_increasing_idx];

        #get indices of results_df where results should be stored
        row_idx = results_df %>% filter_(get.filter.string(run_param_row)) %>% pull(id)

        #accuracy metrics on training set
        y_hat = prediction_function(model, lambda_increasing_idx, coefficients, X_train);
        if (has_sample_weights){
            error_df = compute.error.metrics("train", y_hat, ind_pos_train, ind_neg_train, sample_weights[train_ind]);
        } else {
            error_df = compute.error.metrics("train", y_hat, ind_pos_train, ind_neg_train);
        }
        results_df[row_idx, names(error_df)] = error_df;

        #accuracy metrics on validation set
        if (has_validation_set){
            y_hat = prediction_function(model, lambda_increasing_idx, coefficients, X_valid);
            if (has_sample_weights){
                error_df = compute.error.metrics("valid", y_hat, ind_pos_valid, ind_neg_valid, sample_weights[valid_ind]);
            } else {
                error_df = compute.error.metrics("valid", y_hat, ind_pos_valid, ind_neg_valid);
            }
            results_df[row_idx, names(error_df)] = error_df;
        }

        #accuracy metrics on test set
        if (has_test_set){
            y_hat = prediction_function(model, lambda_increasing_idx, coefficients, X_test);
            if (has_sample_weights){
                error_df = compute.error.metrics("test", y_hat, ind_pos_test, ind_neg_test, sample_weights_test);
            } else {
                error_df = compute.error.metrics("test", y_hat, ind_pos_test, ind_neg_test);
            }
            results_df[row_idx, names(error_df)] = error_df;
        }

        #run time metrics (divide by total # of instances trained in one call since glmnet trains nlambda models at the same time)
        runtime = (proc.time() - call_start_time)[["elapsed"]]/length(row_idx);
        results_df[row_idx, "runtime"] = ifelse(is.na(runtime), 0.0, runtime);

        #model size metrics
        variable_names = rownames(coefficients);
        results_df[row_idx, "model_size"] = apply(coefficients[variable_names != "(Intercept)", ], 2, function(b) sum(b != 0.0));

        #store coefficients to print in reports
        if (method_settings$save_print_models){
            models = as.list(as.data.frame(coefficients)); #convert coefficients matrix to a list (1 row = 1 model)
            model_ids = results_df[row_idx,]$print_model_id;
            names(models) = model_ids
            for (model_id in model_ids){
                names(models[[model_id]]) = variable_names;
                print_models[[model_id]] = models[[model_id]];
            }
        }

        #full model objects (for debugging or other use)
        if (method_settings$save_debug_models){
            model_ids = results_df[row_idx,]$debug_model_id;
            for (model_id in model_ids){
                debug_models[[model_id]] = model;
            }
        }
    }

    #build output
    results = list();
    results$method_name = method_name;
    results$method_settings = method_settings;
    results$results_df = results_df;
    results$print_models = print_models;
    results$debug_models = debug_models;

    #print completion message
    training_end_time = proc.time() - training_start_time;
    results$total_runtime = training_end_time[["elapsed"]];
    print.to.console(sprintf("Trained %d instances in %1.1f seconds", results$results_df %>% filter(fold == 0) %>% nrow(), results$total_runtime));
    print.method.is.ending.message(method_name);
    return(results);
}

train.lars.elasticnet = function(method_settings, rounding_type = "none"){
    #runs L1/L2 penalized logistic regression models using glmnet for many values of alpha
    #returns K + 1 models at each value of alpha and lambda (K-fold CV + one full model)
    #X, Y, X_test, Y_test, folds class_weights, has_class_weights, sample_weights, has_sample_weights, has_test_data

    alpha_values = method_settings$alpha_values;
    subresults = vector("list", length(alpha_values));
    start_time = proc.time();
    for (i in 1:length(alpha_values)){
        subresults[[i]] = train.lars(method_settings, alpha_value = alpha_values[i], rounding_type = rounding_type);
    }
    runtime = proc.time() - start_time

    #merge results_df and frame
    results = merge.results.and.models(results_df_list = lapply(subresults, function(r) r$results_df),
                                       print_models_list = lapply(subresults, function(r) r$print_models),
                                       debug_models_list = lapply(subresults, function(r) r$debug_models));

    #strip other fields returned when merging
    results = results[c("results_df", "print_models", "debug_models")];
    results$method_name = method_name;
    results$method_settings = method_settings;
    results$total_runtime = runtime[["elapsed"]];
    return(results);
}

#black-box
train.svm = function(method_settings, kernel_type = "linear"){
    #runs SVM using the e1071 package
    #returns K+1 models at each free parameter instance
    #X, Y, X_test, Y_test, folds class_weights, has_class_weights, sample_weights, has_sample_weights, has_test_data

    print.method.is.starting.message(method_name);
    training_start_time = proc.time();

    if (!has_class_weights){
        class.weights = c(1,1);
        names(class.weights) = c("0","1");
    } else {
        class.weights = 2*class_weights;
        names(class.weights) = c("0","1");
    }

    #initialize results df (each row contains results from glmnet)
    results_param = method_settings$param;
    results_df = create.results.df(results_param, K);
    n_instances = results_df %>% nrow()
    print_models = setNames(vector("list", n_instances), results_df$print_model_id)
    debug_models = setNames(vector("list", n_instances), results_df$debug_model_id)

    #train instances
    for (row_idx in 1:n_instances){

        run_param_row  = results_df[row_idx,] %>% select(fold, starts_with("parameter"));
        run_param_list = convert.parameter.df.row.to.list(run_param_row);

        #initialize training dataset
        train_ind       = !(folds == run_param_row$fold);
        X_train         = as.matrix(X[train_ind,]);
        Y_train   	    = as.factor(Y[train_ind]);
        ind_pos_train	= Y_train == 1;
        ind_neg_train	= !ind_pos_train;

        #initialize validation dataset
        has_validation_set = run_param_row$fold > 0
        valid_ind       = folds == run_param_row$fold;
        X_valid         = as.matrix(X[valid_ind,]);
        Y_valid    		= as.factor(Y[valid_ind]);
        ind_pos_valid	= Y_valid == 1;
        ind_neg_valid	= !ind_pos_valid;

        #train model
        call_inputs = c(run_param_list,
                        list(x = X_train,
                             y = Y_train,
                             type = "C-classification",
                             class.weights = class.weights,
                             na.action = na.fail,
                             kernel= kernel_type))

        call_start_time = proc.time();
        model = do.call(svm, call_inputs);
        runtime = (proc.time() - call_start_time);

        #accuracy metrics on training set
        y_hat = predict(model, newdata = X_train);
        if (has_sample_weights){
            error_df = compute.error.metrics("train", y_hat, ind_pos_train, ind_neg_train, sample_weights[train_ind]);
        } else {
            error_df = compute.error.metrics("train", y_hat, ind_pos_train, ind_neg_train);
        }
        results_df[row_idx, names(error_df)] = error_df;

        #accuracy metrics on validation set
        if (has_validation_set){
            y_hat = predict(model, newdata = X_valid);
            if (has_sample_weights){
                error_df = compute.error.metrics("valid", y_hat, ind_pos_valid, ind_neg_valid, sample_weights[valid_ind]);
            } else {
                error_df = compute.error.metrics("valid", y_hat, ind_pos_valid, ind_neg_valid);
            }
            results_df[row_idx, names(error_df)] = error_df;
        }

        #accuracy metrics on test set
        if (has_test_set){
            y_hat = predict(model, newdata = X_test);
            if (has_sample_weights){
                error_df = compute.error.metrics("test", y_hat, ind_pos_test, ind_neg_test, sample_weights_test);
            } else {
                error_df = compute.error.metrics("test", y_hat, ind_pos_test, ind_neg_test);
            }
            results_df[row_idx, names(error_df)] = error_df;
        }

        #run time metrics
        results_df[row_idx, "runtime"] = runtime[["elapsed"]]

        #model size metrics
        if (kernel_type == "linear"){
            coefficients = rbind("(Intercept)"= -model$rho, t(t(model$coefs) %*% X_train[model$index,]))
            variable_names = rownames(coefficients);
            results_df[row_idx, "model_size"] = sum(coefficients[variable_names != "(Intercept)", ] != 0.0)
        }

        #full model objects (for debugging or other use)
        if (kernel_type == "linear" && method_settings$save_print_models){
            model_id = results_df[row_idx,]$print_model_id
            print_models[[model_id]] = setNames(array(coefficients), variable_names)
        }

        if (method_settings$save_debug_models){
            model_id = results_df[row_idx,]$debug_model_id
            debug_models[[model_id]] = model;
        }
    }

    #build output
    results = list();
    results$method_name = method_name;
    results$method_settings = method_settings;
    results$results_df = results_df;
    results$print_models = print_models;
    results$debug_models = debug_models;

    #print completion message
    training_end_time = proc.time() - training_start_time;
    results$total_runtime = training_end_time[["elapsed"]]
    print.to.console(sprintf("Trained %d instances in %1.1f seconds", results$results_df %>% nrow(), results$total_runtime));
    print.method.is.ending.message(method_name);

    return(results);
}

train.randomforest = function(method_settings){
    #runs randomForest
    #returns K+1 models at each free parameter instance
    #X, Y, X_test, Y_test, folds class_weights, has_class_weights, sample_weights, has_sample_weights, has_test_data

    print.method.is.starting.message(method_name);
    training_start_time = proc.time();
    weights = 1*class_weights+0;

    #initialize results df (each row contains results from glmnet)
    results_param = method_settings$param;
    results_df = create.results.df(results_param, K);
    n_instances = results_df %>% nrow()
    print_models = setNames(vector("list", n_instances), results_df$print_model_id)
    debug_models = setNames(vector("list", n_instances), results_df$debug_model_id)

    #train instances
    for (row_idx in 1:n_instances){
        run_param_row = results_df[row_idx,] %>% select(fold, starts_with("parameter"))
        run_param_list = convert.parameter.df.row.to.list(run_param_row);

        #initialize training dataset
        train_ind       = !(folds == run_param_row$fold);
        X_train         = as.matrix(X[train_ind,]);
        Y_train   	    = as.factor(Y[train_ind]);
        ind_pos_train	= Y_train == 1;
        ind_neg_train	= !ind_pos_train;

        #initialize validation dataset
        has_validation_set = run_param_row$fold > 0
        valid_ind       = folds == run_param_row$fold;
        X_valid         = as.matrix(X[valid_ind,]);
        Y_valid    		= as.factor(Y[valid_ind]);
        ind_pos_valid	= Y_valid == 1;
        ind_neg_valid	= !ind_pos_valid;

        #update sampesize parameter
        if ("sampsize" %in% names(run_param_list)){
            run_param_list$sampsize = ceiling(run_param_list$sampsize * nrow(X_train))
        }

        #train model
        call_inputs = c(run_param_list,
                        list(x = X_train,
                             y = Y_train,
                             classwt = weights));

        call_start_time = proc.time();
        model = do.call(randomForest, call_inputs);
        runtime = (proc.time() - call_start_time);

        #accuracy metrics on training set
        y_hat = predict(model, newdata = X_train);
        if (has_sample_weights){
            error_df = compute.error.metrics("train", y_hat, ind_pos_train, ind_neg_train, sample_weights[train_ind]);
        } else {
            error_df = compute.error.metrics("train", y_hat, ind_pos_train, ind_neg_train);
        }
        results_df[row_idx, names(error_df)] = error_df;

        #accuracy metrics on validation set
        if (has_validation_set){
            y_hat = predict(model, newdata = X_valid);
            if (has_sample_weights){
                error_df = compute.error.metrics("valid", y_hat, ind_pos_valid, ind_neg_valid, sample_weights[valid_ind]);
            } else {
                error_df = compute.error.metrics("valid", y_hat, ind_pos_valid, ind_neg_valid);
            }
            results_df[row_idx, names(error_df)] = error_df;
        }

        #accuracy metrics on test set
        if (has_test_set){
            y_hat = predict(model, newdata = X_test);
            if (has_sample_weights){
                error_df = compute.error.metrics("test", y_hat, ind_pos_test, ind_neg_test, sample_weights_test);
            } else {
                error_df = compute.error.metrics("test", y_hat, ind_pos_test, ind_neg_test);
            }
            results_df[row_idx, names(error_df)] = error_df;
        }

        #run time metrics
        results_df[row_idx, "runtime"] = runtime[["elapsed"]]

        #full model objects (for debugging or other use)
        if (method_settings$save_print_models){
            #cannot do anything
        }

        if (method_settings$save_debug_models){
            model_id = results_df[row_idx,]$debug_model_id
            debug_models[[model_id]] = model;
        }
    }
    #build output
    results = list();
    results$method_name = method_name;
    results$method_settings = method_settings;
    results$results_df = results_df;
    results$print_models = print_models;
    results$debug_models = debug_models;

    #print completion message
    training_end_time = proc.time() - training_start_time;
    results$total_runtime = training_end_time[["elapsed"]]
    print.to.console(sprintf("Trained %d instances in %1.1f seconds", results$results_df %>% nrow(), results$total_runtime));
    print.method.is.ending.message(method_name);
    return(results);
}

train.sgb = function(method_settings){
    #runs boosting using the gbm package
    #returns K+1 models at each free parameter instance
    #X, Y, X_test, Y_test, folds class_weights, has_class_weights, sample_weights, has_sample_weights, has_test_data

    print.method.is.starting.message(method_name);
    training_start_time = proc.time();

    #setup weights
    if (has_sample_weights){
        weights = sample_weights
    } else {
        weights = rep(1, nrow(X));
    }
    if (has_class_weights) {
        weights[Y!=1] = class_weights[1] * weights[Y!=1];
        weights[Y==1] = class_weights[2] * weights[Y==1];
    }

    #initialize results df (each row contains results from glmnet)
    results_param = method_settings$param;
    results_df = create.results.df(results_param, K);
    n_instances = results_df %>% nrow()
    print_models = setNames(vector("list", n_instances), results_df$print_model_id)
    debug_models = setNames(vector("list", n_instances), results_df$debug_model_id)
    test_data   = data.frame(cbind(X_test,"y"=Y_test));

    #train instances
    for (row_idx in 1:n_instances){

        run_param_row = results_df[row_idx,] %>% select(fold, starts_with("parameter"))
        run_param_list = convert.parameter.df.row.to.list(run_param_row);

        #initialize training dataset
        train_ind       = !(folds == run_param_row$fold);
        train_data      = as.data.frame(cbind(X[train_ind,],"y"= Y[train_ind]));
        ind_pos_train	= Y[train_ind]==1;
        ind_neg_train	= !ind_pos_train;

        #initialize validation dataset
        has_validation_set = run_param_row$fold>0
        valid_ind       = folds == run_param_row$fold;
        valid_data      = as.data.frame(cbind(X[valid_ind,],"y"= Y[valid_ind]))
        ind_pos_valid	= Y[valid_ind] == 1;
        ind_neg_valid	= !ind_pos_valid;

        #train model
        call_inputs = c(run_param_list,
                        list(formula = y~.,
                             data = train_data,
                             weights = weights[train_ind],
                             keep.data = FALSE,
                             distribution = "adaboost",
                             n.minobsinnode = 10,
                             n.cores = 1));

        call_start_time = proc.time();
        model = do.call(gbm, call_inputs);
        runtime = (proc.time() - call_start_time);

        #accuracy metrics on training set
        y_hat = round(predict.gbm(model, newdata = train_data, n.trees = model$n.trees, type='response'));
        if (has_sample_weights){
            error_df = compute.error.metrics("train", y_hat, ind_pos_train, ind_neg_train, sample_weights[train_ind]);
        } else {
            error_df = compute.error.metrics("train", y_hat, ind_pos_train, ind_neg_train);
        }
        results_df[row_idx, names(error_df)] = error_df;

        #accuracy metrics on validation set
        if (has_validation_set){
            y_hat = round(predict.gbm(model, newdata = valid_data, n.trees = model$n.trees, type='response'));
            if (has_sample_weights){
                error_df = compute.error.metrics("valid", y_hat, ind_pos_valid, ind_neg_valid, sample_weights[valid_ind]);
            } else {
                error_df = compute.error.metrics("valid", y_hat, ind_pos_valid, ind_neg_valid);
            }
            results_df[row_idx, names(error_df)] = error_df;
        }

        #accuracy metrics on test set
        if (has_test_set){
            y_hat = round(predict(model, newdata = test_data, n.trees = model$n.trees, type='response'));
            if (has_sample_weights){
                error_df = compute.error.metrics("test", y_hat, ind_pos_test, ind_neg_test, sample_weights_test);
            } else {
                error_df = compute.error.metrics("test", y_hat, ind_pos_test, ind_neg_test);
            }
            results_df[row_idx, names(error_df)] = error_df;
        }

        #run time metrics
        results_df[row_idx, "runtime"] = runtime[["elapsed"]];

        #full model objects (for debugging or other use)
        if (method_settings$save_print_models){
            #cannot do anything
        }

        if (method_settings$save_debug_models){
            model_id = results_df[row_idx,]$debug_model_id
            debug_models[[model_id]] = model;
        }
    }

    #build output
    results = list();
    results$method_name = method_name;
    results$method_settings = method_settings;
    results$results_df = results_df;
    results$print_models = print_models;
    results$debug_models = debug_models;

    #print completion message
    training_end_time = proc.time() - training_start_time;
    results$total_runtime = training_end_time[["elapsed"]]
    print.to.console(sprintf("Trained %d instances in %1.1f seconds", results$results_df %>% nrow(), results$total_runtime));
    print.method.is.ending.message(method_name);
    return(results)
}

# decision trees
train.c50 = function(method_settings, type = "tree"){
    #trains c5.0 models using the C5.0 package
    #returns K+1 models at each free parameter instance
    #type = "tree" returns decision trees (c50_tree)
    #type = "rules"returns rule sets (c50_rule)
    #global variable inputs include:
    #X, Y, X_test, Y_test, folds class_weights, has_class_weights, sample_weights, has_sample_weights, has_test_data

    print.method.is.starting.message(method_name);
    training_start_time = proc.time();

    #setup case weights (cannot figure out bug)
    #     if (has_sample_weights){
    #         weights = sample_weights;
    #     } else {
    #         weights = rep(1, nrow(X));
    #     }

    #setup costs (C5.0 splits on costs)
    cost_matrix 	= matrix(c(0,1.0,1.0,0), byrow=TRUE, nrow=2, dimnames=list(c("0","1"),c("0","1")));
    if (has_class_weights) {
        cost_matrix 	= matrix(c(0,class_weights[1],class_weights[2],0), byrow=TRUE, nrow=2, dimnames=list(c("0","1"),c("0","1")));
    }

    #initialize results df (each row contains results from glmnet)
    results_param = method_settings$param;
    results_df = create.results.df(results_param, K);
    n_instances = results_df %>% nrow()
    print_models = setNames(vector("list", n_instances), results_df$print_model_id)
    debug_models = setNames(vector("list", n_instances), results_df$debug_model_id)

    #train instances
    for (row_idx in 1:n_instances){

        run_param_row  = results_df[row_idx,] %>% select(fold, starts_with("parameter"));
        run_param_list = convert.parameter.df.row.to.list(run_param_row);

        #initialize training dataset
        train_ind       = !(folds == run_param_row$fold);
        X_train         = as.matrix(X[train_ind,]);
        Y_train   	    = as.factor(Y[train_ind]);
        ind_pos_train	= Y_train == 1;
        ind_neg_train	= !ind_pos_train;

        #initialize validation dataset
        has_validation_set = run_param_row$fold > 0
        valid_ind       = folds == run_param_row$fold;
        X_valid         = as.matrix(X[valid_ind,]);
        Y_valid    		= as.factor(Y[valid_ind]);
        ind_pos_valid	= Y_valid == 1;
        ind_neg_valid	= !ind_pos_valid;

        #train model
        call_inputs = list(x = X_train,
                           y = Y_train,
                           rules = (type == "rule"),
                           costs=t(cost_matrix),
                           #weights=weights,
                           control = do.call(C5.0Control, run_param_list))

        call_start_time = proc.time();
        model = do.call(C5.0, call_inputs);
        runtime = (proc.time() - call_start_time);

        #accuracy metrics on training set
        y_hat = predict(model, newdata = X_train)
        if (has_sample_weights){
            error_df = compute.error.metrics("train", y_hat, ind_pos_train, ind_neg_train, sample_weights[train_ind]);
        } else {
            error_df = compute.error.metrics("train", y_hat, ind_pos_train, ind_neg_train);
        }
        results_df[row_idx, names(error_df)] = error_df;

        #accuracy metrics on validation set
        if (has_validation_set){
            y_hat = predict(model, newdata = X_valid)
            if (has_sample_weights){
                error_df = compute.error.metrics("valid", y_hat, ind_pos_valid, ind_neg_valid, sample_weights[valid_ind]);
            } else {
                error_df = compute.error.metrics("valid", y_hat, ind_pos_valid, ind_neg_valid);
            }
            results_df[row_idx, names(error_df)] = error_df;
        }

        #accuracy metrics on test set
        if (has_test_set){
            y_hat = predict(model, newdata = X_test);
            if (has_sample_weights){
                error_df = compute.error.metrics("test", y_hat, ind_pos_test, ind_neg_test, sample_weights_test);
            } else {
                error_df = compute.error.metrics("test", y_hat, ind_pos_test, ind_neg_test);
            }
            results_df[row_idx, names(error_df)] = error_df;
        }

        #run time metrics
        results_df[row_idx, "runtime"] = runtime[["elapsed"]]

        #model metrics
        results_df[row_idx, "model_size"] = model$size;

        #model objects
        if (method_settings$save_print_models){
            model_id = results_df[row_idx,]$print_model_id
            print_models[[model_id]] = summary(model)
        }

        if (method_settings$save_debug_models){
            model_id = results_df[row_idx,]$debug_model_id
            debug_models[[model_id]] = model;
        }
    }

    #build output
    results = list();
    results$method_name = method_name;
    results$method_settings = method_settings;
    results$results_df = results_df;
    results$print_models = print_models;
    results$debug_models = debug_models;

    #print completion message
    training_end_time = proc.time() - training_start_time;
    results$total_runtime = training_end_time[["elapsed"]]
    print.to.console(sprintf("Trained %d instances in %1.1f seconds", results$results_df %>% nrow(), results$total_runtime));
    print.method.is.ending.message(method_name);
    return(results)
}

train.cart = function(method_settings){
    #trains cart models using the rpart package
    #returns K+1 models at each free parameter instance

    print.method.is.starting.message(method_name);
    training_start_time = proc.time();

    #need to explicitly label the outcome variable for model printing
    ylabels = c("y = 0", "y = 1")
    cost_matrix = matrix(c(0,1.0,1.0,0), byrow=TRUE, nrow=2, dimnames=list(ylabels, ylabels))
    if (has_class_weights) {
        cost_matrix = matrix(c(0,class_weights[1],class_weights[2],0), byrow=TRUE, nrow=2, dimnames=list(ylabels,ylabels));
    }

    #initialize results df (each row contains results from glmnet)
    results_param = method_settings$param;
    results_df = create.results.df(results_param, K);
    n_instances = results_df %>% nrow()
    print_models = setNames(vector("list", n_instances), results_df$print_model_id)
    debug_models = setNames(vector("list", n_instances), results_df$debug_model_id)

    test_data = cbind(as.data.frame(X_test), data.frame("y" = ylabels[Y_test+1]));

    #train instances
    for (row_idx in 1:n_instances){

        run_param_row  = results_df[row_idx,] %>% select(fold, starts_with("parameter"));
        run_param_list = convert.parameter.df.row.to.list(run_param_row);

        #initialize training dataset
        train_ind       = !(folds == run_param_row$fold);
        train_data      = cbind(as.data.frame(X[train_ind,]), data.frame("y" = ylabels[Y[train_ind]+1]));
        ind_pos_train	= Y[train_ind] == 1;
        ind_neg_train	= !ind_pos_train;

        #initialize validation dataset
        valid_ind       = folds == run_param_row$fold;
        valid_data      = cbind(as.data.frame(X[valid_ind,]), data.frame("y" = ylabels[Y[valid_ind]+1]));
        ind_pos_valid	= Y[valid_ind] == 1;
        ind_neg_valid	= !ind_pos_valid;

        has_validation_set = run_param_row$fold > 0

        #train model
        call_inputs = list(formula = y ~ .,
                           method = "class",
                           data = train_data,
                           parms=list(loss=cost_matrix),
                           x = FALSE,
                           y = FALSE,
                           na.action = na.fail,
                           control = do.call(rpart.control, run_param_list))

        failed_run = FALSE;
        call_start_time = proc.time();
        tryCatch({
            model = do.call(rpart, call_inputs)
        }, error = function(e) {
            model = NULL;
            failed_run = TRUE;
            print.to.console(sprintf("error: %s",e));
        });
        runtime = (proc.time() - call_start_time);

        #accuracy metrics on training set
        if (!failed_run){

            #accuracy metrics on training set
            y_hat = c(0,1)[predict(model, newdata = train_data, type = "class")]
            if (has_sample_weights){
                error_df = compute.error.metrics("train", y_hat, ind_pos_train, ind_neg_train, sample_weights[train_ind]);
            } else {
                error_df = compute.error.metrics("train", y_hat, ind_pos_train, ind_neg_train);
            }
            results_df[row_idx, names(error_df)] = error_df;

            #accuracy metrics on validation set
            if (has_validation_set){
                y_hat = c(0,1)[predict(model, newdata = valid_data, type = "class")]
                if (has_sample_weights){
                    error_df = compute.error.metrics("valid", y_hat, ind_pos_valid, ind_neg_valid, sample_weights[valid_ind]);
                } else {
                    error_df = compute.error.metrics("valid", y_hat, ind_pos_valid, ind_neg_valid);
                }
                results_df[row_idx, names(error_df)] = error_df;
            }

            #accuracy metrics on test set
            if (has_test_set){
                y_hat = c(0,1)[predict(model, newdata = test_data, type = "class")]
                if (has_sample_weights){
                    error_df = compute.error.metrics("test", y_hat, ind_pos_test, ind_neg_test, sample_weights_test);
                } else {
                    error_df = compute.error.metrics("test", y_hat, ind_pos_test, ind_neg_test);
                }
                results_df[row_idx, names(error_df)] = error_df;
            }

            #other metrics
            results_df[row_idx, "runtime"] = runtime[["elapsed"]];
            results_df[row_idx, "model_size"] = sum(model$frame$var=="<leaf>");

            #model objects
            if (method_settings$save_print_models){
                model_id = results_df[row_idx,]$print_model_id;
                print_model = list();
                class(print_model) = class(model);
                print_model$frame = model$frame;
                print_model$method = model$method;
                print_models[[model_id]] = print_model;
            }

            if (method_settings$save_debug_models){
                model_id = results_df[row_idx,]$debug_model_id
                debug_models[[model_id]] = model;
            }
        }
    }

    #build output
    results = list();
    results$method_name = method_name;
    results$method_settings = method_settings;
    results$results_df = results_df;
    results$print_models = print_models;
    results$debug_models = debug_models;

    #print completion message
    training_end_time = proc.time() - training_start_time;
    results$total_runtime = training_end_time[["elapsed"]]
    print.to.console(sprintf("Trained %d instances in %1.1f seconds", results$results_df %>% nrow(), results$total_runtime));
    print.method.is.ending.message(method_name);
    return(results);
}

# rule lists
train.one_rule = function(method_settings){

    #trains one_rule models using the OneR package
    #returns K+1 models at each free parameter instance
    print.method.is.starting.message(method_name);
    training_start_time = proc.time();

    #initialize results df (each row contains results from glmnet)
    results_param = method_settings$param;
    results_df = create.results.df(results_param, K);
    n_instances = results_df %>% nrow()
    print_models = setNames(vector("list", n_instances), results_df$print_model_id)
    debug_models = setNames(vector("list", n_instances), results_df$debug_model_id)

    ylabels = c("0", "1")
    test_data = cbind(as.data.frame(X_test), data.frame("y" = ylabels[Y_test+1]));

    #train instances
    for (row_idx in 1:n_instances){

        run_param_row  = results_df[row_idx,] %>% select(fold, starts_with("parameter"));
        run_param_list = convert.parameter.df.row.to.list(run_param_row);

        #initialize training dataset
        train_ind       = !(folds == run_param_row$fold);
        train_data      = cbind(as.data.frame(X[train_ind,]), data.frame("y" = ylabels[Y[train_ind]+1]));
        ind_pos_train	= Y[train_ind] == 1;
        ind_neg_train	= !ind_pos_train;

        #initialize validation dataset
        valid_ind       = folds == run_param_row$fold;
        valid_data      = cbind(as.data.frame(X[valid_ind,]), data.frame("y" = ylabels[Y[valid_ind]+1]));
        ind_pos_valid	= Y[valid_ind] == 1;
        ind_neg_valid	= !ind_pos_valid;

        has_validation_set = run_param_row$fold > 0

        #train model
        failed_run = FALSE;
        call_start_time = proc.time();
        train_data = optbin(train_data, method = run_param_list$binning)
        model = OneR::OneR(train_data, ties.method = run_param_list$ties_method)
        runtime = (proc.time() - call_start_time);

        #accuracy metrics on training set
        y_hat = c(0,1)[predict(model, newdata = train_data)]
        if (has_sample_weights){
            error_df = compute.error.metrics("train", y_hat, ind_pos_train, ind_neg_train, sample_weights[train_ind]);
        } else {
            error_df = compute.error.metrics("train", y_hat, ind_pos_train, ind_neg_train);
        }
        results_df[row_idx, names(error_df)] = error_df;

        #accuracy metrics on validation set
        if (has_validation_set){
            y_hat = c(0,1)[predict(model, newdata = valid_data, type = "class")]
            if (has_sample_weights){
                error_df = compute.error.metrics("valid", y_hat, ind_pos_valid, ind_neg_valid, sample_weights[valid_ind]);
            } else {
                error_df = compute.error.metrics("valid", y_hat, ind_pos_valid, ind_neg_valid);
            }
            results_df[row_idx, names(error_df)] = error_df;
        }

        #accuracy metrics on test set
        if (has_test_set){
            y_hat = c(0,1)[predict(model, newdata = test_data, type = "class")]
            if (has_sample_weights){
                error_df = compute.error.metrics("test", y_hat, ind_pos_test, ind_neg_test, sample_weights_test);
            } else {
                error_df = compute.error.metrics("test", y_hat, ind_pos_test, ind_neg_test);
            }
            results_df[row_idx, names(error_df)] = error_df;
        }

        #model objects
        if (method_settings$save_print_models){
            model_id = results_df[row_idx,]$print_model_id;
            raw_model = capture.output(model)
            rules_start_idx = which(raw_model=="Rules:") + 1
            rules_end_idx = which(raw_model=="Accuracy:") - 2;
            print_models[[model_id]] = raw_model[rules_start_idx:rules_end_idx]
        }

        if (method_settings$save_debug_models){
            model_id = results_df[row_idx,]$debug_model_id
            debug_models[[model_id]] = model;
        }

        #other metrics
        results_df[row_idx, "runtime"] = runtime[["elapsed"]];
        results_df[row_idx, "model_size"] = sum(nrow(print_models));

    }

    #build output
    results = list();
    results$method_name = method_name;
    results$method_settings = method_settings;
    results$results_df = results_df;
    results$print_models = print_models;
    results$debug_models = debug_models;

    #print completion message
    training_end_time = proc.time() - training_start_time;
    results$total_runtime = training_end_time[["elapsed"]]
    print.to.console(sprintf("Trained %d instances in %1.1f seconds", results$results_df %>% nrow(), results$total_runtime));
    print.method.is.ending.message(method_name);
    return(results);
}

extract.rule.list.ripper = function(model){
    raw_model = capture.output(print(model));
    rules_start_idx = grep(pattern = "JRIP rules:.*", x = raw_model) + 3;
    rules_end_idx = grep(pattern = "Number of Rules.*", x = raw_model) - 2;
    rule_list = raw_model[rules_start_idx:rules_end_idx];
    return(rule_list)
}

extract.rule.list.part = function(model){

    raw_model = capture.output(print(model))
    raw_model = noquote(strsplit(raw_model, split = '""'));

    #remove crap at start/end
    breaks = which(raw_model=="character(0)");
    start_idx = min(breaks) + 1;
    end_idx = max(breaks) - 1;
    raw_model = raw_model[start_idx:end_idx];

    #iterate over remaining contents to group together lines that are 1 item
    rule_list = rep(NA,0);
    last_row_id = length(grep("Number of Rules.*", raw_model[[1]]));
    i = 1;
    rule_start_row = 1;
    n_rows = length(raw_model);
    while (i < n_rows){
        next_row_id = i + 1
        row_content = raw_model[[next_row_id]]
        if (length(row_content) == 0){
            rule_end_row = i;
            rule = paste(raw_model[rule_start_row:rule_end_row])
            printed_rule = capture.output(cat(rule));
            rule_list = rbind(rule_list, printed_rule);
            rule_start_row = next_row_id + 1;
            i = rule_start_row;
        } else {
            i = i + 1;
        }
    }
    rownames(rule_list) = NULL;
    return(rule_list)
}

print.rule.list.part = function(model){
    tryCatch({
        rule_list = extract.rule.list.part(model)
    }, error = function(e) {
        rule_list = capture.output(print(model))
    })
    return(rule_list);
}

print.rule.list.ripper = function(model){
    tryCatch({
        rule_list = extract.rule.list.ripper(model)
    }, error = function(e) {
        rule_list = capture.output(print(model))
    })
    return(rule_list);
}

train.weka = function(method_settings, method_name){
    #trains ripper/cart from JWeka
    stopifnot(method_name %in% c("ripper", "part"))
    #returns K+1 models at each free parameter instance

    print.method.is.starting.message(method_name);
    training_start_time = proc.time();

    #initialize results df (each row contains results from glmnet)
    results_param = method_settings$param;
    results_df = create.results.df(results_param, K);
    n_instances = results_df %>% nrow()
    print_models = setNames(vector("list", n_instances), results_df$print_model_id)
    debug_models = setNames(vector("list", n_instances), results_df$debug_model_id)

    ylabels = c("0","1")
    test_data = cbind(as.data.frame(X_test), data.frame("y" = ylabels[Y_test+1]) )
    if (method_name == "ripper"){
        training_function = JRip
    } else if (method_name == "part"){
        training_function = PART
    }

    #train instances
    for (row_idx in 1:n_instances){

        run_param_row  = results_df[row_idx,] %>% select(fold, starts_with("parameter"));
        run_param_list = convert.parameter.df.row.to.list(run_param_row);
        run_param_obj =

            #initialize training dataset
            train_ind   = !(folds == run_param_row$fold);
        train_data      = cbind(as.data.frame(X[train_ind,]), data.frame("y" = ylabels[Y[train_ind]+1]));
        ind_pos_train	= Y[train_ind] == 1;
        ind_neg_train	= !ind_pos_train;

        #initialize validation dataset
        valid_ind       = folds == run_param_row$fold;
        valid_data      = cbind(as.data.frame(X[valid_ind,]), data.frame("y" = ylabels[Y[valid_ind]+1]));
        ind_pos_valid	= Y[valid_ind] == 1;
        ind_neg_valid	= !ind_pos_valid;

        has_validation_set = run_param_row$fold > 0

        #train model
        call_start_time = proc.time();
        model = training_function(formula = "y ~ .", data = train_data, control = do.call(Weka_control, run_param_list))
        runtime = (proc.time() - call_start_time);

        #accuracy metrics on training set
        y_hat = c(0,1)[predict(model, newdata = train_data)]
        if (has_sample_weights){
            error_df = compute.error.metrics("train", y_hat, ind_pos_train, ind_neg_train, sample_weights[train_ind]);
        } else {
            error_df = compute.error.metrics("train", y_hat, ind_pos_train, ind_neg_train);
        }
        results_df[row_idx, names(error_df)] = error_df;

        #accuracy metrics on validation set
        if (has_validation_set){
            y_hat = c(0,1)[predict(model, newdata = valid_data, type = "class")]
            if (has_sample_weights){
                error_df = compute.error.metrics("valid", y_hat, ind_pos_valid, ind_neg_valid, sample_weights[valid_ind]);
            } else {
                error_df = compute.error.metrics("valid", y_hat, ind_pos_valid, ind_neg_valid);
            }
            results_df[row_idx, names(error_df)] = error_df;
        }

        #accuracy metrics on test set
        if (has_test_set){
            y_hat = c(0,1)[predict(model, newdata = test_data, type = "class")]
            if (has_sample_weights){
                error_df = compute.error.metrics("test", y_hat, ind_pos_test, ind_neg_test, sample_weights_test);
            } else {
                error_df = compute.error.metrics("test", y_hat, ind_pos_test, ind_neg_test);
            }
            results_df[row_idx, names(error_df)] = error_df;
        }

        #model objects
        if (method_settings$save_print_models){
            if (method_name == "ripper"){
                print_model = print.rule.list.ripper(model)
            } else if (method_name == "part") {
                print_model = print.rule.list.part(model)
            }
            model_id = results_df[row_idx,]$print_model_id;
            print_models[[model_id]] = print_model
        }

        if (method_settings$save_debug_models){
            model_id = results_df[row_idx,]$debug_model_id
            debug_models[[model_id]] = model;
        }

        results_df[row_idx, "runtime"] = runtime[["elapsed"]];
        results_df[row_idx, "model_size"] = length(print_models[[model_id]]);
    }

    #build output
    results = list();
    results$method_name = method_name;
    results$method_settings = method_settings;
    results$results_df = results_df;
    results$print_models = print_models;
    results$debug_models = debug_models;

    #print completion message
    training_end_time = proc.time() - training_start_time;
    results$total_runtime = training_end_time[["elapsed"]]
    print.to.console(sprintf("Trained %d instances in %1.1f seconds", results$results_df %>% nrow(), results$total_runtime));
    print.method.is.ending.message(method_name);
    return(results);
}