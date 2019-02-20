## code for prototype models

ModelInit <- function(update, decision, n_features = 4) {
    model <- list()

    ## initialize model details
    model$update <- update
    model$decision <- decision

    ## model prior
    model$prior <- c(1, 1)
    
    ## initialize model likelihoods with 1 observation in each position
    model$likelihood_one = matrix(1, nrow = 2, ncol = n_features)
    model$likelihood_two = matrix(1, nrow = 2, ncol = n_features)

    return(model)
}

ModelPredict <- function(model) {
    ## get normalized likelihoods
    model <- ModelNormalize(model)

    ## calculate likelihood of curr stim for both categories
    ## prob_one <- 1
    ## prob_two <- 1

    prob_one <- model$normalized_prior[1]
    prob_two <- model$normalized_prior[2]

    if (model$decision == "full") {
        ## use all features to calculate likelihood
        for (i in 1:length(model$curr_stim)) {
            prob_one <- prob_one *
                model$normalized_likelihood_one[model$curr_stim[i] + 1, i]
            prob_two <- prob_two *
                model$normalized_likelihood_two[model$curr_stim[i] + 1, i]
        }
    } else if (model$decision == "limited") {
        ## use only the currently attended feature to calculate likelihood
        prob_one <- prob_one *
            model$normalized_likelihood_one[model$curr_stim[model$feature] + 1,
                                            model$feature]
        prob_two <- prob_two *
            model$normalized_likelihood_two[model$curr_stim[model$feature] + 1,
                                            model$feature]

    }
    ## respond with the category with the maximum likelihood
    predict_label <- which.max(c(prob_one, prob_two)) - 1

    ## otherwise, select a category response random if likelihoods are equal
    if (prob_one == prob_two) {
        predict_label <- sample(c(0, 1), 1)
    }
    
    ## return predicted category label
    return(predict_label)
}

ModelFit <- function(model) {
    ## update likelihood parameters
    ## convert curr_stim into matrix of counts
    counts <- matrix(0, nrow = 2, ncol = length(model$curr_stim))
    
    if (model$update == "full") {
        ## add counts for all features
        for (i in 1:length(model$curr_stim)) {
            counts[model$curr_stim[i] + 1, i] <- counts[model$curr_stim[i] + 1, i] + 1
        }
    } else if (model$update == "limited") {
        ## add counts only for the currently attended feature
        counts[model$curr_stim[model$feature] + 1, model$feature] <-
            counts[model$curr_stim[model$feature] + 1, model$feature] + 1
    }

    ## prior counts
    if (model$curr_label == 0) {
        model$prior[1] <- model$prior[1] + 1
    } else if (model$curr_label == 1) {
        model$prior[2] <- model$prior[2] + 1
    }
    
    ## update by adding counts to current category label
    if (model$curr_label == 0) {
        model$likelihood_one <- model$likelihood_one + counts
    } else if (model$curr_label == 1) {
        model$likelihood_two <- model$likelihood_two + counts
    }
    
    return(model)
}

ModelNormalize <- function(model) {
    ## normalize prior
    model$normalized_prior <- model$prior/sum(model$prior)
    
    ## normalize likelihood
    model$normalized_likelihood_one <-
        model$likelihood_one/colSums(model$likelihood_one)
    model$normalized_likelihood_two <-
        model$likelihood_two/colSums(model$likelihood_two)

    return(model)
}
