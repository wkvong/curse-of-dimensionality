## function to simulate hypothesis testing model across all conditions

## load libraries
library("dplyr")

## load files to generate stimuli and run models
source("stimuli.R")

SimulateRule <- function(n_iters = 100, conditions) {
    ## number of stimuli in the experiment
    n_stims <- 100

    ## data frame to store simulations
    simulations <- list()

    for (i in 1:length(conditions)) {
        ## print each condition
        print(i)

        ## intialize vectors to store information
        curr_condition <- conditions[[i]]
        curr_dimension <- curr_condition$dimension
        curr_structure <- curr_condition$structure
        iter <- rep(NA, n_iters * n_stims)
        trial <- rep(NA, n_iters * n_stims)
        curr_stims <- rep(NA, n_iters * n_stims)
        curr_labels <- rep(NA, n_iters * n_stims)
        predict_labels <- rep(NA, n_iters * n_stims)
        curr_features <- rep(NA, n_iters * n_stims)

        ## counter for each run
        n <- 1

        for (j in 1:n_iters) {
            ## get category structure for current condition
            theta0 <- curr_condition$theta0

            ## generate stimuli according to category structure
            stims <- GenStim(theta0)

            ## create hypothesis testing model
            model <- list()

            ## create all possible hypotheses
            model$hypotheses <- expand.grid(features = 1:length(theta0), values = 0:1)

            ## store weights/utilities for each hypothesis
            model$k <- rep(1, nrow(model$hypotheses))
            model$n <- rep(2, nrow(model$hypotheses))
            model$u <- model$k/model$n
            
            ## initally select an index for a random hypothesis
            model$hypothesis_idx <- sample(1:nrow(model$hypotheses), 1)

            ## get feature and value to attend to for current hypothesis
            model$hypothesis_feature <- model$hypotheses[model$hypothesis_idx, 1]
            model$hypothesis_value <- model$hypotheses[model$hypothesis_idx, 2]
            
            for (k in 1:nrow(stims)) {
                ## get current stim and label
                model$curr_stim <- stims[k, , drop = FALSE]

                ## get current label
                model$curr_label <- model$curr_stim[, length(model$curr_stim)]

                ## remove label from curr stim
                model$curr_stim <- model$curr_stim[1:length(model$curr_stim) - 1]

                ## predict label for current stim using the current hypothesis
                if (model$curr_stim[model$hypothesis_feature] ==
                    model$hypothesis_value) {
                    ## value on attended feature on current stim matches
                    model$predict_label <- 0 
                } else {
                    ## value attended feature on current stim does not match
                    model$predict_label <- 1
                }

                ## update weighted utilities based on predictions
                if(model$predict_label == model$curr_label) {
                    model$k[model$hypothesis_idx] <-
                        model$k[model$hypothesis_idx] + 1
                    model$n[model$hypothesis_idx] <-
                        model$n[model$hypothesis_idx] + 1
                } else {
                    model$n[model$hypothesis_idx] <-
                        model$n[model$hypothesis_idx] + 1

                    model$u <- model$k/model$n
                    model$u <- model$u/sum(model$u)

                    ## sample new hypothesis proportional to normalized utility
                    model$hypothesis_idx <-
                        sample(1:nrow(model$hypotheses), 1, prob = model$u)
                    model$hypothesis_feature <-
                        model$hypotheses[model$hypothesis_idx, 1]
                    model$hypothesis_value <-
                        model$hypotheses[model$hypothesis_idx, 2]
                }

                ## save predictions
                iter[n] <- j
                trial[n] <- k
                curr_stims[n] <- paste(model$curr_stim, collapse = "")
                curr_labels[n] <- model$curr_label
                predict_labels[n] <- model$predict_label
                curr_features[n] <- model$hypothesis_idx
                n <- n + 1 ## increment counter
            }
        }

        ## create data frame with current model information
        curr_simulations <- data.frame(model = "hypothesis-testing",
                                       dimension = curr_dimension,
                                       structure = curr_structure, 
                                       iter = iter,
                                       trial = trial,
                                       curr_stim = curr_stims,
                                       curr_label = curr_labels,
                                       predict_label = predict_labels,
                                       curr_feature = curr_features)

        ## add block as a column variable
        curr_simulations <- curr_simulations %>%
            mutate(block = ceiling(trial/20))

        ## add current set of simulations to overall simulations
        simulations[[i]] <- curr_simulations
    }

    simulations <- bind_rows(simulations)

    simulations_summary <- simulations %>%
        group_by(model, iter, structure, dimension, block) %>%
        summarise(perf = sum(curr_label == predict_label)/n()) %>%
        ungroup() %>%
        group_by(model, structure, dimension, block) %>%
        summarise(perf_mean = mean(perf),
                  perf_se = sd(perf)/sqrt(n()),
                  perf_ci = 1.96 * perf_se)

    rm(simulations)

    return(simulations_summary)
}
