## function to simulate weighted utility prototype models across all conditions

## load libraries
library("dplyr")

## load files to generate stimuli and run models
source("stimuli.R")
source("prototype-model.R")

SimulatePrototype <- function(update = "full", decision = "full", n_iters = 100, conditions) {
    ## number of stimuli in the experiment
    n_stims <- 100

    ## data frame to store simulations
    simulations <- list()

    for (i in 1:length(conditions)) {
        ## print each condition
        print(i)

        ## initialize vectors to store information
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

        for(j in 1:n_iters) {
            ## get category structure for current condition
            theta0 <- curr_condition$theta0

            ## generate stimuli according to category structure
            stims <- GenStim(theta0)

            ## run prototype model with limited updating and limited decision rule
            model <- ModelInit(update = update,
                               decision = decision,
                               n_features <- curr_dimension)

            ## for limited updating/decision models,
            ## randomly select a feature to attend to
            model$feature <- sample(1:length(theta0), 1)

            ## keep track of performance on attended features
            model$k <- rep(1, length(theta0))
            model$n <- rep(2, length(theta0))
            model$u <- model$k/model$n
            
            for(k in 1:nrow(stims)) {
                ## get current stim and label
                model$curr_stim <- stims[k, , drop = FALSE]

                ## get current label
                model$curr_label <- model$curr_stim[, length(model$curr_stim)]

                ## remove label from curr stim
                model$curr_stim <- model$curr_stim[1:length(model$curr_stim) - 1]

                ## predict label for current stim
                model$predict_label <- ModelPredict(model)

                ## update model with current stim
                model <- ModelFit(model)

                ## update model utility counts based on predictions
                if(model$predict_label == model$curr_label) {
                    model$k[model$feature] <- model$k[model$feature] + 1
                    model$n[model$feature] <- model$n[model$feature] + 1
                } else {
                    model$n[model$feature] <- model$n[model$feature] + 1

                    ## calculate normalized utility across all features
                    model$u <- model$k/model$n
                    model$u <- model$u/sum(model$u)

                    ## sample new feature to attend to proportional to utility
                    model$feature <- sample(1:length(theta0), 1, prob = model$u)
                }

                ## save predictions
                iter[n] <- j
                trial[n] <- k
                curr_stims[n] <- paste(model$curr_stim, collapse = "")
                curr_labels[n] <- model$curr_label
                predict_labels[n] <- model$predict_label
                curr_features[n] <- model$feature
                n <- n + 1 ## increment counter
            }
        }

        ## create data frame with current model information
        curr_simulations <- data.frame(model = paste(update, "update",
                                                     decision, "decision",
                                                     sep = '-'),
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
