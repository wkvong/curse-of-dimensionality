## run simulations for the prototype models with different updating and decision rules
## uses weighted utility function for attending/switching hypotheses

## source model code and functions
source("simulate-prototype-model.R")
source("simulate-rule-model.R")

## load libraries
library("dplyr")

RunSimulationsExpOne <- function() {
    set.seed(1)
    n_iters <- 10000

    ## experiment one simulations
    ## conditions in experiment one
    exp_one_conditions <- list(list(structure = "family", dimension = 4,
                                    theta0 = rep(0.9, 4)),
                               list(structure = "family", dimension = 10,
                                    theta0 = rep(0.9, 10)),
                               list(structure = "family", dimension = 16,
                                    theta0 = rep(0.9, 16)),
                               list(structure = "intermediate", dimension = 4,
                                    theta0 = c(0.9, rep(0.7, 3))),
                               list(structure = "intermediate", dimension = 10,
                                    theta0 = c(0.9, rep(0.7, 9))),
                               list(structure = "intermediate", dimension = 16,
                                    theta0 = c(0.9, rep(0.7, 15))),
                               list(structure = "rule", dimension = 4,
                                    theta0 = c(0.9, rep(0.5, 3))),
                               list(structure = "rule", dimension = 10,
                                    theta0 = c(0.9, rep(0.5, 9))),
                               list(structure = "rule", dimension = 16,
                                    theta0 = c(0.9, rep(0.5, 15))))

    ## run simulations for experiment one
    ## optimal model
    exp_one_optimal <- SimulatePrototype(update = "full",
                                         decision = "full",
                                         n_iters = n_iters,
                                         conditions = exp_one_conditions)

    ## limited model
    exp_one_limited <- SimulatePrototype(update = "limited",
                                         decision = "limited",
                                         n_iters = n_iters,
                                         conditions = exp_one_conditions)

    ## rule model
    exp_one_rule <- SimulateRule(n_iters = n_iters,
                                 conditions = exp_one_conditions)

    exp_one_simulations <- bind_rows(list(exp_one_optimal,
                                          exp_one_limited,
                                          exp_one_rule))

    ## convert variables to factors
    exp_one_simulations$model <- factor(exp_one_simulations$model)
    exp_one_simulations$dimension <- factor(exp_one_simulations$dimension)
    exp_one_simulations$block <- factor(exp_one_simulations$block)

    ## save model simulations
    ## saveRDS(exp_one_simulations, "../data/working/experiment-one-simulations.RData")

    return(exp_one_simulations)
}

RunSimulationsExpTwo <- function() {
    set.seed(1)
    n_iters <- 10000
    
    ## experiment two simulations
    exp_two_conditions <- list(list(structure = "family-90", dimension = 4,
                                    theta0 = rep(0.9, 4)),
                               list(structure = "family-90", dimension = 10,
                                    theta0 = rep(0.9, 10)),
                               list(structure = "family-90", dimension = 16,
                                    theta0 = rep(0.9, 16)),
                               list(structure = "family-80", dimension = 4,
                                    theta0 = rep(0.8, 4)),
                               list(structure = "family-80", dimension = 10,
                                    theta0 = rep(0.8, 10)),
                               list(structure = "family-80", dimension = 16,
                                    theta0 = rep(0.8, 16)),
                               list(structure = "family-70", dimension = 4,
                                    theta0 = rep(0.7, 4)),
                               list(structure = "family-70", dimension = 10,
                                    theta0 = rep(0.7, 10)),
                               list(structure = "family-70", dimension = 16,
                                    theta0 = rep(0.7, 16)))

    ## run simulations for experiment two
    ## optimal model
    exp_two_optimal <- SimulatePrototype(update = "full",
                                         decision = "full",
                                         n_iters = n_iters,
                                         conditions = exp_two_conditions)

    ## limited model
    exp_two_limited <- SimulatePrototype(update = "limited",
                                         decision = "limited",
                                         n_iters = n_iters,
                                         conditions = exp_two_conditions)

    ## rule model
    exp_two_rule <- SimulateRule(n_iters = n_iters,
                                 conditions = exp_two_conditions)

    exp_two_simulations <- bind_rows(list(exp_two_optimal,
                                          exp_two_limited,
                                          exp_two_rule))

    ## convert variables to factors
    exp_two_simulations$model <- factor(exp_two_simulations$model)
    exp_two_simulations$dimension <- factor(exp_two_simulations$dimension)
    exp_two_simulations$block <- factor(exp_two_simulations$block)

    ## save model simulations
    ## saveRDS(exp_two_simulations, "../data/working/experiment-two-simulations.RData")

    return(exp_two_simulations)
}
