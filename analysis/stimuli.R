## function to generate stimuli (uses the same method as Dan's code)
GenStim <- function(theta0, theta1 = 1 - theta0, phi = 0.5, n_stims = 100) {
    ## theta0: predictiveness of each feature for category A
    ## theta1: predictiveness of each feature for category B
    ## phi: proportion of stimuli in category A
    ## n_stims: number of stimuli to generate
    
    k <- length(theta0) ## number of features
    stims <- matrix(NA, nrow = n_stims, ncol = k + 1)
    labels <- c()

    for (i in 1:n_stims) {
        labels[i] <- as.numeric(runif(1) < phi)
        if (labels[i] == 0) {
            stims[i, ] <- c(as.numeric(runif(length(theta0)) > theta0), labels[i])
        } else {
            stims[i, ] <- c(as.numeric(runif(length(theta1)) > theta1), labels[i])
        }
    }

    return(stims)
}
