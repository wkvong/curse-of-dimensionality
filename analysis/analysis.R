library(tidyverse)
library(ggthemes)
library(BayesFactor)

########################################
## read data
########################################

## read in data from experiment one
input_file_exp_one <- "../data/input/experiment-one-data.csv"
input_file_exp_one_no_timer_pilot <- "../data/input/experiment-one-no-timer-pilot.csv"
input_file_exp_one_no_timer_full <- "../data/input/experiment-one-no-timer-full.csv"

## temporary output files
output_file_exp_one <- "../data/working/experiment-one-tmp.csv"
output_file_exp_one_no_timer_pilot <- "../data/working/experiment-one-no-timer-pilot-tmp.csv"
output_file_exp_one_no_timer_full <- "../data/working/experiment-one-no-timer-full-tmp.csv"

## run python parser
system(paste('python2.7 parser.py', input_file_exp_one, output_file_exp_one))
system(paste('python2.7 parser.py', input_file_exp_one_no_timer_pilot, output_file_exp_one_no_timer_pilot))
system(paste('python2.7 parser.py', input_file_exp_one_no_timer_full, output_file_exp_one_no_timer_full))

## read the results of parsing into R
parsed_exp_one <- read_csv(output_file_exp_one)
parsed_exp_one_no_timer_pilot <- read_csv(output_file_exp_one_no_timer_pilot)
parsed_exp_one_no_timer_full <- read_csv(output_file_exp_one_no_timer_full)

## add column to indicate type of experiment (timer or no timer)
parsed_exp_one <- parsed_exp_one %>%
    mutate(experiment_type = "timer")
parsed_exp_one_no_timer_pilot <- parsed_exp_one_no_timer_pilot %>%
    mutate(experiment_type = "no timer")
parsed_exp_one_no_timer_full <- parsed_exp_one_no_timer_full %>%
    mutate(experiment_type = "no timer")

## combine pilot and full data from no timer experiments into a single data frame
parsed_exp_one_combined <- bind_rows(list(parsed_exp_one,
                                          parsed_exp_one_no_timer_pilot,
                                          parsed_exp_one_no_timer_full))

## save combined parsed data as a csv
write_csv(parsed_exp_one_combined, path="../data/working/exp-one-combined.csv")

## read in data from experiment two
input_file_exp_two <- "../data/input/experiment-two-data.csv"
output_file_exp_two <- "../data/working/experiment-two-tmp.csv"
input_file_exp_two_no_timer_pilot <- "../data/input/experiment-two-no-timer-pilot.csv"
output_file_exp_two_no_timer_pilot <- "../data/working/experiment-two-no-timer-pilot-tmp.csv"
input_file_exp_two_no_timer_full <- "../data/input/experiment-two-no-timer-full.csv"
output_file_exp_two_no_timer_full <- "../data/working/experiment-two-no-timer-full-tmp.csv"

## run python parser
system(paste('python2.7 parser.py', input_file_exp_two, output_file_exp_two))
system(paste('python2.7 parser.py', input_file_exp_two_no_timer_pilot, output_file_exp_two_no_timer_pilot))
system(paste('python2.7 parser.py', input_file_exp_two_no_timer_full, output_file_exp_two_no_timer_full))

## read the results of parsing into R
parsed_exp_two <- read_csv(output_file_exp_two)
parsed_exp_two_no_timer_pilot <- read_csv(output_file_exp_two_no_timer_pilot)
parsed_exp_two_no_timer_full <- read_csv(output_file_exp_two_no_timer_full)

## add column to indicate experiment (timer or no timer)
parsed_exp_two <- parsed_exp_two %>%
    mutate(experiment_type = "timer")
parsed_exp_two_no_timer_pilot <- parsed_exp_two_no_timer_pilot %>%
    mutate(experiment_type = "no timer")
parsed_exp_two_no_timer_full <- parsed_exp_two_no_timer_full %>%
    mutate(experiment_type = "no timer")

## combine pilot and full data from no timer experiments into a single data frame
parsed_exp_two_combined <- bind_rows(list(parsed_exp_two,
                                          parsed_exp_two_no_timer_pilot,
                                          parsed_exp_two_no_timer_full))

## save combined parsed data as csv
write_csv(parsed_exp_two_combined, path="../data/working/exp-two-combined.csv")

########################################
## clean data
########################################

## experiment one
exp_one_combined <- read_csv("../data/working/exp-one-combined.csv")

## rename columns
exp_one_combined <- exp_one_combined %>%
    rename(structure = structure_condition,
           dim = dimension_condition) %>%
    mutate(model = "Human")

## remove incomplete participants
n_responses <- 100

exp_one_completed <- exp_one_combined %>%
    group_by(subject_ID) %>%
    filter(n() == n_responses)

exp_one_dropouts <- exp_one_combined %>%
    group_by(subject_ID) %>%
    filter(n() < n_responses)

## save responses
write_csv(exp_one_completed, path="../data/working/exp-one-completed.csv")
write_csv(exp_one_dropouts, path="../data/working/exp-one-dropouts.csv")

## experiment two
exp_two_combined <- read_csv("../data/working/exp-two-combined.csv")

## rename columns
exp_two_combined <- exp_two_combined %>%
    rename(coherence = coherence_condition,
           dim = dimension_condition) %>%
    mutate(model = "Human")

## remove incomplete participants
n_responses <- 100

exp_two_completed <- exp_two_combined %>%
    group_by(subject_ID) %>%
    filter(n() == n_responses)

exp_two_dropouts <- exp_two_combined %>%
    group_by(subject_ID) %>%
    filter(n() < n_responses)

## save responses
write_csv(exp_two_completed, path="../data/working/exp-two-completed.csv")
write_csv(exp_two_dropouts, path="../data/working/exp-two-dropouts.csv")

########################################
## descriptive statistics
########################################

## experiment one
exp_one <- read_csv("../data/working/exp-one-completed.csv")

## add factors
## convert columns to factors
cols <- c("structure", "dim", "subject_ID", "block", "experiment_type")
exp_one <- exp_one %>%
    mutate_at(cols, factor)

## rename structure conditions to match paper
exp_one <- exp_one %>%
    mutate(structure = case_when(
               structure == "family" ~ "All",
               structure == "intermediate" ~ "Intermediate",
               structure == "rule" ~ "Single"))

## descriptives
descriptives <- exp_one %>%
    group_by(experiment_type, subject_ID, structure, dim, gender, age, country) %>%
    summarise()

## get number of participants
(num_participants <- nrow(descriptives))

## get number of males/females
(num_gender <- descriptives %>% group_by(gender) %>% summarise(n = n()))

## get age summary
(min_age <- min(descriptives$age))
(max_age <- max(descriptives$age))
(mean_age <- mean(descriptives$age))

## get number from each country
(num_country <- descriptives %>% group_by(country) %>% summarise(n = n()))

## get number in each experiment
(num_experiment_type <- descriptives %>% group_by(experiment_type) %>% summarise(n = n()))

## calculate number of dropouts
exp_one_dropouts <- read_csv("../data/working/exp-one-dropouts.csv")
(exp_one_dropouts %>% select(subject_ID) %>% unique() %>% nrow())

## calculate number of subjects per condition
## by structure
(exp_one %>%
    group_by(structure) %>%
    select(subject_ID) %>%
    unique() %>%
    summarise(n = n()))

## by dimensionality
(exp_one %>%
    group_by(dim) %>%
    select(subject_ID) %>%
    unique() %>%
    summarise(n = n()))

## calculate mean performance by structure and dim
exp_one_individual_perf <- exp_one %>%
    group_by(experiment_type, structure, dim, block, subject_ID) %>%
    summarise(perf_subj = sum(button_value == label) / n())
    
## compare experiments involving timer and no timer to see if there is any difference
exp_one_individual_perf_df <- as.data.frame(exp_one_individual_perf)
exp_one_individual_perf_df$subject_ID <- as.factor(exp_one_individual_perf_df$subject_ID)

if (!file.exists("../data/working/exp-one-model-one.RData")) {
    model_one <- lmBF(perf_subj ~ block + structure + dim + experiment_type + subject_ID,
                      data = exp_one_individual_perf_df,
                      whichRandom = "subject_ID")
    saveRDS(model_one, "../data/working/exp-one-model-one.RData")
} else {
    model_one <- readRDS("../data/working/exp-one-model-one.RData")
}

if (!file.exists("../data/working/exp-one-model-two.RData")) {
    model_two <- lmBF(perf_subj ~ block + structure + dim + subject_ID,
                      data = exp_one_individual_perf_df,
                      whichRandom = "subject_ID")
    saveRDS(model_two, "../data/working/exp-one-model-two.RData")
} else {
    model_two <- readRDS("../data/working/exp-one-model-two.RData")
}

print("Comparing timer vs. no timer experiments")
print(model_two / model_one)

## plot average performance
exp_one_perf <- exp_one_individual_perf %>%
    ungroup() %>%
    group_by(structure, dim, block) %>%
    summarise(perf_mean = mean(perf_subj),
              perf_ci = 1.96 * sd(perf_subj) / sqrt(n()))

limits <- aes(ymax = perf_mean + perf_ci, ymin = perf_mean - perf_ci)
dodge <- position_dodge(width = 0.25)

## for capitalizing facet titles
capitalize <- function(string) {
    substr(string, 1, 1) <- toupper(substr(string, 1, 1))
    return(string)
}

## performance plot for experiment one
exp_one_human_plot <- ggplot(exp_one_perf,
                             aes(x = block, y = perf_mean,
                                 ymin = 0.4, ymax = 1,
                                 group = dim,
                                 colour = dim,
                                 shape = dim,
                                 fill = dim)) +
    geom_hline(yintercept = 0.5, colour = "grey", linetype = "dashed") +
    geom_line(position = dodge) +
    geom_errorbar(limits, position = dodge, width = 1) +
    geom_point(size = 4, position = dodge) +
    labs(x = "Block", y = "Proportion correct") +
    scale_shape_manual(values = c(21, 22, 24),
                       name = "Dimensionality",
                       breaks = c("4", "10", "16"),
                       labels = c("4", "10", "16")) +
    scale_fill_grey(name = "Dimensionality",
                    breaks = c("4", "10", "16"),
                    labels = c("4", "10", "16")) +
    scale_colour_grey(name = "Dimensionality",
                      breaks = c("4", "10", "16"),
                      labels = c("4", "10", "16")) +
    theme_few() +
    theme(legend.position = "bottom",
          panel.border = element_rect(colour = "black", size = 1),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          axis.text.x = element_text(size = 16),
          axis.text.y = element_text(size = 16),
          axis.ticks.x = element_line(colour = "black"),
          axis.ticks.y = element_line(colour = "black"),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 16),
          strip.text.x = element_text(size = 16),
          strip.text.y = element_text(size = 16)) +
    facet_grid(~ structure)

ggsave("../data/output/exp-one-mean-performance.pdf", exp_one_human_plot)

## experiment two
exp_two <- read_csv("../data/working/exp-two-completed.csv")

## add factors
cols <- c("coherence", "dim", "subject_ID", "block", "experiment_type")
exp_two <- exp_two_combined %>%
    mutate_at(cols, factor)

exp_two$coherence <- factor(exp_two$coherence,
                                     levels = c(90, 80, 70),
                                     labels = c("90%", "80%", "70%"))

## descriptives
descriptives <- exp_two %>%
    group_by(experiment_type, subject_ID, coherence, dim, gender, age, country) %>%
    summarise()

## get number of participants
(num_participants <- nrow(descriptives))

## get number of males/females
(num_gender <- descriptives %>% group_by(gender) %>% summarise(n = n()))

## get age summary
(min_age <- min(descriptives$age))
(max_age <- max(descriptives$age))
(mean_age <- mean(descriptives$age))

## get number from each country
(num_country <- descriptives %>% group_by(country) %>% summarise(n = n()))

## get number from each experiment
(num_experiment_type <- descriptives %>% group_by(experiment_type) %>% summarise(n = n()))

## calculate mean performance by coherence and dim
exp_two_individual_perf <- exp_two %>%
    group_by(experiment_type, coherence, dim, block, subject_ID) %>%
    summarise(perf_subj = sum(button_value == label) / n())
    
## compare experiments involving timer and no timer to see if there is any difference
exp_two_individual_perf_df <- as.data.frame(exp_two_individual_perf)

## compare experiments to see if there is any difference
if (!file.exists("../data/working/exp-two-model-one.RData")) {
    model_one <- lmBF(perf_subj ~ block + coherence + dim + experiment_type + subject_ID,
                      data = exp_two_individual_perf_df,
                      whichRandom = "subject_ID")
    saveRDS(model_one, "../data/working/exp-two-model-one.RData")
} else {
    model_one <- readRDS("../data/working/exp-two-model-one.RData")
}   

if (!file.exists("../data/working/exp-two-model-two.RData")) {
    model_two <- lmBF(perf_subj ~ block + coherence + dim + subject_ID,
                      data = exp_two_individual_perf_df,
                      whichRandom = "subject_ID")
    saveRDS(model_one, "../data/working/exp-two-model-two.RData")
} else {
    model_two <- readRDS("../data/working/exp-two-model-two.RData")
}   

print("Comparing timer vs. no timer experiments")
print(model_two / model_one)

## plot average performance
exp_two_perf <- exp_two_individual_perf %>%
    ungroup() %>%
    group_by(coherence, dim, block) %>%
    summarise(perf_mean = mean(perf_subj),
              perf_ci = 1.96 * sd(perf_subj) / sqrt(n()))

limits <- aes(ymax = perf_mean + perf_ci, ymin = perf_mean - perf_ci)
dodge <- position_dodge(width = 0.25)

## for capitalizing facet titles
capitalize <- function(string) {
    substr(string, 1, 1) <- toupper(substr(string, 1, 1))
    return(string)
}

exp_two_human_plot <- ggplot(exp_two_perf,
                             aes(x = block, y = perf_mean,
                                 ymin = 0.4, ymax = 1,
                                 group = interaction(coherence, dim),
                                 colour = dim,
                                 shape = dim,
                                 fill = dim,
                                 linetype = dim)) +
    geom_hline(yintercept = 0.5, colour = "grey", linetype = "dashed") +
    geom_line(position = dodge) +
    geom_errorbar(limits, position = dodge, width = 1) +
    geom_point(size = 4, position = dodge) +
    labs(x = "Block", y = "Proportion correct") +
    scale_shape_manual(values = c(21, 22, 24),
                       name = "Dimensionality",
                       breaks = c("4", "10", "16"),
                       labels = c("4", "10", "16")) +
    scale_fill_grey(name = "Dimensionality",
                    breaks = c("4", "10", "16"),
                    labels = c("4", "10", "16")) +
    scale_colour_grey(name = "Dimensionality",
                      breaks = c("4", "10", "16"),
                      labels = c("4", "10", "16")) +
    scale_linetype_manual(values = c(1, 1, 1),
                          name = "Dimensionality",
                          breaks = c("4", "10", "16"),
                          labels = c("4", "10", "16")) +
    facet_grid(~ coherence, labeller = labeller(coherence = capitalize)) +
    theme_few() +
    theme(legend.position = "bottom",
          panel.border = element_rect(colour = "black", size = 1),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          axis.text.x = element_text(size = 16),
          axis.text.y = element_text(size = 16),
          axis.ticks.x = element_line(colour = "black"),
          axis.ticks.y = element_line(colour = "black"),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 16),
          strip.text.x = element_text(size = 16),
          strip.text.y = element_text(size = 16))

ggsave("../data/output/exp-two-mean-performance.pdf", exp_two_human_plot)

########################################
## bayes factors 
########################################

## experiment one
## load bayes factor analyses if they exist
if(file.exists("../data/working/exp-one-bayes-factors.RData")) {
    bf_exp_one_exists <- TRUE
    bf_exp_one <- readRDS("../data/working/exp-one-bayes-factors.RData")
    intercept_only <- bf_exp_one[[1]]
    block_only <- bf_exp_one[[2]]
    block_estimates <- bf_exp_one[[3]]
    structure_block <- bf_exp_one[[4]]
    structure_block_estimates <- bf_exp_one[[5]]
    structure_block_interaction <- bf_exp_one[[6]]
    dim_block <- bf_exp_one[[7]]
    dim_block_estimates <- bf_exp_one[[8]]
    full_model <- bf_exp_one[[9]]
    full_model_interaction <- bf_exp_one[[10]]
    family_intercept_only <- bf_exp_one[[11]]
    family_dim <- bf_exp_one[[12]]
    family_dim_estimates <- bf_exp_one[[13]]
    intermediate_intercept_only <- bf_exp_one[[14]]
    intermediate_dim <- bf_exp_one[[15]]
    intermediate_dim_estimates <- bf_exp_one[[16]]
    rule_intercept_only <- bf_exp_one[[17]]
    rule_dim <- bf_exp_one[[18]]
    rule_dim_estimates <- bf_exp_one[[19]]
} else {
    bf_exp_one_exists <- FALSE
}

## calculate individual performance per block
exp_one_individual_perf <- exp_one %>%
    group_by(structure, dim, block, subject_ID) %>%
    summarise(perf = sum(button_value == label)/n())

## convert to data frame
exp_one_individual_perf_df <- as.data.frame(exp_one_individual_perf)

## convert block to numeric instead of factor
exp_one_individual_perf_df$block <- as.numeric(exp_one_individual_perf_df$block)

## analyses performed in the order reported in the paper
## intercept only model (baseline model)
if(!bf_exp_one_exists) {
    intercept_only <- lmBF(perf ~ subject_ID,
                           data = exp_one_individual_perf_df,
                           whichRandom = "subject_ID")


    ## main effect of block
    block_only <- lmBF(perf ~ block + subject_ID,
                       data = exp_one_individual_perf_df,
                       whichRandom = "subject_ID")
}

## compare block vs. intercept model, i.e. did people learn across the task?
print("Comparing block to intercept only model")
print(block_only/intercept_only)  ## result: 5 x 10 ^ 77

if(!bf_exp_one_exists) {
    ## get posterior estimates of block, i.e. how fast did people learn across blocks?
    block_estimates <- summary(posterior(block_only,
                                         iterations = 1000,
                                         thin = 10,
                                         columnFilter = "^subject_ID$"))
}

## get means and cis from block
block_statistics <- block_estimates$statistics
block_quantiles <- block_estimates$quantiles
(block_mean <- block_statistics[2, 1])  ## 0.0246
(block_ci <- block_quantiles[2, c(1, 5)])  ## 0.0218 to 0.0269

if(!bf_exp_one_exists) {
    ## were there differences in learning across category structures?
    ## structure + block model
    structure_block <- lmBF(perf ~ structure + block + subject_ID,
                            data = exp_one_individual_perf_df,
                            whichRandom = "subject_ID")
}

## compare structure + block vs. block only model
## i.e., did learning differ across structures?
print("Comparing model with structure and block to model with only block")
print(structure_block/block_only)  ## result: 7 x 10 ^ 90

if(!bf_exp_one_exists) {
    ## get posterior estimates for structure + block model
    structure_block_estimates <- summary(posterior(structure_block,
                                                   iterations = 1000,
                                                   thin = 10,
                                                   columnFilter = "^subject_ID$"))
}

## get means and cis from structure
structure_statistics <- structure_block_estimates$statistics
structure_quantiles <- structure_block_estimates$quantiles

## calculate means and cis for each structure condition
family_mean <- (structure_statistics[1, ] + structure_statistics[2, ])[1]
intermediate_mean <- (structure_statistics[1, ] + structure_statistics[3, ])[1]
rule_mean <- (structure_statistics[1, ] + structure_statistics[4, ])[1]

family_ci <- (structure_quantiles[1, ] + structure_quantiles[2, ])[c(1, 5)]
intermediate_ci <- (structure_quantiles[1, ] + structure_quantiles[3, ])[c(1, 5)]
rule_ci <- (structure_quantiles[1, ] + structure_quantiles[4, ])[c(1, 5)]

## examine differences between pairs of category structures
(family_intermediate_diff_mean <- family_mean - intermediate_mean)  ## 0.1626542
(family_intermediate_diff_ci_low <- family_ci[1] - intermediate_ci[2])  ## 0.1214471 
(family_intermediate_diff_ci_high <- family_ci[2] - intermediate_ci[1])  ## 0.2031296

(intermediate_rule_diff_mean <- intermediate_mean - rule_mean)  ## 0.07716591
(intermediate_rule_diff_ci_low <- intermediate_ci[1] - rule_ci[2])  ## 0.03916844
(intermediate_rule_diff_ci_high <- intermediate_ci[2] - rule_ci[1])  ## 0.1183468

if(!bf_exp_one_exists) {
    ## compare structure + block vs. structure + block interaction models
    ## i.e., did the rate of learning differ across structures?
    ## structure + block interaction model
    structure_block_interaction <-
        lmBF(perf ~ structure * block + subject_ID,
             data = exp_one_individual_perf_df,
             whichRandom = "subject_ID")
}

print("Comparing model with structure and block to model with interaction")
print(structure_block/structure_block_interaction)  ## BF = 40:1 (39.78216 to be precise)

if(!bf_exp_one_exists) {
    ## main effect of dimension
    dim_block <- lmBF(perf ~ dim + block + subject_ID,
                      data = exp_one_individual_perf_df,
                      whichRandom = "subject_ID")
}

## compare dimension + block vs. block only model
## i.e., are there differences in learning with varying number of features?
print("Comparing model with dimension and block to model with only block")
print(dim_block/block_only)  ## BF: 2.4 x 10 ^ 7 : 1

if(!bf_exp_one_exists) {
    ## get posterior estimates for dim + block model
    dim_block_estimates <- summary(posterior(dim_block,
                                             iterations = 1000,
                                             thin = 10,
                                             columnFilter = "^subject_ID$"))
}

## get means and cis from dimension
dim_block_statistics <- dim_block_estimates$statistics
dim_block_quantiles <- dim_block_estimates$quantiles

## calculate means and cis for each dimension condition
four_dim_mean <- (dim_block_statistics[1, ] + dim_block_statistics[2, ])[1]
ten_dim_mean <- (dim_block_statistics[1, ] + dim_block_statistics[3, ])[1]
sixteen_dim_mean <- (dim_block_statistics[1, ] + dim_block_statistics[4, ])[1]

four_dim_ci <- (dim_block_quantiles[1, ] + dim_block_quantiles[2, ])[c(1, 5)]
ten_dim_ci <- (dim_block_quantiles[1, ] + dim_block_quantiles[3, ])[c(1, 5)]
sixteen_dim_ci <- (dim_block_quantiles[1, ] + dim_block_quantiles[4, ])[c(1, 5)]

## get relative mean and ci across dimension conditions
(four_sixteen_diff_mean <- four_dim_mean - sixteen_dim_mean)  ## 0.08201129
(four_sixteen_diff_ci_low <- four_dim_ci[1] - sixteen_dim_ci[2])  ## 0.03694027
(four_sixteen_diff_ci_high <- four_dim_ci[2] - sixteen_dim_ci[1])  ## 0.1291465

(four_ten_diff_mean <- four_dim_mean - ten_dim_mean)  ## 0.05409418
(four_ten_diff_ci_low <- four_dim_ci[1] - ten_dim_ci[2])  ## 0.008841052
(four_ten_diff_ci_high <- four_dim_ci[2] - ten_dim_ci[1])  ## 0.1011046

(ten_sixteen_diff_mean <- ten_dim_mean - sixteen_dim_mean)  ## 0.02791711
(ten_sixteen_diff_ci_low <- ten_dim_ci[1] - sixteen_dim_ci[2])  ## -0.0172425
(ten_sixteen_diff_ci_high <- ten_dim_ci[2] - sixteen_dim_ci[1])  ## 0.07338362

if(!bf_exp_one_exists) {
    ## central theoretical question, is there interaction btw structure + dim?
    ## full model with interaction term
    full_model_interaction <- lmBF(perf ~ structure * dim + block + subject_ID,
                                   data = exp_one_individual_perf_df,
                                   whichRandom = "subject_ID")

    ## full model without interaction
    full_model <- lmBF(perf ~ structure + dim + block + subject_ID,
                       data = exp_one_individual_perf_df,
                       whichRandom = "subject_ID")
}

## compare full model with and without interaction
print("Comparing full model with and without interaction")
print(full_model_interaction/full_model)  ## BF: 6.2 x 10 ^ 8 : 1

## compare within each structure conditions, for results section in Table 1
## family structure
family_perf_df <- exp_one_individual_perf_df %>%
    filter(structure == "All")

if(!bf_exp_one_exists) {
    family_intercept_only <- lmBF(perf ~ block + subject_ID,
                                  data = family_perf_df,
                                  whichRandom = "subject_ID")

    family_dim <- lmBF(perf ~ dim + block + subject_ID,
                       data = family_perf_df,
                       whichRandom = "subject_ID")
}

print("Comparing family with and without dimensionality")
print(family_dim/family_intercept_only)  ## BF = 0.3 : 1

if(!bf_exp_one_exists) {
    ## get posterior estimates for each category structure
    family_dim_estimates <- summary(posterior(family_dim,
                                              iterations = 1000,
                                              thin = 10,
                                              columnFilter = "^subject_ID$"))
}

## get mean and ci of differences between each pair of dimension conditions
family_dim_statistics <- family_dim_estimates$statistics
family_dim_quantiles <- family_dim_estimates$quantiles

family_dim_4_mean <- (family_dim_statistics[1, ] + family_dim_statistics[2, ])[1]
family_dim_10_mean <- (family_dim_statistics[1, ] + family_dim_statistics[3, ])[1]
family_dim_16_mean <- (family_dim_statistics[1, ] + family_dim_statistics[4, ])[1]

family_dim_4_ci <- (family_dim_quantiles[1, ] + family_dim_quantiles[2, ])[c(1, 5)]
family_dim_10_ci <- (family_dim_quantiles[1, ] + family_dim_quantiles[3, ])[c(1, 5)]
family_dim_16_ci <- (family_dim_quantiles[1, ] + family_dim_quantiles[4, ])[c(1, 5)]

(family_dim_4_10_mean_diff <- family_dim_4_mean - family_dim_10_mean)  ## 0.01047057
(family_dim_4_10_mean_ci_low <- family_dim_4_ci[1] - family_dim_10_ci[2])  ## -0.04450229
(family_dim_4_10_mean_ci_high <- family_dim_4_ci[2] - family_dim_10_ci[1])  ## 0.06820879

(family_dim_10_16_mean_diff <- family_dim_10_mean - family_dim_16_mean)  ## -0.02713499
(family_dim_10_16_mean_ci_low <- family_dim_10_ci[1] - family_dim_16_ci[2])  ## -0.08491663
(family_dim_10_16_mean_ci_high <- family_dim_10_ci[2] - family_dim_16_ci[1])  ## 0.02840743

(family_dim_4_16_mean_diff <- family_dim_4_mean - family_dim_16_mean)  ## -0.01666442
(family_dim_4_16_mean_ci_low <- family_dim_4_ci[1] - family_dim_16_ci[2])  ## -0.07445631
(family_dim_4_16_mean_ci_high <- family_dim_4_ci[2] - family_dim_16_ci[1])  ## 0.0416536

## intermediate structure
intermediate_perf_df <- exp_one_individual_perf_df %>%
    filter(structure == "Intermediate")

if(!bf_exp_one_exists) {
    intermediate_intercept_only <- lmBF(perf ~ block + subject_ID,
                                        data = intermediate_perf_df,
                                        whichRandom = "subject_ID")

    intermediate_dim <- lmBF(perf ~ dim + block + subject_ID,
                             data = intermediate_perf_df,
                             whichRandom = "subject_ID")
}

print("Comparing intermediate with and without dimensionality")
print(intermediate_dim/intermediate_intercept_only)  ## BF: 4.0 x 10 ^ 8 : 1

if(!bf_exp_one_exists) {
    ## get posterior estimates for each category structure
    intermediate_dim_estimates <- summary(posterior(intermediate_dim,
                                                    iterations = 1000,
                                                    thin = 10,
                                                    columnFilter = "^subject_ID$"))
}

## get mean and ci of differences between each pair of dimension conditions
intermediate_dim_statistics <- intermediate_dim_estimates$statistics
intermediate_dim_quantiles <- intermediate_dim_estimates$quantiles

intermediate_dim_4_mean <- (intermediate_dim_statistics[1, ] + intermediate_dim_statistics[2, ])[1]
intermediate_dim_10_mean <- (intermediate_dim_statistics[1, ] + intermediate_dim_statistics[3, ])[1]
intermediate_dim_16_mean <- (intermediate_dim_statistics[1, ] + intermediate_dim_statistics[4, ])[1]

intermediate_dim_4_ci <- (intermediate_dim_quantiles[1, ] + intermediate_dim_quantiles[2, ])[c(1, 5)]
intermediate_dim_10_ci <- (intermediate_dim_quantiles[1, ] + intermediate_dim_quantiles[3, ])[c(1, 5)]
intermediate_dim_16_ci <- (intermediate_dim_quantiles[1, ] + intermediate_dim_quantiles[4, ])[c(1, 5)]

(intermediate_dim_4_10_mean_diff <- intermediate_dim_4_mean - intermediate_dim_10_mean)  ## 0.07354429
(intermediate_dim_4_10_mean_ci_low <- intermediate_dim_4_ci[1] - intermediate_dim_10_ci[2])  ## 0.01592644
(intermediate_dim_4_10_mean_ci_high <- intermediate_dim_4_ci[2] - intermediate_dim_10_ci[1])  ## 0.1356503

(intermediate_dim_10_16_mean_diff <- intermediate_dim_10_mean - intermediate_dim_16_mean)  ## 0.03673458
(intermediate_dim_10_16_mean_ci_low <- intermediate_dim_10_ci[1] - intermediate_dim_16_ci[2])  ## -0.02611997
(intermediate_dim_10_16_mean_ci_high <- intermediate_dim_10_ci[2] - intermediate_dim_16_ci[1])  ## 0.09551093

(intermediate_dim_4_16_mean_diff <- intermediate_dim_4_mean - intermediate_dim_16_mean)  ## 0.1102789
(intermediate_dim_4_16_mean_ci_low <- intermediate_dim_4_ci[1] - intermediate_dim_16_ci[2])  ## 0.05196232
(intermediate_dim_4_16_mean_ci_high <- intermediate_dim_4_ci[2] - intermediate_dim_16_ci[1])  ## 0.1690054

## rule structure
rule_perf_df <- exp_one_individual_perf_df %>%
    filter(structure == "Single")

if(!bf_exp_one_exists) {
    rule_intercept_only <- lmBF(perf ~ block + subject_ID,
                                data = rule_perf_df,
                                whichRandom = "subject_ID")

    rule_dim <- lmBF(perf ~ dim + block + subject_ID,
                     data = rule_perf_df,
                     whichRandom = "subject_ID")
}

print("Comparing rule with and without dimensionality")
print(rule_dim/rule_intercept_only)  ## [1] dim + block + subject_ID : 327562807236 ±2.01%

if(!bf_exp_one_exists) {
    ## get posterior estimates for each category structure
    rule_dim_estimates <- summary(posterior(rule_dim,
                                            iterations = 1000,
                                            thin = 10,
                                            columnFilter = "^subject_ID$"))
}

## get mean and ci of differences between each pair of dimension conditions
rule_dim_statistics <- rule_dim_estimates$statistics
rule_dim_quantiles <- rule_dim_estimates$quantiles

rule_dim_4_mean <- (rule_dim_statistics[1, ] + rule_dim_statistics[2, ])[1]
rule_dim_10_mean <- (rule_dim_statistics[1, ] + rule_dim_statistics[3, ])[1]
rule_dim_16_mean <- (rule_dim_statistics[1, ] + rule_dim_statistics[4, ])[1]

rule_dim_4_ci <- (rule_dim_quantiles[1, ] + rule_dim_quantiles[2, ])[c(1, 5)]
rule_dim_10_ci <- (rule_dim_quantiles[1, ] + rule_dim_quantiles[3, ])[c(1, 5)]
rule_dim_16_ci <- (rule_dim_quantiles[1, ] + rule_dim_quantiles[4, ])[c(1, 5)]

(rule_dim_4_10_mean_diff <- rule_dim_4_mean - rule_dim_10_mean)  ## 0.1129578
(rule_dim_4_10_mean_ci_low <- rule_dim_4_ci[1] - rule_dim_10_ci[2])  ## 0.04131588
(rule_dim_4_10_mean_ci_high <- rule_dim_4_ci[2] - rule_dim_10_ci[1])  ## 0.1820857

(rule_dim_10_16_mean_diff <- rule_dim_10_mean - rule_dim_16_mean)  ## 0.03301695
(rule_dim_10_16_mean_ci_low <- rule_dim_10_ci[1] - rule_dim_16_ci[2])  ## -0.03341841
(rule_dim_10_16_mean_ci_high <- rule_dim_10_ci[2] - rule_dim_16_ci[1])  ## 0.1014554

(rule_dim_4_16_mean_diff <- rule_dim_4_mean - rule_dim_16_mean)  ## 0.1459748
(rule_dim_4_16_mean_ci_low <- rule_dim_4_ci[1] - rule_dim_16_ci[2])  ## 0.07426745
(rule_dim_4_16_mean_ci_high <- rule_dim_4_ci[2] - rule_dim_16_ci[1])  ## 0.2171712

if(!bf_exp_one_exists) {
    ## save bayes factor results and posterior analyses from experiment one
    bf_exp_one <- list(intercept_only,
                       block_only,
                       block_estimates,
                       structure_block,
                       structure_block_estimates,
                       structure_block_interaction,
                       dim_block,
                       dim_block_estimates,
                       full_model,
                       full_model_interaction,
                       family_intercept_only,
                       family_dim,
                       family_dim_estimates,
                       intermediate_intercept_only,
                       intermediate_dim,
                       intermediate_dim_estimates,
                       rule_intercept_only,
                       rule_dim,
                       rule_dim_estimates)

    saveRDS(bf_exp_one, file="../data/working/exp-one-bayes-factors.RData")
}

## experiment two
## check if bayes factor analyses are saved and load them if so
if(file.exists("../data/working/exp-two-bayes-factors.RData")) {
    bf_exp_two_exists <- TRUE
    bf_exp_two <- readRDS("../data/working/exp-two-bayes-factors.RData")
    intercept_only <- bf_exp_two[[1]]
    block_only <- bf_exp_two[[2]]
    block_estimates <- bf_exp_two[[3]]
    coherence_block <- bf_exp_two[[4]]
    coherence_block_estimates <- bf_exp_two[[5]]
    dimension_block <- bf_exp_two[[6]]
    full_model <- bf_exp_two[[7]]
    full_model_with_interaction<- bf_exp_two[[8]]
} else {
    bf_exp_two_exists <- FALSE
}

exp_two_individual_perf <- exp_two_completed %>%
    group_by(coherence, dim, block, subject_ID) %>%
    summarise(perf = sum(button_value == label)/n())

## convert to data frame
exp_two_individual_perf_df <- as.data.frame(exp_two_individual_perf)

## convert columns to numeric and factor
exp_two_individual_perf_df$block <- as.numeric(exp_two_individual_perf_df$block)
exp_two_individual_perf_df$subject_ID <- as.factor(exp_two_individual_perf_df$subject_ID)
exp_two_individual_perf_df$dim <- as.factor(exp_two_individual_perf_df$dim)
exp_two_individual_perf_df$coherence <- as.factor(exp_two_individual_perf_df$coherence)

if(!bf_exp_two_exists) {
    ## analyses performed in the order reported in the paper
    ## intercept only model (baseline model)
    intercept_only <- lmBF(perf ~ subject_ID,
                           data = exp_two_individual_perf_df,
                           whichRandom = "subject_ID")

    ## main effect of block
    block_only <- lmBF(perf ~ block + subject_ID,
                       data = exp_two_individual_perf_df,
                       whichRandom = "subject_ID")
}

## compare block vs. intercept model
print("Comparing block to intercept only model")
print(block_only/intercept_only) ## [1] block + subject_ID : 2.337178e+74 ±1.03%

if(!bf_exp_two_exists) {
    block_estimates <- summary(posterior(block_only,
                                         iterations = 1000,
                                         thin = 10,
                                         columnFilter = "^subject_ID$"))
}

## get means and cis from block
block_statistics <- block_estimates$statistics
block_quantiles <- block_estimates$quantiles
(block_mean <- block_statistics[2, 1])  ## 0.02315282
(block_ci <- block_quantiles[2, c(1, 5)])  ## 0.02045670 0.02531492 

if(!bf_exp_two_exists) {
    ## coherence + block model
    coherence_block <- lmBF(perf ~ coherence + block + subject_ID,
                            data = exp_two_individual_perf_df,
                            whichRandom = "subject_ID")
}

## compare coherence to intercept only
print("Comparing model with coherence and block to block only model")
print(coherence_block/block_only)  ## [1] coherence + block + subject_ID : 1.054789e+116 ±2%

if(!bf_exp_two_exists) {
    ## get posterior estimates for coherence model
    coherence_block_estimates <- summary(posterior(coherence_block,
                                                   iterations = 1000,
                                                   thin=10,
                                                   columnFilter="^subject_ID$"))
}

coherence_block_statistics <- coherence_block_estimates$statistics
coherence_block_quantiles <- coherence_block_estimates$quantiles

ninety_mean <- (coherence_block_statistics[1, ] + coherence_block_statistics[4, ])[1]
eighty_mean <- (coherence_block_statistics[1, ] + coherence_block_statistics[3, ])[1]
seventy_mean <- (coherence_block_statistics[1, ] + coherence_block_statistics[2, ])[1]

ninety_ci <- (coherence_block_quantiles[1, ] + coherence_block_quantiles[4, ])[c(1, 5)]
eighty_ci <- (coherence_block_quantiles[1, ] + coherence_block_quantiles[3, ])[c(1, 5)]
seventy_ci <- (coherence_block_quantiles[1, ] + coherence_block_quantiles[2, ])[c(1, 5)]

(ninety_eighty_diff_mean <- ninety_mean - eighty_mean)  ## 0.1088757
(ninety_eighty_diff_ci_low <- ninety_ci[1] - eighty_ci[2])  ## 0.07261369
(ninety_eighty_diff_ci_high <- ninety_ci[2] - eighty_ci[1])  ## 0.1404002

(eighty_seventy_diff_mean <- eighty_mean - seventy_mean)  ## 0.1156494
(eighty_seventy_diff_ci_low <- eighty_ci[1] - seventy_ci[2])  ## 0.08432525
(eighty_seventy_diff_ci_high <- eighty_ci[2] - seventy_ci[1])  ## 0.1482227

if(!bf_exp_two_exists) {
    ## dimension + block model
    dimension_block <- lmBF(perf ~ dim + block + subject_ID,
                            data = exp_two_individual_perf_df,
                            whichRandom = "subject_ID")
}

## compare dimension and block model to block only model
print("Comparing dimension and block to block only model")
print(block_only/dimension_block)  ## block + subject_ID : 18.05508 ±1.65%

if(!bf_exp_two_exists) {
    ## post-hoc analyses
    ## full model
    full_model <- lmBF(perf ~ coherence + dim + block + subject_ID,
                       data = exp_two_individual_perf_df,
                       whichRandom = "subject_ID")

    full_model_with_interaction <- lmBF(perf ~ coherence * dim + block + subject_ID,
                                        data = exp_two_individual_perf_df,
                                        whichRandom = "subject_ID")
}

## compare coherence + dim against full model/full model + interaction
print(coherence_block/full_model)  ## coherence + block + subject_ID : 7.06244 ±3.52%
print(coherence_block/full_model_with_interaction)  ## coherence + block + subject_ID : 147.5235 ±3.42%

## save exp two bayes factor analyses
if(!bf_exp_two_exists) {
    bf_exp_two <- list(intercept_only,
                       block_only,
                       block_estimates,
                       coherence_block,
                       coherence_block_estimates,
                       dimension_block,
                       full_model,
                       full_model_with_interaction)

    saveRDS(bf_exp_two, file="../data/working/exp-two-bayes-factors.RData")
}

########################################
## model comparisons
########################################

source("run-simulations.R")

## experiment one
## load simulations
if(file.exists("../data/working/experiment-one-simulations.RData")) {
    exp_one_simulations <- readRDS("../data/working/experiment-one-simulations.RData")
} else {
    exp_one_simulations <- RunSimulationsExpOne()
    saveRDS(exp_one_simulations, file="../data/working/experiment-one-simulations.RData")
}

## ungroup and rename some columns
exp_one_simulations <- exp_one_simulations %>%
    ungroup() %>%
    rename(perf_model = perf_mean,
           dim = dimension)

## rename structure conditions to match paper
exp_one_simulations <- exp_one_simulations %>%
    mutate_at("structure", factor) %>%
    mutate(structure = case_when(
               structure == "family" ~ "All",
               structure == "intermediate" ~ "Intermediate",
               structure == "rule" ~ "Single"))

## rename models and re-order factors
exp_one_simulations <- exp_one_simulations %>%
    mutate(model = fct_recode(model,
        Optimal = "full-update-full-decision",
        Limited = "limited-update-limited-decision",
        Rule = "hypothesis-testing"),
        model = fct_relevel(model, c("Optimal", "Limited", "Rule"))) 

## append human performance as a column to model simulations
exp_one_simulations <- exp_one_simulations %>%
    mutate(perf_human = rep(exp_one_perf$perf_mean, 3))

## calculate rmse and correlation between model and human for each condition
exp_one_human_model_comparison <- exp_one_simulations %>%
    left_join(exp_one_perf, by=c("structure", "dim", "block")) %>%
    group_by(model, structure) %>%
    mutate(r_value = cor(perf_model, perf_human),
           rmse_value = sqrt(mean((perf_model - perf_human)^2)))

exp_one_human_model_scatterplot <- ggplot(exp_one_human_model_comparison, aes(x = perf_human,  y = perf_model)) +
    theme_few() +
    xlim(0, 1) +
    ylim(0, 1) +
    xlab("Human performance") +
    ylab("Model prediction") +
    geom_point() +
    geom_text(aes(x=0.30, y=0.95, label=paste0("RMSE = ", round(rmse_value, 2))), check_overlap=TRUE) +
    facet_grid(model ~ structure) +
    geom_abline(linetype="dashed") +
    theme(legend.position="bottom",
          plot.title = element_text(hjust = 0.5, face = "bold"),
          panel.border = element_rect(colour = "black", size = 1),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.ticks.x = element_line(colour = "black"),
          axis.ticks.y = element_line(colour = "black"),
          legend.title = element_text(size = 14),
          strip.text.x = element_text(size = 12),
          strip.text.y = element_text(size = 12))

ggsave("../data/output/human-model-correlation-exp-one.pdf", exp_one_human_model_scatterplot)

## comparing individual perf to rmse
exp_one_individual <- exp_one %>%
    group_by(subject_ID, structure, dim, block) %>%
    summarise(perf_subj = sum(button_value == label) / n())

## create random model
random_model <- exp_one_simulations %>%
    filter(model == "Optimal")
random_model$model <- "Random"
random_model$perf_model <- 0.5

exp_one_simulations <- bind_rows(list(
    exp_one_simulations, random_model))

exp_one_individual_rmse <- exp_one_individual %>%
    left_join(exp_one_simulations) %>%
    group_by(subject_ID, structure, dim, model) %>%
    summarise(rmse = sqrt(mean( (perf_subj - perf_model) ^ 2))) %>%
    arrange(rmse) %>%
    top_n(-1)

exp_one_individual_rmse <- exp_one_individual_rmse %>%
    ungroup() %>%
    mutate_at("model", factor) %>%
    mutate(model = fct_relevel(model, c("Optimal", "Limited", "Rule", "Random")))

exp_one_individual_rmse_structure_dim <- exp_one_individual_rmse %>%
    group_by(structure, dim, model) %>%
    summarise(n = n()) %>%
    mutate(prop = n/sum(n))

exp_one_individual_rmse_plot <- ggplot(exp_one_individual_rmse_structure_dim, aes(x = model, y = prop, fill = model)) +
    geom_bar(stat="identity") +
    facet_grid(dim ~ structure, labeller = labeller(structure = capitalize)) +
    labs(x = "\nBlock", y = "Proportion of participants best fit by each model\n") +
    ylim(0, 0.8) +
    scale_fill_grey(name = "Model  ") +
    theme_few() +
    theme(legend.position = "bottom",
          panel.border = element_rect(colour = "black", size = 1),
          axis.title.x = element_text(size = 24),
          axis.title.y = element_text(size = 20),
          axis.text.x = element_text(size = 0),
          axis.text.y = element_text(size = 20),
          axis.ticks.x = element_line(colour = "white"),
          axis.ticks.y = element_line(colour = "black"),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 20),
          strip.text.x = element_text(size = 20),
          strip.text.y = element_text(size = 20))

ggsave("../data/output/individual-best-model-by-rmse-exp-one.pdf", exp_one_individual_rmse_plot)

## experiment two
## load simulations
if(file.exists("../data/working/experiment-two-simulations.RData")) {
    exp_two_simulations <- readRDS("../data/working/experiment-two-simulations.RData")
    exp_two_simulations <- readRDS("../data/working/experiment-two-weighted-utility-simulations-final.RData")
} else {
    exp_two_simulations <- RunSimulationsExpTwo()
    saveRDS(exp_two_simulations, file="../data/working/experiment-two-simulations.RData")
}

## ungroup and rename some columns
exp_two_simulations <- exp_two_simulations %>%
    ungroup() %>%
    rename(perf_model = perf_mean,
           dim = dimension,
           coherence = structure)

## rename structure conditions to match paper
exp_two_simulations <- exp_two_simulations %>%
    mutate_at("coherence", factor) %>%
    mutate(coherence = case_when(
               coherence == "family-70" ~ "70%",
               coherence == "family-80" ~ "80%",
               coherence == "family-90" ~ "90%"),
           coherence = fct_relevel(coherence, c("90%", "80%", "70%"))) %>%
    arrange(model, coherence)

## rename models and re-order factors
exp_two_simulations <- exp_two_simulations %>%
    mutate(model = fct_recode(model,
        Optimal = "full-update-full-decision",
        Limited = "limited-update-limited-decision",
        Rule = "hypothesis-testing"),
        model = fct_relevel(model, c("Optimal", "Limited", "Rule"))) 

## append human performance as a column to model simulations
exp_two_simulations <- exp_two_simulations %>%
    mutate(perf_human = rep(exp_two_perf$perf_mean, 3))

## calculate rmse and correlation between model and human for each condition
exp_two_human_model_comparison <- exp_two_simulations %>%
    left_join(exp_two_perf, by=c("coherence", "dim", "block")) %>%
    group_by(model, coherence) %>%
    mutate(r_value = cor(perf_model, perf_human),
           rmse_value = sqrt(mean((perf_model - perf_human)^2)))

exp_two_human_model_scatterplot <- ggplot(exp_two_human_model_comparison, aes(x = perf_human,  y = perf_model)) +
    theme_few() +
    xlim(0, 1) +
    ylim(0, 1) +
    xlab("Human performance") +
    ylab("Model prediction") +
    geom_point() +
    geom_text(aes(x=0.30, y=0.95, label=paste0("RMSE = ", round(rmse_value, 2))), check_overlap=TRUE) +
    facet_grid(model ~ coherence) +
    geom_abline(linetype="dashed") +
    theme(legend.position="bottom",
          plot.title = element_text(hjust = 0.5, face = "bold"),
          panel.border = element_rect(colour = "black", size = 1),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.ticks.x = element_line(colour = "black"),
          axis.ticks.y = element_line(colour = "black"),
          legend.title = element_text(size = 14),
          strip.text.x = element_text(size = 12),
          strip.text.y = element_text(size = 12))

ggsave("../data/output/human-model-correlation-exp-two.pdf", exp_two_human_model_scatterplot)

## comparing individual perf to rmse
exp_two_individual <- exp_two %>%
    group_by(subject_ID, coherence, dim, block) %>%
    summarise(perf_subj = sum(button_value == label) / n())

## create random model
random_model <- exp_two_simulations %>%
    filter(model == "Optimal")
random_model$model <- "Random"
random_model$perf_model <- 0.5

exp_two_simulations <- bind_rows(list(
    exp_two_simulations, random_model))

exp_two_individual_rmse <- exp_two_individual %>%
    left_join(exp_two_simulations) %>%
    group_by(subject_ID, coherence, dim, model) %>%
    summarise(rmse = sqrt(mean( (perf_subj - perf_model) ^ 2))) %>%
    arrange(rmse) %>%
    top_n(-1)

exp_two_individual_rmse <- exp_two_individual_rmse %>%
    ungroup() %>%
    mutate_at("model", factor) %>%
    mutate(model = fct_relevel(model, c("Optimal", "Limited", "Rule", "Random")))

exp_two_individual_rmse_coherence_dim <- exp_two_individual_rmse %>%
    group_by(coherence, dim, model) %>%
    summarise(n = n()) %>%
    mutate(prop = n/sum(n))

exp_two_individual_rmse_plot <- ggplot(exp_two_individual_rmse_coherence_dim, aes(x = model, y = prop, fill = model)) +
    geom_bar(stat="identity") +
    facet_grid(dim ~ coherence, labeller = labeller(coherence = capitalize)) +
    labs(x = "\nBlock", y = "Proportion of participants best fit by each model\n") +
    ylim(0, 0.8) +
    scale_fill_grey(name = "Model  ") +
    theme_few() +
    theme(legend.position = "bottom",
          axis.title.x = element_text(size = 24),
          axis.title.y = element_text(size = 20),
          axis.text.x = element_text(size = 0),
          axis.text.y = element_text(size = 20),
          axis.ticks.x = element_line(colour = "white"),
          axis.ticks.y = element_line(colour = "black"),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 20),
          strip.text.x = element_text(size = 20),
          strip.text.y = element_text(size = 20))

ggsave("../data/output/individual-best-model-by-rmse-exp-two.pdf", exp_two_individual_rmse_plot)

########################################
## stimulus distribution check
########################################

AllStimProb <- function(stim, label) {
    if(label == 0) {
        n_zeros <- str_count(stim, "0")
        n_ones <- str_count(stim, "1")
        prob <- (0.9 ** n_zeros) * (0.1 ** n_ones)
    } else if(label == 1) {
        n_zeros <- str_count(stim, "0")
        n_ones <- str_count(stim, "1")
        prob <- (0.1 ** n_zeros) * (0.9 ** n_ones)
    }

    return(prob)
}

SingleStimProb <- function(stim, label) {
    if(label == 0) {
        prob <- 1
        if(substr(stim, 0, 1) == "0") {
            prob <- prob * 0.9
        }
        else {
            prob <- prob * 0.1
        }

        prob <- prob * (0.5 ** (nchar(stim) - 1))        
    } else if(label == 1) {
        prob <- 1
        if(substr(stim, 0, 1) == "0") {
            prob <- prob * 0.1
        }
        else {
            prob <- prob * 0.9
        }

        prob <- prob * (0.5 ** (nchar(stim) - 1))
    }

    return(prob)
}

all <- exp_one %>% filter(structure == "All")
single <- exp_one %>% filter(structure == "Single")

all <- all %>%
    rowwise() %>%
    mutate(stim_prob = log(AllStimProb(stim, label)))

single <- single %>%
    rowwise() %>%
    mutate(stim_prob = log(SingleStimProb(stim, label)))

all_stim_prob <- all %>%
    group_by(structure, dim, stim_prob) %>%
    summarise(n = n(),
              perf = mean(button_value == label))

single_stim_prob <- single %>%
    group_by(structure, dim, stim_prob) %>%
    summarise(n = n(),
              perf = mean(button_value == label))

stim_prob <- bind_rows(list(all_stim_prob, single_stim_prob))

ggplot(stim_prob, aes(x = stim_prob, y = perf, colour = dim, size = n)) +
    geom_point() +
    facet_grid( ~ structure)
