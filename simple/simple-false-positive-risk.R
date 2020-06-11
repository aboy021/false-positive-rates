rm(list = ls()) # clear all vars from the current workspace


#########################
# Simulation Parameters #
#
# | prob_hyp_true | prob_hyp_true_factor |
# | --------------|----------------------|
# | 0.1           | 9                    |
# | 0.2           | 4                    |
# | 0.3           | 7/3                  |
# | 0.4           | 3/2                  |
# | 0.5           | 1                    |
# | 0.6           | 2/3                  |
# | 0.7           | 3/7                  |
# | 0.8           | 1/4                  |
# | 0.9           | 1/9                  |
#
#########################

# Trial size and probabilities
control_prob = 0.5
treatment_prob = 0.6
num_subjects = 100

# Number of simulations and probability the hypothesis was true
base_num_simulations = 10000
prob_hyp_true_factor = 1
num_simulations_control_vs_treatment = base_num_simulations
num_simulations_control_vs_control = base_num_simulations * prob_hyp_true_factor

# Tested p value significant range
min_p_value = 0.0
max_p_value = 0.05

test = fisher.test
#test = chisq.test


###########################
# Virtual trial functions #
###########################
is_significant <- function(p_value){
    return(min_p_value <= p_value & p_value <= max_p_value)
}

do_test <- function(control=0.5, treatment=0.5, test=chisq.test, num_subjects=100){
    control_alive <- rbinom(1,num_subjects,control)
    control_dead <- num_subjects - control_alive
    control_dead_alive <- c(control_alive, control_dead)
    
    treatment_alive <- rbinom(1,num_subjects,treatment)
    treatment_dead <- num_subjects - treatment_alive
    treatment_dead_alive <- c(treatment_alive, treatment_dead)
    
    matrix_dead_alive <- rbind(control_dead_alive,treatment_dead_alive)
    cs <- test(matrix_dead_alive)
    return(cs$p.value)
}


######################
# Run virtual trials #
######################
control_vs_control <- replicate(num_simulations_control_vs_control, do_test(control_prob,control_prob, test, num_subjects))
treatment_vs_control <- replicate(num_simulations_control_vs_treatment, do_test(control_prob,treatment_prob, test, num_subjects))


############################
# Probability calculations #
############################
significant_control_vs_control = length(Filter(is_significant, control_vs_control))
significant_treatment_vs_control = length(Filter(is_significant, treatment_vs_control))

true_positives  = significant_treatment_vs_control
false_positives = significant_control_vs_control
true_negatives  = num_simulations_control_vs_control - significant_control_vs_control
false_negatives = num_simulations_control_vs_treatment - significant_treatment_vs_control

total_number_of_trials = num_simulations_control_vs_control + num_simulations_control_vs_treatment

prob_positive_trial = (true_positives + false_positives) / total_number_of_trials
prob_negative_trial = (true_negatives + false_negatives) / total_number_of_trials

# Positive predictive value (PPV): Proportion of positive trials that are true
positive_predictive_value = true_positives / (true_positives + false_positives)

# False positive risk (FPR):Proportion of positive trials that are false
false_positive_risk = false_positives / (true_positives + false_positives)

# Negative predictive value (NPV): Proportion of negative trials that are true
negative_predictive_value = true_negatives / (true_negatives + false_negatives)

# False negative risk (FNR): Proportion of negative trials that are false
false_negative_risk = false_negatives / (true_negatives + false_negatives)


###################
# Display Results #
###################
cat("\n",
"total_number_of_trials:", "\t", total_number_of_trials, "\n",
"true_positives: ", "\t", true_positives, "\n",
"false_positives:", "\t", false_positives, "\n",
"true_negatives: ", "\t", true_negatives, "\n",
"false_negatives:", "\t", false_negatives, "\n",
"prob_positive_trial:", "\t", prob_positive_trial, "\n",
"prob_negative_trial:", "\t", prob_negative_trial, "\n",
"positive_predictive_value:", "\t", positive_predictive_value, "\n",
"false_positive_risk:", "\t", false_positive_risk, "\n",
"negative_predictive_value:", "\t", negative_predictive_value, "\n",
"false_negative_risk:", "\t", false_negative_risk, "\n\n")


############
# Graphing #
############
subtitle <- ifelse(identical(test, fisher.test), "Fisher's Exact", "Chi-squared")

hist(control_vs_control, sub=subtitle)
hist(treatment_vs_control, sub=subtitle)