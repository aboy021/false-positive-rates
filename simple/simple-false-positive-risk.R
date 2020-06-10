rm(list = ls()) # clear all vars from the current workspace


#########################
# Simulation Parameters #
#########################
control_prob = 0.5
treatment_prob = 0.6
num_subjects = 100
num_simulations_control_vs_control = 10000
num_simulations_control_vs_treatment = 10000
min_p_value = 0.0
max_p_value = 0.05

# test = fisher.test
test = chisq.test


###########################
# Virtual trial functions #
###########################
is_significant <- function(p_value){
    return(min_p_value <= p_value & p_value <= max_p_value)
}

do_test <- function(control=0.5, treatment=0.5, test=chisq.test, num_subjects=100){
    test_1 <- rbinom(1,num_subjects,control)
    test_1_minus100 <- num_subjects - test_1
    dead_alive_1 <- c(test_1, test_1_minus100)
    test_2 <- rbinom(1,num_subjects,treatment)
    test_2_minus100 <- num_subjects - test_2
    dead_alive_2 <- c(test_2, test_2_minus100)
    matrix_dead_alive <- rbind(dead_alive_1,dead_alive_2)
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
"true_positives ", "\t",
"false_positives", "\t",
"true_negatives ", "\t",
"false_negatives", "\t",
"total_number_of_trials", "\t",
"prob_positive_trial", "\t",
"prob_negative_trial", "\t",
"positive_predictive_value", "\t",
"false_positive_risk", "\t",
"negative_predictive_value", "\t",
"false_negative_risk", "\n",
true_positives , "\t",
false_positives, "\t",
true_negatives , "\t",
false_negatives, "\t",
total_number_of_trials, "\t",
prob_positive_trial, "\t",
prob_negative_trial, "\t",
positive_predictive_value, "\t",
false_positive_risk, "\t",
negative_predictive_value, "\t",
false_negative_risk, "\n\n")


############
# Graphing #
############
subtitle <- ifelse(identical(test, fisher.test), "Fisher's Exact", "Chi-squared")

hist(control_vs_control, sub=subtitle)
hist(treatment_vs_control, sub=subtitle)