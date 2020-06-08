rm(list = ls()) # clear all vars from the current workspace

#########################
# Simulation Parameters #
#########################

placebo_prob = 0.5
treatment_prob = 0.6
num_subjects = 100
# test = fisher.test
test = chisq.test
num_simulations = 10000
max_p_value = 0.05

#########################


is_significant <- function(p_value){
    return(p_value <= max_p_value)
}

do_test <- function(placebo=0.5, treatment=0.5, test=chisq.test, num_subjects=100){
    test_1 <- rbinom(1,num_subjects,placebo)
    test_1_minus100 <- num_subjects - test_1
    dead_alive_1 <- c(test_1, test_1_minus100)
    test_2 <- rbinom(1,num_subjects,treatment)
    test_2_minus100 <- num_subjects - test_2
    dead_alive_2 <- c(test_2, test_2_minus100)
    matrix_dead_alive <- rbind(dead_alive_1,dead_alive_2)
    cs <- test(matrix_dead_alive)
    return(cs$p.value)
}

placebo_vs_placebo <- replicate(num_simulations, do_test(placebo_prob,placebo_prob, test, num_subjects))
treatment_vs_placebo <- replicate(num_simulations, do_test(placebo_prob,treatment_prob, test, num_subjects))

false_positives = length(Filter(is_significant, placebo_vs_placebo))
true_positives = length(Filter(is_significant, treatment_vs_placebo))
false_positive_risk = false_positives / (true_positives + false_positives)

cat("False Positives", "\t", "True Positives", "\t", "False Positive Risk\n", 
    false_positives, "\t", true_positives, "\t", false_positive_risk, "\n")

subtitle <- ifelse(identical(test, fisher.test), "Fisher's Exact", "Chi-squared")

hist(placebo_vs_placebo, sub=subtitle)
hist(treatment_vs_placebo, sub=subtitle)