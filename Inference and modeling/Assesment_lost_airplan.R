options(digits = 3)
pr_B <- 0.6
pr_missed <- 0.1    # prob plane in B but not found
pr_not_found <-  1 - pr_B + pr_B*pr_missed
pr_missed * pr_B / pr_not_found

pr_C <- 0.15
pr_missed_not_present <- 1
pr_C * pr_missed_not_present / pr_not_found

pr_A <- 0.2
pr_D <- 0.05

pr_A_post <- pr_A * pr_missed_not_present / pr_not_found
pr_B_post <-  pr_missed * pr_B / pr_not_found
pr_C_post <- pr_C * pr_missed_not_present / pr_not_found
pr_D_post <- pr_D * pr_missed_not_present / pr_not_found

results <- c(pr_A_post, pr_B_post, pr_C_post, pr_D_post)
names(results) <- c("A", "B", "C", "D")
names(which.max(results))

pr_B <- 0.6
pr_missed <- 0.1    # prob of failed search
pr_B * (1 - pr_missed)

# use maximum posterior probability from previous question: region A
pr_A <- 0.2
pr_missed <- 0.1    # prob plane in B but not found
pr_not_found_in_B <-  1 - pr_B + pr_B*pr_missed
pr_missed_not_present <- 1
pr_A_post <- pr_A * pr_missed_not_present / pr_not_found

# probability plane found on day 2
pr_found_in_A <- pr_A_post*(1-pr_missed)

# probability plane not found on day 1 but found on day 2
pr_not_found_in_B * pr_found_in_A
