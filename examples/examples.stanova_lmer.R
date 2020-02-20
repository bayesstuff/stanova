
data("Machines", package = "MEMSS")

m_machines <- stanova_lmer(score ~ Machine + (Machine|Worker),
                           data=Machines, chains = 2, iter = 500)

out_list <- summary(m_machines)
str(out_list)

out_list

data("obk.long", package = "afex")

m2 <- stanova_lmer(value ~ treatment * phase + (1|id), obk.long,
                   chains = 2, iter = 500)

m2
summary(m2)
