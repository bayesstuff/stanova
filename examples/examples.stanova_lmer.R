
data("Machines", package = "MEMSS")

m_machines <- stanova_lmer(score ~ Machine + (Machine|Worker),
                           data=Machines, chains = 2, iter = 500)

out_list <- summary(m_machines)
str(out_list)

out_list

