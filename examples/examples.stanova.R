
library("rstanarm")
m1 <- stanova(yield ~ block + N*P*K, npk, prior = R2(0.5))
m1
summary(m1)
