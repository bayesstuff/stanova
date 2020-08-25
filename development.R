library("devtools")
load_all()

document()

library("testthat")
devtools::test()
devtools::check()

### basic package things
usethis::use_build_ignore("development.R")
usethis::use_build_ignore("examples/")

use_package("rstanarm")
use_package("lme4")
use_package("emmeans", type = "Depends")
use_package("rstan")
use_package("MASS")

use_package("MEMSS", type = "Suggests")
use_package("afex", type = "Suggests")
use_package("glmmTMB", type = "Suggests")
use_package("bayesplot", type = "Suggests")
use_package("tidyverse", type = "Suggests")
use_test("stanova")
use_test("stanova_brm")

library(usethis)

# Create a new package -------------------------------------------------
create_package("stanova")
use_git()
use_testthat()
use_test("single_chain")
use_roxygen_md()
use_readme_rmd()
use_travis()
use_github_action_check_standard()



### still fails (due to emmeans):

# ## beta regression (fails somehow)
# data("GasolineYield", package = "betareg")
# str(GasolineYield)
#
# fit_beta <- stanova(yield ~ batch + temp, data = GasolineYield,
#                     model_fun = "betareg",
#                     link = "logit", link.phi = "log",
#                     chains = 2, iter = 500)
# summary(fit_beta)
