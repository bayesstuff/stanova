library("devtools")
load_all()

document()

library("testthat")

### basic package things
usethis::use_build_ignore("development.R")
usethis::use_build_ignore("examples/")

use_package("rstanarm")
use_package("lme4")
use_package("emmeans")
use_package("rstan")
use_package("stats")

use_package("MEMSS", type = "Suggests")
use_package("afex", type = "Suggests")
use_package("glmmTMB", type = "Suggests")
use_package("bayesplot", type = "Suggests")
use_package("tidybayes", type = "Suggests")
use_test("stanova_samples")

library(usethis)

# Create a new package -------------------------------------------------
create_package("stanova")
use_git()
use_testthat()
use_test("single_chain")
use_roxygen_md()
use_readme_rmd()
use_travis()



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
