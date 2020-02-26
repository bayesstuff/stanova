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

library(usethis)

# Create a new package -------------------------------------------------
create_package("stanova")
use_git()
use_testthat()
use_test("single_chain")
use_roxygen_md()
use_readme_rmd()
