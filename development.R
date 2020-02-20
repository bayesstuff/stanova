library("devtools")
load_all()

document()

### basic package things
usethis::use_build_ignore("development.R")
usethis::use_build_ignore("examples/")

use_package("rstanarm")
use_package("lme4")
use_package("emmeans")
use_package("rstan")

library(usethis)

# Create a new package -------------------------------------------------
create_package("stanova")
use_git()

use_roxygen_md()
