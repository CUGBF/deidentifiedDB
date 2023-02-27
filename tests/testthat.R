library(testthat)
githubinstall(dplyr)
githubinstall(tidyselect)
library(dplyr)
library(tidyselect)
library(deidentifiedDB)

test_check("deidentifiedDB")
