#
# ------------ Mastering R: Best Practices and Essential Tools ----------- #
#
# This script:
# - Shows that you can create the functions as complex as you want
# - Shows a small note in tidy eval
# ------------------------------------------------------------------------ #

# 1. Load packages --------------------------------------------------------

## Packages
library(tidyverse)

## Functions
source("R/utils.R")

# 2. Load data ------------------------------------------------------------

## Load data
iris_tbl <- as_tibble(iris) 

# 3. Analyze data ---------------------------------------------------------

## Calculate mean of numeric variables in Iris dataset, and sort them
iris_tbl |> 
    calc_iris_mean()


# 4. General function -----------------------------------------------------

## https://ggplot2.tidyverse.org/reference/tidyeval.html

## Apply to other datasets

## FUNCTION

calc_numeric_mean <- function(data, group) {
    data |> 
        summarize(
            across(
                where(is.numeric), \(x) mean(x, na.rm = TRUE) #anonymous function
            ),
            .by = {{ group }}
        ) |> 
        pivot_longer(
            cols      = where(is.numeric),
            names_to  = "measure",
            values_to = "mean"
        ) |> 
        group_by(measure) |> 
        arrange(
            desc(mean),  
            .by_group = TRUE        
        ) |> 
        ungroup()   
}


airquality |> 
    calc_numeric_mean(
        group = Month
    ) 

