

## FUNCTION: convert girth from inches to centimeters
convert_in_to_cm <- function(x) {
    x * 2.54
}


## FUNCTION: convert height from feet to meters
convert_ft_to_m <- function(x) {
    x * 0.3048
}


## FUNCTION: calculate volume in cubic meters
calculate_volume <- function(diameter, height) {
    pi / 4 * (diameter / 100)^2 * height
}


## FUNCTION: summarize iris dataset
calc_iris_mean <- function(data) {
    data |> 
        summarize(
            across(
                where(is.numeric), mean
            ),
            .by = Species
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


## FUNCTION: summarize mean of number variables
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

