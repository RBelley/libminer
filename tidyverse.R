library(dplyr)

var_summary <- function(data,var) {
  data |>
    summarise(
      min_var = min(var),
      max_var = max(var)
    )
}

mtcars |>
  group_by(cyl) |>
  var_summarympg

var_summary <- function(data,var) {
  data |>
    summarise(
      min_var = min({{ var }}),
      max_var = max({{ var }})
    )
}

mtcars |>
  group_by(cyl) |>
  var_summary(mpg, disp)



var_summary <- function(data, var1, var2) {
  data |>
    summarise(
      min_var = min(.data[[var1]]),
      max_var = max(.data[[var2]])
    )
}

mtcars |>
  group_by(cyl) |>
  var_summary("mpg", "disp")

big_cars_summary <- function(var) {
  mtcars |>
    filter(.data$cyl >= 6) |>
    group_by(.data$cyl) |>
    summarise(
      n = n(),
      mean = mean({{ var }})
    )
}

big_cars_summary(mpg)

# Exercise
# Write a function that summarizes starwars by any grouping variable

head(starwars)

# my try but not a function
starwars |>
  group_by(height) |>
  var_summary("mass", "hair_color")

# Nick's example
starwars_mass_summary <- function(group_var) {
  starwars |>
    group_by({{group_var}}) |>
    summarize(
      n = n(),
      mean_mass = mean(.data$mass, na.rm = TRUE),
      sd_mass = sd(.data$mass, na.rm = TRUE)
    )
}

starwars_mass_summary(species)

# Andy's notes
height_sum <- function(data, group_var) {
  data |>
    dplyr::group_by({{ group_var }}) |>
    dplyr::summarise(
      n = dplyr::n(),
      mean_height = mean(.data$height)
    )
}
height_sum(starwars, hair_color)

# Exercise 2
# Modify that function to take > 1 grouping variables

height_sum <- function(data, group_var) {
  data |>
    dplyr::group_by({{ group_var }}) |>
    dplyr::summarise(
      n = dplyr::n(),
      mean_height = mean(.data$height)
    )
}

height_sum(starwars, hair_color)

# solution:

height_sum <- function(data, ...) { # the ... solve the question
  data |>
    dplyr::group_by(...) |> # the ... solve the question but have to remove {{}}
    dplyr::summarise(
      n = dplyr::n(),
      mean_height = mean(.data$height)
    )
}

height_sum(starwars, hair_color, homeworld)


# Dynamic column naming with :=
var_summary <- function(data, var, var_name) {
  data |>
    summarise(
      "{var_name}" := min({{var}})
    )
}

mtcars |>
  group_by(cyl) |>
  var_summary(mpg, "min_mpg")

# Use {{}} syntax to dynamically add a new column name
var_summary <- function(data, var) {
  data |>
    summarise(
      "{{var}}_min" := min({{var}})
    )
}

mtcars |>
  group_by(cyl) |>
  var_summary(mpg)

# Write a function that summarizes starwars, and dynamically creates new column names

dynamic_sum <- function(data, group_var, sum_var) {
  data |>
    dplyr::group_by({{ group_var }}) |>
    dplyr::summarise(
      n = dplyr::n(),
      "mean_{{sum_var}}" := mean({{ sum_var }})
    )
}
dynamic_sum(starwars, hair_color, mass)

# across() inside data-masking vers

summy <- function(df, group_var, cols) {
  df |>
    group_by({{ group_var }}) |>
    summarise(
      across({{ cols }}, .fns = list(min = min, max = max))
    )
}

mtcars |>
  summy(cyl, c(mpg,disp))

mtcars |>
  summy(cyl, starts_with("mp"))

mtcars |>
  summy(cyl, where(is.numeric))
