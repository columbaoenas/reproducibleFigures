## ---------------------------
##
## Script name: cleaningFunctions.r
##
## Purpose of script: 
##      Cleaning up the raw penguin data set by changing column names and removing columns. 
##
## Date Created: 2023-05-12
##
##
## ---------------------------
##
## Notes:
##    Functions in section 1 and 3 taken from cleaning.r file by Lydia France (2023):
##    https://github.com/LydiaFrance/PenguinProject/blob/main/functions/cleaning.r
##
## ---------------------------


# Section 1: basic cleaning functions

# A function to make sure the column names are cleaned up, 
# eg lower case and snake case
clean_column_names <- function(penguins_data) {
  penguins_data %>%
    clean_names()
}

# A function to make sure the species names are shortened
shorten_species <- function(penguins_data) {
  penguins_data %>%
    mutate(species = case_when(
      species == "Adelie Penguin (Pygoscelis adeliae)" ~ "Adelie",
      species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "Chinstrap",
      species == "Gentoo penguin (Pygoscelis papua)" ~ "Gentoo"
    ))
}

# A function to remove any empty columns or rows
remove_empty_columns_rows <- function(penguins_data) {
  penguins_data %>%
    remove_empty(c("rows", "cols"))
}


# A function to subset the data based on the list of column names
columns_needed <- c("species", "culmen_length_mm", "flipper_length_mm")
subset_columns <- function(penguins_data, column_names) {
  penguins_data %>%
    select(all_of(column_names))
}

# A function to subset the penguins data set based on species
filter_by_species <- function(penguins_data, selected_species) {
  penguins_data %>%
    filter(species == selected_species)
}


# A function to remove rows which contain NA values
remove_NA <- function(penguins_data) {
  penguins_data %>%
    na.omit()
}

# Section 2: new cleaning function, combining all cleaning functions to speed up the process

full_clean <- function(penguins_data) {
  penguins_data %>% 
    clean_column_names() %>% 
    shorten_species() %>% 
    subset_columns(columns_needed) %>% 
    remove_empty_columns_rows() %>% 
    remove_NA()
}


# Section 3: functions to subset data by species

# A function to subset the penguins data set based on species
filter_by_species <- function(penguins_data, selected_species) {
  penguins_data %>%
    filter(species == selected_species)
}
