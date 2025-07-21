# function 1
get_region_data <- function(region_codes, file_path = "D:/Sose 25 Software R 2/Exercise 6/regions_cleaned.csv") {
  library(tidyverse)
  
  data <- read_csv(file_path, show_col_types = FALSE) %>%
    mutate(Region = str_trim(toupper(Region)))
  
  region_codes <- str_trim(toupper(region_codes))
  
  result <- data %>% 
    filter(Region %in% region_codes)
  
  
  not_found <- setdiff(region_codes, result$Region)
  if (length(not_found) > 0) {
    warning("Region not found:", paste(not_found, collapse = ", "))
  }
  return(result)
}

get_region_details(c("T03", "T04"))



# function 2
library(readxl)
library(dplyr)
library(ggplot2)

raw <- read_excel("D:/Sose 25 Software R 2/Exercise 6/179_2025_57_g_tourismus_05_2025_eckdaten_tourismusregionen.xlsx",
                  sheet = "Tab4-S7", skip = 17, col_names = FALSE)


raw_renamed <- raw %>% rename(
  Region = ...2,
  Region_Name = ...3,
  Ankunft = ...6,
  Ausland = ...8,
  Uebernachtung = ...32,
  Auslastung = ...34
)

cleaned <- raw_renamed %>%
  filter(!is.na(Region_Name)) %>%
  mutate(
    Ankunft = as.numeric(gsub(",", "", Ankunft)),
    Ausland = as.numeric(gsub(",", "", Ausland)),
    Uebernachtung = as.numeric(gsub(",", "", Uebernachtung)),
    Auslastung = as.numeric(gsub(",", "", Auslastung))
  )


plot_indicator <- function(data, indicator) {
  ggplot(data, aes(x = reorder(Region_Name, .data[[indicator]]), y = .data[[indicator]])) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(
      title = paste("Tourism indicator:", indicator),
      x = "Region",
      y = indicator
    ) +
    theme_minimal()
}


plot_indicator(cleaned, "Ausland")

