library(dplyr)
library(readr)
library(leaflet)
library(DT)
library(htmltools)

# read in dataset
db <- read_csv("./data/calendar1920_final.csv")

# set user-friendly column names for display version of dataset
clean_db <- db %>% select(
            Name = full_name,
            `Undergraduate Degree` = ug_degree,
            `Undergraduate Institution` = ug_institution,
            `Undergraduate Year` = ug_grad_year,
            `Master's Degree` = m_degree,
            `Masters Institution` = m_institution,
            `Masters Year` = m_grad_year,
            `Doctoral Degree` = d_degree,
            `Doctoral Institution` = d_institution,
            `Doctoral Year` = d_grad_year,
            `Current Occupation` = current_position,
            `Current Occupation Year` = pos_year,
            Location = dataTXT,
            Country = country,
            `Scientific Discipline(s)` = science,
            `Major or Career?` = major_career) %>% 
      arrange(Name)
