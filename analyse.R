library(readr)
library(tidyverse)

# Reading the data to R -----------------------------------------------------------

df_wbl <- read_rds("parental_leave.rds") 


# Descriptive analysis -------------------------------------------------------------------

# Are there missing values in the data?

sum(is.na(df_wbl))


# What is the average length of paternity and maternity leave in a specific year?
df_wbl |>
    filter(year == 2000) |>
    summarise(
    "Average length of paternity leave" = mean(paternityleave_length),
    "Average length of maternity leave" = mean(maternityleave_length)
    )


# How many countries have paternity and maternity leave in a specific year?

df_wbl |>
    filter(year == 2024) |>
    summarise(
    "N of countries with paternity leave" = sum(paternityleave_log == 1),
    "N of countries with maternity leave" = sum(maternityleave_log == 1)
    )

# 35 and 77 in year 2000, 22 and 61 in year 1985, 123 and 123 in year 2024

# Income group differences ---------------------------------------------------------------

# Are there differences between income groups in the average 
# length of paternity and maternity leave in the years after 2015?

summary_leave_length <- df_wbl |>
    group_by(income_group) |>
    filter(year == 2024) |>
    summarise(
        "mean_maternityleave" = mean(maternityleave_length),
        "mean_paternityleave" = mean(paternityleave_length),
        "mean_parentalleave" = mean(shared_length)
    ) |>
    arrange(desc(mean_maternityleave))

View(summary_leave_length)

# saving the data for report

write_csv(summary_leave_length, "data/summary_leave_length.csv")

# Average length of parental leaves by year --------------------------------------------

summary_leave_length_yearly <- df_parental_leave_clean |>
    group_by(year) |>
    summarise(
        "mean_maternityleave" = mean(maternityleave_length),
        "mean_paternityleave" = mean(paternityleave_length),
        "mean_parentalleave" = mean(shared_length)
    )


View(summary_leave_length_yearly)

# saving the data for report

write_csv(summary_leave_length_yearly, "data/summary_leave_length_yearly.csv")

# which countries have the longest leaves in 2024? -------------------------------------

maternity_2024 <- df_parental_leave_clean |>
    group_by(country) |>
    filter(year == 2024) |>
    arrange(desc(maternityleave_length))

maternity_2024 |>
    head()

paternity_2024 <- df_parental_leave_clean |>
    group_by(country) |>
    filter(year == 2024) |>
    arrange(desc(paternityleave_length))
    
paternity_2024 |>    
    head(5)

shared_2024 <- df_parental_leave_clean |>
    group_by(country) |>
    filter(year == 2024) |>
    arrange(desc(shared_length))

shared_2024 |>
    head(5)




