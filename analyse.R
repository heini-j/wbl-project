library(readr)
library(tidyverse)

df_parental_leave <- read_rds("parental_leave.rds") 


# Renaming columns ----------------------------------------------------------------

# Although clear, some columns have names that are too long for practical use, so they are renamed
df_parental_leave_clean <- df_parental_leave |>
    rename(
        country = "Economy",
        country_code = "Economy Code",
        ISO_code = "ISO Code",
        region = "Region",
        income_group = "Income Group",
        year = "Report Year",
        parenthood_score = "PARENTHOOD SCORE",
        maternityleave_minimum = "Is paid leave of at least 14 weeks available to mothers?",
        maternityleave_log = "Score...9",
        maternity_law = "Legal Basis...10",
        maternityleave_length = "Length of paid maternity leave",
        maternityleave_benefits = "Does the government administer 100 percent of maternity leave benefits?",
        maternityleave_benefits_log = "Score...13",
        maternitityleave_benefits_law = "Legal Basis...14",
        paternity_leave = "Is there paid leave available to fathers?",
        paternityleave_log = "Score...16",
        paternity_law = "Legal Basis...17",
        paternityleave_length = "Length of paid paternity leave",
        parentalleave = "Is there paid parental leave?",
        parentalleave_log = "Score...20",
        parentalleave_law = "Legal Basis...21",
        shared_length = "Shared days",
        shared_length_mother = "Days for the mother",
        shared_length_father = "Days for the father",
        dismissal_prohibited = "Is dismissal of pregnant workers prohibited?",
        dismissal_prohibited_log = "Score...26",
        dismissal_prohibited_law = "Legal Basis...27"
    )

View(df_parental_leave_clean)

# Improving readability -----------------------------------------------------------

# changing "false" and "true" to 0 and 1 for easier use in further analysis
df_parental_leave_clean <-
    df_parental_leave_clean |>
    mutate(
        maternityleave_log = as.numeric(maternityleave_log),
        maternityleave_benefits_log = as.numeric(maternityleave_benefits_log),
        paternityleave_log = as.numeric(paternityleave_log),
        parentalleave_log = as.numeric(parentalleave_log),
        dismissal_prohibited_log = as.numeric(dismissal_prohibited_log)
    )

max(df_parental_leave_clean$year)

# Filtering data -------------------------------------------------------------------

# What is the average length of paternity and maternity leave in each year?
df_parental_leave_clean |>
    filter(year == 2000) |>
    summarise(
    "mean_paternityleave" = mean(paternityleave_length),
    "mean_maternityleave" = mean(maternityleave_length)
    )

# Analysing descriptively -----------------------------------------------------------

# Are there missing values in the data?

sum(is.na(df_parental_leave_clean))

# How many countries have paternity and maternity leave in each year?

df_parental_leave_clean |>
    filter(year == 2024) |>
    summarise(
    "countries_with_paternityleave" = sum(paternityleave_log == 1),
    "countries_with_maternityleave" = sum(maternityleave_log == 1)
    )

# 35 and 77 in year 2000, 22 and 61 in year 1985, 123 and 123 in year 2024

# Are there differences between income groups in the average 
# length of paternity and maternity leave in the years after 2015?

summary_leave_length <- df_parental_leave_clean |>
    group_by(income_group) |>
    filter(year == 2024) |>
    summarise(
        "mean_maternityleave" = mean(maternityleave_length),
        "mean_paternityleave" = mean(paternityleave_length),
        "mean_parentalleave" = mean(shared_length)
    ) |>
    arrange(desc(mean_maternityleave))

View(summary_leave_length)

# average length of parental leaves by year

summary_leave_length_yearly <- df_parental_leave_clean |>
    group_by(year) |>
    summarise(
        "mean_maternityleave" = mean(maternityleave_length),
        "mean_paternityleave" = mean(paternityleave_length),
        "mean_parentalleave" = mean(shared_length)
    )


View(summary_leave_length_yearly)

# which countries have the longest leaves in 2024?

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


# How many countries had paternity leave each year?

paternity_yearly <- df_parental_leave_clean |>
    group_by(year) |>
    summarise(
        "Countries with paternity leave" = sum(paternityleave_log == 1)
    )

View(paternity_yearly)

