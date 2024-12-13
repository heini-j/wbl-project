library(readxl)
library(readr)


# Importing from excel file -----------------------------------------------

# Importing the sheet we want and specifying the column types

df_parental_leave <- read_excel("WBL2024-1-0-Historical-Panel-Data.xlsx", 
                                sheet = "Parenthood",
                                col_types = c("text", "text", "text", "text", "text", 
                                              "numeric", "numeric", "text", "logical", "text",
                                              "numeric", "text", "logical", "text", "text",
                                              "logical", "text", "numeric", "text", "logical",
                                              "text", "numeric", "numeric", "numeric","text",
                                              "logical", "text"))

# checking the names of the columns
names(df_parental_leave)

# checking the first rows of the data

df_parental_leave |>
  head()

# checking the dataframe

df_parental_leave |>
    View()

# Saving the dataframe ---------------------------------------------------

saveRDS(df_parental_leave, "parental_leave.rds")


