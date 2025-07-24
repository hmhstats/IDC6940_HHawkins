library(dplyr)

library(readr)
Data_For_Hailee <- read_csv("Data For Hailee.csv")
View(Data_For_Hailee)

#pull STS and attrition columns into new dataset
new_df <- Data_For_Hailee %>% select(Q3_1, Q3_2, Q3_3, Q3_4, Q3_5, Q3_6, Q3_7, Q3_8,
                                     Q3_9, Q3_10, Q3_11, Q3_12, Q3_13, Q3_14, Q3_15, 
                                     Q3_16, Q3_17, Q5,
                                     Q4_2, Q4_5, Q4_7, Q4_9, Q4_11, Q4_13, Q4_14, Q4_23, Q4_25, Q4_28
                                     )
View(new_df)

# Remove the first two rows
df_cleaned <- new_df[-c(1, 2), ]

# View the cleaned data
head(df_cleaned)

# Find columns named Q3_1 to Q3_17
likert_cols <- paste0("Q3_", 1:17)

# Apply regex to extract numeric value from strings like "Never (1)"
df_cleaned[likert_cols] <- lapply(df_cleaned[likert_cols], function(x) {
  as.numeric(sub(".*\\((\\d+)\\)", "\\1", x))
})

# Get the column indices for the last 10 columns
last_10_cols <- tail(names(df_cleaned), 10)

# Apply regex extraction and convert to numeric
df_cleaned[last_10_cols] <- lapply(df_cleaned[last_10_cols], function(x) {
  as.numeric(sub(".*\\((\\d+)\\)", "\\1", x))
})



# Recode Q5 to 1 if any of 2, 3, or 4 are mentioned, otherwise 0
df_cleaned$Q5_binary <- ifelse(
  grepl("Retire|Leave the teaching profession|Become an administrator", df_cleaned$Q5),
  1,
  0
)

View(df_cleaned)

#changing Other (explain) responses in Q5 to 0 or 1
df_cleaned$Q5_binary[5] <- 0
df_cleaned$Q5_binary[15] <- 0
df_cleaned$Q5_binary[21] <- 0
df_cleaned$Q5_binary[73] <- 0
df_cleaned$Q5_binary[120] <- 1

#count the number of nas in each column
colSums(is.na(df_cleaned))

# Remove rows 15, 21, 73
df_cleaned <- df_cleaned[-c(15, 21, 73), ]

View(df_cleaned)

#calculating scores for intrusion, arousal and avoidance
df_cleaned$intrusion_score <- rowSums(df_cleaned[, c("Q3_2", "Q3_3", "Q3_6", "Q3_10", "Q3_13")], na.rm = TRUE)
df_cleaned$avoidance_score <- rowSums(df_cleaned[, c("Q3_1", "Q3_5", "Q3_7", "Q3_9", "Q3_12", "Q3_14", "Q3_17")], na.rm = TRUE)
df_cleaned$arousal_score <- rowSums(df_cleaned[, c("Q3_4", "Q3_8", "Q3_11", "Q3_15", "Q3_16")], na.rm = TRUE)

#calculate total score
likert_cols <- paste0("Q3_", 1:17)
df_cleaned$stss_total_score <- rowSums(df_cleaned[, likert_cols], na.rm = TRUE)
df_cleaned$proqol_sts_score <- rowSums(df_cleaned[, last_10_cols], na.rm = TRUE)


View(df_cleaned)

#change Q5_binary name
sts_data <- df_cleaned %>%
  rename(leave_teaching = Q5_binary)
View(sts_data)

# Remove rows where all of the first 18 columns are NA
sts_data <- sts_data[!apply(is.na(sts_data[, 1:18]), 1, all), ]
View(sts_data)

write.csv(sts_data, "sts_data.csv", row.names = FALSE)
