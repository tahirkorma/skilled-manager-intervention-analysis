library(here)
df <- read.csv(here("df.csv"))
print(colSums(is.na(df)))
str(df)

# Load necessary library
library(dplyr)

# Define the mapping dictionaries
mapping_dict <- c(
  "Agree" = 1,
  "Disagree" = 2,
  "Don't know" = 3,
  "Neither agree nor disagree" = 4,
  "Strongly Agree" = 5,
  "Strongly Disagree" = 6
)

manager_mapping_dict <- c(
  "Yes" = 1,
  "No" = 0
)

gender_mapping_dict <- c(
  "Male" = 1,
  "Female" = 0,
  "Non-binary" = 2,
  "Other" = 3
)

group_mapping_dict <- c(
  "Control" = 0,
  "Treatment" = 1
)

sector_mapping_dict <- c(
  "101Care" = 1,
  "102Housing association" = 2,
  "103Electrical installation/training" = 3,
  "104Residential care" = 4,
  "105Charity" = 5,
  "106Retail" = 6,
  "107Hospitality" = 7,
  "108Charity" = 8,
  "109Social Care" = 9,
  "110Construction" = 10,
  "111Hospitality" = 11,
  "112Healthcare" = 12,
  "113Insurance" = 13,
  "114Social Care" = 14,
  "115Manufacturing" = 15,
  "116Education" = 16,
  "117Charity" = 17,
  "118Social Care" = 18,
  "119Charity" = 19,
  "120Charity" = 20,
  "121Charity/Education/Care" = 21,
  "122Travel" = 22,
  "123Healthcare" = 23,
  "124Charity" = 24
)

ethnic_mapping_dict <- c(
  "British" = 1,
  "White and Asian" = 2,
  "I do not wish to disclose my ethnic origin" = 3,
  "Other" = 4,
  "Any other Ethnic Group" = 5,
  "African" = 6,
  "Indian" = 7,
  "Irish" = 8,
  "Pakistani" = 9,
  "White and Black Caribbean" = 10,
  "Any other mixed background" = 11,
  "Caribbean" = 12,
  "Bangladeshi" = 13,
  "Any other black background" = 14,
  "Any other Asian background" = 15,
  "White and Black African" = 16,
  "Chinese" = 17
)

# List of columns to convert
columns_to_convert <- paste0("q", 2:10, "_s")

# Function to apply a mapping dictionary
apply_mapping <- function(column, mapping_dict) {
  if (is.factor(column)) column <- as.character(column)
  mapped_values <- mapping_dict[match(column, names(mapping_dict))]
  mapped_values[is.na(mapped_values)] <- NA
  return(mapped_values)
}

# Convert the columns
for (column_name in columns_to_convert) {
  if (column_name %in% names(df)) {
    df[[column_name]] <- apply_mapping(df[[column_name]], mapping_dict)
  }
}

if ("manager" %in% names(df)) {
  df[["manager"]] <- apply_mapping(df[["manager"]], manager_mapping_dict)
}

if ("gender" %in% names(df)) {
  df[["gender"]] <- apply_mapping(df[["gender"]], gender_mapping_dict)
}

if ("group" %in% names(df)) {
  df[["group"]] <- apply_mapping(df[["group"]], group_mapping_dict)
}

if ("sector" %in% names(df)) {
  df[["sector"]] <- apply_mapping(df[["sector"]], sector_mapping_dict)
}

if ("ethnic" %in% names(df)) {
  df[["ethnic"]] <- apply_mapping(df[["ethnic"]], ethnic_mapping_dict)
}

# Print the modified dataframe and mapping dictionaries
print(df)
print(mapping_dict)
print(manager_mapping_dict)
print(gender_mapping_dict)
print(group_mapping_dict)
print(sector_mapping_dict)
print(ethnic_mapping_dict)

##log
min_los <- min(df$los, na.rm = TRUE)
print(min_los)
df$los <- log1p(df$los)
str(df)

#df$org_nor <- log1p(df$org_nor)
#df$num_managers <- log1p(df$num_managers)
str(df)


df$group<-as.factor(df$group)
df$wave<-as.factor(df$wave)
df$manager<-as.factor(df$manager)
df$stratum<-as.factor(df$stratum)


#male
male<-subset(df, gender == "1")
model_male <-lm(q9_s ~ group * wave +  q1_s + q2_s + q3_s + q4_s + q5_s + q6_s +
                q7_s + q8_s + q10_s + manager + ethnic + stratum + sector +
                female_mgr + progress + num_managers + org_nor + los, data = male)
options(scipen = 999)
summary(model_male)



female<-subset(df, gender == "0")
model_female <- lm(q9_s ~ group * wave + q1_s + q2_s + q3_s + q4_s + q5_s + q6_s +
                      q7_s + q8_s + q10_s + manager + ethnic + stratum + sector +
                      female_mgr + progress + num_managers + org_nor + los, data = female)
options(scipen = 999)
summary(model_female)


# Load necessary libraries
library(broom)
library(kableExtra)
library(dplyr)
library(officer)
library(flextable)

# Tidy up the model output for males
male_model_tidy <- tidy(model_male)

# Rename coefficients with more descriptive labels for males
male_model_tidy$term <- recode(male_model_tidy$term,
                               "(Intercept)" = "Intercept: Baseline value of q9_s when all predictors are set to their reference levels",
                               "group1" = "group1: Difference in q9_s for the treatment group compared to the control group",
                               "wave2" = "wave2: Difference in q9_s between Wave 2 and Wave 1",
                               "group:wave" = "group:wave: Combined effect on q9_s of being in the Treatment Group versus Control Group across different waves",
                               "q1_s" = "q1_s: Effect on q9_s",
                               "q2_s" = "q2_s: Effect on q9_s",
                               "q3_s" = "q3_s: Effect on q9_s",
                               "q4_s" = "q4_s: Effect on q9_s",
                               "q5_s" = "q5_s: Effect on q9_s",
                               "q6_s" = "q6_s: Effect on q9_s",
                               "q7_s" = "q7_s: Effect on q9_s",
                               "q8_s" = "q8_s: Effect on q9_s",
                               "q10_s" = "q10_s: Effect on q9_s",
                               "manager1" = "manager1: Difference in q9_s for managers who manage employees versus those who do not",
                               "gender" = "gender: Effect of gender on q9_s",
                               "ethnic" = "ethnic: Effect of ethnicity on q9_s",
                               "stratum2" = "stratum2: Difference in q9_s for being in Back Office compared to Front Office",
                               "sector" = "sector: Effect of industry sector on q9_s",
                               "female_mgr" = "female_mgr: Effect of proportion of female managers on q9_s",
                               "progress" = "progress: Effect of average progress made by managers in completing the online training on q9_s",
                               "num_managers" = "num_managers: Effect of number of managers in the organisation on q9_s",
                               "org_nor" = "org_nor: Effect of number of employees in the organisation on q9_s",
                               "los" = "los: Effect of length of service (years) on q9_s",
                               "group1:wave2" = "group1:wave2: Combined effect on q9_s for the Treatment Group in Wave 2 compared to the Control Group in Wave 1",
                               "group1:stratum2" = "group1:stratum2: Combined effect on q9_s for the Treatment Group in the Back Office compared to the Control Group in the Front Office",
                               "wave2:stratum2" = "wave2:stratum2: Combined effect on q9_s for Wave 2 in the Back Office compared to Wave 1 in the Front Office",
                               "group1:wave2:stratum2" = "group1:wave2:stratum2: Combined effect on q9_s for the Treatment Group in Wave 2 within the Back Office, compared to the Control Group in Wave 1 within the Front Office")

male_model_tidy <- male_model_tidy %>%
  rename(Coefficients = term,
         Estimate = estimate,
         `Std. Error` = std.error,
         `t value` = statistic,
         `p-value` = p.value)


# Create a table for the male model with kableExtra
kable(male_model_tidy, digits = 3, caption = "Estimation Table for the Impact of Treatment, Time, on `q9_s` in Males`",
      col.names = c("Term", "Estimate", "Std. Error", "t value", "p-value")) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed", "responsive"))

# Tidy up the model output for females
female_model_tidy <- tidy(model_female)

# Rename coefficients with more descriptive labels for females
female_model_tidy$term <- recode(female_model_tidy$term,
                                 "(Intercept)" = "Intercept: Baseline value of q9_s when all predictors are set to their reference levels",
                                 "group1" = "group1: Difference in q9_s for the treatment group compared to the control group",
                                 "wave2" = "wave2: Difference in q9_s between Wave 2 and Wave 1",
                                 "group:wave" = "group:wave: Combined effect on q9_s of being in the Treatment Group versus Control Group across different waves",
                                 "q1_s" = "q1_s: Effect on q9_s",
                                 "q2_s" = "q2_s: Effect on q9_s",
                                 "q3_s" = "q3_s: Effect on q9_s",
                                 "q4_s" = "q4_s: Effect on q9_s",
                                 "q5_s" = "q5_s: Effect on q9_s",
                                 "q6_s" = "q6_s: Effect on q9_s",
                                 "q7_s" = "q7_s: Effect on q9_s",
                                 "q8_s" = "q8_s: Effect on q9_s",
                                 "q10_s" = "q10_s: Effect on q9_s",
                                 "manager1" = "manager1: Difference in q9_s for managers who manage employees versus those who do not",
                                 "gender" = "gender: Effect of gender on q9_s",
                                 "ethnic" = "ethnic: Effect of ethnicity on q9_s",
                                 "stratum2" = "stratum2: Difference in q9_s for being in Back Office compared to Front Office",
                                 "sector" = "sector: Effect of industry sector on q9_s",
                                 "female_mgr" = "female_mgr: Effect of proportion of female managers on q9_s",
                                 "progress" = "progress: Effect of average progress made by managers in completing the online training on q9_s",
                                 "num_managers" = "num_managers: Effect of number of managers in the organisation on q9_s",
                                 "org_nor" = "org_nor: Effect of number of employees in the organisation on q9_s",
                                 "los" = "los: Effect of length of service (years) on q9_s",
                                 "group1:wave2" = "group1:wave2: Combined effect on q9_s for the Treatment Group in Wave 2 compared to the Control Group in Wave 1",
                                 "group1:stratum2" = "group1:stratum2: Combined effect on q9_s for the Treatment Group in the Back Office compared to the Control Group in the Front Office",
                                 "wave2:stratum2" = "wave2:stratum2: Combined effect on q9_s for Wave 2 in the Back Office compared to Wave 1 in the Front Office",
                                 "group1:wave2:stratum2" = "group1:wave2:stratum2: Combined effect on q9_s for the Treatment Group in Wave 2 within the Back Office, compared to the Control Group in Wave 1 within the Front Office")

# Rename columns in female_model_tidy
female_model_tidy <- female_model_tidy %>%
  rename(Coefficients = term,
         Estimate = estimate,
         `Std. Error` = std.error,
         `t value` = statistic,
         `p-value` = p.value)


# Create a table for the female model with kableExtra
kable(female_model_tidy, digits = 3, caption = "Estimation Table for the Impact of Treatment, Time, on `q9_s` in Females`",
      col.names = c("Term", "Estimate", "Std. Error", "t value", "p-value")) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed", "responsive"))

# Create and save Word document with separate tables
doc <- read_docx()

# Create and add the flextable for the male model
male_ft <- qflextable(male_model_tidy %>%
                        mutate(across(c(Estimate, `Std. Error`, `t value`, `p-value`), ~ round(.x, 3))))
male_ft <- set_caption(male_ft, caption = "Estimation Table for the Impact of Treatment, Time on `q9_s in Males`")
doc <- body_add_flextable(doc, male_ft)

# Add a page break
doc <- body_add_par(doc, value = "", style = "Normal")

# Create and add the flextable for the female model
female_ft <- qflextable(female_model_tidy %>%
                          mutate(across(c(Estimate, `Std. Error`, `t value`, `p-value`), ~ round(.x, 3))))
female_ft <- set_caption(female_ft, caption = "Estimation Table for the Impact of Treatment, Time, on `q9_s` in Females")
doc <- body_add_flextable(doc, female_ft)

# Save the Word document
#print(doc, target = "heterogenity_effect.docx")
