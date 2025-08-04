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
df$stratum<-as.factor(df$stratum)
df$q9_s<-as.numeric(df$q9_s)


##total effect
a_model  <- lm(q9_s ~ group * wave,  data = df)
summary(a_model)

#direct effect
a_1model  <- lm(q9_s ~ group * wave + q7_s * wave, data = df)
summary(a_1model)

#indirect effect
b_model  <- lm(q7_s ~ group * wave, data = df)
summary(b_model )

#mediator effect
c_model  <- lm(q9_s ~ q7_s * wave, data = df)
summary(c_model )


# Extract coefficients
total_effect_coef <- coef(a_model)['group1:wave2']
direct_effect_coef <- coef(a_1model)['group1:wave2']
indirect_effect_coef_mediator <- coef(b_model)['group1:wave2']
indirect_effect_coef_outcome <- coef(c_model)['q7_s']


# Calculate indirect effect as the sum of effects across levels
indirect_effect <- indirect_effect_coef_outcome*indirect_effect_coef_mediator # Adjust based on model specifics
indirect_effect



# Direct effect (from model with mediator included)
direct_effect <- direct_effect_coef

# Total effect (from model without mediator)
total_effect <- total_effect_coef


# Display results
cat("Total Effect:", total_effect, "\n")
cat("Direct Effect:", direct_effect, "\n")
cat("Indirect Effect:", indirect_effect, "\n")


library(mediation)

# Assuming your data is in a dataframe called df
# Fit the mediator model
mediator_model <- lm(q7_s ~ group * wave, data = df)

# Fit the outcome model
outcome_model <- lm(q9_s ~ group * wave + q7_s * wave, data = df)

# Perform mediation analysis using bootstrap
med_analysis <- mediate(mediator_model, outcome_model, 
                        treat = "group", mediator = "q7_s",
                        boot = TRUE, sims = 1000)

# Summarize the mediation analysis results
summary(med_analysis)


# Load necessary libraries
library(flextable)
library(officer)

# Create a data frame for the mediation analysis results
mediation_results <- data.frame(
  Term = c("ACME", "ADE", "Total Effect", "Prop. Mediated"),
  Estimate = c(0.0208, -0.0652, -0.0444, -0.4682),
  `95% CI Lower` = c(-0.0019, -0.1895, -0.1696, -3.9864),
  `95% CI Upper` = c(0.04, 0.06, 0.09, 3.94),
  `p-value` = c(0.074, 0.352, 0.522, 0.564)
)

# Round numeric values for better readability
mediation_results <- mediation_results %>%
  mutate(across(where(is.numeric), round, 4))

# Create a flextable object
ft <- flextable(mediation_results)

# Add a caption to the flextable
ft <- set_caption(ft, caption = "Mediation Analysis Results for `q7_s`")

# Format the table for better presentation
ft <- theme_vanilla(ft)
ft <- autofit(ft)

# Create a new Word document
doc <- read_docx()

# Add the flextable to the document
doc <- body_add_flextable(doc, ft)

# Save the Word document
#print(doc, target = "mediation_analysis_q7s.docx")


