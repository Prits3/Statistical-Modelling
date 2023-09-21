# Installing the package 
install.packages("effsize")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("maps")
install.packages("ResourceSelection")
install.packages("pROC")
install.packages("reshape2")
install.packages("heatmaply")
install.packages("Metrics")

# Loading the libraries
library(ggplot2)
library(gridExtra)
library(knitr)
library(effsize)
library(tidyverse)
library(dplyr)
library(maps)
library(ResourceSelection)
library(pROC)
library(corrplot)
library(reshape2)
library(heatmaply)
library(Metrics)

#Loading the dataset
df <- read.csv("/Users/pritika_timsina/Desktop/startup.csv", header = TRUE)
head(df)

#Counting the number of missing values in the dataset
print(sapply(df, function(x) sum(is.na(x))))

# Changing column names for ease amd Dropping the column X
newcols <- c("X", "Company", "Valuation_in_Billion", "Date_Joined", 
             "Country", "City", "Industry", "Investors")
colnames(df) <- newcols
df <- df[, colnames(df) != "X"]
df <- df[, !colnames(df) %in% "Date_Joined"]
# Removing the $ symbol from Valuation_in_Billion to make it numerical dataset
df$Valuation_in_Billion <- as.numeric(gsub("\\$", "", df$Valuation_in_Billion))
# Checkin the data type of each column
print(sapply(df, class))

# Checking for duplicates
df <- unique(df)

# Checking the summary of the data
summary(df)

# Data Visualisation 

# For Valuation
ggplot(df, aes(x = Valuation_in_Billion)) +
  geom_histogram(binwidth = 10, fill = "gray", color = "black") +
  labs(title = "Distribution of Valuation", x = "Valuation in Billions",
       y = "Frequency")

# Industry Distribution 
ggplot(df, aes(x = Industry)) +
  geom_bar(fill = "lightblue") +
  labs(title = "Industry Distribution", x = "Industry", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Mean Valuation in Countries 
world_map <- map_data("world")
merged_data <- merge(world_map, country_valuation, by.x = "region",
                     by.y = "Country", all.x = TRUE)
world_map <- ggplot(merged_data, aes(x = long, y = lat, group = group, 
                                          fill = Mean_Valuation)) +  
  labs(title = "Mean Valuation by Country on World Map") +
  theme_void()
print(world_map)

# Bar Plot of suppliers in each industry
industry_supplier_counts <- df %>%
  group_by(Industry) %>%
  summarize(Num_Suppliers = n_distinct(Investors))
ggplot(industry_supplier_counts, aes(x = reorder(Industry, -Num_Suppliers), 
                                     y = Num_Suppliers)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Number of Suppliers by Industry", x = "Industry", 
       y = "Number of Suppliers") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Logistic Regression
df$Valuation_binary <- ifelse(df$Valuation_in_Billion > 
                                median(df$Valuation_in_Billion), 1, 0)
df <- df %>%
  mutate(Country = as.factor(Country),
         City = as.factor(City),
         Industry = as.factor(Industry),
         Investors = as.factor(Investors))

# Fitting the logistic regression model
model <- glm(Valuation_binary ~ Country + City + Industry + Investors, 
             data = df, family = "binomial", maxit = 1000)

# Akaike information criterion
AIC(model)

# ROC and AUC
predicted_probs <- predict(model, type = "response")
roc <- roc(df$Valuation_binary, predicted_probs)
plot(roc, print.auc = TRUE, main = "ROC")
auc(roc)

# Linear Regression
model <- lm(Valuation_in_Billion ~ Country + City + Industry, data = df)

# Extracting Coefficients
coefficients <- summary(model)$coefficients

# R-squared
r_squared <- summary(model)$r.squared
cat("R-squared:", r_squared, "\n")

# p-value
p_values <- summary(model)$coefficients[, 4]
sorted_p_values <- sort(p_values)
cat("Top 5 p-Values:\n")
print(sorted_p_values[1:5])

# Confidence Intervals
confidence_intervals <- confint(model)[1:5,]
cat("Confidence Intervals:\3")
print(confidence_intervals)

#Regression plots 
ggplot(df, aes(x = Industry, y = Valuation_in_Billion)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  labs(title = "Regression Plot for Valuation vs. Industry", 
       x = "Industry", y = "Valuation in Billions")

ggplot(df, aes(x = Country, y = Valuation_in_Billion)) +
  geom_point() +  # Scatter plot of data points
  geom_smooth(method = "lm", se = FALSE) +  # Add the regression line
  labs(title = "Regression Plot for Valuation vs. Country", 
       x = "Country", y = "Valuation in Billions")

# Residual Value
residuals <- residuals(model)
ggplot(data.frame(Predicted = predict(model), Residuals = residuals), 
       aes(x = Predicted, y = Residuals)) +
  geom_point() + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +  
  labs(title = "Residual Plot", x = "Predicted Values", y = "Residuals") +
  theme_minimal()

# Fisher's Exact Test for feature selection
categorical_features <- df[, c("Country", "City", "Industry", "Investors")]
fisher_test_results <- list()
for (feature in colnames(categorical_features)) {
  cross_table <- table(categorical_features[[feature]], df$Valuation_binary)
  fisher_test_result <- fisher.test(cross_table, simulate.p.value = TRUE)
  fisher_test_results[[feature]] <- fisher_test_result
}

sorted_feature_indices <- order(sapply(fisher_test_results, function(x) x$p.value))
sorted_features <- names(fisher_test_results)[sorted_feature_indices]

N <- 5  
top_n_features <- sorted_features[1:N]
cat("Top", N, "features based on p-value:\n")
cat(top_n_features, sep = "\n")

# Hypotheses Analysis

# Performing Anova for company
anova_company <- aov(Valuation_in_Billion ~ Company, data = df)
summary(anova_company)

# Performing Anova for country
anova_country <- aov(Valuation_in_Billion ~ Country, data = df)
summary(anova_country)

# Performing Anova for city
anova_city <- aov(Valuation_in_Billion ~ City, data = df)
summary(anova_city)

#Performing Anova for industry
anova_industry <- aov(Valuation_in_Billion ~ Industry, data = df)
summary(anova_industry)

#Performing Anova for investors
anova_investor <- aov(Valuation_in_Billion ~ Investors, data = df)
summary(anova_investor)

# Goodness of Fit
goodness_of_fit_anova <- anova(anova_country, test = "F")
summary(goodness_of_fit_anova)

goodness_of_fit_anova <- anova(anova_city, test = "F")
summary(goodness_of_fit_anova)

goodness_of_fit_anova <- anova(anova_industry, test = "F")
summary(goodness_of_fit_anova)

goodness_of_fit_anova <- anova(anova_investor, test = "F")
summary(goodness_of_fit_anova)

#Levene's test
leveneTest(Valuation_in_Billion ~ Company, data = df)

# Anova model
anova_model <- aov(Valuation_in_Billion ~ Company, data = df)
residuals <- residuals(anova_model)
hist(residuals, main = "Histogram of Residuals")
qqnorm(residuals)


