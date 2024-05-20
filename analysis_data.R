

###########Thesis#############

## Specify the path to your TXT file
file_path <- "C:/Users/V110/OneDrive - UvA/Desktop/GIORGIA/UNI/Year 3/thesis/analysis/R analysys/real ANALYSIS/NEMOFinal.txt"

# Read the TXT file into a data frame
data <- read.table(file_path, header = TRUE, sep = "\t")
# 'header = TRUE' assumes the first row contains column names
# 'sep = "\t"' specifies the tab as the delimiter, you can adjust it based on your fileon your file

# Calculate variable S
data$s <- (data$secondEstimate - data$firstEstimate) / (data$socialInfo - data$firstEstimate)

# Filter out rows where s < 10
filtered_data <- subset(data, s >= 10)

# Filter 's' between 0 and 1, and set others as NA
data$s[data$s < 0 | data$s > 1] <- NA

## this did not work for me
newData <- matrix(nrow = 0, ncol = ncol(data))

summMat <- matrix(nrow = 0, ncol = 10)
summMat <- data.frame(summMat)


## loop over all families (i.e. groups) to create a 'summary matrix'
## with 1 family on each row
for (fam in unique(data$groupNr)) {
  b <- subset(data, groupNr == fam)
  
  # include if both parent and child have at least 3 sane responses
  saneDyad <- 1
  for (ind in unique(b$playerNr)) {
    d <- subset(b, playerNr == ind)
    if (length(which(is.na(d$s))) >= 2)
      saneDyad <- 0
  }
  ## only add sane dyads who both completed the experiment to the summary matrix
  if (saneDyad == 1 & length(unique(b$playerNr)) == 2) {
    ## which player is the child, which is the parent?
    minAge <- min(b$age) # child
    maxAge <- max(b$age) # parent
    dChild <- subset(b, age == minAge)
    
    ## there are 2 treatments for individuals in the old data
    ## anonymous ("unknown") source, and source of a certain age
    ## calculate a mean adjustment S for both cases
    
    SChild_anon <- mean(subset(dChild, socialSource == 'unknown')$s, na.rm =
                          TRUE)
    SChild <- mean(subset(dChild, socialSource != 'unknown')$s, na.rm =
                     TRUE)
    
    dParent <- subset(b, age == maxAge)
    SParent_anon <- mean(subset(dParent, socialSource == 'unknown')$s, na.rm =
                           TRUE)
    SParent <- mean(subset(dParent, socialSource != 'unknown')$s, na.rm =
                      TRUE)
    
    socialSources <- unique(b$socialSource)
    treatmentSource <- which(socialSources != 'unknown')
    treat <- socialSources[treatmentSource]
    
    ## we will store the treatment (child or adult source)
    ## as a number (not a string) in the matrix to avoid data type issues
    treatNumeric <- 0
    if (treat == "child")
      treatNumeric <- 1
    if (treat == "adult")
      treatNumeric <- 2
    
    ## add the relevant statistics to the summary matrix
    newRow <- c(
      b$groupNr[1],
      treatNumeric,
      dChild$age[1],
      dParent$age[1],
      SChild,
      SParent,
      dChild$sex[1],
      dParent$sex[1],
      SChild_anon,
      SParent_anon
    )
    
    summMat <- rbind(summMat, newRow)
  }
}

## give some names to the matrix
names(summMat) <- c(
  'groupNr',
  'socialSource',
  'ageChild',
  'ageParent',
  'SChild',
  'SParent',
  'sexChild',
  'sexParent',
  'SChild_anon',
  'SParent_anon'
)
rownames(summMat) <- NULL
head(summMat)

# Create a new variable 'gender_match'
summMat$gender_match <- ifelse(summMat$sexChild == summMat$sexParent, 1, 0)

# Create a new matrix to store the STANDARDIZED VALUES of S
newMat <- matrix(nrow = 0, ncol = 14)#ncol(summMat)+3)

# Loop through the data to standardize by category {source, age}
for (src in unique(summMat$socialSource)) {
  # Subset the data for the current social source
  a <- subset(summMat, socialSource == src)
  
  meanParentalS <- 0
  if (src == 2)
    meanParentalS <- mean(a$SParent)
  
  # Loop through age groups from 6 to 17
  for (ag in 6:17) {
    # Subset the data for the current age group
    b <- subset(a, ageChild == ag)
    
    # standardize children's S
    b$stSChild <- b$SChild - mean(b$SChild, na.rm = TRUE)
    b$stSChild_anon <- b$SChild_anon - mean(b$SChild_anon, na.rm = TRUE)
    
    # standardize parents' S
    b$stSParent <- b$SParent - mean(b$SParent, na.rm = TRUE)
    if (src == 2)
      b$SParent - meanParentalS
    
    newMat <- rbind(newMat, b)
    
  }
}

names(newMat) <- c(names(summMat), "stSChild", "stSChild_anon", "stSParent")

## for the parents with anonymous ("unknown") sources, we can do this all at once
newMat$stSParent_anon <- newMat$SParent_anon - mean(newMat$SParent_anon, na.rm =
                                                      TRUE)

### now do the same for parents - and standardize separately for each age when they observed a child
### when parents observed another unknown adult, standardization
summMat <- newMat



######### ANALYSIS 1
# Run linear regression
regression_model <- lm(stSChild ~ stSParent, data = summMat)

# Print regression summary
summary(regression_model)

# Scatterplot for Analysis 1
plot(
  summMat$stSParent,
  summMat$stSChild,
  main = "Analysis 1: Scatterplot",
  xlab = "Standardized Parent S",
  ylab = "Standardized Child S",
  col = "blue",
  pch = 16
)
abline(regression_model, col = "red", lwd = 2)


#######ANALYSIS 2
# Run linear regression with moderation
regression_model <- lm(stSChild ~ stSParent * gender_match, data = summMat)

# Print regression summary
summary(regression_model)

# Scatterplot for Analysis 2
plot(
  summMat$stSParent,
  summMat$stSChild,
  main = "Analysis 2: Scatterplot with Moderation",
  xlab = "Standardized Parent S",
  ylab = "Standardized Child S",
  col = "green",
  pch = 16
)

# Add separate regression lines for gender_match
points(summMat$stSParent[summMat$gender_match == 0],
       summMat$stSChild[summMat$gender_match == 0],
       col = "blue",
       pch = 16)
points(summMat$stSParent[summMat$gender_match == 1],
       summMat$stSChild[summMat$gender_match == 1],
       col = "red",
       pch = 16)

# Add legend
legend(
  "topleft",
  legend = c("Gender Match 0", "Gender Match 1"),
  col = c("blue", "red"),
  pch = 16
)

# Regression lines
abline(lm(stSChild ~ stSParent, data = subset(summMat, gender_match == 0)),
       col = "blue",
       lty = 2)
abline(lm(stSChild ~ stSParent, data = subset(summMat, gender_match == 1)),
       col = "red",
       lty = 2)


# Some exploratory graphs
par(mfrow = c(1, 2))
boxplot(SChild ~ ageChild,
        data = subset(summMat, socialSource == 1),
        main = "Source: child")
boxplot(SChild ~ ageChild,
        data = subset(summMat, socialSource == 2),
        main = "Source: adult")

######### Plot the distributions of SParent and SChild for both Child and Adult conditions
child_data <- subset(summMat, socialSource == 1)  # Select rows where socialSource is 'child'
adult_data <- subset(summMat, socialSource == 2)  # Select rows where socialSource is 'adult'

# Set up a 1x3 grid for side-by-side plots
par(mfrow = c(1, 3))

# Plot stSParent distribution for Child condition
hist(
  child_data$SParent,
  main = "Distribution of SParent (Child Condition)",
  xlab = "Parent S",
  col = "blue",
  border = "black",
  breaks = 20
)

# Add labels and a legend if needed
legend("topright", legend = "Parent", fill = "blue")

# Plot SChild distribution for Child condition
hist(
  child_data$SChild,
  main = "Distribution of SChild (Child Condition)",
  xlab = "Child S",
  col = "green",
  border = "black",
  breaks = 20
)

# Add labels and a legend if needed
legend("topright", legend = "Child", fill = "green")

# Plot stSParent distribution for Adult condition
hist(
  adult_data$SParent,
  main = "Distribution of SParent (Adult Condition)",
  xlab = "Parent S",
  col = "red",
  border = "black",
  breaks = 20
)

# Add labels and a legend if needed
legend("topright", legend = "Parent", fill = "red")

# Plot SChild distribution for Adult condition
hist(
  adult_data$SChild,
  main = "Distribution of SChild (Adult Condition)",
  xlab = "Child S",
  col = "purple",
  border = "black",
  breaks = 20
)

# Add labels and a legend if needed
legend("topright", legend = "Child", fill = "purple")


################################ANALYSIS 1 AND 2 with ANON condition
## Analysis 1.2
# Run linear regression
regression_model <- lm(stSChild_anon ~ stSParent_anon, data = summMat)

# Print regression summary
summary(regression_model)

# Scatterplot for Analysis 1.2
plot(
  summMat$stSParent_anon,
  summMat$stSChild_anon,
  main = "Analysis 1: Scatterplot",
  xlab = "Standardized Parent S",
  ylab = "Standardized Child S",
  col = "blue",
  pch = 16
)
abline(regression_model, col = "red", lwd = 2)

##Analysis 2.2
# Run linear regression with moderation
regression_model <- lm(stSChild_anon ~ stSParent_anon * gender_match, data = summMat)

# Print regression summary
summary(regression_model)

# Scatterplot for Analysis 2.2
plot(
  summMat$stSParent_anon,
  summMat$stSChild_anon,
  main = "Analysis 2: Scatterplot with Moderation",
  xlab = "Standardized Parent S",
  ylab = "Standardized Child S",
  col = "green",
  pch = 16
)

# Add separate regression lines for gender_match
points(
  summMat$stSParent_anon[summMat$gender_match == 0],
  summMat$stSChild_anon[summMat$gender_match == 0],
  col = "blue",
  pch = 16
)
points(
  summMat$stSParent_anon[summMat$gender_match == 1],
  summMat$stSChild_anon[summMat$gender_match == 1],
  col = "red",
  pch = 16
)

# Add legend
legend(
  "topleft",
  legend = c("Gender Match 0", "Gender Match 1"),
  col = c("blue", "red"),
  pch = 16
)

# Regression lines
abline(
  lm(
    stSChild_anon ~ stSParent_anon,
    data = subset(summMat, gender_match == 0)
  ),
  col = "blue",
  lty = 2
)
abline(
  lm(
    stSChild_anon ~ stSParent_anon,
    data = subset(summMat, gender_match == 1)
  ),
  col = "red",
  lty = 2
)

#####distributions of SChild and SParent in the unknown condition

# Set up a 1x2 grid for side-by-side plots
par(mfrow = c(1, 2))

# Plot SParent_anon distribution
hist(
  summMat$SParent_anon,
  main = "Distribution of SParent_anon",
  xlab = "Parent_anon S",
  col = "blue",
  border = "black",
  breaks = 20
)

# Add labels and a legend if needed
legend("topright", legend = "Parent_anon", fill = "blue")

# Plot stSChild_anon distribution
hist(
  summMat$SChild_anon,
  main = "Distribution of SChild_anon",
  xlab = "Child_anon S",
  col = "green",
  border = "black",
  breaks = 20
)

# Add labels and a legend if needed
legend("topright", legend = "Child_anon", fill = "green")


#######################################ASSUMPTIONS
#helpful interpretation website https://library.virginia.edu/data/articles/diagnostic-plots
#linearty
plot(regression_model, which = 1)

#independence of residuals
plot(regression_model, which = 2)
# Run the Durbin-Watson test
durbin_watson_test <- durbinWatsonTest(regression_model)
# Display the Durbin-Watson test results
print(durbin_watson_test) #a value close to 2 indicates that there is no significant autocorrelation. Values significantly different from 2 might suggest autocorrelation.

#Homoschedasticity
plot(regression_model, which = 3)

#normality of residuals
plot(regression_model, which = 2)  # Choose "Residuals vs Normal Quantiles" or use 'qqnorm' and 'qqline'
shapiro.test(regression_model$residuals) #p should be > .05

#no perfect multicollinearity #only for model 2
library(car)#install the package
vif(regression_model) #needs to be < 10

#No influential outliers
plot(regression_model, which = 4)  # Cook's distance plot


############################################## descriptive data
##families removed
# Number of unique familyNr before filtering
unique_families_before <- length(unique(data$groupNr))

# Number of unique familyNr after filtering
unique_families_after <- length(unique(summMat$groupNr))

# Calculate the number of unique families removed
unique_families_removed <- unique_families_before - unique_families_after

# Display the result
cat("Number of unique families removed:",
    unique_families_removed,
    "\n")

# Calculate the max and min values of ageChild
max_ageChild <- max(summMat$ageChild, na.rm = TRUE)
min_ageChild <- min(summMat$ageChild, na.rm = TRUE)

# Calculate the max and min values of ageParent
max_ageParent <- max(summMat$ageParent, na.rm = TRUE)
min_ageParent <- min(summMat$ageParent, na.rm = TRUE)

#display the results
max_ageChild
min_ageChild
max_ageParent
min_ageParent

# Calculate the average ageChild
average_ageChild <- mean(summMat$ageChild, na.rm = TRUE)

# Calculate the average ageParent
average_ageParent <- mean(summMat$ageParent, na.rm = TRUE)

# Display the results
cat("Average ageChild:", round(average_ageChild, 2), "\n")
cat("Average ageParent:", round(average_ageParent, 2), "\n")

# Calculate the standard deviation of ageChild
sd_ageChild <- sd(summMat$ageChild, na.rm = TRUE)

# Calculate the standard deviation of ageParent
sd_ageParent <- sd(summMat$ageParent, na.rm = TRUE)

# Display the results
cat("Standard Deviation of ageChild:", round(sd_ageChild, 2), "\n")
cat("Standard Deviation of ageParent:", round(sd_ageParent, 2), "\n")

# Calculate the frequencies of sexChild values
sexChild_freq <- table(summMat$sexChild)

# Calculate the percentage of sexChild = 1
percentage_sexChild_1 <- (sexChild_freq[1] / nrow(summMat)) * 100

# Calculate the percentage of sexChild = 2
percentage_sexChild_2 <- (sexChild_freq[2] / nrow(summMat)) * 100

# Display the results
cat("Percentage of sexChild = 1:",
    round(percentage_sexChild_1, 2),
    "%\n")
cat("Percentage of sexChild = 2:",
    round(percentage_sexChild_2, 2),
    "%\n")

# Calculate the frequencies of sexParent values
sexParent_freq <- table(summMat$sexParent)

# Calculate the percentage of sexParent = 1
percentage_sexParent_1 <- (sexParent_freq[1] / nrow(summMat)) * 100

# Calculate the percentage of sexParent = 2
percentage_sexParent_2 <- (sexParent_freq[2] / nrow(summMat)) * 100

# Display the results
cat("Percentage of sexParent = 1:",
    round(percentage_sexParent_1, 2),
    "%\n")
cat("Percentage of sexParent = 2:",
    round(percentage_sexParent_2, 2),
    "%\n")

# Calculate the frequency of gender_match values
gender_match_freq <- table(summMat$gender_match)

# Calculate the percentage of gender_match = 1
percentage_gender_match_1 <- (gender_match_freq[2] / nrow(summMat)) * 100

# Display the result
cat("Percentage of gender_match = 1:",
    round(percentage_gender_match_1, 2),
    "%\n")

######## APA plot   #useful link: http://www.sthda.com/english/wiki/ggplot2-scatter-plots-quick-start-guide-r-software-and-data-visualization

library(ggplot2)

#analysis1
# Basic scatter
ggplot(summMat, aes(x = stSParent, y = stSChild)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(title = "        Analysis 1", x = "S Parent/(Normalized)", y = "S Child/(Normalized)") +
  theme_classic()

#analysis2
# Convert gender_match to a factor
summMat$gender_match <- factor(
  summMat$gender_match,
  levels = c(0, 1),
  labels = c("No Match", "Match")
)
# Check the structure of the variable
str(summMat$gender_match)
#scatterplot
ggplot(summMat,
       aes(
         x = stSParent,
         y = stSChild,
         color = gender_match,
         shape = gender_match
       )) +
  geom_point() +
  geom_smooth(method = lm, fullrange = TRUE) +
  labs(title = "         Analysis 2", x = "S Parent/(Normalized)", y = "S Child/(Normalized)") +
  theme_classic()
