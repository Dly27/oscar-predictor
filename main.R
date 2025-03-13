install.packages("scales")
library(scales)

data <- read.csv("~/oscar_winner_predictor/oscars.csv", header=TRUE)

# Data prepration
training_data <- data[-(1:10), ]
training_data$Ch <- ifelse(training_data$Ch == 1, 1, 0)
training_data$WR<- training_data$WR / 10
training_data$Ebert <- training_data$Ebert / 4
for (col in c("Nom", "Length", "Days")) {
  training_data[[col]] <- rescale(training_data[[col]])
}

print(training_data)

# Fit model
model <- glm(Ch ~ . -Year -Name, data = training_data, family = binomial )
summary(model)

# Print confidence intervals for different levels
for (level in c(0.95, 0.975, 0.99)) {
  cat("\nConfidence intervals at", level * 100, "% level:\n")
  print(confint(model, level = level))
}

  
