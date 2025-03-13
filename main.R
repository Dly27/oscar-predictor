install.packages("scales")
library(scales)

data <- read.csv("~/oscar_winner_predictor/oscars.csv", header=TRUE)

# Data preparation
training_data <- data[-(1:10), ]
training_data$Ch <- ifelse(training_data$Ch == 1, 1, 0)
training_data$WR<- training_data$WR / 10
training_data$Ebert <- training_data$Ebert / 4
for (col in c("Nom", "Length", "Days")) {
  training_data[[col]] <- rescale(training_data[[col]])
}

print(training_data)

# Fit model
full_model <- glm(Ch ~ . -Year -Name -Pic -Anf -Gf2 -Animation -Docu -U, data = training_data, family = binomial )
summary(full_model)

# Calculate confidence intervals
confint(full_model)

# Attempt stepwise selection to find an optimal model(based on AIC)
backward_model <- step(full_model, direction = "backward", trace = 0)

null_model <- glm(Ch ~ 1, data = training_data, family = binomial )
forward_model <- step(null_model, scope = list(lower = null_model, upper = full_model), direction = "forward", trace = 0)

stepwise_model <- stepwise_model <- step(full_model, direction = "both", trace = 0)

summary(backward_model)
summary(forward_model)
summary(stepwise_model)



  
