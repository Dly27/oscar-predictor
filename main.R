install.packages("scales")
library(scales)

data <- read.csv("~/oscar_winner_predictor/oscars.csv", header=TRUE)

# Data prepration
training_data <- data[-(1:10), ]
training_data$Ch <- ifelse(training_data$Ch == 1, 1, 0)
training_data$WR<- training_data$WR / 10
training_data$Nom <- rescale(training_data$Nom)
training_data$Length <- rescale(training_data$Length)
training_data$Days <- rescale(training_data$Days)


print(training_data)
model <- glm(Ch ~ . -Year -Name, data = training_data, family = binomial )
