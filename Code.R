data <- read.csv("Pokemon.csv")

#Prepare the data
data$Generation <- as.factor(data$Generation)

#Assess the classes
lapply(data, class)

#Run the model with all variables
model <- lm(Attack ~ . , data = data)

#Assess the model
summary(model)

#Generation doesn't seem to be significant for all levels, delete the variable
model2 <- lm(Attack ~ Type + HP + Defense + Speed + Legendary, data = data)

summary(model2)

#Specify various interactions
model3 <- lm(Attack ~ Type + HP * Defense + Speed : Legendary, data = data)
model4 <- lm(Attack ~ Type + HP * Defense + Speed * Legendary, data = data)
model5 <- lm(Attack ~ Type + HP * Defense + Speed+ Speed : Legendary, data = data)

summary(mode3)
summary(mode4)
summary(mode5)

#Change the base level
data$Generation <- relevel(data$Generation, ref = 3)
model6 <- lm(Attack ~ . , data = data)

summary(model6)
