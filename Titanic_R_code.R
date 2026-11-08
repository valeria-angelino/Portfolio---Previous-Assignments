library(tidyverse)
library(ggplot2)
library(dplyr)

titanic_data <- read.csv("Intro/titanic.csv")

view(titanic_data)

        # EDA. Getting to know the data

summary(titanic_data)
str(titanic_data)

titanic_data <- titanic_data %>%
  mutate(across(c(Survived, Pclass, Sex, Embarked), as.factor))


titanic_data[titanic_data == ""  | titanic_data== " "] <- NA 

colSums(is.na(titanic_data))


        # EDA. Udnerstanding Age and median imputation

      boxplot(titanic_data$Age,
              main = "Figure 1. Boxplot of Age",
              ylab = "Age",
              col = "mistyrose"
              )
      
      summary(titanic_data$Age)
      
      
      median_Age <- median(titanic_data$Age, na.rm = TRUE)
      imputed_titanic_data <- titanic_data %>% 
        mutate(
          Age = replace_na(Age, median_Age)
        )
      

      # EDA. Correlation of numeric variables
    
library(corrplot)
      
correlations <- imputed_titanic_data %>%
  select(where(is.numeric)) %>%
  cor(use = "complete.obs")
      
corrplot(correlations,
         method = "number",
         col=colorRampPalette(c("gray", "lightskyblue", "royalblue4"))(200)
         )
      title(
        main = "Figure 2. Correlation of Numeric Variables"
      )
      
      # EDA. More Visualizations - Survival Ratios
        
imputed_titanic_data %>%
  ggplot(aes(x = Pclass, fill = Sex)) +
  geom_bar(alpha = 0.75) +
  labs(
   x = "Passenger Class",
   y = "Frequency",
   fill = "Sex",
   title = "Figure 3. Sex Distribution accross Passenger Class"
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )


imputed_titanic_data %>% 
ggplot(aes(x = Sex, fill = Survived)) +
  geom_bar(position = "fill", alpha = 0.75) +
  labs(
    title = "Figure 4. Survival by Sex",
    x = "Sex",
    y = "Proportion"
  ) +
  scale_fill_manual(
    values = c("0" = "lightslategray", "1" = "darkgreen"),
    labels = c("Died", "Survived")) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )


imputed_titanic_data %>% 
ggplot(aes(x = Pclass, fill = Survived)) +
  geom_bar(position = "fill", alpha = 0.75) +
  labs(
    title = "Figure 5. Survival by Passenger Class",
    x = "Sex",
    y = "Proportion"
  ) +
  scale_fill_manual(
    values = c("0" = "lightslategray", "1" = "darkgreen"),
    labels = c("Died", "Survived")) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )

       # EDA. Possible age graph

imputed_titanic_data <- imputed_titanic_data %>%
  mutate(
    AgeGroup = cut(
      Age,
      breaks = c(0, 12, 17, 30, 50, Inf),
      labels = c("0–12", "13–17", "18–30", "31–50", "51+"),
      include.lowest = TRUE
    )
  )

imputed_titanic_data %>%
  ggplot(aes(x = AgeGroup, fill = Survived)) +
  geom_bar(position = "fill", alpha = 0.75) +
  labs(
    title = "Figure 6. Survival by Age Group",
    x = "Age Group",
    y = "Proportion",
    fill = "Survival"
  ) +
  scale_fill_manual(
    values = c("0" = "lightslategray", "1" = "darkgreen"),
    labels = c("Died", "Survived")) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )


    # Logistic Models

model1 <- imputed_titanic_data %>% 
  glm(formula = Survived ~ Pclass + Sex + Age + SibSp + Parch + Embarked, family = binomial)

summary(model1)


model2 <- imputed_titanic_data %>% 
  glm(formula = Survived ~ Pclass + Sex + Age + SibSp + Parch, family = binomial)

summary(model2)


model3 <- imputed_titanic_data %>% 
  glm(formula = Survived ~ Pclass + Sex + Age + SibSp, family = binomial)

summary(model3)

    # Model and Baseline Accuracy

accuracymod1 <- imputed_titanic_data %>%
  mutate(
    predicted_prob = predict(model1, newdata = ., type = "response"),
    predicted_outcome = ifelse(predicted_prob >= 0.5, "1", "0")
  ) %>%
  summarize(
    accuracy = mean(predicted_outcome == as.character(Survived))
  )

accuracymod1



accuracymod2 <- imputed_titanic_data %>%
  mutate(
    predicted_prob = predict(model2, newdata = ., type = "response"),
    predicted_outcome = ifelse(predicted_prob >= 0.5, "1", "0")
  ) %>%
  summarize(
    accuracy = mean(predicted_outcome == as.character(Survived))
  )

accuracymod2



accuracymod3 <- imputed_titanic_data %>%
  mutate(
    predicted_prob = predict(model3, newdata = ., type = "response"),
    predicted_outcome = ifelse(predicted_prob >= 0.5, "1", "0")
  ) %>%
  summarize(
    accuracy = mean(predicted_outcome == as.character(Survived))
  )

accuracymod3



baseline_accuracy <- imputed_titanic_data %>% 
  summarise(baseline = mean(Survived == "0")) %>% 
  pull(baseline)


          # Testing and having fun

male_passenger1 <- data.frame(
  Pclass = factor("3", levels = levels(imputed_titanic_data$Pclass)),
  Sex = factor("male", levels = levels(imputed_titanic_data$Sex)),
  Age = 30,
  SibSp = 0 )

predict(model3, newdata = male_passenger1, type = "response")


male_passenger2 <- data.frame(
  Pclass = factor("1", levels = levels(imputed_titanic_data$Pclass)),
  Sex = factor("male", levels = levels(imputed_titanic_data$Sex)),
  Age = 30,
  SibSp = 0 )

predict(model3, newdata = male_passenger2, type = "response")


male_passenger3 <- data.frame(
  Pclass = factor("1", levels = levels(imputed_titanic_data$Pclass)),
  Sex = factor("male", levels = levels(imputed_titanic_data$Sex)),
  Age = 30,
  SibSp = 2 )

predict(model3, newdata = male_passenger3, type = "response")

male_passenger4 <- data.frame(
  Pclass = factor("1", levels = levels(imputed_titanic_data$Pclass)),
  Sex = factor("male", levels = levels(imputed_titanic_data$Sex)),
  Age = 30,
  SibSp = 1)

predict(model3, newdata = male_passenger4, type = "response")
