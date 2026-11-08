library(dplyr)
library(ggplot2)
library(scales)
library(corrplot)
library(flextable)
library(faraway)
library(car)

df <- read.csv ("C:/Users/angel/OneDrive/Desktop/School/R Course/Endometriosis Project/structured_endometriosis_data.csv")

View(df)

windowsFonts(TimesNewRoman = windowsFont("Times New Roman"))
par(family = "Times New Roman")

font_add("Times New Roman", regular = "C:/Windows/Fonts/times.ttf")
showtext_auto()

# Changing binary data to named binary data

df <- df %>%
  mutate(
    Menstrual_Irregularity = factor(Menstrual_Irregularity, labels = c("No", "Yes")),
    Hormone_Level_Abnormality = factor(Hormone_Level_Abnormality, labels = c("Normal", "Abnormal")),
    Infertility = factor(Infertility, labels = c("No", "Yes")),
    Diagnosis = factor(Diagnosis, labels = c("No Endometriosis", "Endometriosis"))
  )

summary(df)

# RQ1 - diagnosis and chronic pain t-test

summary(df$Diagnosis)
summary(df$Chronic_Pain_Level)

hist(df$Chronic_Pain_Level,
     col = "mistyrose",
     border = "black",
     main = "Figure 1. Chronic Pain Level Distribution",
     xlab = "Chronic Pain Level",
     ylab= "Frequency"
)

t.test(Chronic_Pain_Level~Diagnosis, alternative='two.sided', 
       conf.level=.95, var.equal=FALSE, data=df)

# Visualization

ggplot(df, aes(x = Diagnosis, y = Chronic_Pain_Level, fill = Diagnosis)) +
  geom_violin(trim = FALSE, alpha = 0.7) +          # violin plot with full tails
  geom_boxplot(width = 0.1, color = "white", outlier.shape = NA) +
  theme_minimal() +
  labs(
    title = "Figure 2. Chronic Pain Levels by Diagnosis",
    x = "Diagnosis",
    y = "Chronic Pain Level"
  ) +
  scale_fill_manual(values = c("lightsalmon", "palegreen3")) +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    panel.grid.major.x = element_line(color = "gray80"),
    panel.grid.minor.x = element_line(color = "gray90")
  ) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 10)
  )

# RQ2 - Correlation Age & Chronic Pain

library(dplyr)
just_endo <- df %>% filter(Diagnosis == 'Endometriosis')

par(mfrow=c(1,2), oma = c(0, 0, 1.5, 0), family = "Times New Roman")

hist(just_endo$Age,
     col = "rosybrown1",
     border = "black",
     main = "Age",
     xlab = "Age",
     ylab= "Frequency",
     cex.main = 0.9,
     font.main = 1,
     cex.axis = 0.75,
     cex.lab = 0.75,
     prob = TRUE
)
curve(dnorm(x, mean = mean(just_endo$Age, na.rm = TRUE),
            sd = sd(just_endo$Age, na.rm = TRUE)),
      col = "rosybrown4", lwd = 1, add = TRUE)

hist(just_endo$Chronic_Pain_Level,
     col = "plum1",
     border = "black",
     main = "Chronic Pain Level",
     xlab = "Chronic Pain Level",
     ylab= "Frequency",
     cex.main = 0.9,
     font.main = 1,
     cex.axis = 0.75,
     cex.lab = 0.75,
     prob = TRUE
)
curve(dnorm(x, mean = mean(just_endo$Chronic_Pain_Level, na.rm = TRUE),
            sd = sd(just_endo$Chronic_Pain_Level, na.rm = TRUE)),
      col = "plum4", lwd = 1, add = TRUE)

mtext("Figure 3. Distribution of Age and Chronic Pain Level for Women with Endometriosis)", 
      outer = TRUE, cex = 1)


shapiro.test(just_endo$Age)
shapiro.test(just_endo$Chronic_Pain_Level)


      # Correlation

library(ggplot2)

cor_result <- cor.test(just_endo$Age, just_endo$Chronic_Pain_Level, method = "spearman")
rho <- round(cor_result$estimate, 3)
pval <- signif(cor_result$p.value, 3)

ggplot(just_endo, aes(x = Age, y = Chronic_Pain_Level)) +
  geom_point(alpha = 0.5, color = "steelblue2") +
  geom_smooth(method = "loess", color = "steelblue4", se = FALSE) +
  labs(title = "Figure 4. Age vs. Chronic Pain Level",
       x = "Age", y = "Chronic Pain Level (0–10)") +
  annotate("text", 
           x = max(just_endo$Age) * 0.41,
           y = max(just_endo$Chronic_Pain_Level)*1.1, 
           label = paste0("Spearman rho = ", rho, "\nP-value = ", pval), 
           hjust = 0, vjust = 1, size = 3) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman")
  ) +
  coord_cartesian(clip = "off")



# RQ3 - Endometriosis and BMI

just_endo <- na.omit(just_endo)

just_endo$BMI_Group <- cut(just_endo$BMI,
                           breaks = c(-Inf, 18.5, 25, 30),
                           labels = c("Underweight", "Healthy Weight", "Overweight"),
                           right = FALSE)

anova_model <- aov(Chronic_Pain_Level ~ BMI_Group, data = just_endo)

          # Checking assumptions

shapiro.test(residuals(anova_model)) # Normality of residuals

par(mfrow = c(2, 2))
plot(anova_model)

          # Kruskall 

kruskal.test(Chronic_Pain_Level ~ BMI_Group, data = just_endo)

          # Visualization

ggplot(just_endo, aes(x = BMI_Group, y = Chronic_Pain_Level, fill = BMI_Group)) +
  geom_violin(trim = FALSE, alpha = 0.7) +          
  geom_boxplot(width = 0.1, color = "white", outlier.shape = NA) +
  theme_minimal() +
  labs(
    title = "Figure 6. Chronic Pain Levels by BMI Group",
    x = "BMI Group",
    y = "Chronic Pain Level"
  ) +
  scale_fill_manual(values = c("slateblue1", "slateblue3", "slateblue4")) +  # Adjust as needed for 3 groups
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    panel.grid.major.x = element_line(color = "gray80"),
    panel.grid.minor.x = element_line(color = "gray90")
  ) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 10)
  )

# RQ4 - diagnosis and infertility

    # Creating a contingency table

Contingency.Table1 <- table( "rows" = df$Diagnosis, "columns" = df$Infertility)

Contingency.Table1

    # Chi square test
Chi.Test1 <- chisq.test(Contingency.Table1)

Chi.Test1

    # VISUALIZATION Barplot
windowsFonts(TimesNewRoman = windowsFont("Times New Roman"))
par(mfrow=c(1,1), family = "Times New Roman")
barplot(Contingency.Table1, 
        beside=T, 
        col=c("rosybrown1", "slategray3"), 
        xlab="Diagnosis", 
        ylab="Frequency",
        main="Figure 7. Endometriosis Diagnosis and Infertility",
        legend.text=c("No Infertility", "Infertility"),  
        args.legend = list(x="topright", bty="n"),
        cex.axis = 0.75, 
        cex.names = 0.7,
        names.arg = c("No Endometriosis", "Endometriosis")
        )