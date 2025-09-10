# 1. Duomenų skaitymas
data <- read.csv("/Users/darjabaranova/Documents/VU/3_kursas/tiesiniai_metodai/salary_prediction_data.csv",
                 stringsAsFactors = FALSE)

# 2. Kategorinių kintamųjų nustatymas
data$Education <- factor(data$Education)
data$Location <- factor(data$Location)
data$Job_Title <- factor(data$Job_Title)
data$Gender <- factor(data$Gender)

# 3. Kintamojo "Salary" analizė
summary(data$Salary)
hist(data$Salary,
     breaks = 30,  # čia parenki kiek dalių norėsi
     main = "Pradinis atlyginimo pasiskirstymas",
     xlab = "Atlyginimas",
     col = "lightblue",   # papildomai spalva, kad būtų gražiau
     border = "white")

ks.test(data$Salary, "pnorm",
        mean=mean(data$Salary, na.rm=TRUE),
        sd=sd(data$Salary, na.rm=TRUE))


data$Gender_num <- ifelse(data$Gender == "Female", 1, 0)

# 4. Koreliacija kiekybinių kintamųjų
num_vars <- data[, c("Salary", "Experience", "Age", "Gender_num")]
print(cor(num_vars, use="complete.obs"))

# 5. Pirmasis modelis (pilnas)
model_full <- lm(Salary ~ Education + Experience + Location + Job_Title + Age + Gender, data=data)
summary(model_full)

# 6. Modelio tinkamumo vertinimas
par(mfrow=c(2,2))
plot(model_full)  # diagnostikos grafikai: liekanos, normalumas, įtakos taškai

# 7. Kintamųjų atranka pagal AIC kriterijų
library(MASS)
model_step <- stepAIC(model_full, direction="both", trace=FALSE)
summary(model_step)

# 8. Galutinio modelio diagnostika
par(mfrow=c(2,2))
plot(model_step)


# 10. Prognozės pavyzdys
new_data <- data.frame(
  Education="High School",
  Experience=5,
  Location="Rural",
  Job_Title="Analyst",
  Age=28,
  Gender="Female"
)
predict(model_step, newdata=new_data, interval="prediction")
