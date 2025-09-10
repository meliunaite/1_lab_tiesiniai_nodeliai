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


#-----------------------------------------Pradinė analizė--------------------------------------------
# --------------Įsitikiname, ar duomenys turi normalųjį skirstinį--------------
#1. Nubraižome histogramą ir kvantilių grafiką
options(scipen = 999) 
hist(df$Salary,
     breaks = 30,         
     col = "darkgray",
     main = "Atlyginimo histograma",
     xlab = "Atlyginimas",
     ylab = "Tankis",
     freq = FALSE)  # freq=FALSE, kad būtų tankio (density) skalė, ne dažniai
x_vals <- seq(min(df$Salary), max(df$Salary), length.out = 100)
y_vals <- dnorm(x_vals, mean = mean(df$Salary), sd = sd(df$Salary))
lines(x_vals, y_vals, col = "red", lwd = 2)

qqPlot(df$Salary, 
       dist = "norm",
       main = "QQ atlyginimo grafikas",
       xlab = "Teoriniai kvantiliai",
       ylab = "Stebėti atlyginimai")

#2. Statistiniai testai
y <- df$Salary
mu <- mean(y)
sigma <- sd(y)
# Kolmogorovo–Smirnovo testas 
ks.test(y, "pnorm", mean = mu, sd = sigma)
#p reikšmė (0.3418) didesnė už reikšmingumo lygmenį alfa = 0.05 -> neatmetame H0, 
#duomenys yra normaliai pasiskirstę.

#Shapiro-Wilk testas
shapiro.test(df$Salary)
#p reikšmė = 5.786e-05 -> skirstinys nėra normalusis.

#Anderson-Darling testas
library(nortest)
ad.test(df$Salary)
#p reikšmė = 0.0001573 -> skirstinys nėra normalusis.

# -----------------------Pradinė duomenų analizė-----------------------
# 1. Stebėjimų patikra
dim(df)          # 1000 stebėjimų, 7 kintamieji 
any(is.na(df))   # nėra trūkstamų reikšmių

# 2. Koreliacinė matrica (kiekybiniai kintamieji)
num_vars <- df[, c("Salary", "Age", "Experience")]
cor(num_vars)

library(corrplot)
num_vars <- df[, c("Salary", "Age", "Experience")]
cor_matrix <- cor(num_vars, use = "complete.obs")
corrplot(cor_matrix, method = "number", type = "upper")

# 3. Stačiakampės diagramos
library(ggplot2)

# Bendras temos šablonas 
mano_tema <- theme_minimal(base_size = 14) +
  theme(             
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(5, "pt"),         
    axis.line.x = element_line(color = "black"), 
    axis.line.y = element_line(color = "black"), 
    plot.title = element_text(hjust = 0.5)     
  )

# Išsilavinimas
ggplot(df, aes(x = Education, y = Salary, fill = Education)) +
  geom_boxplot() +
  labs(title = "Atlyginimas pagal išsilavinimą",
       x = "Išsilavinimas", y = "Atlyginimas (JAV doleriai)") +
  mano_tema

# Vietovė
ggplot(df, aes(x = Location, y = Salary, fill = Location)) +
  geom_boxplot() +
  labs(title = "Atlyginimas pagal vietovę",
       x = "Vietovė", y = "Atlyginimas (JAV doleriai)") +
  mano_tema

# Pareigos
ggplot(df, aes(x = Job_Title, y = Salary, fill = Job_Title)) +
  geom_boxplot() +
  labs(title = "Atlyginimas pagal pareigas",
       x = "Pareigos", y = "Atlyginimas (JAV doleriai)") +
  mano_tema

# Lytis
ggplot(df, aes(x = Gender, y = Salary, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Atlyginimas pagal lytį",
       x = "Lytis", y = "Atlyginimas (JAV doleriai)") +
  mano_tema

#4 . Sklaidos diagramos: atlyginimo prieš regresorius
plot(df$Experience, df$Salary,
     col = "steelblue", pch = 19,
     main = "Sklaidos diagrama: darbo patirtis ir atlyginimas",
     xlab = "Darbo patirtis (m.)",
     ylab = "Atlyginimas (JAV doleriai)")
abline(lm(Salary ~ Experience, data = df), col = "red", lwd = 2) #tiesinė regresijos linija
corr_val <- cor(df$Experience, df$Salary)
legend("topleft",
       legend = paste("Koreliacija:", round(corr_val, 2)),
       bty = "n")


plot(df$Age, df$Salary,
     col = "forestgreen", pch = 19,
     main = "Sklaidos diagrama: amžius ir atlyginimas",
     xlab = "Amžius (m.)",
     ylab = "Atlyginimas (JAV doleriai)")
abline(lm(Salary ~ Age, data = df), col = "red", lwd = 2)
corr_val <- cor(df$Age, df$Salary)
legend("topleft",
       legend = paste("Koreliacija:", round(corr_val, 2)),
       bty = "n")
