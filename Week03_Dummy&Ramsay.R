setwd("/media/glycosylase/EC6A2F256A2EEBD0/Users/miffka/Documents/!DataMining/Econometrics")
library(memisc) #
library(dplyr) #
library(lmtest) # waldtest, resettest
library(sjPlot) # plot_models, plot_model
library(ggplot2) #
library(foreign) #
library(hexbin) #
library(vcd) # mosaic
library(pander) #
library(devtools) #
library(knitr) #



### 1. R графики, переход к логарифмам

h <- diamonds
str(h)
help(diamonds)

# Original plot
h %>% 
  ggplot(aes(carat, price))+
  geom_point()
# Logarithmic plot
h %>% 
  ggplot(aes(log(carat), log(price)))+
  geom_hex()

# price for flats in Moskow
f <- read.csv("flats_moscow.txt", sep = "\t", header = T, dec = ".")
str(f)

# Original plot
f %>% 
  ggplot(aes(totsp, price))+
  geom_point()
# Logarithmic plot
f %>% 
  ggplot(aes(log(totsp), log(price)))+
  geom_point(size = 0.5)

# How to visulasize many categorical variables
mosaic(~walk + brick + floor, data = f, shade = T)



### 2. Графики для качественных и количественных переменных

f <- f %>% 
  mutate_at(.vars = c("walk", "brick", "floor", "code"), .funs = "factor")
str(f)

f %>% 
  ggplot(aes(log(price), fill = brick))+
  geom_histogram(aes(y=..density..), bins = 40, position = "dodge")+
  geom_density(alpha = 0.3)+
  facet_grid(~floor)



### 3. Оцениванием моделей с дамми-переменными

model_0 <- lm(log(price) ~ log(totsp), f)
model_1 <- lm(log(price) ~ log(totsp) + brick, f)
model_2 <- lm(log(price) ~ log(totsp) + brick + brick:log(totsp), f)
model_2b <- lm(log(price) ~ brick*log(totsp), f)

summary(model_0)
mtable(model_2, model_2b)

plot_model(model = model_2, show.values = T)

# Что означает каждая модель с точки зрения интерпретации?
mtable(model_0, model_1, model_2)
plot_models(model_0, model_1, model_2, show.values = T)



### 4. Построение прогнозов и интервалов

nw <- data.frame(totsp=c(60,60), brick=factor(c(1,0)))

predict(model_2, newdata = nw)
exp(predict(model_2, newdata = nw))

# Построение доверительных и предиктивных интервалов

# Доверительный интервал - для среднестатистической квартиры
predict(model_2, newdata = nw, interval="confidence") %>% 
  exp()
# Предиктивный интервал - для конкретной квартиры
predict(model_2, newdata = nw, interval="prediction") %>% 
  exp()



### 5. Проверка гипотезы о линейных ограничениях

# F-test
# model_0 is restricted, models 1 and 2 are unrestricted

# p.value << 1 => H0 is rejected
waldtest(model_0, model_1)

# p.value is 0.013 => H0 is also rejected
waldtest(model_1, model_2)

# H0 is rejected
waldtest(model_0, model_2)

f %>% 
  ggplot(aes(log(totsp), log(price), color = brick))+
  geom_point()+
  stat_smooth(method = "lm")+
  facet_grid(~ walk)



### 6. Ловушка дамми-переменных. Информационные критерии. Тест Рамсея.

f$nonbrick <- memisc::recode(f$brick, 1 <- 0, 0 <- 1)
str(f)
# Не оценен 1 коэффициент из-за сингулярности. R выкидывает лишние дамми-переменные
model_wrong <- lm(log(price) ~ log(totsp) + brick + nonbrick, f)
summary(model_wrong)

# Сравнение информационных критериев
# Чем меньше AIC или BIC, тем лучше модель
mtable(model_0, model_1, model_2)

# Проверка гипотезы о пропущенных переменных
resettest(model_2, power = 2:3)
# На уровне значимости 1% можно не отвергать гипотезу о том, что какие-то переменные,
#  которые нужно было включить, не включены.



### 7. Нано-исследование 

# Бла-бла об отчете в markdown

pander(mtable(model_0, model_1, model_2))



#### Тест


## Попытка 1
# 1
f11 <- (75-25)/2/25*35
qf(0.99, df1 = 2, df2 = 35)
df(x = f11, df1 = 2, df2 = 35)

# 2
f12 <- (95-25)/2/25*20

# 3
var13 <- 21.9 + 70^2*0.01 + 2*70*(-0.46)

# 4
var14 <- 1259.265 + 21.9 + 80^2*0.01 + 2*80*(-0.46)

# 11
data(diamonds)
ncol(diamonds)

# 12
model12 <- lm(log(price) ~ carat, diamonds)
summary(model12)

# 13
model13 <- lm(price ~ carat + y, diamonds)
summary(model13)

# 14
str(diamonds)
levels(diamonds$clarity)
df <- diamonds %>% 
  mutate(cSI2 = factor(ifelse(clarity == "SI2", 1, 0)),
         cSI1 = factor(ifelse(clarity == "SI1", 1, 0)),
         cVS2 = factor(ifelse(clarity == "VS2", 1, 0)),
         cVS1 = factor(ifelse(clarity == "VS1", 1, 0)),
         cVVS2 = factor(ifelse(clarity == "VVS2", 1, 0)),
         cVVS1 = factor(ifelse(clarity == "VVS1", 1, 0)),
         cIF = factor(ifelse(clarity == "IF", 1, 0)))
model141 <- lm(price ~ carat, df)
model142 <- lm(price ~ carat + cSI2 + cSI1 + cVS2 + cVS1 + cVVS2 + cVVS1 + cIF, df)
mtable(model141, model142)
0.895-0.849

# 15
945466.532

# 16
levels(df$cut)
df <- df %>% 
  mutate(cutGood = factor(ifelse(cut == "Good", 1, 0)),
         cutVeryGood = factor(ifelse(cut == "Very Good", 1, 0)),
         cutPremium = factor(ifelse(cut == "Premium", 1, 0)),
         cutIdeal = factor(ifelse(cut == "Ideal", 1, 0)))
model161 <- lm(price ~ carat + depth, df)
model162 <- lm(price ~ carat + depth + cutGood + cutVeryGood + cutPremium + cutIdeal, df)
mtable(model141, model161, model162)

# 17
waldtest(model161, model162)

# 18
resettest(model141)

# 19
df %>% 
  ggplot(aes(log(price), fill = color))+
  geom_density(alpha = 0.5)+
  facet_wrap(~color)

# 20
df %>% 
  ggplot(aes(log(carat), log(price), color = clarity))+
  geom_point()+
  facet_wrap(~cut)


## Попытка 2
# 4
var24 <- 1259.265 + 21.9 + 150^2*0.01 + 2*150*(-0.46)

# 12
model12_2 <- lm(log(price) ~ log(carat), df)
summary(model12_2)

# 13
model13_2 <- lm(price ~ carat + x, df)
summary(model13_2)

# 14
summary(model141)

# 15
mtable(model161)

# 16
mtable(model141, model161, model162)

# 17
waldtest(model141, model161)

# 18
resettest(model162)

# 19
df %>% 
  ggplot(aes(log(price), fill = cut))+
  geom_density(alpha = 0.5)+
  facet_grid(~ cut)

# 20
levels(df$clarity)
