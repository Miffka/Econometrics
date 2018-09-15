setwd("/media/glycosylase/EC6A2F256A2EEBD0/Users/miffka/Documents/!DataMining/Econometrics")
library(dplyr) #
library(lmtest) # waldtest, resettest
library(ggplot2) #
library(car) # vif
library(sandwich) # vcovHC, vcovHAC
library(broom) # манипуляции - augment (adds model chars)
library(Ecdat)
library(memisc) #mtable

### 1. Функции в R
## Пара бесполезных функций
f <- function(x) {
  return(x^2)
}

fs <- function(x, s = 2) {
  return(x^s)
}

d <- cars
d[1,2] <- NA
d[3,1] <- NA
str(d)

# Считаем процент пропущенных наблюдений
# Функция должна проверять правильность ввода
is.na(d)
na_perc <- function(d) {
  if(!is.data.frame(d)) stop("d should be data.frame")
  return(sum(is.na(d)) / nrow(d) / ncol(d))
}


### 2. Циклы в R

cat(c(5:10)^2)

# Чтение большого количества данных
all_data <- NULL
for (fname in c("file01.csv", "file02.csv")) {
  temp <- read.csv(fname)
  all_data <- rbind(alldata, temp)
}


### 3. Прежние оценки для сравнения

h <- read.table("flats_moscow.txt", header = T)
str(h)
# Видим, что РАЗБРОС цены явно больший при большей общей площади
h %>% 
  ggplot(aes(totsp, price))+
  geom_point()

# Начинаем оценку с простой модели
model <- lm(price ~ totsp, data = h)
summary(model)
coeftest(model) # Нельзя применять стандартные ошибки, полученные такими способами
confint(model) # Доверительные интервалы для коэффициентов также неверны
# Ошибки возникают на этапе построения ковариационной матрицы
vcov(model)


### 4. Доверительные интервалы при гетероскедастичности

h <- augment(model, h)
str(h)
h %>% 
  ggplot(aes(totsp, abs(.resid)))+
  geom_point()
# Пробуем получить верные оценки ковариационной матрицы для коэффициентов
vcov(model)
vcovHC(model) # Оценки дисперсий и ковариаций выросли
vcovHC(model, type = "HC2")

# Сравним стадартные ошибки и посмотрим на результаты тестов
coeftest(model)
coeftest(model, vcov. = vcovHC(model))

# Построим доверительный интервал
conftable <- coeftest(model, vcov. = vcovHC(model))
ci <- data.frame(estimate = conftable[,1], 
                 se_hc = conftable[,2])
ci <- ci %>% 
  mutate(left_ci = estimate - 1.96*se_hc,
         right_ci = estimate + 1.96*se_hc)
ci
confint(model)
# Видно, что доверительные интервалы расширились


### 5. Тесты на гетероскедастичность

# Тест Уайта (или Бройша-Пагана)
bptest(model) # гипотеза об условной гомоскедастичности отвергается
# I() - восприятие формулы как есть
bptest(model, data = h, varformula = ~ totsp + I(totsp^2))

# Тест Голдфельда-Квандта
gqtest(formula = model, order.by = ~totsp, data = h, fraction = 0.2)

# Какие методы мы еще можем применять для борьбы с гетероскедастичностью?
# Трансформируем в логарифмы!

qplot(data = h, log(totsp), log(price))
model2 <- lm(log(price) ~ log(totsp), data = h)
# Если посмотрим на статистику GQ, то увидим, что RSS отличаются уже в 2.5 раза, а не в 8
gqtest(formula = model2, order.by = ~totsp, data = h, fraction = 0.2)


#### Промежуточный экзамен 5.

### Попытка 1.

# 5
df <- ChickWeight
str(df)

df %>% 
  filter(Time == 10) %>% 
  summarise(mean(weight)) %>% 
  round(2)

# 6
df %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(mean(weight))

# 7
# Rsq = 0.7453
summary(lm(weight ~ Time + Diet, data = df))

# 8
11 / 1.7

# 12
df2 <- diamonds
df2 %>% 
  ggplot(aes(log(price), color = cut))+
  facet_grid(~cut)+
  geom_histogram()

# 13
summary(lm(price ~ carat + table + x + y + x + depth, data = df2))

# 14
help(diamonds)
model14 <- lm(price ~ carat + table + x + y + depth, data = df2)
confint(model14, level = 0.9)

# 19
df3 <- na.omit(BudgetFood)
str(df3)
model19 <-  lm(wfood ~ totexp + size, data = df3)
x <- data.frame(totexp = 700000, size = 4)
round(predict(model19, newdata = x, interval = "prediction", level = 0.9), 2)

# 20
resettest(model19)

# 21
model21 <- lm(wfood ~ totexp + size + sex + totexp:sex + size:sex, data=df3, na.action = na.exclude)
summary(model21)

waldtest(model21, model19)

# 26
df4 <- mtcars
round(vif(lm(mpg ~ disp + hp + wt, data = df4)), 2)

# 27
round(norm(prcomp(df4[,c("disp", "hp", "wt")], scale. = T)$x[,1], type = "2"), 2)

# 28

df4.pca <- prcomp(df4[,c("disp", "hp", "wt")], scale. = T)$x
model28_1 <- lm(df4$mpg ~ df4.pca[,1:2])
model28_2 <- lm(df4$mpg ~ df4.pca)
mtable(model28_1, model28_2)

