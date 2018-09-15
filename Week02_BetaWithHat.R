setwd("/media/glycosylase/EC6A2F256A2EEBD0/Users/miffka/Documents/!DataMining/Econometrics")
library(memisc)
library(dplyr)
library(psych)
library(lmtest)
library(sjPlot)
library(sgof)
library(ggplot2)
library(foreign)
library(car)
library(hexbin)
library(rlms)
library(haven)

## 1. Работа со случайными величинами в R

# Генерация случайной величины

rnorm(100, mean = 5, sd = 3)
rchisq(100, df = 40)
rt(100, df = 30)

hist(rnorm(100))
qplot(rchisq(100, df =3))

# Строим функцию плотности

x <- seq(-10, 15, by=0.5)
y <- dnorm(x, mean = 5, sd = 3)
qplot(x, y, geom = "line")

# Расчитываем вероятности - обращаемся к функциям распределения

# P(Z<3) = F(3)
pnorm(3, mean = 5, sd = 3)

# P(Z in [4:9]) = P(Z<9) - P(Z<4)
pnorm(9, mean = 5, sd = 3) - pnorm(4, mean = 5, sd = 3)

# Ищем квантили распределения

# P(Z<a)=0.7 a?
qnorm(0.7, mean = 5, sd = 3)


## 2. Проверка гипотез о коэффициентах

# Множественная регрессия

h <- swiss
glimpse(h)
help(swiss)

model1 <- lm(data = h, Fertility ~ Catholic+Agriculture+Examination)
summary(model1)

coeftest(x = model1)
confint(object = model1, level = 0.90)
sjp.lm(model1) # visualization of model coefficients - very useful!
plot_model(model = model1, show.values = T) # does the same

# Проверка гипотезы о равенстве коэффициентов - вспомогательная регрессия

model2 <- lm(data = h, Fertility ~ Catholic+I(Catholic+Agriculture)+Examination)
# I значит "трактовать как сумму двух предикторов"
summary(model2)
# Видно, что коэффициенты равны

# Немного другой способ
linearHypothesis(model1, "Catholic-Agriculture=0")


## 3. Стандартизированные коэффициенты и эксперимент с ложно-значимыми регрессорами

# Стандартизированные коэффициенты

h_st <- h %>% 
  mutate_all(scale)
str(h_st)

model_3 <- lm(data = h_st, Fertility ~ Catholic+Agriculture+Examination)
summary(model_3)
plot_model(model_3, show.values = T, show.intercept = T)

# Искусственный эксперимент

D <- matrix(nrow = 100, rnorm(100*41, mean = 0, sd = 1))
df <- data.frame(D)

model_empty <- lm(data = df, X1 ~ .)
summary(model_empty)
# Некоторые предикторы оказываются значимы, тарам-пам-пам

# Сравнение нескольких моделей

model3 <- lm(data = h, Fertility ~ Catholic+Agriculture)

compar_12 <- mtable(model1, model3) # полезная функция из пакета memisc
compar_12


## 4. Сохранение и загрузка данных

# Рабочая папка - setwd, getwd
# Сохранение модели

# Создаем лист
stuff <- list(data = h, model = model1)
# Сохраняем лист
saveRDS(stuff, file = "fertility_model.Rds")

# Загрузка данных
# Загружаем лист
mylist <- readRDS("fertility_model.Rds")

# Загружаем csv - побольше знаю


## 5. Загрузка данных RLMS

# google - hse rlms
# Скачать как набор данных, так и описание переменных

# Волна 22 - репрезентативная выборка, домохозяйства
# https://www.hse.ru/data/2017/06/07/1313524365/r22h_os26a.sav
# Памяти не хватает. Это странно.
h_rlms <- rlms::rlms_read(file = "r22i_os26a.sav")

h2 <- h_rlms %>% 
  select(wage = rj13.2,
         age = rh6,
         sex = rh5,
         edu = r_diplom,
         type = status,
         sat = rj1.1.1) %>% 
  mutate(age = 2013 - age) %>% 
  filter(!type %in% c("ПГТ", "село")) %>% 
  filter(!sat %in% c("И ДА, И НЕТ",
                     "СКОРЕЕ НЕ УДОВЛЕТВОРЕНЫ",
                     "СОВСЕМ НЕ УДОВЛЕТВОРЕНЫ",
                     "ЗАТРУДНЯЮСЬ ОТВЕТИТЬ",
                     "ОТКАЗ ОТ ОТВЕТА",
                     "НЕТ ОТВЕТА")) %>% 
  filter(!edu %in% c("ЗАТРУДНЯЮСЬ ОТВЕТИТЬ",
                     "ОТКАЗ ОТ ОТВЕТА",
                     "НЕТ ОТВЕТА" ))


#h_foreign <- foreign::read.spss(file = "r22i_os26a.sav", to.data.frame = T)

#h3 <- h_foreign %>% 
#  select(wage = rj13.2,
#         age = rh6,
#         sex = rh5,
#         edu = r_diplom,
#         type = status,
#         sat = rj1.1.1) %>% 
#  mutate(age = 2013 - age) %>% 
#  filter(!type %in% c("ПГТ", "село"))

str(h2)

levels(h2$type)
levels(h2$sat)
sum(is.na(h2$sat))
table(h2$sat)
levels(h2$edu)


# Отбор переменных
h2 <- select(h, qm1, qm2, qh6, qh5)
describe(h2)
h3 <- rename(h2, mass=qm1, height=qm2, sex=qh5, birthyear=qh6)
h3 <- mutate(h3, age=2012-birthyear)
describe(h3)

# Отбор наблюдений
h4 <- filter(h3, sex == "МУЖСКОЙ")
qplot(data = h4, height, mass)
qplot(data = h4, mass)



### Тест 2

## Попытка 2.

# 8
qt(p = 0.95,df = 28)*0.5

# 11
pchisq(q = 9, df = 10)

# 13
library(ggplot2)
data(diamonds)
?diamonds
str(diamonds)

# 14
library(dplyr)
diamonds %>% 
  filter(cut == "Ideal") %>% 
  nrow()

# 15 se(beta1)

task15 <- lm(price ~ carat, diamonds)
summary(task15)

# 18

task18 <- lm(price ~ carat + x + y + z, diamonds)
summary(task18)

# 19

task19 <- lm(price ~ carat + y + x, diamonds)
mtable(task15, task19)

# 20

round(confint(task19, level = 0.90), 2)


## Попытка 3

# 7

qt(p = 0.995, df = 51)

# 8

round(5-qt(0.95, 28)*1, 2)

# 11

pnorm(q = 9, mean = 7, sd = 2)

# 16

summary(task15)

# 18

summary(task18)

# 19

task19_3 <- lm(price ~ carat + y, diamonds)
task19_3_1 <- lm(price ~ y, diamonds)
mtable(task19_3_1, task19_3)

# 20

confint(task19, level = 0.9)
