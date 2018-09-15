setwd("/media/glycosylase/EC6A2F256A2EEBD0/Users/miffka/Documents/!DataMining/Econometrics")
library(dplyr) #
library(lmtest) # waldtest, resettest
library(ggplot2) #
library(car) # vif
library(psych) # describe
library(HSAUR) #
library(glmnet) # glmnet


### 1. Доверительные интервалы при мультиколлинеарности

h <- cars
qplot(data=h, speed, dist)

model <- lm(dist ~ speed, h)
summary(model)

# Добавляем новые данные
h <- h %>% 
  mutate(speed2 = speed^2,
         speed3 = speed^3)
model_mk <- lm(dist ~., h)
summary(model_mk)
vif(model_mk)
# Видно, что ни одного значимого коэффициента нет, однако регрессия значима в целом.
# Это и есть повод задуматься о мультиколлинеарности. Ну и еще vif.
# Посмотрим на корреляцию
x0 <- model.matrix(data = h, dist ~ 0 +.)
cor(x0)

# Посмотрим на доверительные интервалы для предсказания - они практически одинаковые
nd <- data.frame(speed = 10, speed2 = 100, speed3 = 1000)
predict(model, newdata = nd, interval = "prediction")
predict(model_mk, newdata = nd, interval = "prediction")

# Посмотрим на доверительные интервалы для коэффициентов моделей
confint(model)
confint(model_mk)

# Если интересуют предиктивные доверительные интервалы - плевать на мк
# Если интересуют переменные - она мешает


### 2. Лассо-регрессия

y <- h$dist
x0 <- model.matrix(data = h, dist ~ 0 +.)

lambdas <- seq(50, 0.1, length = 30)
m_lasso <- glmnet(x = x0, y = y, alpha = 1, lambda = lambdas)

# Сделаем график зависимости коэффициентов от лямбды
# Видно, что в районе e значение первого коэффициента начинает падать, а к большим
#  лямбда все коэффициенты уходят в 0
plot(m_lasso, xvar = "lambda", label = T)

# Сделаем график зависимости объясненной дисперсии от значений коэффициентов
# Видно, что если пожертвовать совсем небольшой долей дисперсии, то коэффициенты
#  при кубическом компоненте будут уже не равны 0.
plot(m_lasso, xvar = "dev", label = T)

# Сделаем график взаимосвязи суммарного штрафа, которые вносят все коэффициенты
#  и значения коэффициентов.
# Первый коэффициент практически полностью определеяет величину штрафа.
plot(m_lasso, xvar = "norm", label = T)

# Можно показать коэффициенты для нескольких значений лямбда
coef(m_lasso, s = c(0.1, 1))


### 3. Ридж-регрессия

m_ridge <- glmnet(x = x0, y = y, alpha = 0, lambda = lambdas)

# Сделаем график зависимости коэффициентов от лямбды
# Видно, что в районе e значение первого коэффициента начинает падать, а к большим
#  лямбда все коэффициенты уходят в 0
plot(m_ridge, xvar = "lambda", label = T)

# Сделаем график зависимости объясненной дисперсии от значений коэффициентов
# Видно, что если пожертвовать совсем небольшой долей дисперсии, то коэффициенты
#  при кубическом компоненте будут уже не равны 0.
plot(m_ridge, xvar = "dev", label = T)

# Сделаем график взаимосвязи суммарного штрафа, которые вносят все коэффициенты
#  и значения коэффициентов.
# Первый коэффициент практически полностью определеяет величину штрафа.
plot(m_ridge, xvar = "norm", label = T)

# Можно показать коэффициенты для нескольких значений лямбда
coef(m_ridge, s = c(0.1, 1))

# Выбор штрафного коэффициента лямбда.
# По понятным причинам нельзя отбирать по RSS - в этом случае нужно брать всегда 0.
# Стоит использовать кросс-валидацию!

## Кросс-валидация для выбора оптимальной лямбды

cv <- cv.glmnet(x0, y, alpha=1)
# Строим график подбора лямбды
plot(cv)
# Выбираем 2 значения лямбда - с наименьшим RSS и такое значение, при котором лямбда
#  подальше от 0, и RSS не меняется слишком сильно
cv$lambda.min
cv$lambda.1se

coef(cv, s = 'lambda.1se')
coef(cv, s = 'lambda.min')


### 4. Метод главных компонент

h <- heptathlon
help(heptathlon)
str(h)

h <- h %>% 
  select(-score)
describe(h)
cor(h)
# Делаем анализ главных компонент, смотрим на веса
h.pca <- prcomp(h, scale = T)
pca1 <- h.pca$x[,1] # новая компонента
(v1 <- h.pca$rotation[,1]) # веса старых переменных

head(pca1)
summary(h.pca)

# Очки, рассчитанные Олимпийским комитетом, очень сильно коррелируют с первой главной
#  компонентой.
cor(heptathlon$score, pca1)
plot(h.pca)
# Смотрим на биплот - видно, каким образом взаимосвязаны компоненты, 
#  и какие кластеры есть.
biplot(h.pca, xlim = c(-1, 1))


#### Тест 4

### Попытка 1

# 15

data(airquality)
str(airquality)
pairs(airquality)
qplot(x=Ozone, y=Wind, data=airquality) # correct

# 16

lm16_1 <- lm(Ozone ~ Solar.R + Wind + Temp, airquality)
round(vif(lm16_1), 3)

# 17

data1 <- na.omit(airquality)
str(data1)
y <- data1$Ozone
x <- model.matrix(data = data1, Ozone ~ 0 + Solar.R + Wind + Temp)
lambdas <- seq(50, 0.1, length = 30)

lm17_lasso <- glmnet(x = x, y = y, alpha = 1, lambda = lambdas)

round(coef(object = lm17_lasso, s = 1), 3)

# 18

lm18_rr <-  glmnet(x = x, y = y, alpha = 0, lambda = lambdas)

round(coef(lm18_rr, s = 2), 3)

# 19

plot(lm17_lasso, xvar = "dev") # correct
plot(lm17_lasso, xvar = "norm")
plot(lm17_lasso, xvar = "lambda")

# 20

pca_20 <- prcomp(x = x, scale. = T)
pairs(pca_20$x)
