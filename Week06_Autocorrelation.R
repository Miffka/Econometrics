setwd("/media/glycosylase/EC6A2F256A2EEBD0/Users/miffka/Documents/!DataMining/Econometrics")
library(lubridate) # ymd
library(sandwich) #vcovHAC
library(lmtest)
library(car)
library(bstats)
library(zoo) # zoo
library(xts)
library(dplyr)
library(broom)
library(ggplot2)
library(quantmod)
library(sophisthse)
library(rusquant)
library(Quandl)
library(Ecdat)



### 1. Работа с данными в R

x <- c("2012-04-15", "2011-08-17")
y <- ymd(x) # 
y + days(20)
y - years(10)
day(y)
month(y)
year(y)

x <- rnorm(5)
y <- ymd("2014-01-01") + days(0:4)
# Make timeseries
ts <- zoo(x, order.by = y)
# У функции поведение, обратное тому, что было в лекциях
lag.xts(ts, 1) # вчерашнее значение
lag.xts(ts, -1) # завтрашнее значение
diff(ts) # прирост

# Создание регулярных временных рядов
(ts2 <- zooreg(x, start = as.yearqtr("2014-01"), freq = 4))
(ts3 <- zooreg(x, start = as.yearmon("2014-01"), frequency = 12))



### 2. Базовые действия с временными рядами
data("Investment")
str(Investment)
?Investment

# описание временных рядов
start(Investment)
end(Investment)
time(Investment)
coredata(Investment)

# восстановление пропущенных значений
dna <- Investment
dna[1,2] <- NA
dna[5,3] <- NA
dna
# Линейная аппроксимация и копирование предыдущего значения
na.approx(dna) # линейная интерполяция
na.locf(dna) # перенос вперед последнего доступного значения



### 3. Загрузка данных из внешних источников
# finance.google.com
# finance.yahoo.com
# quandl.com
# finam.ru
# sophist.hse.ru
a <- sophisthse("POPNUM_Y") # Численность населения России
b <- Quandl("FRED/GNP") # Валовый национальный продукт США

# finance.google.com
Sys.setlocale("LC_TIME", "C")
getSymbols(Symbols = "AAPL", from = "2010-01-01", to = "2014-02-03", src = "google")
head(AAPL)
tail(AAPL)

# finam.ru
# Не грузится - нет пакета rusquant
# И все равно не грузится
getSymbols(Symbols = "GAZP", from = "2011-01-02", to = "2014-09-09", src = "Finam")

# Графики финансовых временных рядов
plot(AAPL)
autoplot(AAPL[,1:4], facets = NULL)
chartSeries(AAPL)



### 4. Построение робастных доверительных интервалов
data("Investment")
d <- as.zoo(Investment)
autoplot(d[,1:2], facets = NULL)

model <- lm(data = d, formula = RealInv ~ RealInt + RealGNP)
summary(model)
coeftest(model)
confint(model)
# Графики остатков
# Добавляем остатки
d_aug <- augment(model, as.data.frame(d))
str(d_aug)
# График остатков - небольшая автокорреляция есть
d_aug %>% 
  ggplot(aes(lag(.resid), .resid))+
  geom_point()

vcov(model) # несостоятельные оценки для дисперсии
vcovHAC(model) # робастные к автокорреляции оценки

coeftest(model, vcov. = vcovHAC(model)) # строим правильные доверительные интервалы
conftable <- coeftest(model, vcov. = vcovHAC(model))
ci <- data.frame(estimate = conftable[,1], se_ac = conftable[,2])
ci <- ci %>% 
  mutate(left_95 = estimate - 1.96*se_ac,
         right_95 = estimate + 1.96*se_ac)
ci



### 5. Тесты на автокорреляцию в R
# Durbin-Watson
# H0 - нет автокорреляции
# Ha - автокорреляции первого порядка

dwt(model)
dwt(model)$r

# Breusch-Godfrey
# H0: нет автокорреляции
# На: автокорреляция к-го порядка

bgtest(model, order = 2)

# Если один тест гипотезу отвергает, а второй - нет, то мы отвергаем гипотезу



### Тест

## Попытка 1.

# 11
data(Griliches)
str(Griliches)
?Griliches

m11 <- lm(lw80 ~ age80 + iq + school80 + expr80, data = Griliches)
vcov(m11)

# 12
vcov(m11) - vcovHC(m11, type = "HC3")

# 13
vcovHC(m11, type = "HC2")[4,4]
vcovHC(m11, type = "HC4")[4,4]
vcovHC(m11, type = "HC0")[4,4]
vcovHC(m11, type = "HC5")[4,4]

# 14
bptest(m11, varformula = ~ age80, data = Griliches)

# 15
gqtest(formula = m11, order.by = ~expr80, data = Griliches, fraction = 0.2)

# 16
data(Solow)
str(Solow)

m15 <- lm(formula = q ~ k + A, data = Solow)
vcov(m15)-vcovHAC(m15)

# 17
t17 <- dwt(m15)
t17$dw

# 18
m18 <- lm(formula = q ~ A, data = Solow)
t18 <- bgtest(m18, order = 3)
t18$statistic

# 19

Sys.setlocale("LC_TIME","C")
getSymbols(Symbols = "AAPL",from="2010-01-01", to="2014-02-03",src="google")
plot(AAPL$AAPL.Close, main = "")

Sys.setlocale("LC_TIME","C")
getSymbols(Symbols = "GOOG",from="2010-01-01", to="2014-02-03",src="google")
plot(GOOG$GOOG.Close, main = "") # right

Sys.setlocale("LC_TIME","C")
getSymbols(Symbols = "INTC",from="2010-01-01", to="2014-02-03",src="google")
plot(INTC$INTC.Close, main = "")

Sys.setlocale("LC_TIME","C")
getSymbols(Symbols = "MSFT",from="2010-01-01", to="2014-02-03",src="google")
plot(MSFT$MSFT.Close, main = "")

# 20
n20 <- INTC$INTC.Close
head(n20)
head(lag(n20, n = 1))
head(lag(n20, n = 2))

summary(lm(n20 ~ lag(n20, n = 1) + lag(n20, n = 2)))


### Mini-investigation
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
                     "НЕТ ОТВЕТА" )) %>% 
  mutate(type = ifelse(type == "город", 1, 0),
         sat = ifelse(sat == "ПОЛНОСТЬЮ УДОВЛЕТВОРЕНЫ", 1, 0),
         sex = ifelse(sex == "МУЖСКОЙ", 1, 0),
         eduLowCom = ifelse(edu %in% levels(h2$edu)[1:3], 1, 0),
         eduCom = ifelse(edu == "законченное среднее образование", 1, 0),
         eduComSpec = ifelse(edu == "законченное среднее специальное образование", 1, 0),
         eduHigh = ifelse(edu == "законченное высшее образование и выше", 1, 0))
h2$edu <- NULL

levels(h_rlms$status)
levels(h_rlms$rj1.1.1)
levels(h_rlms$r_diplom)

h3 <- h_rlms %>% 
  select(wage = rj13.2,
         age = rh6,
         sex = rh5,
         edu = r_diplom,
         type = status,
         sat = rj1.1.1) %>% 
  mutate(age = 2013 - age) %>% 
  filter(type %in% levels(h_rlms$status)[1:2]) %>% 
  filter(sat %in% levels(h_rlms$rj1.1.1)[1:2]) %>% 
  filter(edu %in% levels(h_rlms$r_diplom)[1:6]) %>% 
  mutate(type = ifelse(type == "город", 1, 0),
         sat = ifelse(sat == "ПОЛНОСТЬЮ УДОВЛЕТВОРЕНЫ", 1, 0),
         sex = ifelse(sex == "МУЖСКОЙ", 1, 0),
         eduLowCom = ifelse(edu %in% levels(h_rlms$r_diplom)[1:3], 1, 0),
         eduCom = ifelse(edu == "законченное среднее образование", 1, 0),
         eduComSpec = ifelse(edu == "законченное среднее специальное образование", 1, 0),
         eduHigh = ifelse(edu == "законченное высшее образование и выше", 1, 0))
h3$edu <- NULL

## Попытка 1

# 1
max(h3$age)

# 2
sum(is.na(h3$wage))

# 3
h4 <- na.omit(h3)
hist(h4$wage)

# 4
# Возраст в годах - вторая
h4 %>% 
  ggplot(aes(age))+
  geom_histogram()+
  facet_grid(~sex)

# Доход в т.р. - первая
h4 %>% 
  ggplot(aes(wage/1000))+
  geom_histogram()+
  facet_grid(~sex)

# 5
m5 <- lm(wage ~ age + sex + eduCom + eduComSpec + eduHigh + sat + type, 
         data = h4)
summary(m5)

# 9
sqrt(vcovHC(m5)[8,8])
