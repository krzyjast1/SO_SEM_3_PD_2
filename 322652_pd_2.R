#Import danych historycznych z plików csv; importuję wyłącznie kursy zamknięcia;
gbppln <- read.csv("gbppln_d.csv", header = T)$Zamkniecie
gbpusd <- read.csv("gbpusd_d.csv", header = T)$Zamkniecie

#Przygotowanie danych do podstawienia do wzorów z wykładu na prostą regresji liniowej
sr_aryt_gbppln <- mean(gbppln)
sr_aryt_gbpusd <- mean(gbpusd)
sum_il <- sum((gbppln * gbpusd)/length(gbppln))
il_srednich <- mean(gbppln) * mean(gbpusd)

licznik <- sum_il - il_srednich

sum_x_kwadrat <- sum(gbpusd^2)
kwadrat_sr_x <- mean(gbpusd)^2

mianownik <- (sum_x_kwadrat/length(gbpusd)) - kwadrat_sr_x

a <- licznik / mianownik

b <- sr_aryt_gbppln - a * sr_aryt_gbpusd

# Wyświetl równanie regresji
cat("y = ", a, "x + ", b)


#Współczynnik determinacji
linear_model <- lm(gbppln ~ gbpusd)
summary(linear_model)$r.squared

#Wykres
plot(gbppln ~ gbpusd, main="Wykres zależności kursu GBPPLN od GBPUSD", cex=0.75)
abline(linear_model, col='blue', lwd = 1.5)

# WNIOSKI:
# 1. Wg modelu regresji liniowej wzrost kursu GBPUSD o 1 spowoduje spadek kursu GBPPLN o 0,22802981
# 2. Współczynnik determinacji wynosi 0,01362313 co świadczy o słabym 
#    przybliżeniu zależności pomiędzy kursami przez liniowy model regresji
#

