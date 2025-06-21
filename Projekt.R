#***Projekt zaliczeniowy - Ekonometria Przestrzenna***#
  #Matueusz Pałczyński
  #Kacper Bareja s206756
  #Jakub Zator s202288
  #Patryk Piotrowski 

#Potrzebne pakiety do zainstalowania:

install.packages("sf")  # jeśli nie masz jeszcze pakietu; wystarczy zainstalować raz i odtwarzać z biblioteki
install.packages("spdep")
install.packages("readxl")
install.packages("spatialreg")
install.packages("naniar")
install.packages("tibble")
install.packages("tidyr")
install.packages("e1071")
install.packages("corrplot")

#pakiety do załadowania:

library(sf)
library(spdep)
library(dplyr)
library(ggplot2)
library(spatialreg)
library(readxl)
library(naniar)
library(tibble)
library(tidyr)
library(e1071)
library(corrplot)

# setwd ("C:/Users/mateu/Desktop/STUDIA MAGISTERSKIE/2 semestr/ekonometria przestrzenna/projekt/Ekonometria_Przestrzenna")


# tworzenie macierzy sąsiedztwa

woj_shp <- st_read ("wojewodztwa.shp")
dane_model3 <- read.csv("model3.csv", sep=";", header=TRUE)
mapa_dane <- merge(woj_shp, dane_model3, by.x = "JPT_NAZWA_", by.y = "JPT_NAZWA_")

str(mapa_dane$Samobojstwa)  # Sprawdzenie struktury
unique(mapa_dane$Samobojstwa)  # Podejrzenie unikalnych wartości
sum(is.na(mapa_dane$Samobojstwa))  # Sprawdzenie liczby wartości NA


nb <- poly2nb (woj_shp)

w<- nb2mat (nb, style = "W", zero.policy = TRUE)
print (w)

write.csv (w, "macierz_sasiedztwa_wojewodztwa.csv", row.names = FALSE)

st_geometry(woj_shp)

nb1<- poly2nb(woj_shp)
nb2 <- nblag (nb1, 2) [[2]]

w2<- nb2mat (nb2, zero.policy = TRUE)

write.csv(w2, "macierz_sasiedztwa_drugiego_rzędu_wojewodztwa.csv", row.names = FALSE)

w_norm <- w / rowSums(w)
print (w_norm)

write.csv (w_norm, "macierz_sąsiedztwa_województwa_std.csv", row.names = FALSE)


#Wizualizacja danych na mapach
ggplot(data = mapa_dane) + 
  geom_sf(aes(fill = Wynagrodzenie)) +
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(title = "Średnie Wynagrodzenie w województwach w 2023")


#Testy autokorelacji
dane <- read.csv("model3.csv", sep=";", header=TRUE)
mapa_dane <- merge(woj_shp, dane, by.x = "JPT_NAZWA_", by.y = "JPT_NAZWA_")

nb <- poly2nb (woj_shp)

lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

colnames (woj_shp)
print (woj_shp$JPT_NAZWA_)

all.equal(woj_shp$JPT_NAZWA_, dane$JPT_NAZWA_)

#Test Morana I
moran.test(dane$prod_mleka, lw)

#wyres rozrzutu Morana

moran.plot(dane$prod_mleka, lw, labels = FALSE, pch = 20,
           xlab = "Produkcja mleka", 
           ylab = "Przestrzenne opóźnienie produkcji mleka")

#Local moran (lisa)

local_moran <- localmoran(dane$prod_mleka, lw)

dane$Ii <- local_moran[, 1]
dane$P.Ii <- local_moran[, 5]
print(dane)

#Globalna statystyka Geary's C

geary.test(dane$prod_mleka, lw)

#Lokalna statystyka Geayrego C

local_geary <- localG(x= dane$prod_mleka, listw= lw,
zero.policy = TRUE)

print (dane)

nsim <- 999
sim <- replicate(nsim, localG(sample(x = dane$bary), lw, zero.policy = TRUE))
pvals <- rowMeans(abs(sim) >= abs(local_geary))

dane$geary_local <- as.numeric(local_geary)
dane$geary_p <- pvals

print(dane)


#konwertowanie danych do numerycznych
dane <- dane %>%
  mutate(
    prod_mleka = as.numeric(gsub(",", ".", prod_mleka)),
    Ceny_nieruchomosci = as.numeric(gsub(",", ".", Ceny_nieruchomosci)),
    Samobojstwa = as.numeric(gsub(",", ".", Samobojstwa)),
    Malzenstwa = as.numeric(gsub(",", ".", Malzenstwa)),
    Rozwody = as.numeric(gsub(",", ".", Rozwody)),
    Dochodnaosobe = as.numeric(gsub(",", ".", Dochodnaosobe)),
    Wynagrodzenie = as.numeric(gsub(",", ".", Wynagrodzenie)),
    bary = as.numeric(gsub(",", ".", bary)),
    Bezrobotni = as.numeric(gsub(",", ".", Bezrobotni)),
    Przemocdomowa = as.numeric(gsub(",", ".", Przemocdomowa)),
    Pow_rol = as.numeric(gsub(",", ".", Pow_rol)),
    Bydło = as.numeric(gsub(",", ".", Bydło)),
    Absolwenci = as.numeric(gsub(",", ".", Absolwenci)),
    Zgony = as.numeric(gsub(",", ".", Zgony)),
    Skup_bydla = as.numeric(gsub(",", ".", Skup_bydla)),
    Skup_mleka = as.numeric(gsub(",", ".", Skup_mleka)),
    Pow_rol = as.numeric(gsub(",", ".", Pow_rol)),
  )

dane_std <- dane %>% mutate (across (where(is.numeric), scale))
print (dane_std)

mapa_dane_std <- woj_shp %>%
  left_join(dane_std, by = c("JPT_NAZWA_" = "JPT_NAZWA_"))

neighbors <- poly2nb (woj_shp)
W.listw <- nb2listw (neighbors, style = "W")


#model statystyczny

model_stat <- lm(
  prod_mleka~
    Bydło+
    Skup_mleka+
    Lasy+
    Wynagrodzenie,
  
  data = dane_std)

summary(model_stat)


#test morgana
moran_test <- moran.test(residuals(model_stat), W.listw, zero.policy = TRUE)
print(moran_test)

#LM test
lm.LMtests(model_stat1, W.listw, test = "all", zero.policy = TRUE)

#4. Wstępna analiza danych (analiza opisowa)
# Braki danych
colSums(is.na(dane)) #przygotowana liczba braków w każdej kolumnie

# wykrycie obserwacji odstających (IQR)
out_iqr <- function(x) {
  which(x < quantile(x, 0.25, na.rm = TRUE) - 1.5 * IQR(x, na.rm = TRUE) |
          x > quantile(x, 0.75, na.rm = TRUE) + 1.5 * IQR(x, na.rm = TRUE))
}
numeric_data <- dane %>% dplyr::select(where(is.numeric))

# Zastosowanie funkcji do każdej zmiennej
outliers <- lapply(numeric_data, out_iqr)

# wyniki
outliers_df <- tibble::tibble(
  Zmienna = names(outliers),
  Odstające_obs = sapply(outliers, function(x) {
    if (length(x) == 0) {
      "Brak"
    } else {
      paste(x, collapse = ", ")}}))
print(outliers_df, n = Inf)

# Obliczenie podstawowych statystyk opisowych 

numeric_data <- dane %>% dplyr::select(where(is.numeric))
statystyki <- data.frame(
  Zmienna = names(numeric_data),
  Srednia = sapply(numeric_data, mean, na.rm = TRUE),
  Mediana = sapply(numeric_data, median, na.rm = TRUE),
  Minimum = sapply(numeric_data, min, na.rm = TRUE),
  Maksimum = sapply(numeric_data, max, na.rm = TRUE),
  Odch_std = sapply(numeric_data, sd, na.rm = TRUE),
  Asymetria = sapply(numeric_data, skewness, na.rm = TRUE)
)
print(statystyki, row.names = FALSE)

# Analiza korelacji między zmiennymi ilościowymi

# Wybór tylko zmiennych ilościowych (numerycznych)
numeric_data <- dane %>% dplyr::select(where(is.numeric))

# Obliczenie macierzy korelacji Pearsona
cor_matrix <- cor(numeric_data, use = "complete.obs", method = "pearson")
cor_matrix_rounded <- round(cor_matrix, 2)

# 4. Wyświetlenie macierzy korelacji oraz wizualizacja korelacji w formie mapy ciepła
print(cor_matrix_rounded) #wyniki macierzy
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8, tl.col = "black") #mapa ciepła

# Wykresy: histogramy, wykresy rozrzutu, gęstości 

# Wybór zmiennych liczbowych
numeric_data <- dane %>% select(where(is.numeric))

# Histogramy
numeric_data %>%
  pivot_longer(cols = everything(), names_to = "Zmienna", values_to = "Wartosc") %>%
  ggplot(aes(x = Wartosc)) +
  geom_histogram(bins = 15, fill = "steelblue", color = "black") +
  facet_wrap(~Zmienna, scales = "free", ncol = 4) +
  labs(title = "Histogramy zmiennych ilościowych", x = "Wartość", y = "Liczba obserwacji") +
  theme_minimal()

# Wykresy gęstości
numeric_data %>%
  pivot_longer(cols = everything(), names_to = "Zmienna", values_to = "Wartosc") %>%
  ggplot(aes(x = Wartosc)) +
  geom_density(fill = "skyblue", alpha = 0.6) +
  facet_wrap(~Zmienna, scales = "free", ncol = 4) +
  labs(title = "Wykresy gęstości zmiennych", x = "Wartość", y = "Gęstość") +
  theme_minimal()

# Wykresy rozrzutu względem produkcji mleka
dane_long <- dane %>%
  select(prod_mleka, where(is.numeric)) %>%
  pivot_longer(
    cols = -prod_mleka,
    names_to = "Zmienna",
    values_to = "Wartosc"
  )

# Wykres rozrzutu z linią trendu
ggplot(dane_long, aes(x = Wartosc, y = prod_mleka)) +
  geom_point(color = "steelblue", size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  facet_wrap(~Zmienna, scales = "free_x", ncol = 4) +
  labs(title = "Wykresy rozrzutu względem produkcji mleka",
       x = "Wartość zmiennej",
       y = "Produkcja mleka") +
  theme_minimal()

# 6. Modelowanie ekonometryczne
# Budowa modelu przestrzennego: SAR

nb <- poly2nb(woj_shp)
W.listw <- nb2listw(nb, style = "W", zero.policy = TRUE)

# Model SAR
model_sar <- lagsarlm(
  prod_mleka ~
    Bydło +
    Skup_mleka +
    Lasy +
    Wynagrodzenie,
  data = dane_std,
  listw = W.listw,
  zero.policy = TRUE
)
summary(model_sar)

# Model SEM
model_sem <- errorsarlm(
  formula = prod_mleka ~ Bydło + Skup_mleka + Lasy + Wynagrodzenie,
  data = dane_std,
  listw = W.listw,
  zero.policy = TRUE
)

# Podsumowanie wyników modelu SEM
summary(model_sem)


# Model SAC (Spatial Durbin Model) – rozszerzenie SAR
model_sac <- lagsarlm(
  formula = prod_mleka ~ Bydło + Skup_mleka + Lasy + Wynagrodzenie,
  data = dane_std,
  listw = W.listw,
  type = "mixed",     
  zero.policy = TRUE
)

# Wyświetlenie wyników
summary(model_sac)



