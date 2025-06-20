install.packages("sf")  # jeśli nie masz jeszcze pakietu; wystarczy zainstalować raz i odtwarzać z biblioteki
library(sf)     # to trzeba zawsze ładować
install.packages("spdep")
install.packages ("readxl")
library(spdep)
library(dplyr)
library(ggplot2)
library(spatialreg)
library(readxl)

setwd ("C:/Users/mateu/Desktop/STUDIA MAGISTERSKIE/2 semestr/ekonometria przestrzenna/projekt/Ekonometria_Przestrzenna")


# tworzenie macierzy sąsiedztwa


woj_shp <- st_read ("wojewodztwa.shp")

kartogram <- read.csv("model3.csv", sep=";", header=TRUE)

mapa_dane <- merge(woj_shp, dane, by.x = "JPT_NAZWA_", by.y = "JPT_NAZWA_")

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

mapa_dane_2023 <-mapa_dane %>%
  filter (Okres == 2023)

ggplot(data = mapa_dane) + 
  geom_sf(aes(fill = Skup_mleka)) +
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(title = "Przemoc domowa w województwach w 2023")


#Testy autokorelacji

woj_shp <- st_read ("wojewodztwa.shp")

dane <- read.csv("model3.csv", sep=";", header=TRUE)

mapa_dane <- merge(woj_shp, dane, by.x = "JPT_NAZWA_", by.y = "JPT_NAZWA_")

nb <- poly2nb (woj_shp)

lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

colnames (woj_shp)
print (woj_shp$JPT_NAZWA_)

all.equal(woj_shp$JPT_NAZWA_, dane$JPT_NAZWA_)

#Test Morana I
data$investments_outlays_pc<- gsub(",", ".", data$investments_outlays_pc)
data$investments_outlays_pc <- as.numeric(data$investments_outlays_pc)





moran.test(dane$hotele, lw)

#wyres rozrzutu Morana

moran.plot(dane$Skup_mleka, lw, labels = FALSE, pch = 20,
           xlab = "rozwody", 
           ylab = "Przestrzenne opóźnienie rozwodów")

#Local moran (lisa)

local_moran <- localmoran(dane$prod_mleka, lw)

dane$Ii <- local_moran[, 1]
dane$P.Ii <- local_moran[, 5]
print(dane)

#Globalna statystyka Geary's C

geary.test(dane$Pow_rol, lw)

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
  Pow_rol~
    Zwierz_lowna+
    prod_mleka,

  data = dane_std)

summary(model_stat)

model_best_stat <- step(model_stat, direction = "backward")

summary(model_best_stat)



#test morgana
moran_test <- moran.test(residuals(model_best_stat), W.listw, zero.policy = TRUE)
print(moran_test)

#LM test

lm.LMtests(model_best_stat, W.listw, test = "all", zero.policy = TRUE)










