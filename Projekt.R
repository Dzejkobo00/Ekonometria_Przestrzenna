install.packages("sf")  # jeśli nie masz jeszcze pakietu; wystarczy zainstalować raz i odtwarzać z biblioteki
library(sf)     # to trzeba zawsze ładować
install.packages("spdep")
library(spdep)
library(dplyr)
library(ggplot2)


setwd ("C:/Users/mateu/Desktop/STUDIA MAGISTERSKIE/2 semestr/ekonometria przestrzenna/projekt/Ekonometria_Przestrzenna")




# tworzenie macierzy sąsiedztwa


woj_shp <- st_read ("wojewodztwa.shp")

dane <- read.csv("model.csv", sep=";", header=TRUE)

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

ggplot(data = mapa_dane_2023) + 
  geom_sf(aes(fill = Samobojstwa)) +
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(title = "Samobójstwa w województwach w 2023")

