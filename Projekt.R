install.packages("sf")  # jeśli nie masz jeszcze pakietu; wystarczy zainstalować raz i odtwarzać z biblioteki
library(sf)     # to trzeba zawsze ładować
install.packages("spdep")
library(spdep)
library(dplyr)

setwd ("C:/Users/mateu/Desktop/STUDIA MAGISTERSKIE/2 semestr/ekonometria przestrzenna/projekt/Ekonometria_Przestrzenna/wojewodztwa.shp")

dane <- st_read ("wojewodztwa.shp")

print (dane)

colnames(dane)     