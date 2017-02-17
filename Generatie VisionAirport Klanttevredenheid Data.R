#selecteer benodigde kolommen om klanttevredenheid te berekenen en schrijf naar nieuwe tabel
library(lubridate)
klanttevredenheid <- subset(sim, month(Datum) == 02, select = c("Vluchtnr", "Datum", "Bezetting", "Vertraging", "Gatewissel", "Terminal", "Destcode", "Planuur"))

#creeer kolommen
klanttevredenheid <- mutate(klanttevredenheid, Operatie = 0)
klanttevredenheid <- mutate(klanttevredenheid, Faciliteiten = 0)
klanttevredenheid <- mutate(klanttevredenheid, Shops = 0)

#bereken klanttevredenheid Operatie
klanttevredenheid <- mutate(klanttevredenheid, Operatie = 8 - 0.5 * Bezetting / 100 - 0.005 * Vertraging - 1 * Gatewissel + rnorm(length(Operatie), 0, 0.5))
klanttevredenheid <- mutate(klanttevredenheid, Operatie = round(Operatie, digits = 1))

#bereken klanttevredenheid Faciliteiten
klanttevredenheid <- mutate(klanttevredenheid, Faciliteiten = 8 - 3 * as.integer(Terminal == "A") - 1.5 * as.integer(Terminal == "B") + rnorm(length(Faciliteiten), 0, 0.5))
klanttevredenheid <- mutate(klanttevredenheid, Faciliteiten = round(Faciliteiten, digits = 1))

#bereken klanttevredenheid Shops
#passagiers naar Alicante (ALC), zijn meer ontevreden
#winkels zijn dicht tussen 23:00 en 6:00

klanttevredenheid <- mutate(klanttevredenheid, Shops = rnorm(length(Shops), 6, 1) - 1 * as.integer(Destcode == "ALC"))
klanttevredenheid$Shops[klanttevredenheid$Planuur > 23 | klanttevredenheid$Planuur < 6] <- NA
klanttevredenheid <- mutate(klanttevredenheid, Shops = round(Shops, digits = 1))
