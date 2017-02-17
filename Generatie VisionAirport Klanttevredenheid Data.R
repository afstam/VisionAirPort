#selecteer benodigde kolommen om klanttevredenheid te berekenen en schrijf naar nieuwe tabel

klanttevredenheid <- subset(sim, month(Datum) == 02, select = c("Vluchtnr", "Datum", "Bezetting", "Vertraging", "Gatewissel", "Terminal", "Destcode"))

#creeer kolommen
klanttevredenheid <- mutate(klanttevredenheid, Operatie = 0)
klanttevredenheid <- mutate(klanttevredenheid, Faciliteiten = 0)
klanttevredenheid <- mutate(klanttevredenheid, Shops = 0)

#bereken klanttevredenheid Operatie
klanttevredenheid <- mutate(klanttevredenheid, Operatie = 8 - 0.5 * Bezetting / 100 - 0.005 * Vertraging - 1 * Gatewissel + rnorm(length(Operatie), 0, 0.5))
klanttevredenheid <- mutate(klanttevredenheid, Operatie = round(Operatie, digits = 1))

#bereken klanttevredenheid Faciliteiten
# 8 - 3*TerminalA - 1.5*TerminalB + Norm(0,0.5)


