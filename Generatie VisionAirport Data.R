# Initialiseren ######################################################################################

# Selecteer bronmap
 setwd("D:\\data\\ast21252\\Documents\\201701 VisionWorks Academy XI\\Eindcasus Vision Airport")
#setwd("D:\\data\\ITH21266\\Documents\\VisionAirPort")

# Packages
library(chron)
library(sn)
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(lubridate)
library(caret)


# Functies ######################################################################################

# funweekdag: date -> integer
# de dag van de week van de ingevoerde datum wordt vertaald in een getal van 1 tot 7
# hierbij is maandag 1 en zondag 7
funweekdag <- function(datum) {
  df <- data.frame(naam = c("ma","di","wo","do","vr","za","zo"), cijfer = 1:7)
  return(df[df$naam == weekdays(datum,T),2])
}

# funmaand: integer -> character
funmaand <- function(maand) {
  df <- data.frame(int = 1:12, naam = c("jan","feb","mar","apr","mei","jun","jul","aug","sep","okt","nov","dec"))
  return(df[df$int == maand,2])
}

# onesample: vector -> element
# van de ingevoerde vector wordt 1 willekeurig element geselecteerd
# dit is een verbetering op de standaard functie sample, omdat de functie ook werkt als de vector 1 element bevat
onesample <- function(x) {
  if (length(x) == 1) { return(x) } else { return(sample(x,1)) }
}

# toampm: integer vector -> string vector
# vertaalt de tijd in dag-minuten (0 tot 1439) in een AM/PM formaat
toampm <- function(v) {
  r <- rep(NA, length(v))
  uur <- v %/% 60
  pm <- uur %in% 12:23
  am <- uur %in% 0:11
  uur <- uur %% 12
  uur[uur == 0] <- 12
  min <- paste0("0",v %% 60)
  min <- substr(min, nchar(min)-1, nchar(min))
  r[am] <- paste0(uur[am],":",min[am]," AM")
  r[pm] <- paste0(uur[pm],":", min[pm]," PM")
  r <- factor(r)
  return(r)
}



# Data inlezen ######################################################################################

# Bronbestanden
banen <- read.csv2("data_banen.csv")
general <- read.csv2("data_general.csv")
luchthaven <- read.csv2("data_luchthavens.csv")
maatschappijen <- read.csv2("data_maatschappijen.csv")
routes <- read.csv2("data_routes.csv")
vliegtuigtype <- read.csv2("data_vliegtuigen.csv")
vracht <- read.csv2("data_vracht.csv")
weer <- read.csv2("data_weer.csv")

# Aanpassingen data formaat
weer$Datum <- as.Date(weer$Datum,"%d-%m-%Y")
weer$RH[weer$RH < 0] <- 0
weer$nat <- weer$RH > 50
weer$vorst <- weer$TG < 0
weer$sneeuw <- weer$TG < -10 & weer$RH > 15
weer$extreemwind <- weer$FXX > 200

routes <- dplyr::rename(routes, Plangate = Gate, Planterminal = Terminal)

# Enkele levels in een vector opslaan
gatelvls <- c(levels(routes$Plangate),"C8")
terminallvls <- c(levels(routes$Planterminal), "F", "G")
richtinglvls <- c("A","D","S")
luchthavens <- c(levels(luchthaven$Luchthavencode),"VAP")
airlines <- unique(c(levels(maatschappijen$IATA), levels(maatschappijen$ICAO)))

# Airlines voor wie VisionAirPort een hub is
home <- c("KL","HV","OR","CAI","MPH")

# Genereren vluchtnummers ######################################################################################

# Vluchten krijgen een willekeurig vluchtnummer toegewezen
# Hierbij krijgen vluchten binnen Europa een vluchtnummer tussen 0 en 999
# Intercontinentale vluchten krijgen een vluchtnummer tussen 1000 en 4999
# Dit onderscheid is niet per se ontleed aan de werkelijke praktijk
# Vluchtnummers boven de 5000 zijn bedoeld voor vluchten die worden gedeeld tussen airlines (komen niet in deze dataset voor)

# Vluchten vanaf een "hub" van een airline krijgen vaak een oneven vluchtnummer, en vluchten naar een hub een even
# VisionAirPort is een hub van 4 airlines: KLM, Transavia, TuiFly, en Corendon
# Omdat (meestal) begonnen wordt met de arriverende vlucht, is dat voor deze 4 airlines een vlucht náár de hub
# Deze 4 airlines beginnen dus op een even vluchtnummer, de rest op een oneven vluchtnummer

# Kolom toevoegen aan df
routes <- mutate(routes, Vluchtnr = 0)

# For-loop per airline (i = Airlinecode)
for(i in levels(routes[,1])) {
  # tijdelijk df met vluchten van deze ene airline
  sub <- filter(routes, Airlinecode == i)
  # als deze airline maar 1 vlucht heeft: geef random vluchtnummer
  if(nrow(sub) == 1) {
    sub[sub$Continent=="Eur","Vluchtnr"] <- floor(runif(1,min=10,max=495)) * 2 + 1
    sub[sub$Continent!="Eur","Vluchtnr"] <- floor(runif(1,min=495,max=2495)) * 2 + 1
  # meer vluchten? voeg dan controle toe dat gegenereerde vluchtnummers niet te dicht opeen zitten
  # sorteer van klein naar groot en check dat opeenvolgende vluchtnummers minstens 8 verschillen
  # door de while-functie wordt geprobeerd tot een willekeurige set is gegenereerd die voldoet
  } else {
    while(min(diff(sub[order(sub$Vluchtnr),"Vluchtnr"])) <= 7) {
      sub[sub$Continent=="Eur","Vluchtnr"] <- floor(runif(nrow(sub[sub$Continent=="Eur",]),min=10,max=495)) * 2 + 1
      sub[sub$Continent!="Eur","Vluchtnr"] <- floor(runif(nrow(sub[sub$Continent!="Eur",]),min=495,max=2995)) * 2 + 1
    }
  }
  # de gegenereerde vluchtnummers worden teruggezet in het bron-dataframe
  routes[routes$Airlinecode == i, "Vluchtnr"] <- sub$Vluchtnr
}
rm(sub)

# Basisnr is het onbewerkte vluchtnummer
# Vliegtuig combineert de airlinecode daarmee als identificatie voor later
routes <- mutate(routes,
                 Basisnr = Vluchtnr,
                 Vliegtuigcode = paste0("V", Airlinecode, Basisnr))

# Sla alle vliegtuigen op
vliegtuigcodes <- unique(routes$Vliegtuigcode)

# Aanpassing voor de airlines voor wie VisionAirport een hub is
routes$Vluchtnr[!(routes$Airlinecode %in% home)] <- routes$Vluchtnr[!(routes$Airlinecode %in% home)] - 1




# Bepalen vluchtperiode ######################################################################################

# Veruit de meeste vluchten worden het hele jaar door gevlogen (periode = 'j')
# Sommige worden alleen in de zomer gevlogen (periode = 'z')
# Deze zomervluchten worden actief in april, mei, of juni en inactief in augustus, september, of oktober
# Deze maanden zijn inclusief, dus 'inactief in augustus' betekent 'vliegt niet meer vanaf september'
# De startmaand en eindmaand wordt per route willekeurig gekozen
# Eerst wordt overal jan - dec ingevoerd, daarna wordt dit voor de zomervluchten overschreven

aantalzomer <- nrow(filter(routes, Periode == "z"))
routes <- mutate(routes, start = 1, eind = 12)
routes[routes$Periode == "z", c("start","eind")] <- c(sample(c(4,5,6),aantalzomer,replace=T),sample(c(8,9,10),aantalzomer,replace=T))

rm(aantalzomer)



# Opstellen planning ######################################################################################

# Tabelstructuren
vars.plantabel <- c("Airlinecode", "Destcode", "Continent", "Zomerdrukte", "Vliegtuigtype", "Planterminal", "Plangate", "Vluchtnr", "Basisnr", "Vliegtuigcode", "start", "eind","Richting","Plantijd","Dag")
str.plantabel <- structure(list(Airlinecode = factor(levels=airlines),
                                Destcode = factor(levels=luchthavens),
                                Continent = factor(levels=levels(routes$Continent)),
                                Zomerdrukte = integer(),
                                Vliegtuigtype = factor(levels=levels(vliegtuigtype$IATA)),
                                Planterminal = factor(levels=terminallvls),
                                Plangate = factor(levels=gatelvls),
                                Vluchtnr = integer(),
                                Basisnr = integer(),
                                Vliegtuigcode = factor(levels=vliegtuigcodes),
                                start = integer(),
                                eind = integer(),
                                Richting = factor(levels=richtinglvls),
                                Plantijd = integer(),
                                Dag = integer()), 
                           class = "data.frame")

# Dit stuk van de code genereert de plantijden voor elke routevlucht
# Dit proces gebeurt per gate afzonderlijk

# Sommige routes worden 2x op een dag gevlogen. Deze vluchten worden als eerst ingepland, aan het begin van de dag
# In de kolom 'Mintijd' wordt de vroegste tijd genoteerd waarop de route dan voor de 2e keer kan worden gevlogen

# Sommige vluchten worden om de dag gevlogen. Deze vluchten hebben een 'delay', dwz. de dag van de 2 dat ze vliegen (0 of 1)
# Dit is zo goed mogelijk eerlijk verdeeld over de 2 mogelijke dagen (handmatig, in het bronbestand)
# Elke dag-0 vlucht wordt willekeurig gekoppeld aan een dag-1 vlucht die op hetzelfde moment de dag erna vertrekt
# De planning herhaalt zich dus elke 2 dagen

# Als de dubbele vluchten allemaal 1 keer zijn ingepland, gaat het proces verder met de overige vluchten
# Dit zijn de enkele vluchten, de helft van de halve vluchten, en een herhaling van de dubbele vluchten
# Er wordt gekeken welke vluchten kunnen worden gevlogen (dat het niet te vroeg is voor een eventuele herhaling)
# Van deze vluchten wordt willekeurig 1 ingepland

# De eerste vlucht die op een dag vertrekt, arriveert de dag ervoor

# Als een vlucht bij een gate is gearriveerd, volgt de 'Turnaround' waarin het vliegtuig weer wordt klaargemaakt voor vertrek
# VisionAirport is erg efficiënt: de turnaround duurt 45 min voor Europese vluchten en 75 minuten voor intercontinentale vluchten

# Nadat een vlucht van een gate is vertrokken, wordt een bufferperiode ingepland tot de volgende arriverende vlucht
# Deze periode is minstens 10 minuten en krijgt daarbij een bufferfactor toegevoegd
# Deze buffer volgt deze formule: buffer = abs(c-0.5*d) / (0.5*d) * (max-min) + min
# Hierbij is c een counter van de hoeveelste vlucht zojuist vertrokken is, en d het totaal aantal vluchten dat van de gate vertrekt
# Er is een verborgen parameter g, de gemiddelde buffer die op een dag gerealiseerd moet worden om de dag mooi op te vullen
# Er is een bandbreedte rondom g, waarvan min en max de grenzen zijn

# Een voorbeeld: een gate met 9 vluchten en een buffer van 10 tot 50 minuten
# curve(abs(x-0.5*8) / (0.5*8) * (10-2) + 2, 0, 8)

# Er zitten een aantal willekeurige invloeden in het planningsproces, om het niet al te voorspelbaar te maken
# Het zou allemaal binnen 23.5 uur moeten passen, maar soms bestrijkt de planning een periode die langer is
# Daarom staat hier een while-statement: we proberen net zo lang tot de planning binnen 23.5 uur past (max. 5 pogingen)
# Omdat de planning met stappen van 5 minuten wordt gegenereerd, komt 23.5 uur neer op 282 x 5 mins
a <- 0
planning <- data.frame(Plantijd = c(0,300))

while ((max(planning$Plantijd) - min(planning$Plantijd)) > 282 & a < 5) {

a <- a + 1
print(paste0("Iteratie ",a))

# Bouw lege dataframes op
planning <- str.plantabel
log <- structure(list(gate = factor(levels=gatelvls),
                      vlucht = integer(),
                      richting = factor(levels=richtinglvls),
                      tijd = integer(),
                      buffer = numeric()),
                 class = "data.frame")
gates <- structure(list(Gate = factor(levels=gatelvls),
                        d = integer(),
                        g = integer(),
                        min = integer(),
                        max = integer()),
                   class = "data.frame")

# Er zijn geen vluchten ingepland op gate C8. Dit om altijd een vrije gate te hebben voor eventuele gatewissels
# Deze gate wordt nu los ingevoerd in dataframe 'gates'
gates[1,] <- list("C8",0L,0L,0L,0L)

for (i in levels(routes$Plangate)) {
  # sub.gate is een selectie van routes met de huidige gate
  sub.gate <- filter(routes,Plangate == i)
  sub.gate <- transform(sub.gate,
                        Mintijd = 0,
                        Klaar = 0,
                        Duur = ceiling(Duur * 60 / 5),
                        Turnaround = ceiling(Turnaround * 60 / 5),
                        Turnaroundtotaal = ceiling(Turnaroundtotaal * 60 / 5),
                        Indeling = ave(Vluchten, Delay, FUN = function(x) rank(x, ties.method = "first")))
  
  # Het aantal dubbele, enkele, en halve vluchten
  aantdubbel <- nrow(subset(sub.gate,Vluchten == 2))
  aantenkel <- nrow(subset(sub.gate,Vluchten == 1))
  aanthalf <- max(nrow(subset(sub.gate,Vluchten == 0.5 & Delay == 1)),nrow(subset(sub.gate,Vluchten == 0.5 & Delay == 0)))
  
  # De kolom 'Indeling' koppelt halve vluchten aan elkaar
  sub.gate$Indeling[sub.gate$Vluchten > 0.5] <- 0
  sub.gate$Indeling[sub.gate$Vluchten > 0.5] <- (max(sub.gate$Indeling)+1):(max(sub.gate$Indeling)+aantdubbel+aantenkel)
  
  # Sorteer de tabel zodat de dubbele vluchten bovenaan staan, aflopend gesorteerd op vluchtduur (dus langste vluchten eerst inplannen)
  sub.gate <- arrange(sub.gate,desc(Vluchten),desc(Duur))
  
  # Plansub is de tabel waarin ingeplande vluchten tijdelijk worden opgeslagen
  plansub <- str.plantabel
  
  # Dit zijn de parameters voor het berekenen van de bufferperiode
  d <- ceiling(sum(sub.gate$Vluchten)) - 1
  g <- floor((245 - sum(sub.gate$Turnaroundtotaal)) / (d+1)) - 2
  min <- max(0,g-20)
  max <- 2 * g - min
  gates[nrow(gates)+1,] <- list(i,as.integer(d),as.integer(g),as.integer(min),as.integer(max))
  
  # Tijd houdt de tijd bij (in stappen van 5 min, dus tussen 0 en 287)
  tijd <- 0
  # c telt het aantal vertrokken vluchten
  c <- 0
  
  # Inplannen van de 1e vlucht van dubbele routes
  if (aantdubbel > 0) {
    for (j in 1:aantdubbel) {
      # Haal vlucht op
      sub.vlucht <- sub.gate[j,]
      
      ### Arriverende vlucht
      
      # Als dit de eerste vlucht is:
      if (c == 0) {
        # Bewaar vlucht tot einde planning
        afsluiter <- sub.vlucht
        
      # Als dit niet de eerste vlucht is:
      } else {
        # Plan arriverende vlucht in
        sub.vlucht <- mutate(sub.vlucht,
                             Richting = "A",
                             Plantijd = tijd,
                             Dag = 0)
        plansub[nrow(plansub)+1,] <- sub.vlucht[,vars.plantabel]
        sub.vlucht$Dag <- 1
        plansub[nrow(plansub)+1,] <- sub.vlucht[,vars.plantabel]
        # Turnaround verstrijkt
        tijd <- tijd + sub.vlucht$Turnaround
        # Log
        log[nrow(log)+1,] <- list(i,c,"A",tijd, sub.vlucht$Turnaround)
      }
      
      # Tel 1 op bij vluchtnummer
      sub.vlucht$Vluchtnr <- sub.vlucht$Vluchtnr + 1
      
      ### Vertrekkende vlucht
      
      # Plan vertrekkende vlucht in
      sub.vlucht <- mutate(sub.vlucht,
                           Richting = "D",
                           Plantijd = tijd,
                           Dag = 0)
      plansub[nrow(plansub)+1,] <- sub.vlucht[,vars.plantabel]
      sub.vlucht$Dag <- 1
      plansub[nrow(plansub)+1,] <- sub.vlucht[,vars.plantabel]
      
      ### Sla informatie op in gate.sub voor de herhaling
      
      # select is een T/F vector die de rijen in sub.gate selecteert die horen bij de vlucht die we nu inplannen
      select <- (sub.gate$Basisnr == sub.vlucht$Basisnr & sub.gate$Airlinecode == sub.vlucht$Airlinecode)
      # Sla de minimale tijd op voor de 2e vlucht
      sub.gate$Mintijd[select] <- tijd + (sub.vlucht$Duur * 2) + 12
      # Verhoog vluchtnummer in sub.gate
      sub.gate$Vluchtnr[select] <- sub.gate$Vluchtnr[select] + 2
      sub.gate$Basisnr[select] <- sub.gate$Basisnr[select] + 2
      
      ### Buffertijd verstrijkt
      
      buffer <- abs(c-0.5*d)/(0.5*d)*(max-min) + min
      tijd <- tijd + 2 + floor(runif(1, 0.5*buffer, 1.25*buffer))
      
      # Log
      log[nrow(log)+1,] <- list(i,c,"D",tijd, buffer)
      # Update counter
      c <- c + 1
    }
  }
  
  # Alle dubbele routes zijn 1x ingepland, ga door met de rest
  for (j in 1:max(sub.gate$Indeling)) {
    # Zoek beschikbare vluchten
    beschikbaar <- unique(sub.gate[sub.gate$Mintijd <= tijd & sub.gate$Klaar == 0, "Indeling"])
    
    # Als er geen beschikbare vluchten zijn: laat tijd verstrijken tot er wel één is
    if(length(beschikbaar) < 1) {
      extratijd <- min(sub.gate[sub.gate$Klaar == 0, "Mintijd"]) - tijd
      tijd <- tijd + extratijd
      beschikbaar <- unique(sub.gate[sub.gate$Mintijd <= tijd & sub.gate$Klaar == 0, "Indeling"])
      log[nrow(log)+1,] <- list(i,c,"S",tijd, extratijd)
    }
    
    # Kies willekeurig 1 van de beschikbare vluchten
    indeling.huidig <- onesample(beschikbaar)
    
    # Haal informatie op
    sub.vlucht <- sub.gate[sub.gate$Indeling == indeling.huidig,]
    
    ### Arriverende vlucht
    
    # Als dit de eerste vlucht is:
    if (c == 0) {
      # Bewaar vlucht tot einde planning
      afsluiter <- sub.vlucht
      
      # Als dit niet de eerste vlucht is:
    } else {
      # Plan arriverende vlucht in
      sub.vlucht <- mutate(sub.vlucht,
                           Richting = "A",
                           Plantijd = tijd)
      if (nrow(sub.vlucht) == 1) {
        sub.vlucht$Dag <- 0
        plansub[nrow(plansub)+1,] <- sub.vlucht[,vars.plantabel]
        sub.vlucht$Dag <- 1
        plansub[nrow(plansub)+1,] <- sub.vlucht[,vars.plantabel]
      } else if (nrow(sub.vlucht) == 2) {
        sub.vlucht$Dag <- sub.vlucht$Delay
        plansub[(nrow(plansub)+1):(nrow(plansub)+2),] <- sub.vlucht[,vars.plantabel]
      } else {
        print("Fout bij het inplannen van vlucht. Printout relevante informatie:")
        print(summary(sub.vlucht))
      }
      # Turnaround verstrijkt
      tijd <- tijd + max(sub.vlucht$Turnaround)
      # Log
      log[nrow(log)+1,] <- list(i,c,"A",tijd, max(sub.vlucht$Turnaround))
    }
    
    # Tel 1 op bij vluchtnummer
    sub.vlucht$Vluchtnr <- sub.vlucht$Vluchtnr + 1
    
    ### Vertrekkende vlucht
    
    # Plan vertrekkende vlucht in
    sub.vlucht <- mutate(sub.vlucht,
                         Richting = "D",
                         Plantijd = tijd)
    if (nrow(sub.vlucht) == 1) {
      sub.vlucht$Dag <- 0
      plansub[nrow(plansub)+1,] <- sub.vlucht[,vars.plantabel]
      sub.vlucht$Dag <- 1
      plansub[nrow(plansub)+1,] <- sub.vlucht[,vars.plantabel]
    } else if (nrow(sub.vlucht) == 2) {
      sub.vlucht$Dag <- sub.vlucht$Delay
      plansub[(nrow(plansub)+1):(nrow(plansub)+2),] <- sub.vlucht[,vars.plantabel]
    } else {
      print("Fout bij het inplannen van vlucht. Printout relevante informatie:")
      print(summary(sub.vlucht))
    }
    
    ### Buffertijd verstrijkt
    
    buffer <- abs(c-0.5*d)/(0.5*d)*(max-min) + min
    tijd <- tijd + 2 + floor(runif(1, 0.5*buffer, 1.25*buffer))  
    
    # Log
    log[nrow(log)+1,] <- list(i,c,"D",tijd, buffer)
    # Update counter
    c <- c + 1
    # Noteer dat vlucht volledig ingedeeld is
    sub.gate$Klaar[sub.gate$Indeling == indeling.huidig] <- 1
  }
  
  # In principe moet er nog altijd 1 vlucht arriveren, maar voor de zekerheid checken we het even
  if (exists("afsluiter")) {
    # Plan laatste arriverende vlucht in
    sub.vlucht <- afsluiter
    sub.vlucht <- mutate(sub.vlucht,
                         Richting = "A",
                         Plantijd = tijd)
    if (nrow(sub.vlucht) == 1) {
      sub.vlucht$Dag <- 0
      plansub[nrow(plansub)+1,] <- sub.vlucht[,vars.plantabel]
      sub.vlucht$Dag <- 1
      plansub[nrow(plansub)+1,] <- sub.vlucht[,vars.plantabel]
    } else if (nrow(sub.vlucht) == 2) {
      sub.vlucht$Dag <- 1 - sub.vlucht$Delay
      plansub[(nrow(plansub)+1):(nrow(plansub)+2),] <- sub.vlucht[,vars.plantabel]
    } else {
      print("Fout bij het inplannen van vlucht. Printout relevante informatie:")
      print(summary(sub.vlucht))
    }
    rm(afsluiter)
  }
  
  # De vluchten zijn nu ingepland vanaf tijd = 0, dit is natuurlijk niet de bedoeling
  # De planning wordt eerst gecentreerd rond 12:00
  # Daarna wordt een willekeurige factor toegevoegd, een verschuiving naar voren of achteren met maximaal 45 mins
  shift <- 144 - floor((max(plansub$Plantijd) + min(plansub$Plantijd)) / 2) + onesample(-9:9)
  plansub$Plantijd <- plansub$Plantijd + shift
  
  # Voeg alle ingeplande vluchten van deze gate toe aan de planning
  planning <- rbind(planning, plansub)
}

rm(sub.gate,plansub,sub.vlucht,aantdubbel,aantenkel,aanthalf,beschikbaar,buffer,extratijd,c,d,g,max,min,shift,select,tijd,indeling.huidig,i,j)
}
rm(a)

# Het vluchtnummer en basisnummer zijn nu definitief en er wordt niet meer aan gerekend
# We kunnen dus de airlinecode er voor plakken
planning <- mutate(planning,
                   Basisnr = paste0(Airlinecode,Basisnr),
                   Vluchtnr = paste0(Airlinecode,Vluchtnr))

# Verschuif de planning zo, dat de laatste vlucht om 23:55 vertrekt
planning$Plantijd <- planning$Plantijd + 287 - max(planning$Plantijd)

# Bereken de daadwerkelijke tijd (dus niet in termen van 5mins)
planning$Plantijd <- planning$Plantijd * 5


# Grafieken van de verdeling over de dag
planuur <- planning$Plantijd %/% 60
plot(table(planuur))
rm(planuur)



# Vliegtuigcodes & vluchtnummers voor vrachtvluchten ############################################################################

vracht <- rbind(vracht,vracht,vracht)
vracht <- mutate(vracht, 
                 Vliegtuigcode = sample(2501:4990,nrow(vracht)) * 2 + as.integer(Airlinecode %in% home),
                 VluchtnrA = Vliegtuigcode - 1,
                 VluchtnrD = Vliegtuigcode)

vracht[,4] <- paste0("V", vracht$Airlinecode, vracht[,4])
for (i in 5:6) {
  vracht[,i] <- paste0(vracht$Airlinecode, vracht[,i])
}

vliegtuigcodes <- c(vliegtuigcodes, unique(vracht$Vliegtuigcode))
levels(planning$Vliegtuigcode) <- vliegtuigcodes



# Vliegtuigcodes voor General Aviation ############################################################################

general <- rbind(general,general,general,general,general)

luchthavens.eur <- as.character(luchthaven$Luchthavencode[luchthaven$Continent == "Eur" & luchthaven$Afstand.in.km < 1600])
general$Locatie <- as.character("VAP")
sel <- runif(nrow(general)) < 0.5
general$Locatie[sel] <- sample(luchthavens.eur,sum(sel),replace=T)

general <- data.frame(general[order(runif(nrow(general))),])
rownames(general) <- NULL
general$Vliegtuigcode <- paste0("VG",rownames(general))




# Alle vliegtuigen
vliegtuig.pax <- planning %>%
  dplyr::group_by(Vliegtuigcode) %>%
  dplyr::arrange(Plantijd) %>%
  dplyr::summarize(freq = n(),
                   ri = first(Richting),
                   type = first(Vliegtuigtype)) %>%
  merge(vliegtuigtype, by.x = "type", by.y="IATA", all.x = TRUE)

vliegtuig.vracht <- vracht %>%
  merge(vliegtuigtype, by.x = "Vliegtuigtype", by.y = "IATA", all.x = TRUE)



# Simulatie ######################################################################################

# De dagen waarvoor de simulatie draait
simdagen <- 1:nrow(weer)

# Zet willekeurig 39 vliegtuigen op non-actief
inactief <- routes$Vliegtuigcode[sample(1:nrow(routes),39)]
planning$Actief <- !(planning$Vliegtuigcode %in% inactief)

# Tabellen
vars.simtabel <- c("Datum","Vluchtnr","Richting","Airlinecode","Destcode","Continent","Zomerdrukte","Vliegtuigcode","Vliegtuigtype","Planterminal","Plangate","Terminal","Gate","Plantijd","Vertraging","Tijd","Baan","Bezetting","Capaciteit","Passagiers","MaxVracht","Vracht","Cancelled")
sim <- data.frame()

for(i in simdagen) {
  # Lees informatie in over deze dag uit dataframe `weer`
  sub.weer <- weer[i,]
  # Bepaal of dit een 0-dag of 1-dag is
  dag <- i %% 2
  
  # Als dit de eerste dag van de maand is:
  if(sub.weer$Dag == 1) {
    # Zet een routevliegtuig op actief
    if (length(inactief) > 0) {
      actief <- onesample(inactief)
      planning$Actief[planning$Vliegtuigcode == actief] <- T
      inactief <- inactief[inactief != actief]
    }
  }
  
  # We slaan de planning voor vandaag op in sub.planning
  sub.planning <- planning %>%
    merge(select(vliegtuig.pax,Vliegtuigcode,Capaciteit,Vracht), by = "Vliegtuigcode", all.x = T) %>%
    dplyr::rename(MaxVracht = Vracht) %>%
    arrange(Plantijd, Basisnr) %>%
    filter(Dag == dag) %>%
    filter(sub.weer$Maand >= start & sub.weer$Maand <= eind) %>%
    filter(Actief == TRUE)
  
  # Voeg kolommen toe
  sub.planning <- data.frame(sub.planning,
                         Vertraging = as.integer(NA),
                         Tijd = as.integer(NA),
                         Gate = factor(NA,levels=gatelvls),
                         Terminal = factor(NA,levels=terminallvls),
                         Baan = as.integer(NA),
                         Cancelled = 0)
  rownames(sub.planning) <- NULL
  
  
  
  
  ###################
  # Vertraging ######
  ###################
  
  ### Vertraging door weer
  
  # Bepaal de uren waarbij wind voor vertraging zorgt
  # Als de gemiddelde windsnelheid (FG) hoger is dan 6.5 m/s (2.5 m/s bij regen, is er de hele dag vertraging door wind
  # Zo niet, als er een windstoot (FXX) is gemeten van meer dan 10.0 m/s (6.0 m/s bij regen), is er 9 uur rondom die windstoot vertraging door wind
  if(sub.weer$FXX > (100 - 28 * sub.weer$nat)) {
    winduren <- max(0,sub.weer$FXXH-5):min(23,sub.weer$FXXH+4)
  } else {
    winduren <- NA
  }
  if(sub.weer$FG > (67 - 28 * sub.weer$nat)) {
    winduren <- 0:23
  }
  
  # Bepaal de uren waarbij slecht zicht voor vertraging zorgt
  # Er is sprake van slecht zicht bij zicht van minder dan 1 km
  # Als het zicht de hele dag minder was dan 1 km (VVX < 10), is er de hele dag vertraging door slecht zicht
  # Als het zicht een deel minder was dan 1 km (VVN < 10), is er een deel van de dag vertraging door slecht zicht
  # Dit duurt van 8 tot 3 uur, afh. van hoe slecht het zicht was
  if(sub.weer$VVN < 10) {
    zichturen <- floor(8 - sub.weer$VVN / 2)
    zichturen <- max(0,sub.weer$VVNH-1-zichturen):min(23,sub.weer$VVNH-1+zichturen)
  } else {
    zichturen <- NA
  }
  if(sub.weer$VVX < 10) {
    zichturen <- 0:23
  }
  
  # Hiermee kunnen voor elke vlucht afzonderlijk worden berekend of ze last hebben van slecht zicht of wind
  slechtzicht <- (sub.planning$Plantijd %/% 60) %in% zichturen
  windstoot <- (sub.planning$Plantijd %/% 60) %in% winduren
  
  # Vertraging door sneeuw is 30 min, door vorst 10 min, door slecht zicht 30 min, door wind 15 min, door regen 10 min
  v.weer <- sub.weer$sneeuw * 30 + sub.weer$vorst * 10 + slechtzicht * 30 + windstoot * 15 + sub.weer$nat * 10 + sub.weer$extreemwind * 60
  # Met een skew-normal verdeling worden deze vertragingen "uitgesmeerd" richting 0
  v.weer <- rsn(n=nrow(sub.planning),omega=0.35*v.weer,alpha=-10) + v.weer
  # Negatieve vertraging door weer kan niet
  v.weer[v.weer < 0] <- 0
  
  ### Vertraging voor intercontinentale vluchten
  
  # Dit is gemiddeld 15 minuten
  # Intercontinentale vluchten staan ingepland op terminal D of E
  continentaal <- sub.planning$Planterminal %in% c("D","E")
  v.continent <- rnorm(n=nrow(sub.planning),mean = 10, sd = 7.5) * continentaal
  
  ### Vertraging door andere factoren
  
  # Door een skew-normal verdeling worden er nog wat minuutjes vertraging toegevoegd
  # Dit kan eventueel negatief zijn
  v.random <- rsn(n=nrow(sub.planning),omega=5,alpha=4)
  
  ### Berekening & verwerking

  # Bereken de totale vertraging
  sub.planning$Vertraging <- as.integer(v.weer + v.continent + v.random)
  
  # Filter arriverende vluchten
  arr <- sub.planning$Richting == "A"
  # Filter vluchten waarbij de arriverende vlucht invloed heeft op de vertraging van de vertrekkende vlucht
  dub <- sub.planning$Basisnr %in% filter(vliegtuig.pax, freq > 1 & ri == "A")$Basisnr
  
  # Voeg de vertraging van een eventuele eerdere, arriverende vlucht toe aan het df
  sub.planning <- arrange(merge(sub.planning, sub.planning[arr,c("Basisnr","Vertraging")], by = "Basisnr", all.x = T),Plantijd,Basisnr)
  sub.planning <- dplyr::rename(sub.planning, Vertraging = Vertraging.x)
  sub.planning[dub&!arr,"Vertraging"] <- sub.planning[dub&!arr,"Vertraging.y"]
  sub.planning$Vertraging.y <- NULL
  
  # Tel vertraging op bij plantijd om de gerealiseerde tijd te berekenen
  sub.planning$Tijd <- sub.planning$Plantijd + sub.planning$Vertraging
  
  
  
  
  
  ###################
  # Bezetting  ######
  ###################
  
  # De bezettingsgraad is het hoogst in juli (maand = 7) en daarom ook het laagst in januari (maand = 1)
  # Elke bestemming heeft een `Zomerdrukte`,  die bepaalt hoe gevoelig de bezetting is voor de tijd van het jaar
  # Daarnaast is er een willekeurige factor tussen -5 en +5 procent
  
  sub.planning$Bezetting <- pmin(90 - 2 * (abs(sub.weer$Maand - 7)-1) * sub.planning$Zomerdrukte + sample(-5:5, nrow(sub.planning),replace=T), rep(100, nrow(sub.planning)))
  sub.planning$Passagiers <- round(sub.planning$Bezetting * sub.planning$Capaciteit / 100)
  sub.planning$Bezetting <- round(sub.planning$Passagiers / sub.planning$Capaciteit * 100)
  
  
  
  
  
  
  ###################
  # Gatewissels #####
  ###################
  
  # Vul een aanname in voor de gate
  sub.planning$Gate <- sub.planning$Plangate
  
  # Gatewissels
  gate <- dcast(data = sub.planning,formula = Gate + Basisnr~Richting,fun.aggregate = mean,value.var = "Tijd")
  wissels <- 0
  
  # Ook dit proces loopt per gate
  for (j in gatelvls) {
    # We selecteren de vluchten die
    # 1. arriveren,
    # 2. aan deze gate,
    # 3. terwijl een ander vliegtuig daar aanwezig is of minder dan 10 min geleden is vertrokken
    sel <- gate$Gate == j
    bezet <- which(sub.planning$Richting == "A" &
                   sub.planning$Gate == j &
                   sapply(sub.planning$Tijd, function(x) any(x > gate$A[sel] & x < gate$D[sel] + 10)))
    
    while(length(bezet) > 0) {
      k <- bezet[1]
      a <- gate[gate$Basisnr == sub.planning$Basisnr[k],"A"] #De aankomsttijd van deze vlucht
      d <- gate[gate$Basisnr == sub.planning$Basisnr[k],"D"] #De vertrektijd van deze vlucht
      rows <- which(sub.planning$Basisnr == sub.planning$Basisnr[k]) #Rownrs van beide vluchten
      d_row <- which(rownames(sub.planning) %in% rows & sub.planning$Richting == "D") #Rownr van de vertrekkende vlucht
      
      # Voor elke vlucht in dataframe `gate` wordt berekend of die conflicteert met deze vlucht
      gate$vrij <- a > gate$D | d < gate$A
      # We summarisen per gate met all(), want alle vluchten mogen geen conflict vormen
      beschikbaar <- dplyr::summarize(group_by(gate,Gate), a = all(vrij))
      # Voeg C8 handmatig toe indien nodig
      if (nrow(filter(beschikbaar, Gate == "C8")) == 0) { beschikbaar <- rbind(beschikbaar, list("C8",T)) }
      
      # Als er een beschikbare gate is:
      if (nrow(filter(beschikbaar, a == TRUE)) > 0) {
        sub.planning$Gate[rows] <- sample(as.character(filter(beschikbaar, a == TRUE)$Gate),1) #Kies een beschikbare gate
        sub.planning$Vertraging[d_row] <- sub.planning$Vertraging[d_row] + sample(11:17,1) #Vertrek 11-17 mins later
        sub.planning$Tijd[d_row] <- sub.planning$Plantijd[d_row] + sub.planning$Vertraging[d_row] #Pas tijden aan
        wissels <- wissels + 1
        gate <- dcast(data = sub.planning,formula = Gate + Basisnr~Richting,fun.aggregate = mean,value.var = "Tijd") #Vernieuw dataframe
        
      # Als er geen beschikbare gate is:
      } else {
        # Cancel vlucht
        sub.planning$Cancelled[rows] <- 1
        sub.planning$Tijd[rows] <- NA
        sub.planning$Gate[rows] <- NA
      }
      
      # Bereken opnieuw vluchten met een conflict
      bezet <- which(sub.planning$Richting == "A" &
                     sub.planning$Gate == j &
                     sapply(sub.planning$Tijd, function(x) any(x > gate$A[sel] & x < gate$D[sel] + 10)))
    }
  }
  
  # Pas terminal aan een eventuele gatewissel aan
  sub.planning$Terminal <- substr(as.character(sub.planning$Gate),1,1)
  
  
  
  
  ###################
  # Annuleringen ####
  ###################
  
  cancelled <- sample(sub.planning$Basisnr,onesample(0:3))
  if(sub.weer$extreemwind) { cancelled <- sample(unique(sub.planning$Basisnr),onesample(40:80)) }
  rows <- (sub.planning$Basisnr %in% cancelled)
  sub.planning$Cancelled[rows] <- 1
  sub.planning$Tijd[rows] <- NA
  sub.planning$Gate[rows] <- NA
  
  
  
  
  ###################
  # Vrachtvluchten ##
  ###################
  
  vracht$tijd <- sample(350:1100, nrow(vracht))
  vr <- sample(nrow(vracht),as.integer(rnorm(1,35,7)))
  
  sub.vracht <- data.frame(
    Vliegtuigcode = vracht[vr,4],
    Airlinecode = vracht[vr,1],
    Destcode = vracht[vr,2],
    Vliegtuigtype = vracht[vr,3],
    Vluchtnr = vracht[vr,5],
    Richting = rep("A",length(vr)),
    Planterminal = "F",
    Terminal = "F",
    Tijd = vracht[vr,7],
    Dag = rep(dag, length(vr)),
    Cancelled = rep(0, length(vr)),
    stringsAsFactors = FALSE
  )
  
  sub.vracht <- sub.vracht %>%
    dplyr::mutate(Richting = "D",
                  Tijd = Tijd + sample(100:150,nrow(sub.vracht),replace=T)) %>%
    rbind(sub.vracht) %>%
    merge(vliegtuig.vracht[,c("Vliegtuigcode","Capaciteit","Vracht")], by = "Vliegtuigcode", all.x = T) %>%
    dplyr::rename(MaxVracht = Vracht) %>%
    dplyr::mutate(Vracht = as.integer(MaxVracht * runif(length(vr)*2,min=0.7,max=1)))
    
  sub.planning <- rbind.fill(sub.planning, sub.vracht)
  
  
  
  
  ###################
  # Baankeuze  ######
  ###################
  
  # Als er weinig wind staat, wordt geland op baan 4 en opgestegen van baan 3
  # Als er meer wind staat, moet de baankeuze worden bepaald o.b.v. de windrichting (DDVEC)
  # Vliegtuigen willen zo goed mogelijk tegen de wind in opstijgen en landen
  
  if(sub.weer$FXX < 100 & sub.weer$FG < 67) {
    # Wind geen invloed op baankeuze
    sub.planning[sub.planning$Richting == "A","Baan"] <- 4
    sub.planning[sub.planning$Richting == "D","Baan"] <- 3
    
  } else {
    # Wind heeft wel invloed op baankeuze
    banen$A_score <- 180 - abs((sub.weer$DDVEC - banen$Landen + 180) %% 360 - 180)    #Verschil tussen windrichting en landrichting
    banen$D_score <- 180 - abs((sub.weer$DDVEC - banen$Opstijgen + 180) %% 360 - 180) #Verschil tussen windrichting en opstijgrichting
    a.baan <- onesample(arrange(filter(banen, Baannummer != 6),A_score)$Baannummer)
    d.baan <- (1:5)[-a.baan]
    d.baan <- onesample(arrange(filter(banen, Baannummer %in% d.baan),D_score)$Baannummer)
    sub.planning[sub.planning$Richting == "A","Baan"] <- a.baan
    sub.planning[sub.planning$Richting == "D","Baan"] <- d.baan
  }
  
  
  
  
  
  ###################
  # General #########
  ###################
  
  general$tijd <- sample(350:1100, nrow(general), replace=T)
  vr <- sample(1:nrow(general), onesample(3:16))
  
  sub.general <- data.frame(
    Airlinecode = rep("-",length(vr)),
    Vliegtuigcode = general[vr,3],
    Destcode = general[vr,2],
    Vliegtuigtype = general[vr,1],
    Richting = rep("A",length(vr)),
    Tijd = general[vr,4],
    Terminal = "G",
    Baan = 6,
    Cancelled = rep(0,length(vr)),
    stringsAsFactors = FALSE
  )
  
  sub.general$Richting[sub.general$Destcode == "VAP"] <- "D"
  sub.general$Destcode[sub.general$Destcode == "VAP"] <- sample(luchthavens.eur, sum(sub.general$Destcode == "VAP"), replace=T)
  
  sub.general <-  merge(sub.general, vliegtuigtype[,c("IATA","Capaciteit","Vracht")], by.x = "Vliegtuigtype", by.y = "IATA", all.x = T) %>%
    dplyr::rename(MaxVracht = Vracht)
  
  sub.planning <- rbind.fill(sub.planning, sub.general)
  
  
  
  
  
  ###################
  # Opslaan  ########
  ###################
  sub.planning$Datum <- sub.weer$Datum
  sub.planning$Datum[sub.planning$Cancelled == 0 & sub.planning$Tijd >= 1440] <- sub.planning$Datum[sub.planning$Cancelled == 0 & sub.planning$Tijd >= 1440] + 1
  sub.planning$Tijd <- sub.planning$Tijd %% 1440
  sub.planning <- dplyr::arrange(sub.planning,Datum,Tijd)
  sim <- rbind(sim, sub.planning[,vars.simtabel])
  print(paste0("Simulatie afgerond van ",sub.weer$Dag," ",funmaand(sub.weer$Maand)," ",sub.weer$Jaar,". Aantal gatewissels: ",wissels))
}
rm(sub.planning,sub.general,sub.vracht,sub.weer,beschikbaar,dag,i,slechtzicht,windstoot,winduren,zichturen,a,a.baan,actief,arr,bezet,cancelled,continentaal,d,d.baan,d_row,dub,gate,gates,inactief,j,k,rows,sel,simdagen,v.continent,v.random,v.weer,vr,wissels)


sim$Gatewissel <- sim$Plangate != sim$Gate & !is.na(sim$Gate)
sim$Type <- "R"
sim$Type[!is.na(sim$Vracht)] <- "F"
sim$Type[sim$Terminal == "G"] <- "G"

# Easter Eggs  ############################################################################

ns <- data.frame(
  Datum = weer$Datum[weer$Dag == 1 & weer$Maand == 4],
  Vluchtnr = "NS001",
  Airlinecode = "NS",
  Destcode = "AMS",
  Vliegtuigcode = "TNS001",
  Vliegtuigtype = "TRN",
  Type = "E",
  Gatewissel = F,
  Tijd = 781,
  Cancelled = 0
)
eek <- data.frame(
  Datum = weer$Datum[weer$Dag == 31 & weer$Maand == 10],
  Vluchtnr = "WF4141",
  Airlinecode = "WF",
  Destcode = "BOO",
  Vliegtuigcode = "VWF707",
  Vliegtuigtype = "DH4",
  Capaciteit = 70,
  Bezetting = 100,
  Passagiers = 70,
  MaxVracht = 1,
  Type = "E",
  Gatewissel = F,
  Richting = c("A","D"),
  Tijd = 427,
  Cancelled = 0
)

sim <- rbind.fill(sim, ns, eek)
rm(ns,eek)

# Cleanup  ############################################################################
sim <- arrange(sim, Datum, Tijd)
rownames(sim) <- NULL
sim$Vluchtid <- as.integer(rownames(sim)) + 935991

sim <- mutate(sim,
              Vluchtnr = factor(Vluchtnr),
              Richting = factor(Richting),
              Destcode = factor(Destcode),
              Vliegtuigcode = factor(Vliegtuigcode),
              Terminal = factor(Terminal),
              Type = factor(Type))




# Klanttevredenheid  ############################################################################

# Maak tabel aan voor klanttevredenheid
# Hierin komen routevluchten uit feb 2016, juli 2016, en feb 2017
klant.vars <- c("Vluchtid","Datum", "Zomerdrukte", "Bezetting", "Vertraging", "Gatewissel", "Terminal", "Destcode", "Plantijd", "Type")
klant <- subset(sim,
                month(Datum) %in% c(2,7) & year(Datum) >= 2016 & Type == "R" & Richting == "D" & !is.na(Tijd),
                select = klant.vars)

# 'Tijd' is het aantal dagen sinds 2016, hiermee kunnen dingen over de tijd beter of slechter worden
klant <- mutate(klant,
                Tijd = (Datum - ymd(20160101)) / ddays(1))

# Selecteer 3000 vluchten uit deze dataset
onderzoek <- createDataPartition(y = (klant$Tijd %/% 160), p = 3000/nrow(klant), list=F)
klant <- klant[onderzoek,]

# Operatie
# Hoe hoger de bezetting, hoe lager de score (tot een max van -0.5)
# Hoe meer vertraging, hoe lager de score (per uur vertraging -1.8)
# Een gatewissel leidt tot 1 punt aftrek
klant <- mutate(klant,
                Operatie = rnorm(nrow(klant), 8, 0.5) - 0.5 * Bezetting / 100 - 0.03 * Vertraging - 1 * Gatewissel)

ggplot(klant, aes(x=Operatie, y=..count..)) +
  geom_density(position="stack",size=1.5, adjust=1.5)

# Faciliteiten
# Gem. score per terminal: A = 5, B = 6.5, C,D,E = 8
# Per jaar gaat de score met 0.3 omlaag
# Vluchten naar een niet-vakantiebestemming scoren 20% lager
klant <- mutate(klant,
                Faciliteiten = (rnorm(nrow(klant), 8, .65) - 3*(Terminal == "A") - 1.5*(Terminal == "B") - Tijd/1100) * 0.8 ^ (Zomerdrukte == 1))

ggplot(klant, aes(x=Faciliteiten, y=..count.., fill=Terminal)) +
  geom_density(position="stack",size=1.5, adjust=1.5) +
  facet_wrap(~(Tijd %/% 160), nrow=3)

# Shops
# Vluchten naar een vakantiebestemming scoren 10% lager
# Er is geen score voor vluchten tussen 23:00 en 6:00
klant <- mutate(klant,
                Shops = rnorm(nrow(klant), 6, .75) * 0.9 ^ (Zomerdrukte == 4) )
klant$Shops[klant$Plantijd > 1380 | klant$Plantijd < 360] <- NA

ggplot(klant, aes(x=Shops, y=..count..)) +
  geom_density(size=1.5, adjust=1.5) +
  facet_wrap(~Zomerdrukte, nrow=2)

# Afronden op 1 decimaal en waarden boven de 10 op 10 zetten
klant <- mutate(klant,
                Faciliteiten = pmin(round(Faciliteiten, digits = 1),10),
                Shops = pmin(round(Shops, digits = 1),10),
                Operatie = pmin(round(Operatie, digits = 1),10))

rm(onderzoek)



# Export  ############################################################################

# Selecteer variabelen voor export
planning.export <- sim %>%
  select(Vluchtnr, Airlinecode, Destcode, Planterminal, Plangate, Plantijd) %>%
  unique() %>%
  filter(!is.na(Vluchtnr)) %>%
  arrange(Vluchtnr) %>%
  mutate(Plantijd = toampm(Plantijd))

vliegtuig.export <- sim %>%
  select(Airlinecode, Vliegtuigcode, Vliegtuigtype) %>%
  unique()
vliegtuig.export$Bouwjaar = sample(1970:2013, nrow(vliegtuig.export), replace=T)

vertrek.export <- sim %>%
  filter(Richting == "D") %>%
  select(Vluchtid, Vliegtuigcode, Terminal, Gate, Baan, Bezetting, Vracht, Tijd, Datum)
vertrek.export$Vertrektijd <- vertrek.export$Datum + minutes(vertrek.export$Tijd) + seconds(sample(0:59, nrow(vertrek.export), replace=T))
vertrek.export <- vertrek.export %>%
  select(-Datum, -Tijd)

aankomst.export <- sim %>%
  filter(Richting == "A") %>%
  select(Vluchtid, Vliegtuigcode, Terminal, Gate, Baan, Bezetting, Vracht, Tijd, Datum)
aankomst.export$Aankomsttijd <- aankomst.export$Datum + minutes(aankomst.export$Tijd) + seconds(sample(0:59, nrow(aankomst.export), replace=T))
aankomst.export <- aankomst.export %>%
  select(-Datum, -Tijd)

vlucht.export <- sim %>%
  select(Vluchtid, Vluchtnr, Airlinecode, Destcode, Vliegtuigcode)

klant.export <- klant %>%
  select(Vluchtid, Operatie, Faciliteiten, Shops)

write.csv2(planning.export, file="export_planning.csv", row.names=F, na="")
write.csv2(vliegtuig.export, file="export_vliegtuig.csv", row.names=F, na="/N")
write.csv2(vertrek.export, file="export_vertrek.csv", row.names=F, na="")
write.csv2(aankomst.export, file="export_aankomst.csv", row.names=F, na="")
write.csv2(vlucht.export, file="export_vlucht.csv", row.names=F, na="")
write.csv2(klant.export, file="export_klant.csv", row.names=F, na="")



# Summary & Plots  ############################################################################
plot(density(sim$Vertraging, adjust=.75, na.rm = T))

summ <- sim %>%
  dplyr::group_by(Datum) %>%
  dplyr::summarise(vertraging = mean(Vertraging,na.rm=T), 
                   gatewissels = sum(as.integer(Gatewissel)),
                   gatewisselperc = sum(as.integer(Gatewissel))/n(),
                   vluchten = n(),
                   cancels = sum(Cancelled)) %>%
  merge(weer, by = "Datum") %>%
  filter(vluchten > 10)

maand <- summ %>%
  dplyr::group_by(Maand) %>%
  dplyr::summarise(vorst = sum(vorst),
                   vertraging = mean(vertraging,na.rm=T),
                   gatewissels = sum(gatewissels),
                   vluchten = sum(vluchten),
                   cancels = sum(cancels))

model <- lm(vertraging ~ vluchten + gatewissels + FXX + FG + VVN + VVX + nat + vorst + sneeuw + RH, data = summ)
summary(model)

plottabel <- sim
plottabel$Datum <- as.factor(plottabel$Datum)
plottabel$continentaal <- plottabel$Planterminal %in% c("D","E")

ggplot(plottabel, aes(x=Vertraging, fill = Gatewissel)) + geom_density(alpha = .5, size=0) + facet_wrap(~continentaal,nrow=5)

plot(summ$FG, summ$vertraging)
plot(summ$FXX, summ$vertraging)
plot(summ$vorst, summ$vertraging)
plot(summ$sneeuw, summ$vertraging)
plot(summ$RH, summ$vertraging)
plot(summ$TG, summ$vertraging)