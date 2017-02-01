# Initialiseren ######################################################################################

# Selecteer bronmap
setwd("D:\\data\\ast21252\\Documents\\201701 VisionWorks Academy XI\\Eindcasus Vision Airport")

# Packages
require(chron)
require(sn)



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

# resample: vector -> element
# van de ingevoerde vector wordt 1 willekeurig element geselecteerd
# dit is een verbetering op de standaard functie sample, omdat de functie ook werkt als de vector 1 element bevat
resample <- function(x) {
  if (length(x) == 1) { return(x) } else { return(sample(x,1)) }
}



# Data inlezen ######################################################################################

# Bronbestanden
routes <- read.csv2("data_routes.csv")
banen <- read.csv2("data_banen.csv")
weer <- read.csv2("data_weer.csv")

# Aanpassingen data formaat
weer$Datum <- as.Date(weer$Datum,"%d-%m-%Y")
weer$RH[weer$RH < 0] <- 0
weer$nat <- weer$RH > 100
weer$vorst <- weer$TG < -10 & weer$RH > 0

names(routes)[names(routes) == "Gate"] <- "Plangate"
names(routes)[names(routes) == "Terminal"] <- "Planterminal"

# Enkele levels in een vector opslaan
gatelvls <- c(levels(routes$Plangate),"C8")
terminallvls <- c(levels(routes$Planterminal))
richtinglvls <- c("A","D","S")

# Tabelstructuren
vars.plantabel <- c("Airlinecode", "Destcode", "Planterminal", "Plangate", "Vluchtnr", "Basisnr", "start", "eind","Richting","Plantijd","Dag")
str.plantabel <- structure(list(Airlinecode = factor(levels=levels(routes$Airlinecode)),
                                Destcode = factor(levels=levels(routes$Destcode)),
                                Planterminal = factor(levels=terminallvls),
                                Plangate = factor(levels=gatelvls),
                                Vluchtnr = integer(),
                                Basisnr = integer(),
                                start = integer(),
                                eind = integer(),
                                Richting = factor(levels=richtinglvls),
                                Plantijd = integer(),
                                Dag = integer()), 
                           class = "data.frame")

vars.vlucht.sim <- c("Richting","Airlinecode","Destcode","Vluchtnr","Basisnr","Planterminal","Plangate","Plantijd")
vars.simtabel <- c("Datum",vars.vlucht.sim,"Terminal","Gate","Tijd","Vertraging")
str.simtabel <- structure(list(Datum = as.Date(character()),
                               Richting = factor(levels=richtinglvls),
                               Airlinecode = factor(levels=levels(routes$Airlinecode)),
                               Destcode = factor(levels=levels(routes$Destcode)),
                               Vluchtnr = integer(),
                               Basisnr = integer(),
                               Planterminal = factor(levels=terminallvls),
                               Plangate = factor(levels=gatelvls),
                               Plantijd = integer(),
                               Terminal = factor(levels=terminallvls),
                               Gate = factor(levels=gatelvls),
                               Tijd = integer(),
                               Vertraging = integer()),
                          class = "data.frame")




# Genereren vluchtnummers ######################################################################################

# Vluchten krijgen een willekeurig vluchtnummer toegewezen
# Hierbij krijgen vluchten binnen Europa een vluchtnummer tussen 0 en 999
# Intercontinentale vluchten krijgen een vluchtnummer tussen 1000 en 4999
# Dit onderscheid is niet per se ontleed aan de werkelijke praktijk
# Vluchtnummers boven de 5000 zijn bedoeld voor vluchten die worden gedeeld tussen airlines (komen niet in deze dataset voor)

# Vluchten vanaf een "hub" van een airline krijgen vaak een oneven vluchtnummer, en vluchten naar een hub een even
# VisionAirPort is een hub van 4 airlines: KLM, Transavia, TuiFly, en Corendon
# Omdat (meestal) begonnen wordt met de arriverende vlucht, is dat voor deze 4 airlines een vlucht náár de hub
# Deze 4 airlines beginnen dus op een even vluchtnummer, de rest op een even vluchtnummer

# Kolom toevoegen aan df
routes$Vluchtnr <- 0

# For-loop per airline (i = Airlinecode)
for(i in levels(routes[,1])) {
  # tijdelijk df met vluchten van deze ene airline
  sub <- routes[routes$Airlinecode == i,]
  # als deze airline maar 1 vlucht heeft: geef random vluchtnummer
  if(nrow(sub) == 1) {
    sub$Vluchtnr[sub$Continent=="Eur"] <- floor(runif(1,min=10,max=495)) * 2 + 1
    sub$Vluchtnr[sub$Continent!="Eur"] <- floor(runif(1,min=495,max=2995)) * 2 + 1
  # meer vluchten? voeg dan controle toe dat gegenereerde vluchtnummers niet te dicht opeen zitten
  # sorteer van klein naar groot en check dat opeenvolgende vluchtnummers minstens 8 verschillen
  # door de while-functie wordt geprobeerd tot een willekeurige set is gegenereerd die voldoet
  } else {
    while(min(diff(sub[order(sub$Vluchtnr),"Vluchtnr"])) <= 7) {
      sub$Vluchtnr[sub$Continent=="Eur"] <- floor(runif(nrow(sub[sub$Continent=="Eur",]),min=10,max=495)) * 2 + 1
      sub$Vluchtnr[sub$Continent!="Eur"] <- floor(runif(nrow(sub[sub$Continent!="Eur",]),min=495,max=2995)) * 2 + 1
    }
  }
  # de gegenereerde vluchtnummers worden teruggezet in het bron-dataframe
  routes[routes$Airlinecode == i, "Vluchtnr"] <- sub$Vluchtnr
}
rm(sub)

# Aanpassing voor de 4 airlines voor wie VisionAirport een hub is
home <- c("KL","HV","TFL","CAI")
routes$Vluchtnr[!(routes$Airlinecode %in% home)] <- routes$Vluchtnr[!(routes$Airlinecode %in% home)] - 1

# Sla basis-vluchtnummer op als identificatie voor later
routes$Basisnr <- routes$Vluchtnr


# Bepalen vluchtperiode ######################################################################################

# Veruit de meeste vluchten worden het hele jaar door gevlogen (periode = 'j')
# Sommige worden alleen in de zomer gevlogen (periode = 'z')
# Deze zomervluchten worden actief in april, mei, of juni en inactief in augustus, september, of oktober
# Deze maanden zijn inclusief, dus 'inactief in augustus' betekent 'vliegt niet meer vanaf september'
# De startmaand en eindmaand wordt per route willekeurig gekozen
# Eerst wordt overal jan - dec ingevoerd, daarna wordt dit voor de zomervluchten overschreven

routes$start <- 1
routes$eind <- 12
routes$start[routes$Periode == "z"] <- sample(c(4,5,6),1)
routes$eind[routes$Periode == "z"] <- sample(c(8,9,10),1)



# Opstellen planning ######################################################################################

# Dit stuk van de code genereert de plantijden voor elke routevlucht
# Dit proces gebeurt per gate afzonderlijk

# Sommige routes worden 2x op een dag gevlogen. Deze vluchten worden als eerst ingepland, aan het begin van de dag
# In de kolom 'Mintijd' wordt de vroegste tijd genoteerd waarop de route dan voor de 2e keer kan worden gevlogen

# Sommige vluchten worden om de dag gevlogen. Deze vluchten hebben een 'delay', dwz. de dag van de 2 dat ze vliegen (0 of 1)
# Dit is zo goed mogelijk eerlijk verdeeld over de 2 mogelijke dagen (handmatig, in de brondata)
# Elke dag-0 vlucht wordt willekeurig gekoppeld aan een dag-1 vlucht die op hetzelfde moment de dag erna vertrekt
# De planning herhaalt zich dus elke 2 dagen

# Als de dubbele vluchten allemaal 1 keer zijn ingepland, gaat het proces verder met de overige vluchten
# Dit zijn de enkele vluchten, de helft van de halve vluchten, en een herhaling van de dubbele vluchten
# Er wordt gekeken welke vluchten kunnen worden gevlogen (dat het niet te vroeg is voor een eventuele herhaling)
# Van deze vluchten wordt willekeurig 1 ingepland

# Als een vlucht bij een gate is gearriveerd, volgt de 'Turnaround' waarin het vliegtuig weer wordt klaargemaakt voor vertrek
# VisionAirport is erg efficiënt: de turnaround duurt 45 min voor Europese vluchten en 75 minuten voor intercontinentale vluchten

# Nadat een vlucht van een gate is vertrokken, wordt een bufferperiode ingepland tot de volgende arriverende vlucht
# Deze periode is minstens 10 minuten en krijgt daarbij een bufferfactor toegevoegd
# Deze buffer volgt deze formule: buffer = abs(c-0.5*d) / (0.5*d) * (max-min) + min
# Hierbij is c een counter van de hoeveelste vlucht zojuist vertrokken is, en d het totaal aantal vluchten dat van de gate vertrekt
# Er is een verborgen parameter g, de gemiddelde buffer die op een dag gerealiseerd moet worden om de dag mooi op te vullen
# Er is een bandbreedte rondom g, waarvan min en max de grenzen zijn

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
  sub.gate <- routes[routes$Plangate == i,]
  sub.gate$Mintijd <- 0
  sub.gate$Klaar <- 0
  # Een aantal tijden staan in uren in de brondata, en worden omgeschreven naar stappen van 5 mins
  sub.gate$Duur <- ceiling(sub.gate$Duur * 60 / 5)
  sub.gate$Turnaround <- ceiling(sub.gate$Turnaround * 60 / 5)
  sub.gate$Turnaroundtotaal <- ceiling(sub.gate$Turnaroundtotaal * 60 / 5)
  
  # Het aantal dubbele, enkele, en halve vluchten
  aantdubbel <- nrow(subset(sub.gate,Vluchten == 2))
  aantenkel <- nrow(subset(sub.gate,Vluchten == 1))
  aanthalf <- max(nrow(subset(sub.gate,Vluchten == 0.5 & Delay == 1)),nrow(subset(sub.gate,Vluchten == 0.5 & Delay == 0)))
  
  # Hier combineren we de halve vluchten met elkaar
  sub.gate <- transform(sub.gate,Indeling = ave(Vluchten, Delay, FUN = function(x) rank(x, ties.method = "first")))
  sub.gate$Indeling[sub.gate$Vluchten > 0.5] <- 0
  sub.gate$Indeling[sub.gate$Vluchten > 0.5] <- (max(sub.gate$Indeling)+1):(max(sub.gate$Indeling)+aantdubbel+aantenkel)
  # Sorteer de tabel zodat de dubbele vluchten bovenaan staan, aflopend gesorteerd op vluchtduur (dus langste vluchten eerst inplannen)
  sub.gate <- sub.gate[order(sub.gate$Vluchten,sub.gate$Duur,decreasing=TRUE),]
  
  # Plansub is de tabel waarin ingeplande vluchten tijdelijk worden opgeslagen
  plansub <- str.plantabel
  
  # Dit zijn de parameters voor het berekenen van de bufferperiode
  d <- ceiling(sum(sub.gate$Vluchten)) - 1
  g <- floor((245 - sum(sub.gate$Turnaroundtotaal)) / (d+1)) - 2
  min <- max(0,g-20)
  max <- 2 * g - min
  gates[nrow(gates)+1,] <- list(i,as.integer(d),as.integer(g),as.integer(min),as.integer(max))
  #d <- d - 1
  
  # Tijd houdt de tijd bij (ook in stappen van 5 min)
  tijd <- 0
  # c telt het aantal vertrokken vluchten
  c <- 0
  
  # Inplannen van de 1e vlucht van dubbele routes
  if (aantdubbel > 0) {
    for (j in 1:aantdubbel) {
      # Haal vlucht op
      sub.vlucht <- sub.gate[j,]
      
      # Als dit niet de eerste vlucht is:
      if (c > 0) {
        # Plan arriverende vlucht in
        sub.vlucht$Richting <- "A"
        sub.vlucht$Plantijd <- tijd
        sub.vlucht$Dag <- 0
        plansub[nrow(plansub)+1,] <- sub.vlucht[,vars.plantabel]
        sub.vlucht$Dag <- 1
        plansub[nrow(plansub)+1,] <- sub.vlucht[,vars.plantabel]
        
        # Turnaround verstrijkt
        tijd <- tijd + sub.vlucht$Turnaround
        
        # Log
        log[nrow(log)+1,] <- list(i,c,"A",tijd, sub.vlucht$Turnaround)
      
      # Als dit wel de eerste vlucht is:
      } else {
        # Bewaar vlucht tot einde planning
        afsluiter <- sub.vlucht
      }
      
      # Tel 1 op bij vluchtnummer
      sub.vlucht$Vluchtnr <- sub.vlucht$Vluchtnr + 1
      
      # Plan vertrekkende vlucht in
      sub.vlucht$Richting <- "D"
      sub.vlucht$Plantijd <- tijd
      sub.vlucht$Dag <- 0
      plansub[nrow(plansub)+1,] <- sub.vlucht[,vars.plantabel]
      sub.vlucht$Dag <- 1
      plansub[nrow(plansub)+1,] <- sub.vlucht[,vars.plantabel]
      
      # select is een T/F vector die de rijen in sub.gate selecteert die horen bij de vlucht die we nu inplannen
      select <- (sub.gate$Basisnr == sub.vlucht$Basisnr & sub.gate$Airlinecode == sub.vlucht$Airlinecode)
      # Sla de minimale tijd op voor de 2e vlucht
      sub.gate$Mintijd[select] <- tijd + (sub.vlucht$Duur * 2) + 12
      # Verhoog vluchtnummer in sub.gate
      sub.gate$Vluchtnr[select] <- sub.gate$Vluchtnr[select] + 2
      
      # Buffertijd verstrijkt
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
    indeling.huidig <- resample(beschikbaar)
    
    # Haal informatie op
    sub.vlucht <- sub.gate[sub.gate$Indeling == indeling.huidig,]
    
    # Als dit niet de eerste vlucht is:
    if (c > 0) {
      # Plan arriverende vlucht in
      sub.vlucht$Richting <- "A"
      sub.vlucht$Plantijd <- tijd
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
      
    # Als dit wel de eerste vlucht is:
    } else {
      # Bewaar vlucht tot einde planning
      afsluiter <- sub.vlucht
    }
    
    # Tel 1 op bij vluchtnummer
    sub.vlucht$Vluchtnr <- sub.vlucht$Vluchtnr + 1
    
    # Plan vertrekkende vlucht in
    sub.vlucht$Richting <- "D"
    sub.vlucht$Plantijd <- tijd
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
    
    # Buffertijd verstrijkt
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
    sub.vlucht$Richting <- "A"
    sub.vlucht$Plantijd <- tijd
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
    rm(afsluiter)
  }
  
  # De vluchten zijn nu ingepland vanaf tijd = 0, dit is natuurlijk niet de bedoeling
  # De planning wordt eerst gecentreerd op de dag
  # Daarna wordt een willekeurige factor toegevoegd, een verschuiving naar voren of achteren met maximaal 45 mins
  shift <- 144 - floor((max(plansub$Plantijd) + min(plansub$Plantijd)) / 2) + sample(-9:9,1)
  plansub$Plantijd <- plansub$Plantijd + shift
  
  # Voeg alle ingeplande vluchten van deze gate toe aan de planning
  planning <- rbind(planning, plansub)
}

rm(sub.gate,plansub,sub.vlucht,aantdubbel,aantenkel,aanthalf,beschikbaar,buffer,extratijd,c,d,g,max,min,shift,select,tijd,indeling.huidig,i,j)
}

rm(a)

# Verschuif de planning zo, dat de laatste vlucht om 23:55 vertrekt
planning$Plantijd <- planning$Plantijd + 287 - max(planning$Plantijd)

# Bereken de daadwerkelijke tijd (dus niet in termen van 5mins)
planning$Plantijd <- planning$Plantijd * 5
planning$Planuur <- planning$Plantijd %/% 60
planning$Planminuut <- planning$Plantijd %% 60

# Grafieken van de verdeling over de dag
plot(density(planning$Plantijd))
plot(table(planning$Planuur))





# Simulatie routevluchten ######################################################################################

# De dagen waarvoor de simulatie draait
simdagen <- 1:5
# h telt het aantal gesimuleerde vluchten
h <- 0
# Zet willekeurig 36 vluchten op non-actief
#inactief <- routes$Vluchtnr[sample(1:nrow(routes),36)]
#planning$Actief <- !(planning$Basisnr %in% inactief)

# Een tabel voor de gesimuleerde vluchten
sim <- str.simtabel

for(i in simdagen) {
  # Lees informatie in over deze dag
  sub.weer <- weer[i,]
  # Bepaal of dit een 0-dag of 1-dag is
  dag <- i %% 2
  
  # Als dit de eerste dag van de maand is: zet een vlucht op actief
  #if(sub.weer$Dag == 1 & length(inactief) > 0) {
  #  actief <- sample(inactief,1)
  #  planning$Actief[planning$Basisnr == actief] <- T
  #  inactief <- inactief[inactief != actief]
  #}
  
  # Bepaal de uren waarbij wind een factor is
  if(sub.weer$FXX > (100 - 40 * sub.weer$nat)) {
    if(sub.weer$FG > (65 - 40 * sub.weer$nat)) {
      winduren <- 0:23
    } else {
      winduren <- max(0,sub.weer$FXXH-3):min(23,sub.weer$FXXH+2)
    }
  } else {
    winduren <- NA
  }
  
  # Is er slecht zicht? (minder dan 1 km)
  if(sub.weer$VVN < 10) {
    zichturen <- floor(5 - sub.weer$VVN / 2)
    zichturen <- max(0,sub.weer$VVNH-1-zichturen):min(23,sub.weer$VVNH-1+zichturen)
  } else {
    zichturen <- NA
  }
  
  # We slaan de planning voor vandaag op in sub.planning
  sub.planning <- data.frame(planning[planning$Dag == dag,])
  sub.planning <- sub.planning[order(sub.planning$Plantijd),]
  
  for(j in 1:nrow(sub.planning)) {
    
    # Check: wordt de vlucht gevlogen?
    if(sub.weer$Maand %in% sub.planning$start[j]:sub.planning$eind[j] ) { #& sub.planning$Actief[j] == T
      
      # Voeg toe aan counter 'h'
      h <- h+1

      # Haal informatie op
      sub.vlucht <- data.frame(Datum = sub.weer$Datum, droplevels(sub.planning[j,vars.vlucht.sim]))
      rownames(sub.vlucht) <- NULL
      
      # Bepaal de vertraging
      sub.vlucht$Planuur <- sub.vlucht$Plantijd %/% 60
      slechtzicht <- sub.vlucht$Planuur %in% zichturen
      windstoot <- sub.vlucht$Planuur %in% winduren
      sub.vlucht$Vertraging <- sub.weer$vorst * 90 + slechtzicht * 30 + windstoot * 15 + sub.weer$nat * 10
      sub.vlucht$Vertraging <- rsn(n=1,omega=0.35*sub.vlucht$Vertraging,alpha=-10) + sub.vlucht$Vertraging
      if (sub.vlucht$Vertraging < 0) { sub.vlucht$Vertraging <- 0 }
      sub.vlucht$Vertraging <- as.integer(sub.vlucht$Vertraging + rsn(n=1,omega=2,alpha=1000))
      
      sub.vlucht$Tijd <- sub.vlucht$Plantijd + sub.vlucht$Vertraging
      
      # Is er een gatewissel nodig?
      #if(nrow(sub.planning[sub.planning$Gate == sub.planning$Plangate[j] &
      #                (sub.planning$Tijd[j] - sub.planning$Tijd) < 60 &
      #                sub.planning$Vluchtnr != sub.planning$Vluchtnr[j] &
      #                sub.planning$Tijd > 0, ]) > 0) {
      #  # Overzicht van gates
      #  gates <- aggregate(abs(sub.planning$Plantijd - sub.planning$Tijd[j]), by=list(gate=sub.planning$Plangate), FUN=min)
      #  if (nrow(gates[gates$gate == "C8",]) == 0) {
      #    levels(gates$gate) <- c(levels(gates$gate), "C8")
      #    gates <- rbind(gates,c("C8",999))
      #  }
      #  
      #} else {
      #  sub.planning$Gate[j] <- sub.planning$Plangate[j]
      #}
      
      sub.vlucht$Gate <- sub.vlucht$Plangate
      sub.vlucht$Terminal <- sub.vlucht$Planterminal
      
      sim[h,] <- sub.vlucht[,vars.simtabel]
    }
  }
  
  print(paste0("Simulatie afgerond van ",sub.weer$Dag," ",funmaand(sub.weer$Maand)," ",sub.weer$Jaar))
}

rm(sub.planning,sub.weer,sub.vlucht,dag,h,i,j,slechtzicht,windstoot,winduren,zichturen)

plot(density(sim$Vertraging, adjust=2))
