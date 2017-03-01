VisionAirPort Eindcasus
=========================================


Het vliegveld
-----------------

 - De vliegveldcode van VisionAirPort is VAP.
 - Er vliegen drie soorten vluchten op VisionAirPort: routevluchten, vrachtvluchten,
   en privévluchten ("General Aviation")
 - Het vliegveld heeft 5 passagiersterminals: A,B,C,D,E. Terminal C heeft 8 gates,
   terminals A,B,D hebben 6 gates, terminal E heeft 5 gates. Er zijn dus in totaal
   31 gates.
 - Daarnaast wordt de code F gebruikt als terminal voor vrachtvluchten, en code G
   als terminal voor privévluchten.
 - Alleen terminals D,E zijn ingericht op intercontinentale vluchten (douane etc.).
   Alle intercontinentale vluchten worden ingepland vanaf deze terminals.
 - VisionAirPort heeft 6 landingsbanen.
 - VisionAirPort is een hub voor 5 airlines: KLM, Transavia, TUIFly, Corendon, en 
   MartinAir.


Operatie
-----------------

 - De aankomst- en vertrektijd van een vliegtuig worden gedefinieerd als het moment
   van arriveren bij en vertrekken van de gate.
 - Tussen aankomst en vertrek vindt de "turnaround" plaats (passagiers verlaten het
   vliegtuig, het vliegtuig wordt bijgetankt en schoongemaakt, nieuwe passagiers boarden).
   Dit proces duurt voor vluchten binnen Europa 45 minuten en voor intercontinentale
   vluchten 75 minuten.
 - Een gatewissel vindt plaats op het moment dat 10 minuten voor aankomst van een
   routevlucht de geplande gate nog bezet is. Hierbij wordt dan uitgeweken naar een
   willekeurige (!) andere vrije gate. Bij dit uitwijken wordt ook geen rekening gehouden
   met of een vlucht intercontinentaal is of niet.
 - Bij een gatewissel duurt de turnaround 15 minuten langer, omdat passagiers van elders
   op het vliegveld moeten komen of omdat benzine / voorraden verplaatst moeten worden.
 - Er worden nooit vluchten ingepland op gate C8, dit omdat er dan vaker een gate 
   beschikbaar is voor gatewissels.


Routevluchten
-----------------

 - Een route bestaat uit een vliegtuig dat eeuwig tussen VisionAirPort en 1 andere
   bestemming heen en weer vliegt. Afhankelijk van de te vliegen afstand gebeurt dit eens
   per 2 dagen, elke dag, of tweemaal per dag.
 - Routevluchten hebben een vluchtnummer dat bestaat uit een combinatie van de
   airlinecode (vaak 2-letterig, behalve bij Corendon en EasyJet), en een getal
 - Dit getal ligt onder de 1000 voor vluchten binnen Europa en tussen 1000 en 5000 voor
   intercontinentale vluchten
 - Dit vluchtnummer is uniek op een dag. Een vluchtnummer kan dus staan voor "de vlucht
   van KLM om 5:15 naar Caïro". De volgende dag kan een vluchtnummer opnieuw worden
   gebruikt, voor hetzelfde soort vlucht. De 'betekenis' van een vluchtnummer is
   tijdens het bestaan van VisionAirPort nooit gewijzigd.
 - Vluchten vanaf een hub van een airline krijgen vaak een oneven vluchtnummer, en
   vluchten naar een hub vaak een even vluchtnummer. Deze regel wordt voor alle
   routevluchten van en naar visionAirPort gevolgd.
 - Sommige routes zijn gecategoriseerd als 'zomervluchten'. Deze beginnen op 1 april,
   1 mei, of 1 juni, en stoppen op 31 augustus, 31 september, of 31 oktober.
 - Routevluchten gebruiken baan 1 tot 5.


Vrachtvluchten
-----------------

 - Vrachtvluchten werken in zekere zin ook met 'routes': één specifiek vliegtuig
   wordt toegewezen aan één specifieke bestemming en vliegt nooit naar of van een
   ander vliegveld. Hier wordt ook een vaste tijd aan gekoppeld.
 - Welke vrachtvluchten wanneer plaatsvinden (en hoe vaak) is volledig willekeurig.
 - Per dag worden er tussen de 8 en 17 vrachtvluchten gevlogen.
 - Vrachtvluchten hebben een vluchtnummer dat bestaat uit de 3-letterige airlinecode
   (altijd) en een getal boven de 5000.
 - Vrachtvluchten vliegen uiteraard niet op een passagiersterminal. Er wordt als
   terminal F (voor 'Freight') ingevuld. Dit staat voor de gehele cargo-afdeling
   van het vliegveld, niet voor een echte terminal.
 - Vrachtvluchten gebruiken baan 1 tot 5.


Privévluchten
-----------------

 - Er bestaat een poule met privéjets en een poule met bestemmingen waar zij op kunnen
   vliegen (allemaal Europees). Elke dag verandert tussen de 3 en 16 privéjets van
   locatie, dus als ze ergens anders zijn komen ze naar VAP en als ze hier zijn vliegen
   ze naar een willekeurige geschikte bestemming. Een vliegtuig vliegt dus nooit 2x per
   dag.
 - Privévluchten hebben geen vluchtnummer.
 - Privévluchten vliegen vanaf een apart deel van het vliegveld (vgl. Schiphol Oost).
   Er wordt als terminal G (voor 'General Aviation') ingevuld. Dit staat voor een deel
   van het vliegveld, niet voor een echte terminal.
 - Van privévluchten is geen informatie beschikbaar over de bezettingsgraad.
 - Privéjets landen en stijgen op van baan 6.


Landingsbanen
-----------------

 - Er zijn 6 landingsbanen: de Anthony Fokkerbaan, Sergej Iljoesjinbaan, Wilbur
   Wrightbaan, Anthony Plesmanbaan, Henri Wijnmalenbaan, en Charles Lindbergbaan.
   Deze worden normaal gesproken aangeduid met de nummers 1-6.
 - Baan 1 ligt van oost naar west (code 09-27)
 - Banen 2-4 liggen van noord naar zuid (code 18R-36L, 18L-36R, en 18C-36C)
 - Baan 5 ligt van noord-oost naar zuid-west (code 06-24)
 - Baan 6 ligt van noord-oost naar zuid-west (code 04-22)
 - Op baan 3 mag alleen worden gevlogen vanuit en naar het zuiden.
 - Op baan 4 mag alleen worden gevlogen vanuit en naar het noorden.
 - Voor de vracht- en routevluchten is sprake van dynamische baankeuze op basis van
   de wind. Privévluchten gebruiken altijd baan 6, ongeacht de weersomstandigheden.
 - Als er weinig wind staat, wordt baan 4 gebruikt om te landen en baan 3 om op te
   stijgen.
 - Er is sprake van 'harde wind' vanaf een gemiddelde windsnelheid van 13 knopen of bij
   windstoten van 20 knopen. Dit komt neer op 6.7 m/s en 10.3 m/s, respectievelijk.
 - Als er sprake is van harde wind, wijken vluchten uit naar een baan waar ze zo goed
   mogelijk tegen de wind in kunnen landen dan wel opstijgen. Hierbij geldt dat
   eerst een baan wordt gekozen voor landen, daarna voor opstijgen.


Vertraging door weer
-----------------

 - Vertraging door neerslag treedt op vanaf 5 mm neerslag en leidt tot 10 min vertraging.
 - Vertraging door wind treedt op bij een gemiddelde windsnelheid van 13 knopen of
   windstoten vanaf 20 knopen. Bij meer dan 5 mm neerslag zijn deze grenzen 5 knopen
   lager. Vertraging door wind leidt tot 15 min vertraging.
 - Vertraging door vorst treedt op bij temperaturen onder het vriespunt. Deze vertraging
   bedraagt 10 minuten.
 - Vertraging door sneeuw treedt op bij temperaturen onder het vriespunt en meer dan 1.5
   mm neerslag. Dit leidt tot 30 minuten vertraging.
 - Vertraging door slecht zicht treedt op vanaf minder dan 1 km zicht. Dit leidt tot
   30 minuten vertraging.
 - Deze vertragingen door weersomstandigheden worden bij elkaar opgeteld. Vervolgens
   krijgen vluchten een vertraging getrokken uit een skew-normal verdeling met het
   zwaartepunt op de aldus berekende vertraging.


Vertraging voor intercontinentale vluchten
-----------------

 - Intercontinentale vluchten hebben gemiddeld 15 minuten meer vertraging.
 - Daarnaast kennen ze ook een grotere spreiding in vertraging.


Vertraging door gatewissels
-----------------

 - Een vliegtuig dat van gate is gewisseld krijgt nog 15 minuten extra vertraging
   bij vertrek (de arriverende vlucht ondervindt geen last).


Overige vertraging
-----------------

 - Er worden nog willekeurige vertragingen toegevoegd uit een skew-normal verdeling met
   een gemiddelde van 3.8. Deze willekeurige factoren kunnen ook leiden tot een
   vervroeging.