# Statisztikai homogenitás-vizsgálatok empirikus elemzése Monte Carlo szimulációs módszerekkel
A kutatás Monte Carlo szimuláció segítségével vizsgálja különböző homogenitásvizsgálati próbák (Sturges-féle Khi-négyzet, Decilis-alapú Khi-négyzet, Kolmogorov–Smirnov, Kruskal–Wallis) teljesítményét különböző eloszláspárok és mintanagyságok esetén.

## szimulacio.py
- Eloszlások definiálása és mintavételezése
- Tesztek elvégzése
- Monte Carlo szimuláció futtatása különböző eloszláspárokra, mintaelemszámokra, véletlen magokra
- Eredmények mentése --> eredmenyek_nyers.csv; eredmenyek_osszefoglalo.csv
- Eloszlástulajdonságok kiszámítása
- Eloszláspárok elméleti jellemzőiből és 50-es elemszámú szimulált p-értékeiből DataFrame létrehozása
- Elemzés DataFrame mentése --> elemzes.xlsx


## abrazolasok.R
- Erőfüggvények ábrázolása (globálisan, csoporton belül és csoportok között)
- P-értékek sűrűségfüggvényei
- P-érték hőtérképek
- Korrelációs mátrix az elemzési adatok és a p-értékek között
- Boxplotok az elemzési adatok és a p-értékek között
- Lineáris regressziós modellek


eredmenyek_nyers.csv: nyers p-értékek 100 véletlen maggal, minden próbára, eloszláspárra és elemszámra

eredmenyek_osszefoglalo.csv: aggregált p-értékek és döntési arányok

elemzes.xlsx: eloszláspárok elméleti tulajdonságai és az 50-es elemszám melletti p-értékek

eloszlás_tulajdonsagok.xlsx: elméleti eloszlások tulajdonságai


## A projekt futtatása
- A teljes szimulációs fájl (szimulacio.py) futtatása után létrejönnek az alábbiak
	- eredmenyek_nyers.csv
	- eredmenyek_osszefoglalo.csv
	- elemzes.xlsx
- Az R környezetbe be kell tölteni a fenti fájlokat, és utána lehet futtatni a kódot az ábrázolásokhoz és az elemzések elvégzéséhez


Amennyiben valamelyik használt csomag nincs letöltve a felhasználó gépén:
- Python esetén pip install
- R esetén install.packages("")


## Megjegyzés
A Python szkript futásideje hosszú (akár 5-7 óra),
ezért a program Windows esetén ideiglenesen kikapcsolja az automatikus alvó módot.
Amennyiben a felhasználó nem szeretné a futási időt megvárni, csak az R kódot is lehet futtatni a megfelelő fájlok importálásával.
Az R kódot UTF-8 kódolással érdemes megnyitni.
