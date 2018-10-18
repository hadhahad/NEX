# NEX - Homework 01
# Written by J. Franc - jiri.franc@fjfi.cvut.cz


# Required libraries
library(car)          # provides a set of useful functions for ANOVA designs and Regression Models;
library(lattice)      # provides some graphical enhancements compared to traditional R graphics, as well as multivariate displays capabilities;
library(lme4)         # the newer and enhanced version of the nlme package, for which additional data structure are available (nested or hierarchical model,. . . );
library(nlme)         # for handling mixed-effects models;
library(pwr)          # power analysis
library(agricolae)    # for Fisher LSD method
library(scatterplot3d)# for 3d scatter plot
# for opening xls files: library(gdata) library(XLConnect) library(xlsReadWrite)

getwd()  # find where are you at the moment 
setwd("D:/Vyuka/NEX/2018/03_Factorial_design")  


# HOMEWORK
#
# Dot in circle problem
#
#
# UKOL: Zjistete jaky má vliv velikost kruhu a použití ruky kterou experiment provadime
#          (leva, prava, obe) na počet teček uvnitr kruhu za 10s (5s)
# Provedení: Využijte šablonu a po dobu 10 sekund (stopuje vam kolega) 
#              se snazte udelat na stridacku do obou kruhu co nejvice tecek.
#            (snazte se trefovat a delat tecky, ne cary pres pulku kruhu. 
#              muzete se treba penalizovat za tecku mimo apod.)
#
# 1) namerte data: experiment nastavte tak, a byl znahodneny !!!!!
#                  poradi mereni si poznamenejte pro dalsi kontrolu pripadne zavislosti na poradi mereni
#                  Jednotlive lidi ve skupine berte jako ruzne operatory experimentu (blokujte)
#                  Pocet replikaci u jednoho cloveka a jednoho casu vemte 1
#
#  2)  Spoctete zakladni statistiky (mean, median a sd pro jednotlive faktory - velikost, ruka, operator)
#      Zobrazte namerena data (box plot, interaction plot, effects plot, ...) a okomentujte je
#      co z danych obrazku muzeme pred samotnou analyzou rici o vysledku?
#  
#   3) Zamerte se zvlaste na faktory ruka a velikost kola. Otestujte hypotezu o schodnosti rozptylu pro jednotlive urovne
#      a vhodnym testem overte stejnost strednich hodnot. 
#      Provedte Tukey HSD a Fisher LSD test pro parove porovnani stednich hodnot jednotlivych skupin s vybranou korekci phodnoty.
#
#   4) Analyzute data pomoci ANOVA (vsechny promenne berte jako faktor)
#          - s predpokladem, ze kazdy z ucastniku experimentu tvori jeden blok.
#          - diskutujte vysledky, overte predpoklady, vykreslete QQ-plot, residua x fitted values, resida x cas, ...
#          - diskutujte vliv znahodneni experimentu a vyvoj rezidui v case (cislo mereni)
#
#   5) Porovnejte a diskutujte vysledky z bodu 3 a 4.
#
#   6) Pokud data nesplnuji predpoklady pro pouziti ANOVA, diskutujte mozne transformace (logaritmicka, Box-Cox, ...)
#      a duvod proc data predpoklady nesplnuji? Vyskyt outlieru, zpusob mereni, divny operator, ...
#
#   7) Vyberte nejvhodnejsi model a i kdyby nesplnoval predpoklady pro pouziti ANOVA reste nasledujici: 
#   - Spoctete silu testu v ANOVA (pro max. dvoufaktorovou analyzu
#                                  - jeden z faktoru zanedbejte, nebo vezmete mereni pro jednu jeho konkretni uroven)
#   - Predpokladejme, ze standartni odchylka disturbanci bude pro provadeny experiment 4
#     a maximalni (pro nas signifikantni) rozdil, ktery chceme detekovat je 5 bodu v kruhu.
#     Spoctete pocet potrebnych replikaci, aby sila vysledneho testu byla vetsi nez 0.9
# 
#   5) Vytvorte regresni model, kde nebudete uvazovat bloky a velikost kruhu bude kvantitativni promenna.
#      - zkuste pridat do modelu i druhou mocninu a porovnejte dva regresni modely mezi s sebou a vyberte vhodnejsi
#      - overte predpoklady pro pouziti vybraneho modelu a vykreslete QQ-plot, residua x fitted values, resida x prumer , ...
#

#   Pozn: V pripade, ze bloky jsou velmi dominantni a zadna z dalsich promennych se nejevy vyznamna,
#      pouzijte pro vyslednou analyzu jen "operatora s daty nejlepsimi pro akademicke ucely".
#
#
#    Pozn: rozsireni ulohy, zmerte jeste zavislost na case. (5s, 10s, 15s)

