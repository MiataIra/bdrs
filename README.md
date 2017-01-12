# Monitorering applikasjon for BDR
Denne applikasjon er for Nasjonalt medisinsk kvalitetsregister for barne- og ungdomsdiabetes (BDR)

For å bruke applikasjonen, må du gjøre som følgende:

1. Lag en ny mappe f.eks "bdr-app"
2. Under mappen "bdr-app", lag en mappe til som heter "data"
3. Kopiere alle filene her og lag under mappen "bdr-app"

Kjør denne kommandoen i R

```R
library(shiny)
runApp("sti/til/mappen")
```

Alle pakker som brukes i denne applikasjonen må også installeres først

```R
pakke.list <- c("ggplot2", "dplyr", "shiny")
ny.pakke <- pakke.list[!(pakke.list %in% installed.packages()[,"Package"])]
if(length(ny.pakke)) install.packages(ny.pakke)
```
