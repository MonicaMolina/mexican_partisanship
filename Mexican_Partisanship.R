#27/09/21

#Mexican Partisanship and independent candidates for federal deputy elections study

pacman::p_load("tidyverse", "stargazer", "Matrix", "foreign", "readxl", "plyr", "lfe", "dplyr")

#Electoral data sets (unit 2000-2015: electoral sections 2018-2021: voting booth)

dip00 <- read.csv("Input/DiputadosMR2000Seccion.csv", header=T)
dip03 <- read.csv("Input/DiputadosMR2003Seccion.csv", header=T)
dip06 <- read.csv("Input/DiputadosMR2006Seccion.csv", header=T)
dip09 <- read.csv("Input/DiputadosMR2009Seccion.csv", header=T)
dip12 <- read.csv("Input/DiputadosMR2012Seccion.csv", header=T)
dip15 <- read.csv("Input/DiputadosMR2015Seccion.csv", header=T)
dip18 <- read.csv("Input/DiputadosMR2018Seccion.csv", header=T, skip = 6, na.strings=c ("-", "NA"))
dip21 <- read.csv("Input/DiputadosMR2021Seccion.csv", header=T, skip = 6, na.strings=c ("-", "NA"), sep = "|")

head(dip00)
head(dip03)
head(dip06)
head(dip09)
head(dip12)
head(dip15)
head(dip18)
head(dip21)

#year

dip00$year <- 2000
dip03$year <- 2003
dip06$year <- 2006
dip09$year <- 2009
dip12$year <- 2012
dip15$year <- 2015
dip18$year <- 2018
dip21$year <- 2021

#ID

dip00$ID <- with (dip00, factor (paste (ID_ESTADO, SECCION, sep="_")))

dip03$ID <- with(dip03, factor (paste (ID_ESTADO, SECCION, sep="_")))

dip06$ID <- with (dip06, factor (paste (ID_ESTADO, SECCION, sep="_")))

dip09$ID <- with(dip09, factor (paste (ID_ESTADO, SECCION, sep="_")))

dip12$ID <- with (dip12, factor (paste (ID_ESTADO, SECCION, sep="_")))

dip15$ID <- with(dip15, factor (paste (ID_ESTADO, SECCION, sep="_")))

dip18$ID <- with (dip18, factor (paste (ID_ESTADO, SECCION, sep="_")))

dip21$ID <- with (dip21, factor (paste (ID_ESTADO, SECCION, sep="_")))

#Summary

summary (dip00)
summary (dip03)
summary (dip06)
summary (dip09)
summary (dip12)
summary (dip15)
summary (dip18)
summary (dip21)

#Variable selection

dip00 <- dip00 %>% select(year,ID_ESTADO, ID_DISTRITO, ID_MUNICIPIO, SECCION, AC, PRI, AM, PCD, PARM, DSPPN,  NUM_VOTOS_CAN_NREG, NUM_VOTOS_NULOS, TOTAL_VOTOS, ID)

dip03 <- dip03 %>% select(year,ID_ESTADO, ID_DISTRITO, ID_MUNICIPIO, SECCION, PAN, APM, PBT, NVA_ALIANZA, ASDC,  NUM_VOTOS_CAN_NREG, NUM_VOTOS_NULOS, TOTAL_VOTOS,LISTA_NOMINAL, ID)

dip06 <- dip06 %>% select(year,ID_ESTADO, ID_DISTRITO, ID_MUNICIPIO, SECCION, PAN, APM, PBT, NVA_ALIANZA, ASDC,  NUM_VOTOS_CAN_NREG, NUM_VOTOS_NULOS, TOTAL_VOTOS,LISTA_NOMINAL, ID)

summary(dip06)

dip09 <- dip09 %>% select(year, ID_ESTADO, ID_DISTRITO, ID_MUNICIPIO, SECCION, PAN, PRI, PRD, PVEM, PT, CONVERGENCIA, NVA_ALIANZA, PSD, PRIMERO_MEXICO, SALVEMOS_MEXICO,  NUM_VOTOS_CAN_NREG, NUM_VOTOS_NULOS, TOTAL_VOTOS,LISTA_NOMINAL, ID)

summary(dip09)

dip12 <- dip12 %>% select(year,ID_ESTADO, ID_DISTRITO, ID_MUNICIPIO, SECCION, PAN, PRI, PRD, PVEM, PT,MC, NVA_ALIANZA, PRI_PVEM, PRD_PT_MC, PRD_PT, PRD_MC, PT_MC, NUM_VOTOS_CAN_NREG, NUM_VOTOS_NULOS, TOTAL_VOTOS, LISTA_NOMINAL, ID)

summary(dip12)

dip15 <- dip15 %>% select(year,ID_ESTADO, ID_DISTRITO, ID_MUNICIPIO, SECCION, PAN, PRI, PRD, PVEM, PT, MC, NVA_ALIANZA, MORENA, PH, ES, PAN_NVA_ALIANZA, PRI_PVEM, PRD_PT, CAND_IND1, CAND_IND2, NUM_VOTOS_CAN_NREG, NUM_VOTOS_NULOS, TOTAL_VOTOS, LISTA_NOMINAL, ID)

summary(dip15)

#plyr doesn't let the group_by and summarise work properly
detach(package:plyr)

#addig voting booths into electoral sections

dip18 <- dip18 %>% group_by(year, ID_ESTADO, ID_DISTRITO, SECCION, ID) %>% summarise(PAN=sum(PAN, na.rm=T), PRI=sum(PRI, na.rm=T), PRD=sum(PRD, na.rm=T), PVEM=sum(PVEM, na.rm=T), PT=sum(PT, na.rm=T), MOVIMIENTO.CIUDADANO=sum(MOVIMIENTO.CIUDADANO, na.rm=T), NUEVA.ALIANZA=sum(NUEVA.ALIANZA, na.rm=T), MORENA=sum(MORENA, na.rm=T), ENCUENTRO.SOCIAL=sum(ENCUENTRO.SOCIAL, na.rm=T), PAN_PRD_MC=sum(PAN_PRD_MC, na.rm=T), PAN_PRD=sum(PAN_PRD, na.rm=T), PAN_MC=sum(PAN_MC, na.rm=T), PRD_MC=sum(PRD_MC, na.rm=T), PRI_PVEM_NA=sum(PRI_PVEM_NA, na.rm=T), PRI_PVEM=sum(PRI_PVEM, na.rm=T), PVEM_NA=sum(PVEM_NA, na.rm=T), PRI_NA=sum(PRI_NA, na.rm=T), PT_MORENA_PES=sum(PT_MORENA_PES, na.rm=T), PT_MORENA=sum(PT_MORENA, na.rm=T), PT_PES=sum(PT_PES, na.rm=T), MORENA_PES=sum(MORENA_PES, na.rm=T), CAND_IND_01=sum(CAND_IND_01, na.rm=T), CAND_IND_02=sum(CAND_IND_02, na.rm=T), CNR=sum(CNR, na.rm=T), VN=sum(VN, na.rm=T), TOTAL_VOTOS_CALCULADOS=sum(TOTAL_VOTOS_CALCULADOS, na.rm=T), LISTA_NOMINAL_CASILLA=sum(LISTA_NOMINAL_CASILLA, na.rm=T))

summary(dip18)

summary(dip18$PVEM_NA)

summary(dip21)

dip21 <- dip21 %>% group_by(year, ID_ESTADO, ID_DISTRITO, ID, SECCION) %>% summarise(PAN=sum(PAN, na.rm=T), PRI=sum(PRI, na.rm=T), PRD=sum(PRD, na.rm=T), PVEM=sum(PVEM, na.rm=T), PT=sum(PT, na.rm=T), MC=sum(MC, na.rm=T), MORENA=sum(MORENA, na.rm=T), PES=sum(PES, na.rm=T), RSP=sum(RSP, na.rm=T), FXM=sum(FXM, na.rm=T), PAN.PRI.PRD=sum(PAN.PRI.PRD, na.rm=T), PAN.PRI=sum(PAN.PRI, na.rm=T), PAN.PRD=sum(PAN.PRD, na.rm=T), PRI.PRD=sum(PRI.PRD, na.rm=T), PVEM.PT.MORENA=sum(PVEM.PT.MORENA, na.rm=T), PVEM.PT=sum(PVEM.PT, na.rm=T), PVEM.MORENA=sum(PVEM.MORENA, na.rm=T), PT.MORENA=sum(PT.MORENA, na.rm=T), CI=sum(CI, na.rm=T), CNR=sum(CANDIDATO.A.NO.REGISTRADO.A, na.rm=T), VN=sum(VOTOS.NULOS, na.rm=T), TOTAL_VOTOS_CALCULADOS=sum(TOTAL_VOTOS_CALCULADOS, na.rm=T), LISTA_NOMINAL_CASILLA=sum(LISTA_NOMINAL_CASILLA, na.rm=T))

# Rename the 2018 and 2021 variables

summary(dip18) 
colnames(dip18) <- c("year","ID_ESTADO", "ID_DISTRITO", "SECCION", "ID", "PAN", "PRI", "PRD", "PVEM", "PT", "MC", "NVA_ALIANZA", "MORENA", "ES", "PAN_PRD_MC", "PAN_PRD", "PAN_MC", "PRD_MC", "PRI_PVEM_NA", "PRI_PVEM", "PVEM_NA", "PRI_NA", "PT_MORENA_PES", "PT_MORENA", "PT_PES", "MORENA_PES", "CAND_IND1", "CAND_IND2", "NUM_VOTOS_CAN_NREG", "NUM_VOTOS_NULOS", "TOTAL_VOTOS", "LISTA_NOMINAL" )
summary(dip18) 

summary(dip21)
colnames(dip21) <- c("year","ID_ESTADO", "ID_DISTRITO", "ID", "SECCION", "PAN", "PRI", "PRD", "PVEM", "PT", "MC", "MORENA", "ES", "RSP", "FXM","PAN_PRI_PRD", "PAN_PRI", "PAN_PRD", "PRI_PRD", "PVEM_PT_MORENA", "PVEM_PT", "PVEM_MORENA", "PT_MORENA", "CAND_IND1", "NUM_VOTOS_CAN_NREG", "NUM_VOTOS_NULOS", "TOTAL_VOTOS", "LISTA_NOMINAL" )
summary(dip21)

#LISTA_NOMINAL 0 -> NA

dip18[, 32][dip18[, 32] == 0] <- NA
summary(dip18) 

#reactivate plyr for merge
library("plyr")

#mergin all elections
elecciones <- rbind.fill (dip00, dip03, dip06, dip09, dip12, dip15, dip18, dip21)
summary(elecciones)

# Reseccionamiento ID

elecciones$id18 <- "NA"
elecciones$id12 <- "NA"
elecciones$id09 <- "NA"
elecciones$id06 <- "NA"
elecciones$id03 <- "NA"
elecciones$id00 <- "NA"

summary(elecciones)
elecciones$idseccion <- with(elecciones, factor (paste (ID_ESTADO, SECCION, sep="_")))
summary(elecciones)

#Data set reseccionamiento, we only use the variables that indicates action, origen/destiny and the time the change was made

eqSeccion <- read.csv("Input/tablaEquivalenciasSeccionalesDesde1994.csv")
summary(eqSeccion)

class(eqSeccion$action)
class(eqSeccion$action2)
class(eqSeccion$action3)
class(eqSeccion$orig.dest)
class(eqSeccion$orig.dest2)
class(eqSeccion$orig.dest3)
class(eqSeccion$when)
class(eqSeccion$when.1)
class(eqSeccion$when2)
