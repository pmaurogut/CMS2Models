# TODO: Add comment
# 
# Author: Paco
###############################################################################

setwd("E:\\CMS2\\CMS2Models")
library("plyr")
library("tidyr")
library("RSQLite")

#read FIADB information for PLOT table
CO_FIADB<-"Data/FIADB_CO.db"
db <- dbConnect(RSQLite::SQLite(),CO_FIADB)
PLOT<-dbGetQuery(db, 'SELECT CN, PREV_PLT_CN, STATECD, INVYR, CYCLE, SUBCYCLE,
				UNITCD, COUNTYCD, PLOT, MEASYEAR,MEASMON, MEASDAY, DESIGNCD, PLOT_STATUS_CD FROM PLOT')
colnames(PLOT)[1]<-"PLT_CN"
PLOT<-PLOT[PLOT$DESIGNCD==1,]

TREE<-dbGetQuery(db, 'SELECT PLT_CN, DRYBIO_AG, TPA_UNADJ FROM TREE')
TREE<-TREE[TREE$PLT_CN%in%PLOT$PLT_CN,]

Biomass_FIA<-ddply(TREE,"PLT_CN",function(x){
			BIOMASS_MG_HA_FIA<-sum(x$DRYBIO_AG*x$TPA_UNADJ,na.rm=TRUE)
			BIOMASS_MG_HA_FIA<-BIOMASS_MG_HA_FIA*(0.453592/0.404686)*(1/1000)
			data.frame(BIOMASS_MG_HA_FIA=BIOMASS_MG_HA_FIA)
		},.progress="tk")
save(Biomass_FIA,file="Data/FIA_Response.Rdata")