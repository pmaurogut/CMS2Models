# TODO: Add comment
# 
# Author: Paco
###############################################################################

setwd("E:\\CMS2\\CMS2Models")
library("plyr")
library("tidyr")
library("RSQLite")

load("Data/Modeling_unfiltered.Rdata")

filtered<-Modeling$unfiltered_modeling_df
filtered<-filtered[!is.na(filtered$PLT_CN),]
filtered<-filtered[!is.na(filtered$MEASYEAR),]
#Keep plots +-2years appart from MEASYEAR
filtered<-filtered[abs(filtered$Lidar_year-filtered$MEASYEAR)<=2,]


#read FIADB information for PLOT and COND tables
CO_FIADB<-"Data/FIADB_CO.db"
db <- dbConnect(RSQLite::SQLite(),CO_FIADB)
PLOT<-dbGetQuery(db, 'SELECT * FROM PLOT')
colnames(PLOT)[1]<-"PLT_CN"
COND<-dbGetQuery(db, 'SELECT * FROM COND')

#proportion of Accessible forest land in COND table (Changes in manual: stocked vs canopy cover)
PROP_FOREST<-ddply(COND,"PLT_CN",function(x){
			
			PROP_1<-sum(c(0,x[x$COND_STATUS_CD==1,"CONDPROP_UNADJ"]),na.rm=TRUE)
			PROP_2<-sum(c(0,x[x$COND_STATUS_CD==2,"CONDPROP_UNADJ"]),na.rm=TRUE)
			PROP_3<-sum(c(0,x[x$COND_STATUS_CD==3,"CONDPROP_UNADJ"]),na.rm=TRUE)
			PROP_4<-sum(c(0,x[x$COND_STATUS_CD==4,"CONDPROP_UNADJ"]),na.rm=TRUE)
			PROP_5<-sum(c(0,x[x$COND_STATUS_CD==5,"CONDPROP_UNADJ"]),na.rm=TRUE)
			PROP_ALL<-PROP_1+PROP_2+PROP_3+PROP_4+PROP_5
			data.frame(PROP_1=PROP_1, PROP_2=PROP_2, PROP_3=PROP_3, 
					PROP_4=PROP_4, PROP_5=PROP_5,PROP_ALL=PROP_ALL)
			
			
		})

#Keep plots with at least 0.75 COND_STATUS_CD=1,2,3 or 4 (sampled)
PLT_CNs_CD1<-PROP_FOREST[PROP_FOREST$PROP_1>=0.75, "PLT_CN"]
filtered<-filtered[filtered$PLT_CN%in%PLT_CNs_CD1,]

#Keep prop_convexhull>=0.95
filtered<-filtered[filtered$prop_convexhull>=0.95,]
Modeling$filtered_modeling_df<-filtered
#By Ecoregion
filtered_by_ECO<-list()
for(i in c("L3_KEY","L2_KEY","L1_KEY")){
	names<-names(filtered_by_ECO)
	filtered_by_ECO<-c(filtered_by_ECO,i=I(list(dlply(filtered,i,function(x){x}))))
	names(filtered_by_ECO)<-c(names,i)
	
}
Modeling$filtered_by_ECO<-filtered_by_ECO
save(Modeling,file="Data/Modeling_filtered.RData")
