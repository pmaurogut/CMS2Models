# TODO: Add comment
# 
# Author: Paco
###############################################################################

setwd("E:\\CMS2\\CMS2Models")
library("plyr")
library("tidyr")
library("RSQLite")

load("Data/Response.Rdata")
load("Data/Predictors.Rdata")
predictors_use<-read.csv("Data/Predictors_Use.csv",stringsAsFactors=FALSE)

Lidar_keep<-predictors_use[predictors_use$Keep,"Column"]
Response<-Responses_modeling$response_df
Predictors<-Predictors_info$Predictors

merge_columns<-c("PLT_CN","PREV_PLT_CN", "STATECD", "INVYR", "CYCLE", "SUBCYCLE",
		"UNITCD", "COUNTYCD", "PLOT", "MEASYEAR", "MEASMON", "MEASDAY",  "DESIGNCD","StandID")
modeling_df<-merge(Response,Predictors,by.x="Stand_CN",by.y="PLT_CN",all.y=TRUE)

Not_in_FVS<-unique(modeling_df[is.na(modeling_df$StandID),"Stand_CN"])
No_crown<-unique(modeling_df[is.na(modeling_df$CBH_m),"Stand_CN"])

FVS<-unique(modeling_df[!is.na(modeling_df$StandID),"Stand_CN"])
With_crown<-unique(modeling_df[!is.na(modeling_df$CBH_m),"Stand_CN"])

#make vectors of column

#read FIADB information for PLOT table
CO_FIADB<-"Data/FIADB_CO.db"
db <- dbConnect(RSQLite::SQLite(),CO_FIADB)
PLOT<-dbGetQuery(db, 'SELECT * FROM PLOT')
colnames(PLOT)[1]<-"PLT_CN"
TREE<-dbGetQuery(db, 'SELECT * FROM TREE')
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

Not_in_TREE<-PLOT[!PLOT$PLT_CN%in%unique(TREE$PLT_CN),]
Not_in_PLOT<-TREE[!TREE$PLT_CN%in%PLOT$PLT_CN,]
COND_NOT_in_TREE<-COND[COND$PLT_CN%in%Not_in_TREE$PLT_CN,
		c("CN","PLT_CN","CONDID","COND_STATUS_CD","COND_NONSAMPLE_REASN_CD")]
PLOTS_NO_TREE<-ddply(COND_NOT_in_TREE,"PLT_CN",function(x){
			
		})


#Not_in_FIADB<-NAs[!NAs%in%PLOT$PLT_CN]
#No_crown_in_FIADB<-No_crown[!No_crown%in%PLOT$PLT_CN]
#IDs_No_Crown<-No_crown[!No_crown%in%unique(TREE$PLT_CN)]
#PLOTS_NO_CROWN<-No_crown[!No_crown%in%unique(PLOT$PLT_CN)]

TREES_Not_in_FVS<-TREE[TREE$PLT_CN%in%Not_in_FVS,]
TREES_No_crown<-TREE[TREE$PLT_CN%in%No_crown,]


Biomass_HW_SW<-ddply(TREES_No_crown,"PLT_CN",function(x){
			SW<-x[x$SPGRPCD<25,]
			HW<-x[x$SPGRPCD>=25,]
			SW<-sum(c(0,SW$DRYBIO_AG*SW$TPA_UNADJ),na.rm=TRUE)
			HW<-sum(c(0,HW$DRYBIO_AG*HW$TPA_UNADJ),na.rm=TRUE)
			data.frame(SW=SW/(SW+HW),
					HW=HW/(SW+HW),
					SW_prop=SW/(SW+HW),
					HW_prop=HW/(SW+HW))
		})