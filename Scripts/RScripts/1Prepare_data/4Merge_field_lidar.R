# TODO: Add comment
# 
# Author: Paco
###############################################################################

setwd("E:\\CMS2\\CMS2Models")
library("plyr")
library("tidyr")
library("RSQLite")

load("Data/FVS_Response.Rdata")
load("Data/FIA_Response.Rdata")
load("Data/Predictors.Rdata")
predictors_use<-read.csv("Data/Predictors_Use.csv",stringsAsFactors=FALSE)
Lidar_keep<-predictors_use[predictors_use$Keep,"Column"]

Modeling<-Responses_modeling
Modeling$response_df_FIA<-Biomass_FIA
Predictors<-Predictors_info$Predictors
Modeling$predictors_df<-Predictors

modeling_df<-merge(Responses_modeling$response_df,Biomass_FIA,
		by.x="Stand_CN",by.y="PLT_CN",all.x=TRUE)
Modeling$columns$Responses<-colnames(modeling_df)[-c(1:15)]

modeling_df<-merge(modeling_df,Predictors,
		by.x="Stand_CN",by.y="PLT_CN",all.y=TRUE)
colnames(modeling_df)[1]<-"PLT_CN"

#make vectors of columnames by categories
Lidar_Keep2<-setdiff(Lidar_keep[-1],Responses_modeling$columns$FVS_Keep)
Modeling$columns$Aux_Lidar<-Predictors_info$Aux_Lidar
Modeling$columns$Predictors_Lidar<-Lidar_Keep2[-c(1:22)]
Modeling$columns$Lidar_ALL<-grep("ALL_",Modeling$columns$Predictors_Lidar,value=TRUE)
Modeling$columns$Lidar_FIRST<-grep("FIRST_",Modeling$columns$Predictors_Lidar,value=TRUE)
Modeling$columns$ECO<-Predictors_info$Eco_predictors
Modeling$columns$Climate<-Predictors_info$Climate_predictors

#read FIADB information for PLOT table
CO_FIADB<-"Data/FIADB_CO.db"
db <- dbConnect(RSQLite::SQLite(),CO_FIADB)
PLOT<-dbGetQuery(db, 'SELECT CN, PREV_PLT_CN, STATECD, INVYR, CYCLE, SUBCYCLE,
				UNITCD, COUNTYCD, PLOT, MEASYEAR,MEASMON, MEASDAY, DESIGNCD, ELEV FROM PLOT')
dbDisconnect(db)
colnames(PLOT)[1]<-"PLT_CN"
FIA_cols<-c("PLT_CN",Predictors_info$PLOT_Climate)
modeling_df<-merge(modeling_df,PLOT,
		by=FIA_cols,all.x=TRUE)
FIA_cols<-c(FIA_cols, "STATECD","CYCLE","SUBCYCLE",
		"UNITCD","COUNTYCD","PLOT","MEASMON","MEASDAY","DESIGNCD")

Modeling$columns$Topo<-"ELEV"
Modeling$columns$All_Predictors<-c(Modeling$columns$Lidar_ALL,
		Modeling$columns$Lidar_FIRST,Modeling$columns$Climate,
		Modeling$columns$Topo,Modeling$columns$ECO)

cols<-unlist(Modeling$columns[c("FVS_Keep","Aux_Lidar","All_Predictors","Responses")])
cols<-c(FIA_cols,cols)

Modeling$unfiltered_modeling_df<-modeling_df[,cols]
#save modeling data frame and vectors with column names by categories for later use
save(Modeling,file="Data/Modeling_unfiltered.Rdata")