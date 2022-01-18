# TODO: Add comment
# 
# Author: Paco
###############################################################################

setwd("E:\\CMS2\\CMS2Models")
library("plyr")
library("RSQLite")

#gets ecoregion
Ecoregion<-read.csv("Data/CO_FIA_Ecoregions_no_coordinates.csv",stringsAsFactors=FALSE)

#gets Climate
Climate<-read.table("Data/climate_metrics_fia_co_no_coords.txt",stringsAsFactors=FALSE,sep=" ",header=TRUE,
		,colClasses=list("PLT_CN"="character","PREV_PLT_CN"="character"))

#Gets areas covered by LIDAR in each plot
Areas<-read.csv("Data/footprint_area_pi_plots.csv",
		stringsAsFactors=FALSE,colClasses=list("plt_cn"="character"))[,1:9]
#Adds proportion of convex_hull wrt plot area for later filtering
Areas$prop_convexhull<-Areas$area_convexhull/(pi*(144*0.3048)^2)
#Keep only essential info
Areas<-Areas[,c("plt_cn","clip_id","lidar_unit","area_convexhull","prop_convexhull",
				"n_flightlines","n_all_returns","n_1st_returns")]
colnames(Areas)[1:2]<-c("PLT_CN","FileTitle")

#Prepares columns and scraps CN from DataFile
All_rtns<-read.csv("Data/CO_FIA_PI_PLOT_AllRtns.csv",stringsAsFactors=FALSE)
All_rtns<-adply(All_rtns,1,function(x){
			cols<-colnames(x)
			info<-strsplit(x$FileTitle,"_")[[1]]
			x$PLT_CN<-info[length(info)]
			x$Lidar_year<-as.integer(info[length(info)-1])
			x[,c("PLT_CN","Lidar_year",cols)]
		},.progress="tk")
#Adds prefix "ALL_" to columns with lidar predictors
colnames(All_rtns)[-c(1:4)]<-paste("ALL",colnames(All_rtns)[-c(1:4)],sep="_")

#Prepares columns and scraps CN from DataFile
First_rtns<-read.csv("Data/CO_FIA_PI_PLOT_1stRtns.csv",stringsAsFactors=FALSE)
First_rtns<-adply(First_rtns,1,function(x){
			cols<-colnames(x)
			info<-strsplit(x$FileTitle,"_")[[1]]
			x$PLT_CN<-info[length(info)]
			x$Lidar_year<-as.integer(info[length(info)-1])
			x[,c("PLT_CN","Lidar_year",cols)]
		},.progress="tk")
#Adds prefix "FIRST_" to columns with lidar predictors
colnames(First_rtns)[-c(1:4)]<-paste("FIRST",colnames(First_rtns)[-c(1:4)],sep="_")

#merges areas ecoregion first and all returns
Predictors<-merge(Areas,Ecoregion,by="PLT_CN",all.x=TRUE)
Predictors<-merge(Predictors,Climate,by="PLT_CN",all.x=TRUE)
Predictors<-merge(Predictors,All_rtns,by=c("PLT_CN","FileTitle"))
Predictors<-merge(Predictors,First_rtns,by=c("PLT_CN","FileTitle","Lidar_year","DataFile"))

#widths<-c(STATECD=4, INVYR=4, CYCLE=2, SUBCYCLE=2,
#		UNITCD=2, COUNTYCD=3, PLOT=5)
#Predictors$StandID<-""
#for(i in names(widths)){
#	Predictors$StandID<-paste(Predictors$StandID,formatC(Predictors[,i],width=widths[i],flag=0),sep="")
#}

#Column names by categories to save
Aux_Lidar<-c("FileTitle", "Lidar_year", "DataFile",
		"lidar_unit","area_convexhull","prop_convexhull",
		"n_flightlines","n_all_returns","n_1st_returns")
Climate_predictors<-colnames(Climate)[6:30]
Eco_predictors<-c("L1_KEY","L2_KEY","L3_KEY")
PLOT_Climate<-colnames(Climate)[2:5]

Predictors_info<-list(Predictors=Predictors,Aux_Lidar=Aux_Lidar,
		Climate_predictors=Climate_predictors,
		Eco_predictors=Eco_predictors,
		PLOT_Climate=PLOT_Climate)
save(Predictors_info,file="Data/Predictors.Rdata")


