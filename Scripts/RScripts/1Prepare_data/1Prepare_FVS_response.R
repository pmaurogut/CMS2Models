# TODO: Add comment
# 
# Author: Paco
###############################################################################

setwd("E:\\CMS2\\CMS2Models")
library("plyr")
library("tidyr")
library("RSQLite")


#Smooth canopy profile with 14 foot running means
smooth_canopy<-function(x){
	
	heights<-data.frame(Height_ft=c(0:(max(x$Height_ft,na.rm=TRUE)+14)))
	x<-merge(heights,x,by="Height_ft",all.x=TRUE)
	x$Canopy_Fuel_kg_m3<-ifelse(is.na(x$Canopy_Fuel_kg_m3),0,x$Canopy_Fuel_kg_m3)
	x<-x[order(x$Height_ft,decreasing=TRUE),]
	for(i in 1:dim(x)[1]){
		from<-x$Height_ft[i]
		to<-from-14
		x[i,"Canopy_Fuel_kg_m3_smoothed"]<-mean(x[x$Height_ft<=from&x$Height_ft>=to,"Canopy_Fuel_kg_m3"])
	}
	
	return(x)
}
#calculates CBD and CBH from smooothed profiles
get_canopy_attributes<-function(x){
#	res<-x[1,c(10,11,2:4,8,9,12)]
	res<-x[1,]
	res$Eff_CBD<-max(x[,"Canopy_Fuel_kg_m3_smoothed"],na.rm=TRUE)
	res$Ht_Eff_CBD_m<-mean(x[x[,"Canopy_Fuel_kg_m3_smoothed"]==res$Eff_CBD,"Height_ft"],na.rm=TRUE)*0.3048
	res$CBH_m<-min(x[x[,"Canopy_Fuel_kg_m3_smoothed"]>0.011,"Height_ft"],na.rm=TRUE)*0.3048
	res$CBH_m<-ifelse(res$CBH_m==Inf,NA,res$CBH_m)
	res
}

dbOut<-"Data/FVS_project/FVSOut.db"
dbOutConn <- dbConnect(RSQLite::SQLite(),dbOut)

cases<-dbGetQuery(dbOutConn, "SELECT * FROM FVS_Cases")[,c("Stand_CN","StandID")]
tables<-c("FVS_Summary2","FVS_Fuels","FVS_Carbon","FVS_CanProfile")
names(tables)<-tables
tables<-llply(tables,function(x){
			print(x)
			query<-paste0("SELECT * FROM ",x)
			res<-dbGetQuery(dbOutConn, query)
			res<-merge(cases,res,by="StandID")
			if(x=="FVS_Cases"){
				return(res)
			}else{
				res[res$Year==1990,]
			}
			
		})

tables$FVS_CanProfile<-ddply(tables$FVS_CanProfile,c("StandID"),smooth_canopy,.progress="tk")
tables$CanopyPars<-ddply(tables$FVS_CanProfile,c("StandID"),get_canopy_attributes,.progress="tk")

response_df<-merge(tables$FVS_Summary2,tables$FVS_Fuels,by=c("StandID","Stand_CN","CaseID","Year"))
response_df<-merge(response_df,tables$FVS_Carbon,by=c("StandID","Stand_CN","CaseID","Year"))
response_df<-merge(response_df,tables$CanopyPars,by=c("StandID","Stand_CN","CaseID","Year"),all.x=TRUE)

#Add info from cases
cases<-dbGetQuery(dbOutConn, "SELECT * FROM FVS_Cases")
response_df<-merge(cases,response_df,by=c("CaseID","Stand_CN","StandID"),all.y=TRUE)
dbDisconnect(dbOutConn)

#Column names to keep
FVS_Keep<-c("StandID","Year","CaseID",
		"Stand_CN", "MgmtID",
		"RunTitle", "KeywordFile",
		"SamplingWt", "Variant",
		"Version", "RV",
		"Groups", "RunDateTime" )
Responses<-setdiff(colnames(response_df),c(FVS_Keep))

Responses_modeling<-list(tables=tables,response_df=response_df,
		columns=list(FVS_Kepp=FVS_Keep,Responses=Responses))
save(Responses_modeling,file="Data/FVS_Response.Rdata")
