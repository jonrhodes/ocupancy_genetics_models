#FUNCTIONS
get.winpred <- function(Survey,SitesperGrid,WinPred,GridID,ID)
{
	#create output
	Output <- matrix(NA,nrow=nrow(Points_HabOnly),ncol=2)

	Output[,1] <- Survey[,ID]
	
	#get predictions
	for (i in 1:nrow(SitesperGrid))
	#loop through grids
	{
		for (j in 1:SitesperGrid[i,2])
		#loop through sites
		{
			Pred <- mean(WinPred[,paste("predp.",i,".",j,".",sep="")])
			
			PID <- Survey[which(Survey[,GridID] == SitesperGrid[i,1]),][j,ID]
			
			Output[which(Output[,1] == PID),2] <- Pred		
		}
	}
	
	return(Output)
}			

#SCRIPT

#aflav
WinPred1 <- read.table(paste(getwd(),"/antechinuscoda_pred_nogen_speciesaflav_conmod1_chain1.csv",sep=""),header=T,sep=",")
WinPred2 <- read.table(paste(getwd(),"/antechinuscoda_pred_nogen_speciesaflav_conmod1_chain2.csv",sep=""),header=T,sep=",")
WinPred3 <- read.table(paste(getwd(),"/antechinuscoda_pred_nogen_speciesaflav_conmod1_chain3.csv",sep=""),header=T,sep=",")
WinPred <- rbind(WinPred1,WinPred2,WinPred3)	
rm(WinPred1)
rm(WinPred2)
rm(WinPred3)
PointPreds <- get.winpred(Points_HabOnly,NumPointsGrid,WinPred,"ID_2_5km","GID")
write.table(PointPreds,"points_preds.csv",sep=",",row.names=F)

#koala
WinPred1 <- read.table(paste(getwd(),"/koalacoda_pred_nogen_specieskoala_conmod1_chain1.csv",sep=""),header=T,sep=",")
WinPred2 <- read.table(paste(getwd(),"/koalacoda_pred_nogen_specieskoala_conmod1_chain2.csv",sep=""),header=T,sep=",")
WinPred3 <- read.table(paste(getwd(),"/koalacoda_pred_nogen_specieskoala_conmod1_chain3.csv",sep=""),header=T,sep=",")
WinPred <- rbind(WinPred1,WinPred2,WinPred3)
rm(WinPred1)
rm(WinPred2)
rm(WinPred3)
PointPreds <- get.winpred(Points_HabOnly,NumPointsGrid,WinPred,"ID_2_5km","GID")
write.table(PointPreds,"points_preds.csv",sep=",",row.names=F)

#pnorf
WinPred1 <- read.table(paste(getwd(),"/squirrel_glidercoda_pred_nogen_speciespnorf_conmod1_chain1.csv",sep=""),header=T,sep=",")
WinPred2 <- read.table(paste(getwd(),"/squirrel_glidercoda_pred_nogen_speciespnorf_conmod1_chain2.csv",sep=""),header=T,sep=",")
WinPred3 <- read.table(paste(getwd(),"/squirrel_glidercoda_pred_nogen_speciespnorf_conmod1_chain3.csv",sep=""),header=T,sep=",")
WinPred <- rbind(WinPred1,WinPred2,WinPred3)
rm(WinPred1)
rm(WinPred2)
rm(WinPred3)
PointPreds <- get.winpred(Points_HabOnly,NumPointsGrid,WinPred,"ID_2_5km","GID")
write.table(PointPreds,"points_preds.csv",sep=",",row.names=F)

#pbrev
WinPred1 <- read.table(paste(getwd(),"/sugar_glidercoda_pred_nogen_speciespbrev_conmod1_chain1.csv",sep=""),header=T,sep=",")
WinPred2 <- read.table(paste(getwd(),"/sugar_glidercoda_pred_nogen_speciespbrev_conmod1_chain2.csv",sep=""),header=T,sep=",")
WinPred3 <- read.table(paste(getwd(),"/sugar_glidercoda_pred_nogen_speciespbrev_conmod1_chain3.csv",sep=""),header=T,sep=",")
WinPred <- rbind(WinPred1,WinPred2,WinPred3)
rm(WinPred1)
rm(WinPred2)
rm(WinPred3)
PointPreds <- get.winpred(Points_HabOnly,NumPointsGrid,WinPred,"ID_2_5km","GID")
write.table(PointPreds,"points_preds.csv",sep=",",row.names=F)







