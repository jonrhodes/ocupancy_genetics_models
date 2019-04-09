library("foreign")
library("R2WinBUGS")

#DATA SET UP

#read in data
#Surveys <- read.table("fauna_join_5km_2_5km_100m_250m_500m.csv",sep=",",header=T) #don't load this again as I have changed this table to correct errors
Grid5km <- read.table("fin_5km_grid.csv",sep=",",header=T)
Grid2_5km <- read.table("fin_2_5km_grid.csv",sep=",",header=T)
Grid_100m <- read.table("sub_100m_grid_CRE.csv",sep=",",header=T)
Grid_250m <- read.table("sub_250m_grid_CRE.csv",sep=",",header=T)
Grid_500m <- read.table("sub_500m_grid_CRE.csv",sep=",",header=T)

#get species specific surveys
Surveys_Pnor <- Surveys[which(Surveys[,"SPECIES"]=="P.nor"),]
Surveys_Pbre <- Surveys[which(Surveys[,"SPECIES"]=="P.bre"),]
Surveys_Aflav <- Surveys[which(Surveys[,"SPECIES"]=="A.flav"),]
Surveys_Pcin <- Surveys[which(Surveys[,"SPECIES"]=="P.cin"),]

#work out how many sites for each grid (the first column is the site ID and the second is the number of repeats)
#5km
Sites_Grid5km <- aggregate(Surveys_Pnor[,"ID_5km"],by=list(Surveys_Pnor[,"ID_5km"]),FUN=length)
#2.5km
Sites_Grid2_5km <- aggregate(Surveys_Pnor[,"ID_2_5km"],by=list(Surveys_Pnor[,"ID_2_5km"]),FUN=length)

#get the cummulative number of sites in each grid
Sites_Grid5km_Cum <- Sites_Grid5km
for (i in 2:nrow(Sites_Grid5km_Cum))
{
	Sites_Grid5km_Cum[i,2] <- Sites_Grid5km_Cum[i - 1,2] + Sites_Grid5km[i,2] 
}
Sites_Grid2_5km_Cum <- Sites_Grid2_5km
for (i in 2:nrow(Sites_Grid2_5km_Cum))
{
	Sites_Grid2_5km_Cum[i,2] <- Sites_Grid2_5km_Cum[i - 1,2] + Sites_Grid2_5km[i,2] 
}
rm(i)

#set up counts data array
#5km
Counts5km_Pnor <- get.3dgrid(Surveys_Pnor,Sites_Grid5km,c("NC1","NC2","NC3","NC4","NC5"),"ID_5km",TRUE)
Counts5km_Pbre <- get.3dgrid(Surveys_Pbre,Sites_Grid5km,c("NC1","NC2","NC3","NC4","NC5"),"ID_5km",TRUE)
Counts5km_Aflav <- get.3dgrid(Surveys_Aflav,Sites_Grid5km,c("NC1","NC2","NC3","NC4","NC5"),"ID_5km",TRUE)
Counts5km_Pcin <- get.3dgrid(Surveys_Pcin,Sites_Grid5km,c("NC1","NC2","NC3","NC4","NC5"),"ID_5km",TRUE)
#2.5km
Counts2_5km_Pnor <- get.3dgrid(Surveys_Pnor,Sites_Grid2_5km,c("NC1","NC2","NC3","NC4","NC5"),"ID_2_5km",TRUE)
Counts2_5km_Pbre <- get.3dgrid(Surveys_Pbre,Sites_Grid2_5km,c("NC1","NC2","NC3","NC4","NC5"),"ID_2_5km",TRUE)
Counts2_5km_Aflav <- get.3dgrid(Surveys_Aflav,Sites_Grid2_5km,c("NC1","NC2","NC3","NC4","NC5"),"ID_2_5km",TRUE)
Counts2_5km_Pcin <- get.3dgrid(Surveys_Pcin,Sites_Grid2_5km,c("NC1","NC2","NC3","NC4","NC5"),"ID_2_5km",TRUE)

#set up presence/absence data array
#5km
PA5km_Pnor <- get.3dgrid(Surveys_Pnor,Sites_Grid5km,c("NC1","NC2","NC3","NC4","NC5"),"ID_5km",FALSE)
PA5km_Pbre <- get.3dgrid(Surveys_Pbre,Sites_Grid5km,c("NC1","NC2","NC3","NC4","NC5"),"ID_5km",FALSE)
PA5km_Aflav <- get.3dgrid(Surveys_Aflav,Sites_Grid5km,c("NC1","NC2","NC3","NC4","NC5"),"ID_5km",FALSE)
PA5km_Pcin <- get.3dgrid(Surveys_Pcin,Sites_Grid5km,c("NC1","NC2","NC3","NC4","NC5"),"ID_5km",FALSE)
#2.5km
PA2_5km_Pnor <- get.3dgrid(Surveys_Pnor,Sites_Grid2_5km,c("NC1","NC2","NC3","NC4","NC5"),"ID_2_5km",FALSE)
PA2_5km_Pbre <- get.3dgrid(Surveys_Pbre,Sites_Grid2_5km,c("NC1","NC2","NC3","NC4","NC5"),"ID_2_5km",FALSE)
PA2_5km_Aflav <- get.3dgrid(Surveys_Aflav,Sites_Grid2_5km,c("NC1","NC2","NC3","NC4","NC5"),"ID_2_5km",FALSE)
PA2_5km_Pcin <- get.3dgrid(Surveys_Pcin,Sites_Grid2_5km,c("NC1","NC2","NC3","NC4","NC5"),"ID_2_5km",FALSE)

#work out how many repeats there are for each grid by site combination
#5km
Site_Repeats5km_Pnor <- get.repeats(PA5km_Pnor,Sites_Grid5km)
Site_Repeats5km_Pbre <- get.repeats(PA5km_Pbre,Sites_Grid5km)
Site_Repeats5km_Aflav <- get.repeats(PA5km_Aflav,Sites_Grid5km)
Site_Repeats5km_Pcin <- get.repeats(PA5km_Pcin,Sites_Grid5km)
#2.5km
Site_Repeats2_5km_Pnor <- get.repeats(PA2_5km_Pnor,Sites_Grid2_5km)
Site_Repeats2_5km_Pbre <- get.repeats(PA2_5km_Pbre,Sites_Grid2_5km)
Site_Repeats2_5km_Aflav <- get.repeats(PA2_5km_Aflav,Sites_Grid2_5km)
Site_Repeats2_5km_Pcin <- get.repeats(PA2_5km_Pcin,Sites_Grid2_5km)

#get the presence/absence for the grids based on Wildnet data - since 2000
#5km
GridPA_5km_Pnor <- ifelse(Grid5km[,"sq_a00"]==-9999,NA,Grid5km[,"sq_a00"])
GridPA_5km_Pbre <- ifelse(Grid5km[,"sg_a00"]==-9999,NA,Grid5km[,"sg_a00"])
GridPA_5km_Aflav <- ifelse(Grid5km[,"yfa_a00"]==-9999,NA,Grid5km[,"yfa_a00"])
GridPA_5km_Pcin <- ifelse(Grid5km[,"kol_a00"]==-9999,NA,Grid5km[,"kol_a00"])
#2.5km
GridPA_2_5km_Pnor <- ifelse(Grid2_5km[,"sq_a00"]==-9999,NA,Grid2_5km[,"sq_a00"])
GridPA_2_5km_Pbre <- ifelse(Grid2_5km[,"sg_a00"]==-9999,NA,Grid2_5km[,"sg_a00"])
GridPA_2_5km_Aflav <- ifelse(Grid2_5km[,"yfa_a00"]==-9999,NA,Grid2_5km[,"yfa_a00"])
GridPA_2_5km_Pcin <- ifelse(Grid2_5km[,"kol_a00"]==-9999,NA,Grid2_5km[,"kol_a00"])

#get grid level covariates
#percent urban
URB5km <- Grid5km[,"urbanPerc"]
URB2_5km <- Grid2_5km[,"urbanPerc"]
#percent native vegetation
NV5km <- Grid5km[,"RE_NF"] + Grid5km[,"RE_RF"] + Grid5km[,"RE_DF"] + Grid5km[,"RE_SF"]   
NV2_5km <- Grid2_5km[,"RE_NF"] + Grid2_5km[,"RE_RF"] + Grid2_5km[,"RE_DF"] + Grid2_5km[,"RE_SF"]   
#percent native forest
NF5km <- Grid5km[,"RE_RF"] + Grid5km[,"RE_DF"] + Grid5km[,"RE_SF"]   
NF2_5km <- Grid2_5km[,"RE_RF"] + Grid2_5km[,"RE_DF"] + Grid2_5km[,"RE_SF"]   

#get home range level covariates
#FPC > 0.5 m
FPCH3_5km_500 <- get.hr.covariate(Surveys_Pnor,Sites_Grid5km,"ID_5km","ID_500m",Grid_500m,"FPC_H3")
FPCH3_5km_250 <- get.hr.covariate(Surveys_Pnor,Sites_Grid5km,"ID_5km","ID_250m",Grid_250m,"FPC_H3")
FPCH3_5km_100 <- get.hr.covariate(Surveys_Pnor,Sites_Grid5km,"ID_5km","ID_100m",Grid_100m,"FPC_H3")
FPCH3_2_5km_500 <- get.hr.covariate(Surveys_Pnor,Sites_Grid2_5km,"ID_2_5km","ID_500m",Grid_500m,"FPC_H3")
FPCH3_2_5km_250 <- get.hr.covariate(Surveys_Pnor,Sites_Grid2_5km,"ID_2_5km","ID_250m",Grid_250m,"FPC_H3")
FPCH3_2_5km_100 <- get.hr.covariate(Surveys_Pnor,Sites_Grid2_5km,"ID_2_5km","ID_100m",Grid_100m,"FPC_H3")
#Highways and main roads
RTHIGH_5km_500 <- get.hr.covariate(Surveys_Pnor,Sites_Grid5km,"ID_5km","ID_500m",Grid_500m,"RT_HIGH")
RTHIGH_5km_250 <- get.hr.covariate(Surveys_Pnor,Sites_Grid5km,"ID_5km","ID_250m",Grid_250m,"RT_HIGH")
RTHIGH_5km_100 <- get.hr.covariate(Surveys_Pnor,Sites_Grid5km,"ID_5km","ID_100m",Grid_100m,"RT_HIGH")
RTHIGH_2_5km_500 <- get.hr.covariate(Surveys_Pnor,Sites_Grid2_5km,"ID_2_5km","ID_500m",Grid_500m,"RT_HIGH")
RTHIGH_2_5km_250 <- get.hr.covariate(Surveys_Pnor,Sites_Grid2_5km,"ID_2_5km","ID_250m",Grid_250m,"RT_HIGH")
RTHIGH_2_5km_100 <- get.hr.covariate(Surveys_Pnor,Sites_Grid2_5km,"ID_2_5km","ID_100m",Grid_100m,"RT_HIGH")
#other roads
RTLOW_5km_500 <- get.hr.covariate(Surveys_Pnor,Sites_Grid5km,"ID_5km","ID_500m",Grid_500m,"RT_LOW")
RTLOW_5km_250 <- get.hr.covariate(Surveys_Pnor,Sites_Grid5km,"ID_5km","ID_250m",Grid_250m,"RT_LOW")
RTLOW_5km_100 <- get.hr.covariate(Surveys_Pnor,Sites_Grid5km,"ID_5km","ID_100m",Grid_100m,"RT_LOW")
RTLOW_2_5km_500 <- get.hr.covariate(Surveys_Pnor,Sites_Grid2_5km,"ID_2_5km","ID_500m",Grid_500m,"RT_LOW")
RTLOW_2_5km_250 <- get.hr.covariate(Surveys_Pnor,Sites_Grid2_5km,"ID_2_5km","ID_250m",Grid_250m,"RT_LOW")
RTLOW_2_5km_100 <- get.hr.covariate(Surveys_Pnor,Sites_Grid2_5km,"ID_2_5km","ID_100m",Grid_100m,"RT_LOW")

#get site level covariates
RE_5km_Site <- get.site.covariate(Surveys_Pnor,Sites_Grid5km,"ID_5km","RE_TYPE")
RE_2_5km_Site <- get.site.covariate(Surveys_Pnor,Sites_Grid2_5km,"ID_2_5km","RE_TYPE")

#survey level covariates
Month_5km_Survey <- get.site.covariate(Surveys_Pnor,Sites_Grid5km,"ID_5km","MONTH")
Month_2_5km_Survey <- get.site.covariate(Surveys_Pnor,Sites_Grid2_5km,"ID_2_5km","MONTH")

#repeat level covariates
Effort_5km_Pnor <- get.3dgrid(Surveys_Pnor,Sites_Grid5km,c("TE1","TE2","TE3","TE4","TE5"),"ID_5km",TRUE)
Effort_5km_Pbre <- get.3dgrid(Surveys_Pbre,Sites_Grid5km,c("TE1","TE2","TE3","TE4","TE5"),"ID_5km",TRUE)
Effort_5km_Aflav <- get.3dgrid(Surveys_Aflav,Sites_Grid5km,c("TE1","TE2","TE3","TE4","TE5"),"ID_5km",TRUE)
Effort_2_5km_Pnor <- get.3dgrid(Surveys_Pnor,Sites_Grid2_5km,c("TE1","TE2","TE3","TE4","TE5"),"ID_2_5km",TRUE)
Effort_2_5km_Pbre <- get.3dgrid(Surveys_Pbre,Sites_Grid2_5km,c("TE1","TE2","TE3","TE4","TE5"),"ID_2_5km",TRUE)
Effort_2_5km_Aflav <- get.3dgrid(Surveys_Aflav,Sites_Grid2_5km,c("TE1","TE2","TE3","TE4","TE5"),"ID_2_5km",TRUE)

#BUGS MODELS

#MODEL - NO SPATIAL AUTOCORRELATION
sink("bmod_noauto_zib.txt")
cat("model
{
	#standardise covariates
	#grid level standardisation
	for (i in 1:G)
	{
		GURBs[i] <- (GURB[i] - mean(GURB[])) / sd(GURB[]);
		GNFs[i] <- (GNF[i] - mean(GNF[])) / sd(GNF[]);
	}
	#site level standardisation
	for (i in 1:GS)
	{
		for (j in 1:REPSI[i])
		#get values for site level covariates
		{
			HRHv[REPSIC[i] - REPSI[i] + j] <- HRH[i,j];
			HRLv[REPSIC[i] - REPSI[i] + j] <- HRL[i,j];
			HFPCv[REPSIC[i] - REPSI[i] + j] <- HFPC[i,j];
		}
	}
	#standardise site level covariates
	for (i in 1:GS)
	{
		for (j in 1:REPSI[i])
		{
			HRHs[i,j] <- (HRH[i,j] - mean(HRHv[])) / sd(HRHv[]);
			HRLs[i,j] <- (HRL[i,j] - mean(HRLv[])) / sd(HRLv[]);
			HFPCs[i,j] <- (HFPC[i,j] - mean(HFPCv[])) / sd(HFPCv[]);
		}
	}
		
	#likelihood
	for (i in 1:G)
	{
		#true occupancy of grids
		p_lp[i] <- pbar + purb * GURBs[i] + pnf * GNFs[i];
		p_lim[i] <- min(999,max(-999,p_lp[i])); #to prevent overflows
		logit(p[i]) <- p_lim[i];
		mp[i] <- p[i]; #this is here so we can do something with AREA - not sure yet, might ignore it?
		PAG[i] ~ dbern(mp[i]);
	}

	for (i in 1:GS)
	{
		for (j in 1:REPSI[i])
		#loop through sites in each grid
		{
			#true occupancy of sites
			q_lp[i,j] <- qbar + qrh * HRHs[i,j] + qrl * HRLs[i,j] + qfpc * HFPCs[i,j] + qre[SRE[i,j]];
			q_lim[i,j] <- min(999,max(-999,q_lp[i,j])); #to prevent overflows
			logit(q[i,j]) <- q_lim[i,j];
			uq[i,j] <- PAG[GRIDID[i]] * q[i,j];
			PAS[i,j] ~ dbern(uq[i,j]);
			
			for (k in 1:REPSU[i,j])
			#loop through repeat surveys in each site 
			{
				#observed occupancy of sites
				r_lp[i,j,k] <- rran[MONTH[i,j]];
				r_lim[i,j,k] <- min(999,max(-999,r_lp[i,j,k])); #to prevent overflows
				logit(r[i,j,k]) <- r_lim[i,j,k];
				ur[i,j,k] <- PAS[i,j] * r[i,j,k];
				PA[i,j,k] ~ dbern(ur[i,j,k]);
			}
		}
	}
	
	#set dectability as a random-effect grouped by month
	for (i in 1:12)
	{
		rran[i] ~ dnorm(rbar,rtau);
	}
		
	#priors
	pbar ~ dnorm(0,0.1); #intercept for p
	purb ~ dnorm(0,0.1); #urban effect on p
	pnf ~ dnorm(0,0.1); #native forest effect on p 
	qbar ~ dnorm(0,0.1); #intercept for q
	qrh ~ dnorm(0,0.1); #high volume roads effect at home range for q
	qrl ~ dnorm(0,0.1); #low volume roads effect at home range for q
	qfpc ~ dnorm(0,0.1); #FPC effect at home range for q
	qre[1] <- 0; # baseline for RE effect at site for q
	for (i in 2:3)
	{
		qre[i] ~ dnorm(0,0.1);
	}
	rbar ~ dnorm(0,0.1); #mean for r
	rtau <- pow(rsig,-2);
	rsig ~ dunif(0,5); #standard deviation for random-effect for r
}",fill=TRUE)
sink()

#MODEL - WITH SPATIAL AUTOCORRELATION
sink("bmod_auto_zib.txt")
cat("model
{
	#standardise covariates
	#grid level standardisation
	GURBmn <- mean(GURB[]);
	GURBsd <- sd(GURB[]);
	GNFmn <- mean(GNF[]);
	GNFsd <- sd(GNF[]);
	
	for (i in 1:G)
	{
		GURBs[i] <- (GURB[i] - GURBmn) / GURBsd;
		GNFs[i] <- (GNF[i] - GNFmn) / GNFsd;
	}
	#site level standardisation
	for (i in 1:GS)
	{
		for (j in 1:REPSI[i])
		#get values for site level covariates
		{
			HRHv[REPSIC[i] - REPSI[i] + j] <- HRH[i,j];
			HRLv[REPSIC[i] - REPSI[i] + j] <- HRL[i,j];
			HFPCv[REPSIC[i] - REPSI[i] + j] <- HFPC[i,j];
		}
	}
	#standardise site level covariates
	for (i in 1:GS)
	{
		for (j in 1:REPSI[i])
		{
			HRHs[i,j] <- (HRH[i,j] - mean(HRHv[])) / sd(HRHv[]);
			HRLs[i,j] <- (HRL[i,j] - mean(HRLv[])) / sd(HRLv[]);
			HFPCs[i,j] <- (HFPC[i,j] - mean(HFPCv[])) / sd(HFPCv[]);
		}
	}
		
	#likelihood
	for (i in 1:G)
	{
		#true occupancy of grids
		for (j in 1:NCELL[i])
		{
			AutoC[i,j] <- PAG[AGID[i,j]] * (1 / AGRE[i,j])
		}
				
		p_lp[i] <- pbar + purb * GURBs[i] + pnf * GNFs[i] + paut * ((sum(AutoC[i,1:NCELL[i]])) / NCELL[i]);
		p_lim[i] <- min(999,max(-999,p_lp[i])); #to prevent overflows
		logit(p[i]) <- p_lim[i];
		mp[i] <- p[i]; #this is here so we can do something with AREA - not sure yet, might ignore it?
		PAG[i] ~ dbern(mp[i]);
	}

	for (i in 1:GS)
	{
		for (j in 1:REPSI[i])
		#loop through sites in each grid
		{
			#true occupancy of sites
			q_lp[i,j] <- qbar + qrh * HRHs[i,j] + qrl * HRLs[i,j] + qfpc * HFPCs[i,j] + qre[SRE[i,j]];
			q_lim[i,j] <- min(999,max(-999,q_lp[i,j])); #to prevent overflows
			logit(q[i,j]) <- q_lim[i,j];
			uq[i,j] <- PAG[GRIDID[i]] * q[i,j];
			PAS[i,j] ~ dbern(uq[i,j]);
			
			for (k in 1:REPSU[i,j])
			#loop through repeat surveys in each site 
			{
				#observed occupancy of sites
				r_lp[i,j,k] <- rran[MONTH[i,j]];
				r_lim[i,j,k] <- min(999,max(-999,r_lp[i,j,k])); #to prevent overflows
				logit(r[i,j,k]) <- r_lim[i,j,k];
				ur[i,j,k] <- PAS[i,j] * r[i,j,k];
				PA[i,j,k] ~ dbern(ur[i,j,k]);
			}
		}
	}
	
	#set dectability as a random-effect grouped by month
	for (i in 1:12)
	{
		rran[i] ~ dnorm(rbar,rtau);
	}
		
	#priors
	pbar ~ dnorm(0,0.1); #intercept for p
	purb ~ dnorm(0,0.1); #urban effect on p
	pnf ~ dnorm(0,0.1); #native forest effect on p
	paut ~ dnorm(0,0.1); #autocorrelation effect on p
	qbar ~ dnorm(0,0.1); #intercept for q
	qrh ~ dnorm(0,0.1); #high volume roads effect at home range for q
	qrl ~ dnorm(0,0.1); #low volume roads effect at home range for q
	qfpc ~ dnorm(0,0.1); #FPC effect at home range for q
	qre[1] <- 0; # baseline for RE effect at site for q
	for (i in 2:3)
	{
		qre[i] ~ dnorm(0,0.1);
	}
	rbar ~ dnorm(0,0.1); #mean for r
	rtau <- pow(rsig,-2);
	rsig ~ dunif(0,5); #standard deviation for random-effect for r
}",fill=TRUE)
sink()

#SET UP DATA INPUTS AND RUN WINBUGS

#koalas with no autocorrelation (5km grids)
Params <- c("pbar","purb","pnf","qbar","qrh","qrl","qfpc","qre","rbar","rsig","p")
ni <- 150000
nt <- 10
nb <- 50000
nc <- 3
Bugs_Data <- list(length(as.vector(Grid5km[,1])),length(as.vector(Sites_Grid5km[,1])),as.vector(Sites_Grid5km[,1]),URB5km,NF5km,RTHIGH_5km_500,RTLOW_5km_500,FPCH3_5km_500,RE_5km_Site,Month_5km_Survey,GridPA_5km_Pcin,PA5km_Pcin,as.vector(Sites_Grid5km[,2]),as.vector(Sites_Grid5km_Cum[,2]),Site_Repeats5km_Pcin)
names(Bugs_Data) <- list("G","GS","GRIDID","GURB","GNF","HRH","HRL","HFPC","SRE","MONTH","PAG","PA","REPSI","REPSIC","REPSU")
writeDatafileR(Bugs_Data,"bdata_pcin_5km_noauto_zib.txt")
inits <- function(){list(pbar=runif(1,-2,2),purb=runif(1,-2,2),pnf=runif(1,-2,2),qbar=runif(1,-2,2),qrh=runif(1,-2,2),qrl=runif(1,-2,2),qfpc=runif(1,-2,2),qre=as.vector(c(NA,runif(1,-2,2),runif(1,-2,2))),rbar=runif(1,-2,2),rsig=runif(1,0,4),PAG=as.vector(ifelse(is.finite(GridPA_5km_Pcin),NA,1)),PAS=get.pas.start(PA5km_Pcin))}
writeDatafileR(inits(),"binit1_pcin_5km_no_auto_zib.txt")
writeDatafileR(inits(),"binit2_pcin_5km_no_auto_zib.txt")
writeDatafileR(inits(),"binit3_pcin_5km_no_auto_zib.txt")
MCMC_Pcin_5km_NoAuto_Zib <- bugs(Bugs_Data,inits,Params,"bmod_noauto_zib.txt",n.chains=nc,n.thin=nt,n.iter=ni,n.burnin=nb,debug=F,bugs.directory="c:/Program Files (x86)/WinBUGS14/",working.directory=getwd())
for (j in 1:3)
{
	write.table(as.mcmc.list(MCMC_Pcin_5km_NoAuto_Zib)[[j]],paste(getwd(),"/pcin_5km_output/pcin_5km_mod",0,"_chain",j,".csv",sep=""),sep=",",row.names=F)
}
rm(Params)
rm(ni)
rm(nt)
rm(nb)
rm(nc)
rm(Bugs_Data)
rm(inits)
rm(ResMod)
rm(ResGrid5km_Mod)
rm(MCMC_Model)

#koalas with no autocorrelation (2.5km grids)
Params <- c("pbar","purb","pnf","qbar","qrh","qrl","qfpc","qre","rbar","rsig","p")
ni <- 150000
nt <- 10
nb <- 50000
nc <- 3
Bugs_Data <- list(length(as.vector(Grid2_5km[,1])),length(as.vector(Sites_Grid2_5km[,1])),as.vector(Sites_Grid2_5km[,1]),URB2_5km,NF2_5km,RTHIGH_2_5km_500,RTLOW_2_5km_500,FPCH3_2_5km_500,RE_2_5km_Site,Month_2_5km_Survey,GridPA_2_5km_Pcin,PA2_5km_Pcin,as.vector(Sites_Grid2_5km[,2]),as.vector(Sites_Grid2_5km_Cum[,2]),Site_Repeats2_5km_Pcin)
names(Bugs_Data) <- list("G","GS","GRIDID","GURB","GNF","HRH","HRL","HFPC","SRE","MONTH","PAG","PA","REPSI","REPSIC","REPSU")
writeDatafileR(Bugs_Data,"bdata_pcin_2_5km_noauto_zib.txt")
inits <- function(){list(pbar=runif(1,-2,2),purb=runif(1,-2,2),pnf=runif(1,-2,2),qbar=runif(1,-2,2),qrh=runif(1,-2,2),qrl=runif(1,-2,2),qfpc=runif(1,-2,2),qre=as.vector(c(NA,runif(1,-2,2),runif(1,-2,2))),rbar=runif(1,-2,2),rsig=runif(1,0,4),PAG=as.vector(ifelse(is.finite(GridPA_2_5km_Pcin),NA,1)),PAS=get.pas.start(PA2_5km_Pcin))}
writeDatafileR(inits(),"binit1_pcin_2_5km_no_auto_zib.txt")
writeDatafileR(inits(),"binit2_pcin_2_5km_no_auto_zib.txt")
writeDatafileR(inits(),"binit3_pcin_2_5km_no_auto_zib.txt")
MCMC_Pcin_2_5km_NoAuto_Zib <- bugs(Bugs_Data,inits,Params,"bmod_noauto_zib.txt",n.chains=nc,n.thin=nt,n.iter=ni,n.burnin=nb,debug=F,bugs.directory="c:/Program Files (x86)/WinBUGS14/",working.directory=getwd())
for (j in 1:3)
{
	write.table(as.mcmc.list(MCMC_Pcin_2_5km_NoAuto_Zib)[[j]],paste(getwd(),"/pcin_2_5km_output/pcin_2_5km_mod",0,"_chain",j,".csv",sep=""),sep=",",row.names=F)
}
rm(Params)
rm(ni)
rm(nt)
rm(nb)
rm(nc)
rm(Bugs_Data)
rm(inits)
rm(ResMod)
rm(ResGrid2_5km_Mod)
rm(MCMC_Model)

#koalas with autocorrelation (5km grids)
Params <- c("pbar","purb","pnf","paut","qbar","qrh","qrl","qfpc","qre","rbar","rsig","p")
ni <- 150000
nt <- 10
nb <- 50000
nc <- 3
inits <- function(){list(pbar=runif(1,-2,2),purb=runif(1,-2,2),pnf=runif(1,-2,2),paut=runif(1,-2,2),qbar=runif(1,-2,2),qrh=runif(1,-2,2),qrl=runif(1,-2,2),qfpc=runif(1,-2,2),qre=as.vector(c(NA,runif(1,-2,2),runif(1,-2,2))),rbar=runif(1,-2,2),rsig=runif(1,0,4),PAG=as.vector(ifelse(is.finite(GridPA_5km_Pcin),NA,1)),PAS=get.pas.start(PA5km_Pcin))}
writeDatafileR(inits(),"binit1_pcin_5km_auto_zib.txt")
writeDatafileR(inits(),"binit2_pcin_5km_auto_zib.txt")
writeDatafileR(inits(),"binit3_pcin_5km_auto_zib.txt")
for (i in 1:1)
{
	ResMod <- read.table(paste(getwd(),"/5km_resistance_models/model",i,".csv",sep=""),sep=",",header=T)
	ResGrid5km_Mod <- get.ordered.connectivity(Grid5km,ResMod,c("id_UL","id_UM","id_UR","id_ML","id_MR","id_LL","id_LM","id_LR"),c(paste("model",i,"_UL",sep=""),paste("model",i,"_UM",sep=""),paste("model",i,"_UR",sep=""),paste("model",i,"_ML",sep=""),paste("model",i,"_MR",sep=""),paste("model",i,"_LL",sep=""),paste("model",i,"_LM",sep=""),paste("model",i,"_LR",sep="")))
	Bugs_Data <- list(length(as.vector(Grid5km[,1])),length(as.vector(Sites_Grid5km[,1])),as.vector(Sites_Grid5km[,1]),URB5km,NF5km,RTHIGH_5km_500,RTLOW_5km_500,FPCH3_5km_500,RE_5km_Site,Month_5km_Survey,GridPA_5km_Pcin,PA5km_Pcin,as.vector(Sites_Grid5km[,2]),as.vector(Sites_Grid5km_Cum[,2]),Site_Repeats5km_Pcin,ResGrid5km_Mod[[1]][,2:9],ResGrid5km_Mod[[2]][,2:9],as.vector(ResGrid5km_Mod[[3]]))
	names(Bugs_Data) <- list("G","GS","GRIDID","GURB","GNF","HRH","HRL","HFPC","SRE","MONTH","PAG","PA","REPSI","REPSIC","REPSU","AGID","AGRE","NCELL")
	writeDatafileR(Bugs_Data,paste("bdata_pcin_5km_auto_zib_mod",i,".txt",sep=""))
	#MCMC_Model <- bugs(Bugs_Data,inits,Params,"bmod_auto_zib.txt",n.chains=nc,n.thin=nt,n.iter=ni,n.burnin=nb,debug=F,bugs.directory="c:/Program Files (x86)/WinBUGS14/",working.directory=getwd())
	MCMC_Model <- as.mcmc.list(bugs(Bugs_Data,inits,Params,"bmod_auto_zib.txt",n.chains=nc,n.thin=nt,n.iter=ni,n.burnin=nb,debug=F,bugs.directory="c:/Program Files (x86)/WinBUGS14/",working.directory=getwd()))
	for (j in 1:3)
	{
		write.table(MCMC_Model[[j]],paste(getwd(),"/pcin_5km_output/pcin_5km_mod",i,"_chain",j,".csv",sep=""),sep=",",row.names=F)
	}
}
rm(Params)
rm(ni)
rm(nt)
rm(nb)
rm(nc)
rm(Bugs_Data)
rm(inits)
rm(ResMod)
rm(ResGrid5km_Mod)
rm(MCMC_Model)

#koalas with autocorrelation (2.5km grids)
Params <- c("pbar","purb","pnf","paut","qbar","qrh","qrl","qfpc","qre","rbar","rsig","p")
ni <- 150000
nt <- 10
nb <- 50000
nc <- 3
inits <- function(){list(pbar=runif(1,-2,2),purb=runif(1,-2,2),pnf=runif(1,-2,2),paut=runif(1,-2,2),qbar=runif(1,-2,2),qrh=runif(1,-2,2),qrl=runif(1,-2,2),qfpc=runif(1,-2,2),qre=as.vector(c(NA,runif(1,-2,2),runif(1,-2,2))),rbar=runif(1,-2,2),rsig=runif(1,0,4),PAG=as.vector(ifelse(is.finite(GridPA_2_5km_Pcin),NA,1)),PAS=get.pas.start(PA2_5km_Pcin))}
writeDatafileR(inits(),"binit1_pcin_2_5km_auto_zib.txt")
writeDatafileR(inits(),"binit2_pcin_2_5km_auto_zib.txt")
writeDatafileR(inits(),"binit3_pcin_2_5km_auto_zib.txt")
for (i in 1:1)
{
	ResMod <- read.table(paste(getwd(),"/2_5km_resistance_models/model",i,".csv",sep=""),sep=",",header=T)
	ResGrid2_5km_Mod <- get.ordered.connectivity(Grid2_5km,ResMod,c("id_UL","id_UM","id_UR","id_ML","id_MR","id_LL","id_LM","id_LR"),c(paste("model",i,"_UL",sep=""),paste("model",i,"_UM",sep=""),paste("model",i,"_UR",sep=""),paste("model",i,"_ML",sep=""),paste("model",i,"_MR",sep=""),paste("model",i,"_LL",sep=""),paste("model",i,"_LM",sep=""),paste("model",i,"_LR",sep="")))
	Bugs_Data <- list(length(as.vector(Grid2_5km[,1])),length(as.vector(Sites_Grid2_5km[,1])),as.vector(Sites_Grid2_5km[,1]),URB2_5km,NF2_5km,RTHIGH_2_5km_500,RTLOW_2_5km_500,FPCH3_2_5km_500,RE_2_5km_Site,Month_2_5km_Survey,GridPA_2_5km_Pcin,PA2_5km_Pcin,as.vector(Sites_Grid2_5km[,2]),as.vector(Sites_Grid2_5km_Cum[,2]),Site_Repeats2_5km_Pcin,ResGrid2_5km_Mod[[1]][,2:9],ResGrid2_5km_Mod[[2]][,2:9],as.vector(ResGrid2_5km_Mod[[3]]))
	names(Bugs_Data) <- list("G","GS","GRIDID","GURB","GNF","HRH","HRL","HFPC","SRE","MONTH","PAG","PA","REPSI","REPSIC","REPSU","AGID","AGRE","NCELL")
	writeDatafileR(Bugs_Data,paste("bdata_pcin_2_5km_auto_zib_mod",i,".txt",sep=""))
	#MCMC_Model <- bugs(Bugs_Data,inits,Params,"bmod_auto_zib.txt",n.chains=nc,n.thin=nt,n.iter=ni,n.burnin=nb,debug=F,bugs.directory="c:/Program Files (x86)/WinBUGS14/",working.directory=getwd())
	MCMC_Model <- as.mcmc.list(bugs(Bugs_Data,inits,Params,"bmod_auto_zib.txt",n.chains=nc,n.thin=nt,n.iter=ni,n.burnin=nb,debug=F,bugs.directory="c:/Program Files (x86)/WinBUGS14/",working.directory=getwd()))
	for (j in 1:3)
	{
		write.table(MCMC_Model[[j]],paste(getwd(),"/pcin_2_5km_output/pcin_2_5km_mod",i,"_chain",j,".csv",sep=""),sep=",",row.names=F)
	}
}
rm(Params)
rm(ni)
rm(nt)
rm(nb)
rm(nc)
rm(Bugs_Data)
rm(inits)
rm(ResMod)
rm(ResGrid2_5km_Mod)
rm(MCMC_Model)

#sugar gliders with no autocorrelation (5km grids)
Params <- c("pbar","purb","pnf","qbar","qrh","qrl","qfpc","qre","rbar","rsig","p")
ni <- 150000
nt <- 10
nb <- 50000
nc <- 3
Bugs_Data <- list(length(as.vector(Grid5km[,1])),length(as.vector(Sites_Grid5km[,1])),as.vector(Sites_Grid5km[,1]),URB5km,NF5km,RTHIGH_5km_250,RTLOW_5km_250,FPCH3_5km_250,RE_5km_Site,Month_5km_Survey,GridPA_5km_Pbre,PA5km_Pbre,as.vector(Sites_Grid5km[,2]),as.vector(Sites_Grid5km_Cum[,2]),Site_Repeats5km_Pbre)
names(Bugs_Data) <- list("G","GS","GRIDID","GURB","GNF","HRH","HRL","HFPC","SRE","MONTH","PAG","PA","REPSI","REPSIC","REPSU")
writeDatafileR(Bugs_Data,"bdata_pbre_5km_noauto_zib.txt")
inits <- function(){list(pbar=runif(1,-2,2),purb=runif(1,-2,2),pnf=runif(1,-2,2),qbar=runif(1,-2,2),qrh=runif(1,-2,2),qrl=runif(1,-2,2),qfpc=runif(1,-2,2),qre=as.vector(c(NA,runif(1,-2,2),runif(1,-2,2))),rbar=runif(1,-2,2),rsig=runif(1,0,4),PAG=as.vector(ifelse(is.finite(GridPA_5km_Pbre),NA,1)),PAS=get.pas.start(PA5km_Pbre))}
writeDatafileR(inits(),"binit1_pbre_5km_no_auto_zib.txt")
writeDatafileR(inits(),"binit2_pbre_5km_no_auto_zib.txt")
writeDatafileR(inits(),"binit3_pbre_5km_no_auto_zib.txt")
MCMC_Pbre_5km_NoAuto_Zib <- bugs(Bugs_Data,inits,Params,"bmod_noauto_zib.txt",n.chains=nc,n.thin=nt,n.iter=ni,n.burnin=nb,debug=F,bugs.directory="c:/Program Files (x86)/WinBUGS14/",working.directory=getwd())
for (j in 1:3)
{
	write.table(as.mcmc.list(MCMC_Pbre_5km_NoAuto_Zib)[[j]],paste(getwd(),"/pbre_5km_output/pbre_5km_mod",0,"_chain",j,".csv",sep=""),sep=",",row.names=F)
}
rm(Params)
rm(ni)
rm(nt)
rm(nb)
rm(nc)
rm(Bugs_Data)
rm(inits)
rm(ResMod)
rm(ResGrid5km_Mod)
rm(MCMC_Model)

#sugar gliders with no autocorrelation (2.5km grids)
Params <- c("pbar","purb","pnf","qbar","qrh","qrl","qfpc","qre","rbar","rsig","p")
ni <- 150000
nt <- 10
nb <- 50000
nc <- 3
Bugs_Data <- list(length(as.vector(Grid2_5km[,1])),length(as.vector(Sites_Grid2_5km[,1])),as.vector(Sites_Grid2_5km[,1]),URB2_5km,NF2_5km,RTHIGH_2_5km_250,RTLOW_2_5km_250,FPCH3_2_5km_250,RE_2_5km_Site,Month_2_5km_Survey,GridPA_2_5km_Pbre,PA2_5km_Pbre,as.vector(Sites_Grid2_5km[,2]),as.vector(Sites_Grid2_5km_Cum[,2]),Site_Repeats2_5km_Pbre)
names(Bugs_Data) <- list("G","GS","GRIDID","GURB","GNF","HRH","HRL","HFPC","SRE","MONTH","PAG","PA","REPSI","REPSIC","REPSU")
writeDatafileR(Bugs_Data,"bdata_pbre_2_5km_noauto_zib.txt")
inits <- function(){list(pbar=runif(1,-2,2),purb=runif(1,-2,2),pnf=runif(1,-2,2),qbar=runif(1,-2,2),qrh=runif(1,-2,2),qrl=runif(1,-2,2),qfpc=runif(1,-2,2),qre=as.vector(c(NA,runif(1,-2,2),runif(1,-2,2))),rbar=runif(1,-2,2),rsig=runif(1,0,4),PAG=as.vector(ifelse(is.finite(GridPA_2_5km_Pbre),NA,1)),PAS=get.pas.start(PA2_5km_Pbre))}
writeDatafileR(inits(),"binit1_pbre_2_5km_no_auto_zib.txt")
writeDatafileR(inits(),"binit2_pbre_2_5km_no_auto_zib.txt")
writeDatafileR(inits(),"binit3_pbre_2_5km_no_auto_zib.txt")
MCMC_Pbre_2_5km_NoAuto_Zib <- bugs(Bugs_Data,inits,Params,"bmod_noauto_zib.txt",n.chains=nc,n.thin=nt,n.iter=ni,n.burnin=nb,debug=F,bugs.directory="c:/Program Files (x86)/WinBUGS14/",working.directory=getwd())
for (j in 1:3)
{
	write.table(as.mcmc.list(MCMC_Pbre_2_5km_NoAuto_Zib)[[j]],paste(getwd(),"/pcin_2_5km_output/pbre_2_5km_mod",0,"_chain",j,".csv",sep=""),sep=",",row.names=F)
}
rm(Params)
rm(ni)
rm(nt)
rm(nb)
rm(nc)
rm(Bugs_Data)
rm(inits)
rm(ResMod)
rm(ResGrid2_5km_Mod)
rm(MCMC_Model)

#sugar gliders with autocorrelation (5km grids)
Params <- c("pbar","purb","pnf","paut","qbar","qrh","qrl","qfpc","qre","rbar","rsig","p")
ni <- 150000
nt <- 10
nb <- 50000
nc <- 3
inits <- function(){list(pbar=runif(1,-2,2),purb=runif(1,-2,2),pnf=runif(1,-2,2),paut=runif(1,-2,2),qbar=runif(1,-2,2),qrh=runif(1,-2,2),qrl=runif(1,-2,2),qfpc=runif(1,-2,2),qre=as.vector(c(NA,runif(1,-2,2),runif(1,-2,2))),rbar=runif(1,-2,2),rsig=runif(1,0,4),PAG=as.vector(ifelse(is.finite(GridPA_5km_Pbre),NA,1)),PAS=get.pas.start(PA5km_Pbre))}
writeDatafileR(inits(),"binit1_pbre_5km_auto_zib.txt")
writeDatafileR(inits(),"binit2_pbre_5km_auto_zib.txt")
writeDatafileR(inits(),"binit3_pbre_5km_auto_zib.txt")
for (i in 1:1)
{
	ResMod <- read.table(paste(getwd(),"/5km_resistance_models/model",i,".csv",sep=""),sep=",",header=T)
	ResGrid5km_Mod <- get.ordered.connectivity(Grid5km,ResMod,c("id_UL","id_UM","id_UR","id_ML","id_MR","id_LL","id_LM","id_LR"),c(paste("model",i,"_UL",sep=""),paste("model",i,"_UM",sep=""),paste("model",i,"_UR",sep=""),paste("model",i,"_ML",sep=""),paste("model",i,"_MR",sep=""),paste("model",i,"_LL",sep=""),paste("model",i,"_LM",sep=""),paste("model",i,"_LR",sep="")))
	Bugs_Data <- list(length(as.vector(Grid5km[,1])),length(as.vector(Sites_Grid5km[,1])),as.vector(Sites_Grid5km[,1]),URB5km,NF5km,RTHIGH_5km_500,RTLOW_5km_500,FPCH3_5km_500,RE_5km_Site,Month_5km_Survey,GridPA_5km_Pbre,PA5km_Pbre,as.vector(Sites_Grid5km[,2]),as.vector(Sites_Grid5km_Cum[,2]),Site_Repeats5km_Pbre,ResGrid5km_Mod[[1]][,2:9],ResGrid5km_Mod[[2]][,2:9],as.vector(ResGrid5km_Mod[[3]]))
	names(Bugs_Data) <- list("G","GS","GRIDID","GURB","GNF","HRH","HRL","HFPC","SRE","MONTH","PAG","PA","REPSI","REPSIC","REPSU","AGID","AGRE","NCELL")
	writeDatafileR(Bugs_Data,paste("bdata_pbre_5km_auto_zib_mod",i,".txt",sep=""))
	#MCMC_Model <- bugs(Bugs_Data,inits,Params,"bmod_auto_zib.txt",n.chains=nc,n.thin=nt,n.iter=ni,n.burnin=nb,debug=F,bugs.directory="c:/Program Files (x86)/WinBUGS14/",working.directory=getwd())
	MCMC_Model <- as.mcmc.list(bugs(Bugs_Data,inits,Params,"bmod_auto_zib.txt",n.chains=nc,n.thin=nt,n.iter=ni,n.burnin=nb,debug=F,bugs.directory="c:/Program Files (x86)/WinBUGS14/",working.directory=getwd()))
	for (j in 1:3)
	{
		write.table(MCMC_Model[[j]],paste(getwd(),"/pbre_5km_output/pbre_5km_mod",i,"_chain",j,".csv",sep=""),sep=",",row.names=F)
	}
}
rm(Params)
rm(ni)
rm(nt)
rm(nb)
rm(nc)
rm(Bugs_Data)
rm(inits)
rm(ResMod)
rm(ResGrid5km_Mod)
rm(MCMC_Model)

#sugar gliders with autocorrelation (2.5km grids)
Params <- c("pbar","purb","pnf","paut","qbar","qrh","qrl","qfpc","qre","rbar","rsig","p")
ni <- 150000
nt <- 10
nb <- 50000
nc <- 3
inits <- function(){list(pbar=runif(1,-2,2),purb=runif(1,-2,2),pnf=runif(1,-2,2),paut=runif(1,-2,2),qbar=runif(1,-2,2),qrh=runif(1,-2,2),qrl=runif(1,-2,2),qfpc=runif(1,-2,2),qre=as.vector(c(NA,runif(1,-2,2),runif(1,-2,2))),rbar=runif(1,-2,2),rsig=runif(1,0,4),PAG=as.vector(ifelse(is.finite(GridPA_2_5km_Pbre),NA,1)),PAS=get.pas.start(PA2_5km_Pbre))}
writeDatafileR(inits(),"binit1_pbre_2_5km_auto_zib.txt")
writeDatafileR(inits(),"binit2_pbre_2_5km_auto_zib.txt")
writeDatafileR(inits(),"binit3_pbre_2_5km_auto_zib.txt")
for (i in 1:1)
{
	ResMod <- read.table(paste(getwd(),"/2_5km_resistance_models/model",i,".csv",sep=""),sep=",",header=T)
	ResGrid2_5km_Mod <- get.ordered.connectivity(Grid2_5km,ResMod,c("id_UL","id_UM","id_UR","id_ML","id_MR","id_LL","id_LM","id_LR"),c(paste("model",i,"_UL",sep=""),paste("model",i,"_UM",sep=""),paste("model",i,"_UR",sep=""),paste("model",i,"_ML",sep=""),paste("model",i,"_MR",sep=""),paste("model",i,"_LL",sep=""),paste("model",i,"_LM",sep=""),paste("model",i,"_LR",sep="")))
	Bugs_Data <- list(length(as.vector(Grid2_5km[,1])),length(as.vector(Sites_Grid2_5km[,1])),as.vector(Sites_Grid2_5km[,1]),URB2_5km,NF2_5km,RTHIGH_2_5km_500,RTLOW_2_5km_500,FPCH3_2_5km_500,RE_2_5km_Site,Month_2_5km_Survey,GridPA_2_5km_Pbre,PA2_5km_Pbre,as.vector(Sites_Grid2_5km[,2]),as.vector(Sites_Grid2_5km_Cum[,2]),Site_Repeats2_5km_Pbre,ResGrid2_5km_Mod[[1]][,2:9],ResGrid2_5km_Mod[[2]][,2:9],as.vector(ResGrid2_5km_Mod[[3]]))
	names(Bugs_Data) <- list("G","GS","GRIDID","GURB","GNF","HRH","HRL","HFPC","SRE","MONTH","PAG","PA","REPSI","REPSIC","REPSU","AGID","AGRE","NCELL")
	writeDatafileR(Bugs_Data,paste("bdata_pbre_2_5km_auto_zib_mod",i,".txt",sep=""))
	#MCMC_Model <- bugs(Bugs_Data,inits,Params,"bmod_auto_zib.txt",n.chains=nc,n.thin=nt,n.iter=ni,n.burnin=nb,debug=F,bugs.directory="c:/Program Files (x86)/WinBUGS14/",working.directory=getwd())
	MCMC_Model <- as.mcmc.list(bugs(Bugs_Data,inits,Params,"bmod_auto_zib.txt",n.chains=nc,n.thin=nt,n.iter=ni,n.burnin=nb,debug=F,bugs.directory="c:/Program Files (x86)/WinBUGS14/",working.directory=getwd()))
	for (j in 1:3)
	{
		write.table(MCMC_Model[[j]],paste(getwd(),"/pbre_2_5km_output/pbre_2_5km_mod",i,"_chain",j,".csv",sep=""),sep=",",row.names=F)
	}
}
rm(Params)
rm(ni)
rm(nt)
rm(nb)
rm(nc)
rm(Bugs_Data)
rm(inits)
rm(ResMod)
rm(ResGrid2_5km_Mod)
rm(MCMC_Model)

#squirrel gliders with no autocorrelation (5km grids)
Params <- c("pbar","purb","pnf","qbar","qrh","qrl","qfpc","qre","rbar","rsig","p")
ni <- 150000
nt <- 10
nb <- 50000
nc <- 3
Bugs_Data <- list(length(as.vector(Grid5km[,1])),length(as.vector(Sites_Grid5km[,1])),as.vector(Sites_Grid5km[,1]),URB5km,NF5km,RTHIGH_5km_250,RTLOW_5km_250,FPCH3_5km_250,RE_5km_Site,Month_5km_Survey,GridPA_5km_Pnor,PA5km_Pnor,as.vector(Sites_Grid5km[,2]),as.vector(Sites_Grid5km_Cum[,2]),Site_Repeats5km_Pnor)
names(Bugs_Data) <- list("G","GS","GRIDID","GURB","GNF","HRH","HRL","HFPC","SRE","MONTH","PAG","PA","REPSI","REPSIC","REPSU")
writeDatafileR(Bugs_Data,"bdata_pnor_5km_noauto_zib.txt")
inits <- function(){list(pbar=runif(1,-2,2),purb=runif(1,-2,2),pnf=runif(1,-2,2),qbar=runif(1,-2,2),qrh=runif(1,-2,2),qrl=runif(1,-2,2),qfpc=runif(1,-2,2),qre=as.vector(c(NA,runif(1,-2,2),runif(1,-2,2))),rbar=runif(1,-2,2),rsig=runif(1,0,4),PAG=as.vector(ifelse(is.finite(GridPA_5km_Pnor),NA,1)),PAS=get.pas.start(PA5km_Pnor))}
writeDatafileR(inits(),"binit1_pnor_5km_no_auto_zib.txt")
writeDatafileR(inits(),"binit2_pnor_5km_no_auto_zib.txt")
writeDatafileR(inits(),"binit3_pnor_5km_no_auto_zib.txt")
MCMC_Pnor_5km_NoAuto_Zib <- bugs(Bugs_Data ,inits,Params,"bmod_noauto_zib.txt",n.chains=nc,n.thin=nt,n.iter=ni,n.burnin=nb,debug=F,bugs.directory="c:/Program Files (x86)/WinBUGS14/",working.directory=getwd())
for (j in 1:3)
{
	write.table(as.mcmc.list(MCMC_Pnor_5km_NoAuto_Zib)[[j]],paste(getwd(),"/pnor_5km_output/pnor_5km_mod",0,"_chain",j,".csv",sep=""),sep=",",row.names=F)
}
rm(Params)
rm(ni)
rm(nt)
rm(nb)
rm(nc)
rm(Bugs_Data)
rm(inits)
rm(ResMod)
rm(ResGrid5km_Mod)
rm(MCMC_Model)

#squirrel gliders with no autocorrelation (2.5km grids)
Params <- c("pbar","purb","pnf","qbar","qrh","qrl","qfpc","qre","rbar","rsig","p")
ni <- 150000
nt <- 10
nb <- 50000
nc <- 3
Bugs_Data <- list(length(as.vector(Grid2_5km[,1])),length(as.vector(Sites_Grid2_5km[,1])),as.vector(Sites_Grid2_5km[,1]),URB2_5km,NF2_5km,RTHIGH_2_5km_250,RTLOW_2_5km_250,FPCH3_2_5km_250,RE_2_5km_Site,Month_2_5km_Survey,GridPA_2_5km_Pnor,PA2_5km_Pnor,as.vector(Sites_Grid2_5km[,2]),as.vector(Sites_Grid2_5km_Cum[,2]),Site_Repeats2_5km_Pnor)
names(Bugs_Data) <- list("G","GS","GRIDID","GURB","GNF","HRH","HRL","HFPC","SRE","MONTH","PAG","PA","REPSI","REPSIC","REPSU")
writeDatafileR(Bugs_Data,"bdata_pnor_2_5km_noauto_zib.txt")
inits <- function(){list(pbar=runif(1,-2,2),purb=runif(1,-2,2),pnf=runif(1,-2,2),qbar=runif(1,-2,2),qrh=runif(1,-2,2),qrl=runif(1,-2,2),qfpc=runif(1,-2,2),qre=as.vector(c(NA,runif(1,-2,2),runif(1,-2,2))),rbar=runif(1,-2,2),rsig=runif(1,0,4),PAG=as.vector(ifelse(is.finite(GridPA_2_5km_Pnor),NA,1)),PAS=get.pas.start(PA2_5km_Pnor))}
writeDatafileR(inits(),"binit1_pnor_2_5km_no_auto_zib.txt")
writeDatafileR(inits(),"binit2_pnor_2_5km_no_auto_zib.txt")
writeDatafileR(inits(),"binit3_pnor_2_5km_no_auto_zib.txt")
MCMC_Pnor_2_5km_NoAuto_Zib <- bugs(Bugs_Data,inits,Params,"bmod_noauto_zib.txt",n.chains=nc,n.thin=nt,n.iter=ni,n.burnin=nb,debug=F,bugs.directory="c:/Program Files (x86)/WinBUGS14/",working.directory=getwd())
for (j in 1:3)
{
	write.table(as.mcmc.list(MCMC_Pnor_2_5km_NoAuto_Zib)[[j]],paste(getwd(),"/pnor_2_5km_output/pnor_2_5km_mod",0,"_chain",j,".csv",sep=""),sep=",",row.names=F)
}
rm(Params)
rm(ni)
rm(nt)
rm(nb)
rm(nc)
rm(Bugs_Data)
rm(inits)
rm(ResMod)
rm(ResGrid2_5km_Mod)
rm(MCMC_Model)

#squirrel gliders with autocorrelation (5km grids)
Params <- c("pbar","purb","pnf","paut","qbar","qrh","qrl","qfpc","qre","rbar","rsig","p")
ni <- 150000
nt <- 10
nb <- 50000
nc <- 3
inits <- function(){list(pbar=runif(1,-2,2),purb=runif(1,-2,2),pnf=runif(1,-2,2),paut=runif(1,-2,2),qbar=runif(1,-2,2),qrh=runif(1,-2,2),qrl=runif(1,-2,2),qfpc=runif(1,-2,2),qre=as.vector(c(NA,runif(1,-2,2),runif(1,-2,2))),rbar=runif(1,-2,2),rsig=runif(1,0,4),PAG=as.vector(ifelse(is.finite(GridPA_5km_Pnor),NA,1)),PAS=get.pas.start(PA5km_Pnor))}
writeDatafileR(inits(),"binit1_pnor_5km_auto_zib.txt")
writeDatafileR(inits(),"binit2_pnor_5km_auto_zib.txt")
writeDatafileR(inits(),"binit3_pnor_5km_auto_zib.txt")
for (i in 1:1)
{
	ResMod <- read.table(paste(getwd(),"/5km_resistance_models/model",i,".csv",sep=""),sep=",",header=T)
	ResGrid5km_Mod <- get.ordered.connectivity(Grid5km,ResMod,c("id_UL","id_UM","id_UR","id_ML","id_MR","id_LL","id_LM","id_LR"),c(paste("model",i,"_UL",sep=""),paste("model",i,"_UM",sep=""),paste("model",i,"_UR",sep=""),paste("model",i,"_ML",sep=""),paste("model",i,"_MR",sep=""),paste("model",i,"_LL",sep=""),paste("model",i,"_LM",sep=""),paste("model",i,"_LR",sep="")))
	Bugs_Data <- list(length(as.vector(Grid5km[,1])),length(as.vector(Sites_Grid5km[,1])),as.vector(Sites_Grid5km[,1]),URB5km,NF5km,RTHIGH_5km_500,RTLOW_5km_500,FPCH3_5km_500,RE_5km_Site,Month_5km_Survey,GridPA_5km_Pnor,PA5km_Pnor,as.vector(Sites_Grid5km[,2]),as.vector(Sites_Grid5km_Cum[,2]),Site_Repeats5km_Pnor,ResGrid5km_Mod[[1]][,2:9],ResGrid5km_Mod[[2]][,2:9],as.vector(ResGrid5km_Mod[[3]]))
	names(Bugs_Data) <- list("G","GS","GRIDID","GURB","GNF","HRH","HRL","HFPC","SRE","MONTH","PAG","PA","REPSI","REPSIC","REPSU","AGID","AGRE","NCELL")
	writeDatafileR(Bugs_Data,paste("bdata_pnor_5km_auto_zib_mod",i,".txt",sep=""))
	#MCMC_Model <- bugs(Bugs_Data,inits,Params,"bmod_auto_zib.txt",n.chains=nc,n.thin=nt,n.iter=ni,n.burnin=nb,debug=F,bugs.directory="c:/Program Files (x86)/WinBUGS14/",working.directory=getwd())
	MCMC_Model <- as.mcmc.list(bugs(Bugs_Data,inits,Params,"bmod_auto_zib.txt",n.chains=nc,n.thin=nt,n.iter=ni,n.burnin=nb,debug=F,bugs.directory="c:/Program Files (x86)/WinBUGS14/",working.directory=getwd()))
	for (j in 1:3)
	{
		write.table(MCMC_Model[[j]],paste(getwd(),"/pnor_5km_output/pnor_5km_mod",i,"_chain",j,".csv",sep=""),sep=",",row.names=F)
	}
}
rm(Params)
rm(ni)
rm(nt)
rm(nb)
rm(nc)
rm(Bugs_Data)
rm(inits)
rm(ResMod)
rm(ResGrid5km_Mod)
rm(MCMC_Model)

#squirrel gliders with autocorrelation (2.5km grids)
Params <- c("pbar","purb","pnf","paut","qbar","qrh","qrl","qfpc","qre","rbar","rsig","p")
ni <- 150000
nt <- 10
nb <- 50000
nc <- 3
inits <- function(){list(pbar=runif(1,-2,2),purb=runif(1,-2,2),pnf=runif(1,-2,2),paut=runif(1,-2,2),qbar=runif(1,-2,2),qrh=runif(1,-2,2),qrl=runif(1,-2,2),qfpc=runif(1,-2,2),qre=as.vector(c(NA,runif(1,-2,2),runif(1,-2,2))),rbar=runif(1,-2,2),rsig=runif(1,0,4),PAG=as.vector(ifelse(is.finite(GridPA_2_5km_Pnor),NA,1)),PAS=get.pas.start(PA2_5km_Pnor))}
writeDatafileR(inits(),"binit1_pnor_2_5km_auto_zib.txt")
writeDatafileR(inits(),"binit2_pnor_2_5km_auto_zib.txt")
writeDatafileR(inits(),"binit3_pnor_2_5km_auto_zib.txt")
for (i in 1:1)
{
	ResMod <- read.table(paste(getwd(),"/2_5km_resistance_models/model",i,".csv",sep=""),sep=",",header=T)
	ResGrid2_5km_Mod <- get.ordered.connectivity(Grid2_5km,ResMod,c("id_UL","id_UM","id_UR","id_ML","id_MR","id_LL","id_LM","id_LR"),c(paste("model",i,"_UL",sep=""),paste("model",i,"_UM",sep=""),paste("model",i,"_UR",sep=""),paste("model",i,"_ML",sep=""),paste("model",i,"_MR",sep=""),paste("model",i,"_LL",sep=""),paste("model",i,"_LM",sep=""),paste("model",i,"_LR",sep="")))
	Bugs_Data <- list(length(as.vector(Grid2_5km[,1])),length(as.vector(Sites_Grid2_5km[,1])),as.vector(Sites_Grid2_5km[,1]),URB2_5km,NF2_5km,RTHIGH_2_5km_500,RTLOW_2_5km_500,FPCH3_2_5km_500,RE_2_5km_Site,Month_2_5km_Survey,GridPA_2_5km_Pnor,PA2_5km_Pnor,as.vector(Sites_Grid2_5km[,2]),as.vector(Sites_Grid2_5km_Cum[,2]),Site_Repeats2_5km_Pnor,ResGrid2_5km_Mod[[1]][,2:9],ResGrid2_5km_Mod[[2]][,2:9],as.vector(ResGrid2_5km_Mod[[3]]))
	names(Bugs_Data) <- list("G","GS","GRIDID","GURB","GNF","HRH","HRL","HFPC","SRE","MONTH","PAG","PA","REPSI","REPSIC","REPSU","AGID","AGRE","NCELL")
	writeDatafileR(Bugs_Data,paste("bdata_pnor_2_5km_auto_zib_mod",i,".txt",sep=""))
	#MCMC_Model <- bugs(Bugs_Data,inits,Params,"bmod_auto_zib.txt",n.chains=nc,n.thin=nt,n.iter=ni,n.burnin=nb,debug=F,bugs.directory="c:/Program Files (x86)/WinBUGS14/",working.directory=getwd())
	MCMC_Model <- as.mcmc.list(bugs(Bugs_Data,inits,Params,"bmod_auto_zib.txt",n.chains=nc,n.thin=nt,n.iter=ni,n.burnin=nb,debug=F,bugs.directory="c:/Program Files (x86)/WinBUGS14/",working.directory=getwd()))
	for (j in 1:3)
	{
		write.table(MCMC_Model[[j]],paste(getwd(),"/pnor_2_5km_output/pnor_2_5km_mod",i,"_chain",j,".csv",sep=""),sep=",",row.names=F)
	}
}
rm(Params)
rm(ni)
rm(nt)
rm(nb)
rm(nc)
rm(Bugs_Data)
rm(inits)
rm(ResMod)
rm(ResGrid2_5km_Mod)
rm(MCMC_Model)

#yellow footed antechinus with no autocorrelation (5km grids)
Params <- c("pbar","purb","pnf","qbar","qrh","qrl","qfpc","qre","rbar","rsig","p")
ni <- 150000
nt <- 10
nb <- 50000
nc <- 3
Bugs_Data <- list(length(as.vector(Grid5km[,1])),length(as.vector(Sites_Grid5km[,1])),as.vector(Sites_Grid5km[,1]),URB5km,NV5km,RTHIGH_5km_100,RTLOW_5km_100,FPCH3_5km_100,RE_5km_Site,Month_5km_Survey,GridPA_5km_Aflav,PA5km_Aflav,as.vector(Sites_Grid5km[,2]),as.vector(Sites_Grid5km_Cum[,2]),Site_Repeats5km_Aflav)
names(Bugs_Data) <- list("G","GS","GRIDID","GURB","GNF","HRH","HRL","HFPC","SRE","MONTH","PAG","PA","REPSI","REPSIC","REPSU")
writeDatafileR(Bugs_Data,"bdata_aflav_5km_noauto_zib.txt")
inits <- function(){list(pbar=runif(1,-2,2),purb=runif(1,-2,2),pnf=runif(1,-2,2),qbar=runif(1,-2,2),qrh=runif(1,-2,2),qrl=runif(1,-2,2),qfpc=runif(1,-2,2),qre=as.vector(c(NA,runif(1,-2,2),runif(1,-2,2))),rbar=runif(1,-2,2),rsig=runif(1,0,4),PAG=as.vector(ifelse(is.finite(GridPA_5km_Aflav),NA,1)),PAS=get.pas.start(PA5km_Aflav))}
writeDatafileR(inits(),"binit1_aflav_5km_no_auto_zib.txt")
writeDatafileR(inits(),"binit2_aflav_5km_no_auto_zib.txt")
writeDatafileR(inits(),"binit3_aflav_5km_no_auto_zib.txt")
MCMC_Aflav_5km_NoAuto_Zib <- bugs(Bugs_Data,inits,Params,"bmod_noauto_zib.txt",n.chains=nc,n.thin=nt,n.iter=ni,n.burnin=nb,debug=F,bugs.directory="c:/Program Files (x86)/WinBUGS14/",working.directory=getwd())
for (j in 1:3)
{
	write.table(as.mcmc.list(MCMC_Aflav_5km_NoAuto_Zib)[[j]],paste(getwd(),"/aflav_5km_output/aflav_5km_mod",0,"_chain",j,".csv",sep=""),sep=",",row.names=F)
}
rm(Params)
rm(ni)
rm(nt)
rm(nb)
rm(nc)
rm(Bugs_Data)
rm(inits)
rm(ResMod)
rm(ResGrid5km_Mod)
rm(MCMC_Model)

#yellow footed antechinus with no autocorrelation (2.5km grids)
Params <- c("pbar","purb","pnf","qbar","qrh","qrl","qfpc","qre","rbar","rsig","p")
ni <- 150000
nt <- 10
nb <- 50000
nc <- 3
Bugs_Data <- list(length(as.vector(Grid2_5km[,1])),length(as.vector(Sites_Grid2_5km[,1])),as.vector(Sites_Grid2_5km[,1]),URB2_5km,NV2_5km,RTHIGH_2_5km_100,RTLOW_2_5km_100,FPCH3_2_5km_100,RE_2_5km_Site,Month_2_5km_Survey,GridPA_2_5km_Aflav,PA2_5km_Aflav,as.vector(Sites_Grid2_5km[,2]),as.vector(Sites_Grid2_5km_Cum[,2]),Site_Repeats2_5km_Aflav)
names(Bugs_Data) <- list("G","GS","GRIDID","GURB","GNF","HRH","HRL","HFPC","SRE","MONTH","PAG","PA","REPSI","REPSIC","REPSU")
writeDatafileR(Bugs_Data,"bdata_aflav_2_5km_noauto_zib.txt")
inits <- function(){list(pbar=runif(1,-2,2),purb=runif(1,-2,2),pnf=runif(1,-2,2),qbar=runif(1,-2,2),qrh=runif(1,-2,2),qrl=runif(1,-2,2),qfpc=runif(1,-2,2),qre=as.vector(c(NA,runif(1,-2,2),runif(1,-2,2))),rbar=runif(1,-2,2),rsig=runif(1,0,4),PAG=as.vector(ifelse(is.finite(GridPA_2_5km_Aflav),NA,1)),PAS=get.pas.start(PA2_5km_Aflav))}
writeDatafileR(inits(),"binit1_aflav_2_5km_no_auto_zib.txt")
writeDatafileR(inits(),"binit2_aflav_2_5km_no_auto_zib.txt")
writeDatafileR(inits(),"binit3_aflav_2_5km_no_auto_zib.txt")
MCMC_Aflav_2_5km_NoAuto_Zib <- bugs(Bugs_Data,inits,Params,"bmod_noauto_zib.txt",n.chains=nc,n.thin=nt,n.iter=ni,n.burnin=nb,debug=F,bugs.directory="c:/Program Files (x86)/WinBUGS14/",working.directory=getwd())
for (j in 1:3)
{
	write.table(as.mcmc.list(MCMC_Aflav_2_5km_NoAuto_Zib)[[j]],paste(getwd(),"/aflav_2_5km_output/aflav_2_5km_mod",0,"_chain",j,".csv",sep=""),sep=",",row.names=F)
}
rm(Params)
rm(ni)
rm(nt)
rm(nb)
rm(nc)
rm(Bugs_Data)
rm(inits)
rm(ResMod)
rm(ResGrid2_5km_Mod)
rm(MCMC_Model)

#yellow footed antechinus with autocorrelation (5km grids)
Params <- c("pbar","purb","pnf","paut","qbar","qrh","qrl","qfpc","qre","rbar","rsig","p")
ni <- 150000
nt <- 10
nb <- 50000
nc <- 3
inits <- function(){list(pbar=runif(1,-2,2),purb=runif(1,-2,2),pnf=runif(1,-2,2),paut=runif(1,-2,2),qbar=runif(1,-2,2),qrh=runif(1,-2,2),qrl=runif(1,-2,2),qfpc=runif(1,-2,2),qre=as.vector(c(NA,runif(1,-2,2),runif(1,-2,2))),rbar=runif(1,-2,2),rsig=runif(1,0,4),PAG=as.vector(ifelse(is.finite(GridPA_5km_Aflav),NA,1)),PAS=get.pas.start(PA5km_Aflav))}
writeDatafileR(inits(),"binit1_aflav_5km_auto_zib.txt")
writeDatafileR(inits(),"binit2_aflav_5km_auto_zib.txt")
writeDatafileR(inits(),"binit3_aflav_5km_auto_zib.txt")
for (i in 1:1)
{
	ResMod <- read.table(paste(getwd(),"/5km_resistance_models/model",i,".csv",sep=""),sep=",",header=T)
	ResGrid5km_Mod <- get.ordered.connectivity(Grid5km,ResMod,c("id_UL","id_UM","id_UR","id_ML","id_MR","id_LL","id_LM","id_LR"),c(paste("model",i,"_UL",sep=""),paste("model",i,"_UM",sep=""),paste("model",i,"_UR",sep=""),paste("model",i,"_ML",sep=""),paste("model",i,"_MR",sep=""),paste("model",i,"_LL",sep=""),paste("model",i,"_LM",sep=""),paste("model",i,"_LR",sep="")))
	Bugs_Data <- list(length(as.vector(Grid5km[,1])),length(as.vector(Sites_Grid5km[,1])),as.vector(Sites_Grid5km[,1]),URB5km,NF5km,RTHIGH_5km_500,RTLOW_5km_500,FPCH3_5km_500,RE_5km_Site,Month_5km_Survey,GridPA_5km_Aflav,PA5km_Aflav,as.vector(Sites_Grid5km[,2]),as.vector(Sites_Grid5km_Cum[,2]),Site_Repeats5km_Aflav,ResGrid5km_Mod[[1]][,2:9],ResGrid5km_Mod[[2]][,2:9],as.vector(ResGrid5km_Mod[[3]]))
	names(Bugs_Data) <- list("G","GS","GRIDID","GURB","GNF","HRH","HRL","HFPC","SRE","MONTH","PAG","PA","REPSI","REPSIC","REPSU","AGID","AGRE","NCELL")
	writeDatafileR(Bugs_Data,paste("bdata_aflav_5km_auto_zib_mod",i,".txt",sep=""))
	#MCMC_Model <- bugs(Bugs_Data,inits,Params,"bmod_auto_zib.txt",n.chains=nc,n.thin=nt,n.iter=ni,n.burnin=nb,debug=F,bugs.directory="c:/Program Files (x86)/WinBUGS14/",working.directory=getwd())
	MCMC_Model <- as.mcmc.list(bugs(Bugs_Data,inits,Params,"bmod_auto_zib.txt",n.chains=nc,n.thin=nt,n.iter=ni,n.burnin=nb,debug=F,bugs.directory="c:/Program Files (x86)/WinBUGS14/",working.directory=getwd()))
	for (j in 1:3)
	{
		write.table(MCMC_Model[[j]],paste(getwd(),"/aflav_5km_output/aflav_5km_mod",i,"_chain",j,".csv",sep=""),sep=",",row.names=F)
	}
}
rm(Params)
rm(ni)
rm(nt)
rm(nb)
rm(nc)
rm(Bugs_Data)
rm(inits)
rm(ResMod)
rm(ResGrid5km_Mod)
rm(MCMC_Model)

#yellow footed antechinus with autocorrelation (2.5km grids)
Params <- c("pbar","purb","pnf","paut","qbar","qrh","qrl","qfpc","qre","rbar","rsig","p")
ni <- 150000
nt <- 10
nb <- 50000
nc <- 3
inits <- function(){list(pbar=runif(1,-2,2),purb=runif(1,-2,2),pnf=runif(1,-2,2),paut=runif(1,-2,2),qbar=runif(1,-2,2),qrh=runif(1,-2,2),qrl=runif(1,-2,2),qfpc=runif(1,-2,2),qre=as.vector(c(NA,runif(1,-2,2),runif(1,-2,2))),rbar=runif(1,-2,2),rsig=runif(1,0,4),PAG=as.vector(ifelse(is.finite(GridPA_2_5km_Aflav),NA,1)),PAS=get.pas.start(PA2_5km_Aflav))}
writeDatafileR(inits(),"binit1_aflav_2_5km_auto_zib.txt")
writeDatafileR(inits(),"binit2_aflav_2_5km_auto_zib.txt")
writeDatafileR(inits(),"binit3_aflav_2_5km_auto_zib.txt")
for (i in 1:1)
{
	ResMod <- read.table(paste(getwd(),"/2_5km_resistance_models/model",i,".csv",sep=""),sep=",",header=T)
	ResGrid2_5km_Mod <- get.ordered.connectivity(Grid2_5km,ResMod,c("id_UL","id_UM","id_UR","id_ML","id_MR","id_LL","id_LM","id_LR"),c(paste("model",i,"_UL",sep=""),paste("model",i,"_UM",sep=""),paste("model",i,"_UR",sep=""),paste("model",i,"_ML",sep=""),paste("model",i,"_MR",sep=""),paste("model",i,"_LL",sep=""),paste("model",i,"_LM",sep=""),paste("model",i,"_LR",sep="")))
	Bugs_Data <- list(length(as.vector(Grid2_5km[,1])),length(as.vector(Sites_Grid2_5km[,1])),as.vector(Sites_Grid2_5km[,1]),URB2_5km,NF2_5km,RTHIGH_2_5km_500,RTLOW_2_5km_500,FPCH3_2_5km_500,RE_2_5km_Site,Month_2_5km_Survey,GridPA_2_5km_Aflav,PA2_5km_Aflav,as.vector(Sites_Grid2_5km[,2]),as.vector(Sites_Grid2_5km_Cum[,2]),Site_Repeats2_5km_Aflav,ResGrid2_5km_Mod[[1]][,2:9],ResGrid2_5km_Mod[[2]][,2:9],as.vector(ResGrid2_5km_Mod[[3]]))
	names(Bugs_Data) <- list("G","GS","GRIDID","GURB","GNF","HRH","HRL","HFPC","SRE","MONTH","PAG","PA","REPSI","REPSIC","REPSU","AGID","AGRE","NCELL")
	writeDatafileR(Bugs_Data,paste("bdata_aflav_2_5km_auto_zib_mod",i,".txt",sep=""))
	#MCMC_Model <- bugs(Bugs_Data,inits,Params,"bmod_auto_zib.txt",n.chains=nc,n.thin=nt,n.iter=ni,n.burnin=nb,debug=F,bugs.directory="c:/Program Files (x86)/WinBUGS14/",working.directory=getwd())
	MCMC_Model <- as.mcmc.list(bugs(Bugs_Data,inits,Params,"bmod_auto_zib.txt",n.chains=nc,n.thin=nt,n.iter=ni,n.burnin=nb,debug=F,bugs.directory="c:/Program Files (x86)/WinBUGS14/",working.directory=getwd()))
	for (j in 1:3)
	{
		write.table(MCMC_Model[[j]],paste(getwd(),"/aflav_2_5km_output/aflav_2_5km_mod",i,"_chain",j,".csv",sep=""),sep=",",row.names=F)
	}
}
rm(Params)
rm(ni)
rm(nt)
rm(nb)
rm(nc)
rm(Bugs_Data)
rm(inits)
rm(ResMod)
rm(ResGrid2_5km_Mod)
rm(MCMC_Model)




















-------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#FUNCTIONS
get.3dgrid <- function(DataInput,SitesperGrid,CaptureCol,GridID,Cnt=TRUE)
#creates a 3d array for input into winbugs with dimensions GRIDS X SITES X REPEATS
#Data Input is the raw input, SitesperGird is a nx2 matrix of the grid IDs and the number
#of sites within each grid, CaptureCol is a vector of the names of the fields
#containing the number of captures, GridID is the name of the field containing the
#grid ID number (text), Cnt indicates whether counts or presence/absence is returned 
{
	#create output
	Output <- array(NA,dim=c(nrow(SitesperGrid),max(SitesperGrid[,2]),length(CaptureCol)))

	#get data
	for (i in 1:nrow(SitesperGrid))
	#loop through grids
	{
		for (j in 1:SitesperGrid[i,2])
		#loop through sites
		{
			#loop through repeats
			for (k in 1:5)
			{
				Output[i,j,k] <- DataInput[which(DataInput[,GridID] == SitesperGrid[i,1]),][j,CaptureCol[k]]
				
				if (Output[i,j,k] == -9999)
				{
					Output[i,j,k] <- NA
				}
								
				if (!is.na(Output[i,j,k]))
				{
					if (!Cnt & Output[i,j,k] > 0)
					{
						Output[i,j,k] <- 1
					}
				}		
			}
		}
	}
	
	return(Output)
}

get.repeats <- function(Array,SitesperGrid)
#gets the repeats for each grid by site combination
#Array is an output from get.3dgrid()
#SitesperGrid is an output from aggregate() and the number of 
#sites per grid in the second field   
{
	Output <- matrix(NA,nrow=dim(Array)[1],ncol=dim(Array)[2])
	
	for (i in 1:nrow(Output))
	#loop through grids
	{
		for (j in 1:SitesperGrid[i,2])
		#loop through sites within each grid
		{
			Output[i,j] <- max(length(which(!is.na(Array[i,j,]))),1)
		}
	}

	return(Output)
}

get.hr.covariate <- function(DataInput,SitesperGrid,GridID,HrID,HrData,Covariate)
#creates a 3d array for input into winbugs with dimensions GRIDS X SITES
#Data Input is the raw input, SitesperGird is a nx2 matrix of the grid IDs and the number
#of sites within each grid, CaptureCol is a vector of the names of the fields
#containing the number of captures, GridID is the name of the field containing the
#grid ID number (text), HrID is the name of the field containing the home range data ID
#HrData is the home range data, Covariate is the name of the field for the covariate of interest 
{
	#create output
	Output <- matrix(NA,nrow=nrow(SitesperGrid),ncol=max(SitesperGrid[,2]))

	#get data
	for (i in 1:nrow(SitesperGrid))
	#loop through grids
	{
		for (j in 1:SitesperGrid[i,2])
		#loop through sites
		{
			Output[i,j] <- HrData[DataInput[which(DataInput[,GridID] == SitesperGrid[i,1]),][j,HrID],Covariate]
		}
	}
	
	return(Output)
}

get.site.covariate <- function(DataInput,SitesperGrid,GridID,Covariate)
#creates a 3d array for input into winbugs with dimensions GRIDS X SITES
#Data Input is the raw input, SitesperGird is a nx2 matrix of the grid IDs and the number
#of sites within each grid, CaptureCol is a vector of the names of the fields
#containing the number of captures, GridID is the name of the field containing the
#grid ID number (text), Covariate is the name of the field for the covariate of interest 
{
	#create output
	Output <- matrix(NA,nrow=nrow(SitesperGrid),ncol=max(SitesperGrid[,2]))

	#get data
	for (i in 1:nrow(SitesperGrid))
	#loop through grids
	{
		for (j in 1:SitesperGrid[i,2])
		#loop through sites
		{
			Output[i,j] <- DataInput[which(DataInput[,GridID] == SitesperGrid[i,1]),][j,Covariate]
		}
	}
	
	return(Output)
}

get.pas.start <- function(PA_Data)
#get the starting values for PAS
{
	#get the maximum value for each grid ans site
	Output <- apply(PA_Data,c(1,2),max,na.rm=TRUE)
	#if not surveyed then set starting vlaue to 1
	Output[!is.finite(Output)] <- NA
	
	return(Output)
}

get.ordered.connectivity <- function(Grid,ConModel,ID_Fields,Res_Fields)
#outputs two matrices of size no. grids x 8
#with one being the indexes of the connections
#and the other being the resistances between those
#connections. In each case the non-connections are put
#in the last columns of the matrices. Also output the number
#of neighbour cells for each grid 
{
	Indices <- matrix(NA,nrow=nrow(Grid),ncol=9)
	Resistances <- matrix(NA,nrow=nrow(Grid),ncol=9)
	Number <- matrix(NA,nrow=nrow(Grid),ncol=1)

	#get the grid id's
	Indices[,1] <- Grid[,"id"]
	Resistances[,1] <- ConModel[,"id"]
	
	for (i in 1:nrow(Grid))
	{
		Index <- Grid[i,ID_Fields][which(Grid[i,ID_Fields] > 0)]
		Indices[i,2:(length(Index) + 1)] <- t(as.matrix(Index))
		
		Res <- ConModel[i,Res_Fields][which(ConModel[i,Res_Fields] > 0)]
		Resistances[i,2:(length(Res) + 1)] <- t(as.matrix(Res))
		
		Number[i,1] <- length(Index)
	}
	
	return(list(Ind = Indices, Res = Resistances,Num = Number))
}

------------------------------------------------------------------------------------------------------------------------------------------------------

#OLD STUFF





   