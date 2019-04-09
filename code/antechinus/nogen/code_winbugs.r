### REQUIRED LIBRARIES ###

library("foreign")
library("R2WinBUGS")

### FUNCTIONS ###

"writeDatafileR" <- function(DATA, towhere = "toWinBUGS.txt", fill = 80)
{
#
# Writes from R to file "towhere" text defining a list containing "DATA" in a form compatable with WinBUGS.
# Required arguments:
# DATA - either a data frame or else a list consisting of any combination of scalars, vectors, arrays or data frames (but not lists).
#   If a list, all list elements that are not data.frames must be named. Names of data.frames in DATA are ignored.
# Optional arguments:
# towhere - file to receive output. Is "toWinBUGS.txt" by default.
# fill - If numeric, number of columns for output. (Default is 80.) If FALSE, output will be on one line. If TRUE, number of
#   columns is given by .Options$width.
# Value:
# Text defining a list is output to file "towhere". 
# Details:
#  The function performs considerable checking of DATA argument. Since WinBUGS requires numeric input, no factors or character vectors
# are allowed. All data must be named, either as named elements of DATA (if it is a list) or else using the names given in data frames.
# Data frames may contain matrices. 
# Arrays of any dimension are rearranged to be in row-major order, as required by WinBUGS. Scientific notation is also handled properly.
# In particular, the number will consist of a mantissa _containing a decimal point_ followed by "E", then either "+" or "-", and finally 
# a _two-digit_ number. 
# Written by Terry Elrod. Disclaimer: This function is used at the user's own risk. 
# Please send comments to Terry.Elrod@UAlberta.ca.
# Revision history: 2003-11-14: Fixed to handle missing values properly. (Thanks to Kjetil Halvorsen.)
#					2003-11-14:	Tests for valid Winbugs names. Forces single precision for all numbers.
	formatDataR <- 
	#
	# Prepared DATA for input to WinBUGS.
	function(DATA)
	{
		testWinbugsNames <-
		#
		# Checks to see that all names are valid...
		function(na){
			baseTestString <- c(
				"The following variable names are invalid in R: ",
				"The following variable names are used more than once: ",
				"The following variable names have more than 8 characters: ",
				"The following variable names contain two or more periods in a row: ",
				"The following variable names end in a period: ")
			# Testing for invalid R names ...
			nameTest1 <- make.names(na, unique = FALSE)
			nameTest1 <- (nameTest1 != na)
			# Testing for duplicate names....
			nameTest2 <- make.names(na, unique = TRUE)
			nameTest2 <- (nameTest2 != na)
			# Testing for excess length...
			nameTest3 <- substring(na, 1, 8)
			nameTest3 <- (na != nameTest3)
			# Testing for presence of two or more successive periods ...
			nameTest4 <- regexpr("\\.\\.", na)
			nameTest4 <- (nameTest4 > 0)
			# Testing for presence of ending period ...
			nameTest5 <- regexpr("\\.$", na)
			nameTest5 <- (nameTest5 > 0)
			# Assembling tests and reporting results...
			nameTest <- cbind(nameTest1, nameTest2, nameTest3, nameTest4, nameTest5)
			if(any(nameTest)){
				nameTestInd <- apply(nameTest, 2, any)
				whichTest <- seq(along=nameTestInd)[nameTestInd]
				testString <- "There were problems with names of one or more variables:"
				if(nameTestInd[1])
					testString <- paste(testString, paste(baseTestString[1], paste(na[nameTest[,1]], collapse = ", "), sep=""), sep="\n")
				if(nameTestInd[2])
					testString <- paste(testString, paste(baseTestString[2], paste(unique(na[nameTest[,2]]), collapse = ", "), sep=""), sep="\n")
				if(nameTestInd[3])
					testString <- paste(testString, paste(baseTestString[3], paste(na[nameTest[,3]], collapse = ", "), sep="") ,sep="\n")
				if(nameTestInd[4])
					testString <- paste(testString, paste(baseTestString[4], paste(na[nameTest[,4]], collapse = ", "), sep="") ,sep="\n")
				if(nameTestInd[5])
					testString <- paste(testString, paste(baseTestString[5], paste(na[nameTest[,5]], collapse = ", "), sep="") ,sep="\n")
				stop(testString)
			}
			invisible(0)
		}
		toSingle <- 
		#
		# Takes numeric vector, adds period to mantissa in scientific notation (if necessary),
		#	converts "e" to "E", expresses mantissa with at most 10 characters,
		#	and eliminates trailing zeros from mantissa.
		function(x)
		{
			myRegMatchPos <- 
			#
			# Duplicates regMatchPos in the S4 engine...
			function(w, txt)
			{
				st <- regexpr(txt, w)
				pplusind <- (st > 0)
				fin <- st + attr(st, "match.length") - 1
				pplus <- cbind(st, fin)
				pplus[!pplusind,  ] <- NA
				pplus
			}
			xdim <- dim(x)
			x <- as.single(x)
			x <- sapply(x,function(y) format(y, digits=7, trim=TRUE))
			# First to look for positives:
			pplus <- myRegMatchPos(x, "e\\+0")
			pplusind <- apply(pplus, 1, function(y)
			(!any(is.na(y))))
			if(any(pplusind)) {
				# Making sure that periods are in mantissa...
				init <- substring(x[pplusind], 1, pplus[
					pplusind, 1] - 1)
				#...preceeding exponent
				pper <- myRegMatchPos(init, "\\.")
				pperind <- apply(pper, 1, function(y)
				(all(is.na(y))))
				if(any(pperind))
					init[pperind] <- paste(init[pperind],
						".0", sep = "")
				# Changing the format of the exponent...
				x[pplusind] <- paste(init, "E+", substring(
					x[pplusind], pplus[pplusind, 2] + 1),
					sep = "")
			}
			# Then to look for negatives:
			pminus <- myRegMatchPos(x, "e\\-0")
			pminusind <- apply(pminus, 1, function(y)
			(!any(is.na(y))))
			if(any(pminusind)) {
				# Making sure that periods are in mantissa...
				init <- substring(x[pminusind], 1, pminus[
					pminusind, 1] - 1)
				#...preceeding exponent
				pper <- myRegMatchPos(init, "\\.")
				pperind <- apply(pper, 1, function(y)
				(all(is.na(y))))
				if(any(pperind))
					init[pperind] <- paste(init[pperind],
						".0", sep = "")
				# Changing the format of the exponent...
				x[pminusind] <- paste(init, "E-", substring(
					x[pminusind], pminus[pminusind, 2] +
					1), sep = "")
			}
			x
		}
		if(!is.list(DATA))
			stop("DATA must be a named list or data frame.")
		dlnames <- names(DATA)
		if(is.data.frame(DATA))
			DATA <- as.list(DATA)
		#
		# Checking for lists in DATA....
		lind <- sapply(DATA, is.list)
		# Checking for data frames in DATA....
		dfind <- sapply(DATA, is.data.frame)
		# Any lists that are not data frames?...
		if(any(lind & !dfind)) stop("DATA may not contain lists.")
		# Checking for unnamed elements of list that are not data frames....
		if(any(dlnames[!dfind] == "")) stop(
				"When DATA is a list, all its elements that are not data frames must be named."
				)
		if(any(dfind)) {
			dataold <- DATA
			DATA <- vector("list", 0)
			for(i in seq(along = dataold)) {
				if(dfind[i])
					DATA <- c(DATA, as.list(dataold[[i]]))
				else DATA <- c(DATA, dataold[i])
			}
			dataold <- NULL
		}
		dlnames <- names(DATA)
		# Making sure all names are valid ...
		testWinbugsNames(dlnames)
		# Checking for factors....
		factorind <- sapply(DATA, is.factor)
		if(any(factorind))
			stop(paste(
				"DATA may not include factors. One or more factor variables were detected:",
				paste(dlnames[factorind], collapse = ", ")))
		# Checking for character vectors....
		charind <- sapply(DATA, is.character)
		if(any(charind))
			stop(paste(
				"WinBUGS does not handle character data. One or more character variables were detected:",
				paste(dlnames[charind], collapse = ", ")))
		# Checking for complex vectors....
		complexind <- sapply(DATA, is.complex)
		if(any(complexind))
			stop(paste(
				"WinBUGS does not handle complex data. One or more complex variables were detected:",
				paste(dlnames[complexind], collapse = ", ")))
		# Checking for values farther from zero than 1E+38 (which is limit of single precision)....
		toobigind <- sapply(DATA, function(x)
		{
			y <- abs(x[!is.na(x)])
			any(y[y > 0] > 1.0e+038)
		}
		)
		if(any(toobigind))
			stop(paste(
				"WinBUGS works in single precision. The following variables contain data outside the range +/-1.0E+38: ",
				paste(dlnames[toobigind], collapse = ", "),
				".\n", sep = ""))
		# Checking for values in range +/-1.0E-38 (which is limit of single precision)....
		toosmallind <- sapply(DATA, function(x)
		{
			y <- abs(x[!is.na(x)])
			any(y[y > 0] < 1.0e-038)
		}
		)
		n <- length(dlnames)
		data.string <- as.list(rep(NA, n))
		for(i in 1:n) {
			ldi <- length(DATA[[i]])
			if(ldi == 1) {
				ac <- toSingle(DATA[[i]])
				data.string[[i]] <- c(
					names(DATA)[i], 
					"=",
					paste(ac), 
					"," )
				next
			}
			if(is.vector(DATA[[i]]) & ldi > 1) {
				ac <- toSingle(DATA[[i]])
				data.string[[i]] <- c(
					names(DATA)[i],
					"= c(",
					paste(ac[-ldi], ",", sep=""), 
					paste(ac[ldi], ")", sep=""),
					"," )
				next
			}
			if(is.array(DATA[[i]])) {
				ac <- toSingle(aperm(DATA[[i]]))
				data.string[[i]] <- c(
					names(DATA)[i], 
					"= structure(.Data = c(", 
					paste(ac[-ldi], ",", sep=""),
					paste(ac[ldi], "),", sep=""), 
					".Dim=c(",
					paste(as.character(dim(DATA[[i]])),collapse = ", "),
					"))",
					"," )
			}
		}
		data.string <- unlist(data.string)			
		data.tofile <- c(
			"list(", 
			data.string[-length(data.string)], 
			")" )
		if(any(toosmallind))
			warning(paste(
				"WinBUGS works in single precision. The following variables contained nonzero data",
				"\ninside the range +/-1.0E-38 that were set to zero: ",
				paste(dlnames[toosmallind], collapse = ", "),
				".\n", sep = ""))
		return(data.tofile)
	}
	cat(formatDataR(DATA), file = towhere, fill = fill)
	formatDataR(DATA)
	invisible(0)
}

get.cum.numsites <-function(SitesPerGrid)
#gets the cumulative number of sites or points per grid 
#SitesPerGrid is the number of sites per grid using aggregate
{
	Output <- matrix(NA,nrow=nrow(SitesPerGrid),ncol=1)
	
	Cum = 0
	for (i in 1:nrow(SitesPerGrid))
	{
		Cum <- Cum + SitesPerGrid[i,2]
		Output[i,1] <- Cum
	}
	
	return(Output)
}

get.siteobs <- function(Survey,SitesperGrid,CaptureCol,GridID,Cnt=TRUE)
#creates a 3d array of observed presence/absence for input into winbugs with dimensions GRIDS X SITES X REPEATS
#Survey is the raw survey data, SitesperGird is a nx2 matrix of the grid IDs and the number
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
			for (k in 1:length(CaptureCol))
			{
				Output[i,j,k] <- Survey[which(Survey[,GridID] == SitesperGrid[i,1]),][j,CaptureCol[k]]
				
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
#gets the repeats for each grid by site combination as a 2d matrix
#Array is an output from get.3dgrid()
#SitesperGrid is an output from aggregate() and the number of 
#sites per grid is in the second field   
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

get.gridatsitepred <- function(Grid,SitesperGrid,Pred)
#creates a 1d array of a predictor at the 2.5 km grid level for the site with dimensions GRIDS
#Grid is the grid data
#SitesperGird is a nx2 matrix of the grid IDs and the number of sites within each grid
#Pred is the name of the field containing the predictor (text)
{
	#create output
	Output <- matrix(NA,nrow=nrow(SitesperGrid),ncol=1)

	#get data
	for (i in 1:nrow(SitesperGrid))
	#loop through grids
	{
		Output[i] <- Grid[which(Grid[,"GID"] == SitesperGrid[i,1]),Pred]		
	}
	
	Mean <- mean(Output,na.rm=T)
	Output <- ifelse(Output[,1]==-9999,Mean,Output[,1])
		
	return(Output)
}

get.sitepred <- function(Survey,SitesperGrid,Pred,GridID)
#creates a 2d array of a predictors at the site scale with dimension grids x sites
#Survey is the raw survey data
#SitesperGird is a nx2 matrix of the grid IDs and the number of sites within each grid
#Pred is the name of the field containing the predictor (text)
#GridID is the name of the field containing the grid ID number (text)
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
			Output[i,j] <- Survey[which(Survey[,GridID] == SitesperGrid[i,1]),][j,Pred]
		}
	}
	
	Mean <- mean(Output,na.rm=T)
	Output <- ifelse(Output==-9999,Mean,Output)
		
	return(Output)
}			

get.ordered.connectivity <- function(Grid,ConModel,ID_Fields,Res_Fields,NumPointsGrid)
#first outputs two matrices of size no. grids x 8
#with one being the indexes of the connections in terms of the indices of the points layer
#and the other being the resistances between those
#connections. In each case the non-connections are put
#in the last columns of the matrices. Also outputs the number
#of neighbour cells for each grid 
{
	Indices <- matrix(NA,nrow=nrow(Grid),ncol=9)
	Resistances <- matrix(NA,nrow=nrow(Grid),ncol=9)
	Number <- matrix(NA,nrow=nrow(Grid),ncol=1)

	#get the grid id's
	Indices[,1] <- Grid[,"id"]
	Resistances[,1] <- ConModel[,"id"]
	
	MeanRes <- mean(as.matrix(ConModel[,Res_Fields]),na.rm=T)
		
	for (i in 1:nrow(Grid))
	{
		#get IDs and resistances for connections
		TempGrid <- as.matrix(Grid[Indices[i,1],ID_Fields])
		TempRes <- as.matrix(ConModel[Resistances[i,1],Res_Fields])
		
		for (j in 1:8)
		{
			if ((TempGrid[j] > 0) & is.na(TempRes[j]))
			{
				TempRes[j] <- MeanRes 
			}
			
			if (length(which(NumPointsGrid[,1]==TempGrid[j])) == 0)
			{
				TempGrid[j] <- NA
				TempRes[j] <- NA				
			}
		}
				
		#get resistances
		Res <- TempRes[which(TempRes >= 0)]
		if (length(Res) > 0)
		{
			Resistances[i,2:(length(Res) + 1)] <- t(as.matrix(Res))
		}
		
		#get indices 		
		Index <- TempGrid[which(TempGrid >= 0)]
		if (length(Index) > 0)
		{
			Indices[i,2:(length(Index) + 1)] <- t(as.matrix(Index))
			
			for (j in 1:length(Index))
			{
				Indices[i,(j + 1)] <- which(NumPointsGrid[,1]==Indices[i,(j + 1)])
			}
		}	
		
		Number[i,1] <- length(Index)
	}
	
	return(list(Ind = Indices, Res = Resistances,Num = Number))
}

get.boot.sample <- function(GenDist,Res)
#gets a bootstrap sample (without replacement)
{

	Sample <- sample(1:nrow(GenDist),nrow(GenDist))

	#split sample in half
	Sample1 <- Sample[1:(floor(nrow(GenDist) / 2))]
	Sample2 <- Sample[((floor(nrow(GenDist) / 2)) + 1):((floor(nrow(GenDist) / 2)) * 2)]
	
	Output <- matrix(NA,nrow=length(Sample1),ncol=2)
	
	for (i in 1: nrow(Output))
	{
		Output[i,1] <- GenDist[Sample1[i],Sample2[i]]
		Output[i,2] <- Res[Sample1[i],Sample2[i]]
	}
		
	return(Output)
}

get.ocsite.start <- function(ObsData_Data)
#get the observed state as starting values for each site
{
	#get the maximum value for each grid and site combination
	Output <- apply(ObsData_Data,c(1,2),max,na.rm=TRUE)
	#if not surveyed then set starting value to NA
	Output[!is.finite(Output)] <- NA
		
	return(Output)
}
		
### SCRIPT ###

#read in data
Surveys <- read.dbf("E:/Projects/seq_genetics/analysis/distribution_models/models/data/fauna_data_final.dbf")
SurveysAflav <- Surveys[which(Surveys[,"SPECIES"]=="A.flav"),]
SurveysKoala <- Surveys[which(Surveys[,"SPECIES"]=="P.cin"),]
SurveysPbrev <- Surveys[which(Surveys[,"SPECIES"]=="P.bre"),]
SurveysPnorf <- Surveys[which(Surveys[,"SPECIES"]=="P.nor"),]
Grid2_5km <- read.dbf("E:/Projects/seq_genetics/analysis/distribution_models/models/data/grid_2_5km_final.dbf")
Points <- read.dbf("E:/Projects/seq_genetics/analysis/distribution_models/models/data/grid_pts_500m_final.dbf")
Points_HabOnly <- Points[which(Points[,"dbvg5m"]>0),] #only sites in REs that are habitat and sampled
Dbvg1 <- read.dbf("E:/Projects/seq_genetics/analysis/distribution_models/models/data/dbvg1m.dbf")
Dbvg2 <- read.dbf("E:/Projects/seq_genetics/analysis/distribution_models/models/data/dbvg2m.dbf")
Dbvg5 <- read.dbf("E:/Projects/seq_genetics/analysis/distribution_models/models/data/dbvg5m.dbf")
Aflav_Gdist <- read.table("E:/Projects/seq_genetics/analysis/distribution_models/models/data/gen_dist/aflav_gendist.csv",header=F,sep=",")
Koala_Gdist <- read.table("E:/Projects/seq_genetics/analysis/distribution_models/models/data/gen_dist/koala_gendist.csv",header=F,sep=",")
Pbrev_Gdist <- read.table("E:/Projects/seq_genetics/analysis/distribution_models/models/data/gen_dist/pbrev_gendist.csv",header=F,sep=",")
Pnorf_Gdist <- read.table("E:/Projects/seq_genetics/analysis/distribution_models/models/data/gen_dist/pnorf_gendist.csv",header=F,sep=",")

#set up data for WinBugs

#response variables

#response variables - partially observed true occupancy - 2.5km grid level
OcGridAflav <- ifelse((Grid2_5km[,"yfa_b90"] + Grid2_5km[,"yfa_a90b00"] + Grid2_5km[,"yfa_a00"])>0,1,NA) #occupancy at the 2.5km grid level based on WILDNET data (all years)  
OcGridKoala <- ifelse((Grid2_5km[,"kol_b90"] + Grid2_5km[,"kol_a90b00"] + Grid2_5km[,"kol_a00"])>0,1,NA) #occupancy at the 2.5km grid level based on WILDNET data (all years) 
OcGridPbrev <- ifelse((Grid2_5km[,"sg_b90"] + Grid2_5km[,"sg_a90b00"] + Grid2_5km[,"sg_a00"])>0,1,NA) #occupancy at the 2.5km grid level based on WILDNET data (all years) 
OcGridPnorf <- ifelse((Grid2_5km[,"sq_b90"] + Grid2_5km[,"sq_a90b00"] + Grid2_5km[,"sq_a00"])>0,1,NA) #occupancy at the 2.5km grid level based on WILDNET data (all years) 

#response variables - observed presence/absence - site level
#get number of sites per grid
NumSitesGrid <- aggregate(SurveysAflav[,"ID_2_5km"],by=list(SurveysAflav[,"ID_2_5km"]),FUN=length)
NumSitesGridCum <- get.cum.numsites(NumSitesGrid)
#construct 3d matrix for response data 
ObSiteAflav <- get.siteobs(SurveysAflav,NumSitesGrid,c("NC1","NC2","NC3","NC4","NC5"),"ID_2_5km",FALSE)
ObSiteKoala <- get.siteobs(SurveysKoala,NumSitesGrid,c("NC1","NC2","NC3","NC4","NC5"),"ID_2_5km",FALSE)
ObSitePbrev <- get.siteobs(SurveysPbrev,NumSitesGrid,c("NC1","NC2","NC3","NC4","NC5"),"ID_2_5km",FALSE)
ObSitePnorf <- get.siteobs(SurveysPnorf,NumSitesGrid,c("NC1","NC2","NC3","NC4","NC5"),"ID_2_5km",FALSE)
#get number of repeats per site
NumRepSitesAflav <- get.repeats(ObSiteAflav,NumSitesGrid)
NumRepSitesKoala <- get.repeats(ObSiteKoala,NumSitesGrid)
NumRepSitesPbrev <- get.repeats(ObSitePbrev,NumSitesGrid)
NumRepSitesPnorf <- get.repeats(ObSitePnorf,NumSitesGrid)

#response variables - unobserved true occupancy - site level (grid x sites matrix)
OcSite <- matrix(nrow=dim(ObSiteAflav)[1],ncol=dim(ObSiteAflav)[2],NA)

#response variables - unobserved true occupancy - points in grids level (grid x points matrix)
NumPointsGrid <- aggregate(Points_HabOnly[,"ID_2_5km"],by=list(Points_HabOnly[,"ID_2_5km"]),FUN=length)
#construct 2d matrix of true occupancy
OcPoints <- matrix(nrow=dim(NumPointsGrid)[1],ncol=max(NumPointsGrid[,2]),NA)

#response variables - genetic distance
GDistAflav <- Aflav_Gdist
GDistKoala <- Koala_Gdist
GDistPbrev <- Pbrev_Gdist
GDistPnorf <- Pnorf_Gdist

#predictors

#predictors - 2.5km grid level
MeanVal <- mean(ifelse(Grid2_5km[,"meant"]==-9999,NA,Grid2_5km[,"meant"]),na.rm=T)
GMeanTemp <- ifelse(Grid2_5km[,"meant"]==-9999,MeanVal,Grid2_5km[,"meant"])
MeanVal <- mean(ifelse(Grid2_5km[,"maxt"]==-9999,NA,Grid2_5km[,"maxt"]),na.rm=T)
GMaxTemp <- ifelse(Grid2_5km[,"maxt"]==-9999,MeanVal,Grid2_5km[,"maxt"])
MeanVal <- mean(ifelse(Grid2_5km[,"mint"]==-9999,NA,Grid2_5km[,"mint"]),na.rm=T)
GMinTemp <- ifelse(Grid2_5km[,"mint"]==-9999,MeanVal,Grid2_5km[,"mint"])
MeanVal <- mean(ifelse(Grid2_5km[,"meanr"]==-9999,NA,Grid2_5km[,"meanr"]),na.rm=T)
GMeanRain <- ifelse(Grid2_5km[,"meanr"]==-9999,MeanVal,Grid2_5km[,"meanr"]) 
MeanVal <- mean(ifelse(Grid2_5km[,"meanfpc"]==-9999,NA,Grid2_5km[,"meanfpc"]),na.rm=T)
GMeanFPC <- ifelse(Grid2_5km[,"meanfpc"]==-9999,MeanVal,Grid2_5km[,"meanfpc"]) 
MeanVal <- mean(ifelse(Grid2_5km[,"urbanpt"]==-9999,NA,Grid2_5km[,"urbanpt"]),na.rm=T)
GUrbP <- ifelse(Grid2_5km[,"urbanpt"]==-9999,MeanVal,Grid2_5km[,"urbanpt"]) 
MeanVal <- mean(ifelse(Grid2_5km[,"waterp"]==-9999,NA,Grid2_5km[,"waterp"]),na.rm=T)
GWatP <- ifelse(Grid2_5km[,"waterp"]==-9999,MeanVal,Grid2_5km[,"waterp"]) 
MeanVal <- mean(ifelse(Grid2_5km[,"nativept"]==-9999,NA,Grid2_5km[,"nativept"]),na.rm=T)
GNatP <- ifelse(Grid2_5km[,"nativept"]==-9999,MeanVal,Grid2_5km[,"nativept"]) 
MeanVal <- mean(ifelse(Grid2_5km[,"patchd"]==-9999,NA,Grid2_5km[,"patchd"]),na.rm=T)
GPatD <- ifelse(Grid2_5km[,"patchd"]==-9999,MeanVal,Grid2_5km[,"patchd"]) 
rm(MeanVal)
GridCor <- cor(cbind(GMeanTemp,GMaxTemp,GMinTemp,GMeanRain),method="spearman") #temperature variables all highly correlated so used MeanTemp and MeanRain only

#predictors - site level
GSMeanFPC <- get.gridatsitepred(Grid2_5km,NumSitesGrid,"meanfpc")
GSUrbP <- get.gridatsitepred(Grid2_5km,NumSitesGrid,"urbanpt")
GSWatP <- get.gridatsitepred(Grid2_5km,NumSitesGrid,"waterp")
GSNatP <- get.gridatsitepred(Grid2_5km,NumSitesGrid,"nativept")
GSPatDP <- get.gridatsitepred(Grid2_5km,NumSitesGrid,"patchd")
Sb250FPC <- get.sitepred(SurveysAflav,NumSitesGrid,"b250fpc","ID_2_5km")
Sb125FPC <- get.sitepred(SurveysAflav,NumSitesGrid,"b125fpc","ID_2_5km")
Sb50FPC <- get.sitepred(SurveysAflav,NumSitesGrid,"b50fpc","ID_2_5km")
Sb250PNat <- get.sitepred(SurveysAflav,NumSitesGrid,"b250pnat","ID_2_5km")
Sb125PNat <- get.sitepred(SurveysAflav,NumSitesGrid,"b125pnat","ID_2_5km")
Sb50PNat <- get.sitepred(SurveysAflav,NumSitesGrid,"b50pnat","ID_2_5km")
Sb250RDens <- get.sitepred(SurveysAflav,NumSitesGrid,"b250rdens","ID_2_5km")
Sb125RDens <- get.sitepred(SurveysAflav,NumSitesGrid,"b125rdens","ID_2_5km")
Sb50RDens <- get.sitepred(SurveysAflav,NumSitesGrid,"b50rdens","ID_2_5km")
Sb250RElev <- get.sitepred(SurveysAflav,NumSitesGrid,"b250elev","ID_2_5km")
Sb125RElev <- get.sitepred(SurveysAflav,NumSitesGrid,"b125elev","ID_2_5km")
Sb50RElev <- get.sitepred(SurveysAflav,NumSitesGrid,"b50elev","ID_2_5km")
Sbd30 <- get.sitepred(SurveysAflav,NumSitesGrid,"bd30","ID_2_5km")
Sclay30 <- get.sitepred(SurveysAflav,NumSitesGrid,"clay30","ID_2_5km")
Spawc1m <- get.sitepred(SurveysAflav,NumSitesGrid,"pawc1m","ID_2_5km")
Sdbvg1m <- get.sitepred(SurveysAflav,NumSitesGrid,"dbvg1m","ID_2_5km")
Sdbvg2m <- get.sitepred(SurveysAflav,NumSitesGrid,"dbvg2m","ID_2_5km")
Sdbvg5m <- get.sitepred(SurveysAflav,NumSitesGrid,"dbvg5m","ID_2_5km")
SMonth <- get.sitepred(SurveysAflav,NumSitesGrid,"dbvg5m","ID_2_5km")
SiteCor <- cor(cbind(GSMeanFPC,GSUrbP,GSWatP,GSNatP,GSPatDP,Sb250FPC,Sb125FPC,Sb50FPC,Sb250PNat,Sb125PNat,Sb50PNat,Sb250RDens,Sb125RDens,Sb50RDens,Sb250RElev,Sb125RElev,Sb50RElev,Sbd30,Sclay30,Spawc1m),method="spearman")
#based on correlations > 0.6 meaning collinearity and some common sense - use the following predictors: GSNatP, GSPatD, [Sb250FPC,Sb125FPC,Sb50FPC], [Sb250RDens,Sb125RDens,Sb50RDens], [Sb250Elev,Sb125Elev,Sb50Elev],
# Sbd30, Sclay30, Spawc1m, [Sdbvg1m,Sdbvg2m,Sdbvg5m], SMonth
#this would result in 12 predictor variables (maybe too much?) 

#predictors - points in grids level
GPMeanFPC <- get.gridatsitepred(Grid2_5km,NumPointsGrid,"meanfpc")
GPUrbP <- get.gridatsitepred(Grid2_5km,NumPointsGrid,"urbanpt")
GPWatP <- get.gridatsitepred(Grid2_5km,NumPointsGrid,"waterp")
GPNatP <- get.gridatsitepred(Grid2_5km,NumPointsGrid,"nativept")
GPPatDP <- get.gridatsitepred(Grid2_5km,NumPointsGrid,"patchd")
Pb250FPC <- get.sitepred(Points_HabOnly,NumPointsGrid,"b250fpc","ID_2_5km")
Pb125FPC <- get.sitepred(Points_HabOnly,NumPointsGrid,"b125fpc","ID_2_5km")
Pb50FPC <- get.sitepred(Points_HabOnly,NumPointsGrid,"b50fpc","ID_2_5km")
Pb250PNat <- get.sitepred(Points_HabOnly,NumPointsGrid,"b250pnat","ID_2_5km")
Pb125PNat <- get.sitepred(Points_HabOnly,NumPointsGrid,"b125pnat","ID_2_5km")
Pb50PNat <- get.sitepred(Points_HabOnly,NumPointsGrid,"b50pnat","ID_2_5km")
Pb250RDens <- get.sitepred(Points_HabOnly,NumPointsGrid,"b250rden","ID_2_5km")
Pb125RDens <- get.sitepred(Points_HabOnly,NumPointsGrid,"b125rden","ID_2_5km")
Pb50RDens <- get.sitepred(Points_HabOnly,NumPointsGrid,"b50rden","ID_2_5km")
Pb250RElev <- get.sitepred(Points_HabOnly,NumPointsGrid,"b250elev","ID_2_5km")
Pb125RElev <- get.sitepred(Points_HabOnly,NumPointsGrid,"b125elev","ID_2_5km")
Pb50RElev <- get.sitepred(Points_HabOnly,NumPointsGrid,"b50elev","ID_2_5km")
Pbd30 <- get.sitepred(Points_HabOnly,NumPointsGrid,"bd30","ID_2_5km")
Pclay30 <- get.sitepred(Points_HabOnly,NumPointsGrid,"clay30","ID_2_5km")
Ppawc1m <- get.sitepred(Points_HabOnly,NumPointsGrid,"pawc1m","ID_2_5km")
Pdbvg1m <- get.sitepred(Points_HabOnly,NumPointsGrid,"dbvg1m","ID_2_5km")
Pdbvg2m <- get.sitepred(Points_HabOnly,NumPointsGrid,"dbvg2m","ID_2_5km")
Pdbvg5m <- get.sitepred(Points_HabOnly,NumPointsGrid,"dbvg5m","ID_2_5km")
PREArea <- get.sitepred(Points_HabOnly,NumPointsGrid,"rearea","ID_2_5km")

#MODEL - WITH SPATIAL AUTOCORRELATION - BUT NOT GENETIC DATA
sink("mod_auto.txt")
cat("model
{
	#standardise continuous covariates
	#grid level standardisation
	GTEMPmn <- mean(GTEMP[]);
	GTEMPsd <- sd(GTEMP[]);
	GRAINmn <- mean(GRAIN[]);
	GRAINsd <- sd(GRAIN[]);
	GNATPmn <- mean(GNATP[]);
	GNATPsd <- sd(GNATP[]);
	GPADmn <- mean(GPAD[]);
	GPADsd <- sd(GPAD[]);
	for (i in 1:(GCO+GNC))
	{
		GTEMPs[i] <- (GTEMP[i] - GTEMPmn) / GTEMPsd;
		GRAINs[i] <- (GRAIN[i] - GRAINmn) / GRAINsd;
		GNATPs[i] <- (GNATP[i] - GNATPmn) / GNATPsd;
		GPADs[i] <- (GPAD[i] - GPADmn) / GPADsd;
	}
	#site and point level standardisation
	#get means and sds
	for (i in 1:GS)
	{
		for (j in 1:NSTS[i])
		{
			SFPCv[NSTC[i] - NSTS[i] + j] <- SFPC[i,j];
			SRDENv[NSTC[i] - NSTS[i] + j] <- SRDEN[i,j];
			SELEVv[NSTC[i] - NSTS[i] + j] <- SELEV[i,j];
			SBDv[NSTC[i] - NSTS[i] + j] <- SBD[i,j];
			SCLAYv[NSTC[i] - NSTS[i] + j] <- SCLAY[i,j];
			SPAWv[NSTC[i] - NSTS[i] + j] <- SPAW[i,j];
		}
	}
	#site level
	for (i in 1:GS)
	{
		for (j in 1:NSTS[i])
		{
			SFPCs[i,j] <- (SFPC[i,j] - mean(SFPCv[])) / sd(SFPCv[]);
			SRDENs[i,j] <- (SRDEN[i,j] - mean(SRDENv[])) / sd(SRDENv[]);
			SELEVs[i,j] <- (SELEV[i,j] - mean(SELEVv[])) / sd(SELEVv[]);
			SBDs[i,j] <- (SBD[i,j] - mean(SBDv[])) / sd(SBDv[]);
			SCLAYs[i,j] <- (SCLAY[i,j] - mean(SCLAYv[])) / sd(SCLAYv[]);
			SPAWs[i,j] <- (SPAW[i,j] - mean(SPAWv[])) / sd(SPAWv[]);
		}
	}
	
	#point level
	for (i in 1:GP)
	{
		for (j in 1:NPTS[i])
		{
			PFPCs[i,j] <- (PFPC[i,j] - mean(SFPCv[])) / sd(SFPCv[]);
			PRDENs[i,j] <- (PRDEN[i,j] - mean(SRDENv[])) / sd(SRDENv[]);
			PELEVs[i,j] <- (PELEV[i,j] - mean(SELEVv[])) / sd(SELEVv[]);
			PBDs[i,j] <- (PBD[i,j] - mean(SBDv[])) / sd(SBDv[]);
			PCLAYs[i,j] <- (PCLAY[i,j] - mean(SCLAYv[])) / sd(SCLAYv[]);
			PPAWs[i,j] <- (PPAW[i,j] - mean(SPAWv[])) / sd(SPAWv[]);
		}
	}
	
	#likelihood
	
	#OCCUPANCY
	
	#loop through grids with connections to other grids
	for (i in 1:GCO)
	{
		#true occupancy of grids
		g_lp[GCOID[i]] <- gbar + gtemp * GTEMPs[GCOID[i]] + grain * GRAINs[GCOID[i]];
		g_lim[GCOID[i]] <- min(999,max(-999,g_lp[GCOID[i]])); #to prevent overflows
		logit(g[GCOID[i]]) <- g_lim[GCOID[i]];
		OCG[GCOID[i]] ~ dbern(g[GCOID[i]]);
		
		#autocorrelation component
		#loop through adjacent grids
		for (j in 1:NADJ[GCOID[i]])
		{
			Wgt[GCOID[i],j] <- sum(OCP[IDADJ[GCOID[i],j],1:NPTS[IDADJ[GCOID[i],j]]]) * (0.01 / ADJR[GCOID[i],j])
		}
		AutoC[GCOID[i]] <- ((sum(Wgt[GCOID[i],1:NADJ[GCOID[i]]])) / NADJ[GCOID[i]])
				
		#grid contribution to sites and points
		gspcov[GCOID[i]] <- sgnat * GNATPs[GCOID[i]] + sgpad * GPADs[GCOID[i]];
	}

	#loop through grids with no connections
	for (i in 1:GNC)
	{
		#true occupancy of grids
		g_lp[GNCID[i]] <- gbar + gtemp * GTEMPs[GNCID[i]] + grain * GRAINs[GNCID[i]];
		g_lim[GNCID[i]] <- min(999,max(-999,g_lp[GNCID[i]])); #to prevent overflows
		logit(g[GNCID[i]]) <- g_lim[GNCID[i]];
		OCG[GNCID[i]] ~ dbern(g[GNCID[i]]);
				
		AutoC[GNCID[i]] <- 0;
				
		#grid contribution to sites and points
		gspcov[GNCID[i]] <- sgnat * GNATPs[GNCID[i]] + sgpad * GPADs[GNCID[i]];
	}
	
	#loop through grids that have sites
	for (i in 1:GS)
	{
		#loop through sites in each grid
		for (j in 1:NSTS[i])
		{
			#true occupancy of sites
			s_lp[i,j] <- gspcov[GSID[i]] + sfpc * SFPCs[i,j] + srden * SRDENs[i,j] + selev * SELEVs[i,j] + sbd * SBDs[i,j] + sclay * SCLAYs[i,j] + spaw * SPAWs[i,j] + sveg[SVEG[i,j]] + saut * AutoC[GSID[i]];
			s_lim[i,j] <- min(999,max(-999,s_lp[i,j])); #to prevent overflows
			logit(s[i,j]) <- s_lim[i,j];
			us[i,j] <- OCG[GSID[i]] * s[i,j];
			OCS[i,j] ~ dbern(us[i,j]);
			
			#loop through repeat surveys in each site 
			for (k in 1:NREP[i,j])
			{
				#observed occupancy of sites
				r_lp[i,j,k] <- rmon[MONTH[i,j]];
				r_lim[i,j,k] <- min(999,max(-999,r_lp[i,j,k])); #to prevent overflows
				logit(r[i,j,k]) <- r_lim[i,j,k];
				ur[i,j,k] <- OCS[i,j] * r[i,j,k];
				PAS[i,j,k] ~ dbern(ur[i,j,k]);
			}
		}
	}
	
	#loop through grids that have points
	for (i in 1:GP)
	{
		#loop through points in each grid
		for (j in 1:NPTS[i])
		{
			#true occupancy of points
			p_lp[i,j] <- gspcov[GPID[i]] + sfpc * PFPCs[i,j] + srden * PRDENs[i,j] + selev * PELEVs[i,j] + sbd * PBDs[i,j] + sclay * PCLAYs[i,j] + spaw * PPAWs[i,j] + sveg[PVEG[i,j]] + saut * AutoC[GPID[i]];
			p_lim[i,j] <- min(999,max(-999,p_lp[i,j])); #to prevent overflows
			logit(p[i,j]) <- p_lim[i,j];
			up[i,j] <- OCG[GPID[i]] * p[i,j];
			OCP[i,j] ~ dbern(up[i,j]);
			predp[i,j] <- g[GPID[i]] * p[i,j];	
		}
	}
		
	#set vegetation type as a random-effect 
	for (i in 1:NVEG)
	{
		sveg[i] ~ dnorm(sbar,vtau);
	}
	#set month as a random-effect
	for (i in 1:12)
	{
		rmon[i] ~ dnorm(rbar,mtau);
	}
	
	#priors
	gbar ~ dnorm(0,0.01);
	gtemp ~ dnorm(0,0.01);
	grain ~ dnorm(0,0.01);
	sbar ~ dnorm(0,0.01);
	sgnat ~ dnorm(0,0.01);
	sgpad ~ dnorm(0,0.01);
	sfpc ~ dnorm(0,0.01);
	srden ~ dnorm(0,0.01);
	selev ~ dnorm(0,0.01);
	sbd ~ dnorm(0,0.01);
	sclay ~ dnorm(0,0.01);
	spaw ~ dnorm(0,0.01);
	saut ~ dunif(0,5);
	rbar ~ dnorm(0,0.01);
	vtau <- pow(vsig,-2);
	vsig ~ dunif(0,5);
	mtau <- pow(msig,-2);
	msig ~ dunif(0,5);
}",fill=TRUE)
sink()

#MODEL - WITH SPATIAL AUTOCORRELATION - AND WITH GENETIC DATA
sink("mod_auto_gen.txt")
cat("model
{
	#standardise continuous covariates
	#grid level standardisation
	GTEMPmn <- mean(GTEMP[]);
	GTEMPsd <- sd(GTEMP[]);
	GRAINmn <- mean(GRAIN[]);
	GRAINsd <- sd(GRAIN[]);
	GNATPmn <- mean(GNATP[]);
	GNATPsd <- sd(GNATP[]);
	GPADmn <- mean(GPAD[]);
	GPADsd <- sd(GPAD[]);
	for (i in 1:(GCO+GNC))
	{
		GTEMPs[i] <- (GTEMP[i] - GTEMPmn) / GTEMPsd;
		GRAINs[i] <- (GRAIN[i] - GRAINmn) / GRAINsd;
		GNATPs[i] <- (GNATP[i] - GNATPmn) / GNATPsd;
		GPADs[i] <- (GPAD[i] - GPADmn) / GPADsd;
	}
	#site and point level standardisation
	#get means and sds
	for (i in 1:GS)
	{
		for (j in 1:NSTS[i])
		{
			SFPCv[NSTC[i] - NSTS[i] + j] <- SFPC[i,j];
			SRDENv[NSTC[i] - NSTS[i] + j] <- SRDEN[i,j];
			SELEVv[NSTC[i] - NSTS[i] + j] <- SELEV[i,j];
			SBDv[NSTC[i] - NSTS[i] + j] <- SBD[i,j];
			SCLAYv[NSTC[i] - NSTS[i] + j] <- SCLAY[i,j];
			SPAWv[NSTC[i] - NSTS[i] + j] <- SPAW[i,j];
		}
	}
	#site level
	for (i in 1:GS)
	{
		for (j in 1:NSTS[i])
		{
			SFPCs[i,j] <- (SFPC[i,j] - mean(SFPCv[])) / sd(SFPCv[]);
			SRDENs[i,j] <- (SRDEN[i,j] - mean(SRDENv[])) / sd(SRDENv[]);
			SELEVs[i,j] <- (SELEV[i,j] - mean(SELEVv[])) / sd(SELEVv[]);
			SBDs[i,j] <- (SBD[i,j] - mean(SBDv[])) / sd(SBDv[]);
			SCLAYs[i,j] <- (SCLAY[i,j] - mean(SCLAYv[])) / sd(SCLAYv[]);
			SPAWs[i,j] <- (SPAW[i,j] - mean(SPAWv[])) / sd(SPAWv[]);
		}
	}
	
	#point level
	for (i in 1:GP)
	{
		for (j in 1:NPTS[i])
		{
			PFPCs[i,j] <- (PFPC[i,j] - mean(SFPCv[])) / sd(SFPCv[]);
			PRDENs[i,j] <- (PRDEN[i,j] - mean(SRDENv[])) / sd(SRDENv[]);
			PELEVs[i,j] <- (PELEV[i,j] - mean(SELEVv[])) / sd(SELEVv[]);
			PBDs[i,j] <- (PBD[i,j] - mean(SBDv[])) / sd(SBDv[]);
			PCLAYs[i,j] <- (PCLAY[i,j] - mean(SCLAYv[])) / sd(SCLAYv[]);
			PPAWs[i,j] <- (PPAW[i,j] - mean(SPAWv[])) / sd(SPAWv[]);
		}
	}
	
	#genetic resistance standardisation
	GRESmn <- mean(GRES[]);
	GRESsd <- sd(GRES[]);
	for (i in 1:NGEN)
	{
		GRESs[i] <- (GRES[i] - GRESmn) / GRESsd;
	}
		
	#likelihood
	
	#OCCUPANCY
	
	#loop through grids with connections to other grids
	for (i in 1:GCO)
	{
		#true occupancy of grids
		g_lp[GCOID[i]] <- gbar + gtemp * GTEMPs[GCOID[i]] + grain * GRAINs[GCOID[i]];
		g_lim[GCOID[i]] <- min(999,max(-999,g_lp[GCOID[i]])); #to prevent overflows
		logit(g[GCOID[i]]) <- g_lim[GCOID[i]];
		OCG[GCOID[i]] ~ dbern(g[GCOID[i]]);
		
		#autocorrelation component
		#loop through adjacent grids
		for (j in 1:NADJ[GCOID[i]])
		{
			Wgt[GCOID[i],j] <- sum(OCP[IDADJ[GCOID[i],j],1:NPTS[IDADJ[GCOID[i],j]]]) * (0.01 / ADJR[GCOID[i],j])
		}
		AutoC[GCOID[i]] <- ((sum(Wgt[GCOID[i],1:NADJ[GCOID[i]]])) / NADJ[GCOID[i]])
				
		#grid contribution to sites and points
		gspcov[GCOID[i]] <- sgnat * GNATPs[GCOID[i]] + sgpad * GPADs[GCOID[i]];
	}

	#loop through grids with no connections
	for (i in 1:GNC)
	{
		#true occupancy of grids
		g_lp[GNCID[i]] <- gbar + gtemp * GTEMPs[GNCID[i]] + grain * GRAINs[GNCID[i]];
		g_lim[GNCID[i]] <- min(999,max(-999,g_lp[GNCID[i]])); #to prevent overflows
		logit(g[GNCID[i]]) <- g_lim[GNCID[i]];
		OCG[GNCID[i]] ~ dbern(g[GNCID[i]]);
				
		AutoC[GNCID[i]] <- 0;
				
		#grid contribution to sites and points
		gspcov[GNCID[i]] <- sgnat * GNATPs[GNCID[i]] + sgpad * GPADs[GNCID[i]];
	}
	
	#loop through grids that have sites
	for (i in 1:GS)
	{
		#loop through sites in each grid
		for (j in 1:NSTS[i])
		{
			#true occupancy of sites
			s_lp[i,j] <- gspcov[GSID[i]] + sfpc * SFPCs[i,j] + srden * SRDENs[i,j] + selev * SELEVs[i,j] + sbd * SBDs[i,j] + sclay * SCLAYs[i,j] + spaw * SPAWs[i,j] + sveg[SVEG[i,j]] + saut * AutoC[GSID[i]];
			s_lim[i,j] <- min(999,max(-999,s_lp[i,j])); #to prevent overflows
			logit(s[i,j]) <- s_lim[i,j];
			us[i,j] <- OCG[GSID[i]] * s[i,j];
			OCS[i,j] ~ dbern(us[i,j]);
			
			#loop through repeat surveys in each site 
			for (k in 1:NREP[i,j])
			{
				#observed occupancy of sites
				r_lp[i,j,k] <- rmon[MONTH[i,j]];
				r_lim[i,j,k] <- min(999,max(-999,r_lp[i,j,k])); #to prevent overflows
				logit(r[i,j,k]) <- r_lim[i,j,k];
				ur[i,j,k] <- OCS[i,j] * r[i,j,k];
				PAS[i,j,k] ~ dbern(ur[i,j,k]);
			}
		}
	}
	
	#loop through grids that have points
	for (i in 1:GP)
	{
		#loop through points in each grid
		for (j in 1:NPTS[i])
		{
			#true occupancy of points
			p_lp[i,j] <- gspcov[GPID[i]] + sfpc * PFPCs[i,j] + srden * PRDENs[i,j] + selev * PELEVs[i,j] + sbd * PBDs[i,j] + sclay * PCLAYs[i,j] + spaw * PPAWs[i,j] + sveg[PVEG[i,j]] + saut * AutoC[GPID[i]];
			p_lim[i,j] <- min(999,max(-999,p_lp[i,j])); #to prevent overflows
			logit(p[i,j]) <- p_lim[i,j];
			up[i,j] <- OCG[GPID[i]] * p[i,j];
			OCP[i,j] ~ dbern(up[i,j]);
			predp[i,j] <- g[GPID[i]] * p[i,j];
		}
	}
		
	#set vegetation type as a random-effect 
	for (i in 1:NVEG)
	{
		sveg[i] ~ dnorm(sbar,vtau);
	}
	#set month as a random-effect
	for (i in 1:12)
	{
		rmon[i] ~ dnorm(rbar,mtau);
	}
	
	#GENETICS
	
	for (i in 1:NGEN)
	{
		gd_lp[i] <- gdbar + gdres * GRESs[i];
		GEND[i] ~ dnorm(gd_lp[i],gdtau);
	}
		
	#priors
	gbar ~ dnorm(0,0.01);
	gtemp ~ dnorm(0,0.01);
	grain ~ dnorm(0,0.01);
	sbar ~ dnorm(0,0.01);
	sgnat ~ dnorm(0,0.01);
	sgpad ~ dnorm(0,0.01);
	sfpc ~ dnorm(0,0.01);
	srden ~ dnorm(0,0.01);
	selev ~ dnorm(0,0.01);
	sbd ~ dnorm(0,0.01);
	sclay ~ dnorm(0,0.01);
	spaw ~ dnorm(0,0.01);
	saut ~ dunif(0,5);
	rbar ~ dnorm(0,0.01);
	vtau <- pow(vsig,-2);
	vsig ~ dunif(0,5);
	mtau <- pow(msig,-2);
	msig ~ dunif(0,5);
	gdbar ~ dnorm(0,0.01);
	gdres ~ dnorm(0,0.01);
	gdtau <- pow(gdsig,-2);
	gdsig ~ dunif(0,5);
}",fill=TRUE)
sink()

#run models

SpeciesList <- c("aflav")

#specify winbugs parameters
ni <- 50000
nt <- 10
nb <- 20000
nc <- 3

#loop through species and get connectivity models

ModsNoGen <- vector(mode="list")
ModsGen <- vector(mode="list")

for (i in 1:1)
{
	#add the lists for the connectivity models
	ConListNoGen <- vector(mode="list")
	ConListGen <- vector(mode="list")
	ModsNoGen[[i]] <- ConListNoGen
	ModsGen[[i]] <- ConListGen
		
	if (SpeciesList[i]=="aflav")
	{
		OCG <- OcGridAflav
		PAS <- ObSiteAflav
		NREP <- NumRepSitesAflav
		SVEG <- Sdbvg2m
		PVEG <- Pdbvg2m
		SFPC <- Sb250FPC
		SRDEN <- Sb250RDens
		SELEV <- Sb250RElev
		PFPC <- Pb250FPC
		PRDEN <- Pb250RDens
		PELEV <- Pb250RElev
				
		names(ModsNoGen)[i] <- "aflav"
		names(ModsGen)[i] <- "aflav"				
	}
	else if (SpeciesList[i]=="koala")
	{
		OCG <- OcGridKoala
		PAS <- ObSiteKoala
		NREP <- NumRepSitesKoala
		SVEG <- Sdbvg2m
		PVEG <- Pdbvg2m
		SFPC <- Sb250FPC
		SRDEN <- Sb250RDens
		SELEV <- Sb250RElev
		PFPC <- Pb250FPC
		PRDEN <- Pb250RDens
		PELEV <- Pb250RElev

		names(ModsNoGen)[i] <- "koala"
		names(ModsGen)[i] <- "koala"	
	}
	else if (SpeciesList[i]=="pbrev")
	{
		OCG <- OcGridPbrev
		PAS <- ObSitePbrev
		NREP <- NumRepSitesPbrev
		SVEG <- Sdbvg2m
		PVEG <- Pdbvg2m
		SFPC <- Sb250FPC
		SRDEN <- Sb250RDens
		SELEV <- Sb250RElev
		PFPC <- Pb250FPC
		PRDEN <- Pb250RDens
		PELEV <- Pb250RElev

		names(ModsNoGen)[i] <- "pbrev"
		names(ModsGen)[i] <- "pbrev"		
	}
	else if (SpeciesList[i]=="pnorf")
	{
		OCG <- OcGridPnorf
		PAS <- ObSitePnorf
		NREP <- NumRepSitesPnorf
		SVEG <- Sdbvg2m
		PVEG <- Pdbvg2m
		SFPC <- Sb250FPC
		SRDEN <- Sb250RDens
		SELEV <- Sb250RElev
		PFPC <- Pb250FPC
		PRDEN <- Pb250RDens
		PELEV <- Pb250RElev

		names(ModsNoGen)[i] <- "pnorf"
		names(ModsGen)[i] <- "pnorf"
	}
	else
	{
		stop("species not valid")
	}
			
	#loop through connectivity models
	for (j in 2:3)
	{
		#get connectivity model for the 2.5km grids
		ResModGrid <- as.matrix(read.table(paste("E:/Projects/seq_genetics/analysis/distribution_models/models/data/resistances2_5km_grid/model",j,".csv",sep=""),sep=",",header=T))
		ResModGrid <- ifelse(ResModGrid==-9999,NA,ResModGrid)
		ResModGrid <- get.ordered.connectivity(Grid2_5km,ResModGrid,c("id_UL","id_UM","id_UR","id_ML","id_MR","id_LL","id_LM","id_LR"),c(paste("model",j,"_UL",sep=""),paste("model",j,"_UM",sep=""),paste("model",j,"_UR",sep=""),paste("model",j,"_ML",sep=""),paste("model",j,"_MR",sep=""),paste("model",j,"_LL",sep=""),paste("model",j,"_LM",sep=""),paste("model",j,"_LR",sep="")),NumPointsGrid)
		
		#fit model without genetic data
		inits <- function(){list(gbar=runif(1,-2,2),gtemp=runif(1,-2,2),grain=runif(1,-2,2),sbar=runif(1,-2,2),sgnat=runif(1,-2,2),sgpad=runif(1,-2,2),sfpc=runif(1,-2,2),srden=runif(1,-2,2),selev=runif(1,-2,2),sbd=runif(1,-2,2),sclay=runif(1,-2,2),spaw=runif(1,-2,2),saut=runif(1,0,4),rbar=runif(1,-2,2),vsig=runif(1,0,4),msig=runif(1,0,4),OCG=as.vector(ifelse(is.finite(OCG),NA,1)),OCS=get.ocsite.start(PAS),sveg=as.vector(matrix(0,nrow=max(c(max(SVEG,na.rm=T),max(PVEG,na.rm=T))),ncol=1)),rmon=as.vector(matrix(0,nrow=12,ncol=1)))};
		writeDatafileR(inits(),"inits_auto1.txt");
		writeDatafileR(inits(),"inits_auto2.txt");
		writeDatafileR(inits(),"inits_auto3.txt");
				
		#set parameters to record
		Params <- c("gbar","gtemp","grain","sbar","sgnat","sgpad","sfpc","srden","selev","sbd","sclay","spaw","sveg","saut","rbar","vsig","msig");
		#Params <- c("predp");
						
		#set up data
		Bugs_Data <- list(length(as.vector(ResModGrid$Ind[which(ResModGrid$Num>0),1])),as.vector(ResModGrid$Ind[which(ResModGrid$Num>0),1]),length(as.vector(ResModGrid$Ind[which(ResModGrid$Num==0),1])),as.vector(ResModGrid$Ind[which(ResModGrid$Num==0),1]),length(as.vector(NumSitesGrid[,1])),as.vector(NumSitesGrid[,1]),length(as.vector(NumPointsGrid[,1])),as.vector(NumPointsGrid[,1]),as.vector(NumSitesGrid[,2]),as.vector(NumSitesGridCum[,1]),NREP,as.vector(NumPointsGrid[,2]),max(c(max(SVEG,na.rm=T),max(PVEG,na.rm=T))),as.vector(ResModGrid$Num),OCG,PAS,GMeanTemp,GMeanRain,GNatP,GPatD,ResModGrid$Ind[,2:9],ResModGrid$Res[,2:9],SFPC,SRDEN,SELEV,Sbd30,Sclay30,Spawc1m,SVEG,PFPC,PRDEN,PELEV,Pbd30,Pclay30,Ppawc1m,PVEG,SMonth)   
		names(Bugs_Data) <- list("GCO","GCOID","GNC","GNCID","GS","GSID","GP","GPID","NSTS","NSTC","NREP","NPTS","NVEG","NADJ","OCG","PAS","GTEMP","GRAIN","GNATP","GPAD","IDADJ","ADJR","SFPC","SRDEN","SELEV","SBD","SCLAY","SPAW","SVEG","PFPC","PRDEN","PELEV","PBD","PCLAY","PPAW","PVEG","MONTH")
		writeDatafileR(Bugs_Data,"data_auto.txt")
		#fit model
		ModsNoGen[[i]][[j]] <- bugs(Bugs_Data,inits,Params,"mod_auto.txt",n.chains=nc,n.thin=nt,n.iter=ni,n.burnin=nb,debug=F,bugs.directory="c:/Program Files (x86)/WinBUGS14/",working.directory=getwd())
		
		#populate names
		names(ModsNoGen[[i]])[j] <- paste("Con",j,sep="")
				
		#get Bugs format
		Bugs <- ModsNoGen[[i]][[j]]
		#get MCMC format
		MCMC <- as.mcmc.list(ModsNoGen[[i]][[j]])
		
		#write Bugs
		save(Bugs,file=paste(getwd(),"/coda_nogen_species",SpeciesList[i],"_conmod",j,".RData",sep=""))
		
		#write MCMC
		for (k in 1:3)
		{
			write.table(MCMC[[k]],paste(getwd(),"/coda_nogen_species",SpeciesList[i],"_conmod",j,"_chain",k,".csv",sep=""),sep=",",row.names=F)			
		}	
				
		#get connectivity model for the genetic data
		ResModGen <- read.table(paste("E:/Projects/seq_genetics/analysis/distribution_models/models/data/resistances_gen_data/",SpeciesList[i],"/model",j,"_resistances",".out",sep=""),sep=" ",header=T,row.names=1)	

		#add the lists for the connectivity models
		RepListGen <- vector(mode="list")
		ModsGen[[i]][[j]] <- RepListGen
		names(ModsGen[[i]])[j] <- paste("Con",j,sep="")
		
		#loop through replicates
		for (k in 1:1)
		{
			if (SpeciesList[i]=="aflav")
			{
				ResGenBoot <- get.boot.sample(GDistAflav,ResModGen)
			}
			else if (SpeciesList[i]=="koala")
			{
				ResGenBoot <- get.boot.sample(GDistKoala,ResModGen)
			}
			else if (SpeciesList[i]=="pbrev")
			{
				ResGenBoot <- get.boot.sample(GDistPbrev,ResModGen)
			}
			else if (SpeciesList[i]=="pnorf")
			{
				ResGenBoot <- get.boot.sample(GDistPnorf,ResModGen)
			}
			else
			{
				stop("species not valid")
			}
				
			#fit model with genetic data
			
			inits <- function(){list(gbar=runif(1,-2,2),gtemp=runif(1,-2,2),grain=runif(1,-2,2),sbar=runif(1,-2,2),sgnat=runif(1,-2,2),sgpad=runif(1,-2,2),sfpc=runif(1,-2,2),srden=runif(1,-2,2),selev=runif(1,-2,2),sbd=runif(1,-2,2),sclay=runif(1,-2,2),spaw=runif(1,-2,2),saut=runif(1,0,4),rbar=runif(1,-2,2),vsig=runif(1,0,4),msig=runif(1,0,4),OCG=as.vector(ifelse(is.finite(OCG),NA,1)),OCS=get.ocsite.start(PAS),gdbar=runif(1,-2,2),gdres=runif(1,-2,2),gdsig=runif(1,0,4),sveg=as.vector(matrix(0,nrow=max(c(max(SVEG,na.rm=T),max(PVEG,na.rm=T))),ncol=1)),rmon=as.vector(matrix(0,nrow=12,ncol=1)))};
			writeDatafileR(inits(),"inits_auto_gen1.txt");
			writeDatafileR(inits(),"inits_auto_gen2.txt");
			writeDatafileR(inits(),"inits_auto_gen3.txt");
						
			#set parameters to record
			Params <- c("gbar","gtemp","grain","sbar","sgnat","sgpad","sfpc","srden","selev","sbd","sclay","spaw","sveg","saut","rbar","vsig","msig","gdbar","dgres","gdsig")
			#Params <- c("predp");	
			
			#set up data
			Bugs_Data <- list(length(as.vector(ResModGrid$Ind[which(ResModGrid$Num>0),1])),as.vector(ResModGrid$Ind[which(ResModGrid$Num>0),1]),length(as.vector(ResModGrid$Ind[which(ResModGrid$Num==0),1])),as.vector(ResModGrid$Ind[which(ResModGrid$Num==0),1]),length(as.vector(NumSitesGrid[,1])),as.vector(NumSitesGrid[,1]),length(as.vector(NumPointsGrid[,1])),as.vector(NumPointsGrid[,1]),as.vector(NumSitesGrid[,2]),as.vector(NumSitesGridCum[,1]),NREP,as.vector(NumPointsGrid[,2]),max(c(max(SVEG,na.rm=T),max(PVEG,na.rm=T))),as.vector(ResModGrid$Num),OCG,PAS,GMeanTemp,GMeanRain,GNatP,GPatD,ResModGrid$Ind[,2:9],ResModGrid$Res[,2:9],SFPC,SRDEN,SELEV,Sbd30,Sclay30,Spawc1m,SVEG,PFPC,PRDEN,PELEV,Pbd30,Pclay30,Ppawc1m,PVEG,SMonth,length(as.vector(ResGenBoot[,1])),as.vector(ResGenBoot[,1]),as.vector(ResGenBoot[,2]))   
			names(Bugs_Data) <- list("GCO","GCOID","GNC","GNCID","GS","GSID","GP","GPID","NSTS","NSTC","NREP","NPTS","NVEG","NADJ","OCG","PAS","GTEMP","GRAIN","GNATP","GPAD","IDADJ","ADJR","SFPC","SRDEN","SELEV","SBD","SCLAY","SPAW","SVEG","PFPC","PRDEN","PELEV","PBD","PCLAY","PPAW","PVEG","MONTH","NGEN","GEND","GRES")
			writeDatafileR(Bugs_Data,"data_auto_gen.txt")
			#fit model
			#ModsGen[[i]][[j]][[k]] <- bugs(Bugs_Data,inits,Params,"mod_auto_gen.txt",n.chains=nc,n.thin=nt,n.iter=ni,n.burnin=nb,debug=T,bugs.directory="c:/Program Files (x86)/WinBUGS14/",working.directory=getwd())
			
			#populate names
			#names(ModsGen[[i]])[[j]][k] <- paste("Rep",k,sep="")
			
			#get Bugs format
			#Bugs <- ModsGen[[i]][[j]][[k]]
			#get MCMC format
			#MCMC <- as.mcmc.list(ModsGen[[i]][[j]][[k]])
			
			#write Bugs
			#save(Bugs,file=paste(getwd(),"/coda_gen_species",SpeciesList[i],"_conmod",j,"_bootrep",k,".RData",sep=""))
			#write MCMC
			for (l in 1:3)
			{
				#write.table(MCMC[[l]],paste(getwd(),"/coda_gen_species",SpeciesList[i],"_conmod",j,"_bootrep",k,"_chain",l,".csv",sep=""),sep=",",row.names=F)
			}	
		}
	}
}
