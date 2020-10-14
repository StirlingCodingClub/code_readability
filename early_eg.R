library(SIN)
library(corpcor)

rm(list=ls())

SyconiaSim <- function(syconia,tmax=100,N=10,Ptime=0,IS1time=0,IS2time=0,Pxmin=1,Pxmax=10,Pymin=1,Pymax=10,IS1xmin=1,IS1xmax=10,IS1ymin=1,IS1ymax=10,IS2xmin=1,IS2xmax=10,IS2ymin=1,IS2ymax=10){
sycs <- NULL	
for(j in 1:syconia){
	pollchoice <- matrix(data=c(round(runif(n=1,min=Pxmin,max=Pxmax)),round(runif(n=1,min=Pymin,max=Pymax))),ncol=2) #choice for pollinator oviposition
	IS1choice <- matrix(data=c(round(runif(n=1,min=IS1xmin,max=IS1xmax)),round(runif(n=1,min=IS1ymin,max=IS1ymax))),ncol=2) #choice for IdarnesS1 oviposition
	IS2choice <- matrix(c(round(runif(n=1,min=IS2xmin,max=IS2xmax)),round(runif(n=1,min=IS2ymin,max=IS2ymax))),ncol=2) #choice for IdarnesS2 oviposition
	plot(pollchoice,pch=20,xlim=c(1,N),ylim=c(1,N),yaxt="n",xaxt="n",ylab="",xlab="",type="n",main="Syconia") #Make an initial plot
	m <- function(x) x/max(x) #Makes a relativization function
	for(i in 1:tmax){ #For each time step in one to the amount of time wasps have to oviposit. NOTE: if both try the same, they share
		###########Pollinators
		if(i >= Ptime){ #If the time step is greater than or equal to the minimum start time for Pollinators
		newpoll <- matrix(data=c(round(runif(n=1,min=Pxmin,max=Pxmax)),round(runif(n=1,min=Pymin,max=Pymax))),ncol=2) #A new selection for pollinators
			used <- rbind(IS1choice,IS2choice,pollchoice) #Bind together used oviposition sites
			usedcount <- rep(0,length(used)) #Make a vector of zeroes the length of the filled sites
			for(jj in 1:length(used[,1])){ #for every element in the used sites
				if(sum(newpoll[1,]==used[jj,])>1){ #if the new oviposition site is the same as a used
				usedcount[jj] <- 1 #add one to the point in the used count vector
				}	
			}
		if(sum(usedcount)<1){ #if there are no ones in the used count vector
		pollchoice <- rbind(pollchoice,newpoll) #Add the new site
		}
		points(pollchoice,pch=20,cex=4,col="green") #Put a plot of the new point down.
		}
		###########IdarnesS1
		if(i >= IS1time){ #If the time step is greater than or equal to the minimum start time for IS1s
		newIS1 <- matrix(data=c(round(runif(n=1,min=IS1xmin,max=IS1xmax)),round(runif(n=1,min=IS1ymin,max=IS1ymax))),ncol=2) #A new selection for Idarnes1
			used <- rbind(IS1choice,IS2choice,pollchoice) #Bind together used oviposition sites
			usedcount <- rep(0,length(used)) #Make a vector of zeroes the length of the filled sites
			for(jj in 1:length(used[,1])){ #for every element in the used sites
				if(sum(newIS1[1,]==used[jj,])>1){ #if the new oviposition site is the same as a used
				usedcount[jj] <- 1 #add one to the point in the used count vector
				}	
			}
		if(sum(usedcount)<1){ #if there are no ones in the used count vector
		IS1choice <- rbind(IS1choice,newIS1) #Add the new site for IS1
		}
		points(IS1choice,pch=20,cex=4,col="blue") #Put a plot of the new point down
		}
		###########IdarnesS2
		if(i >= IS2time){ #If the time step is greater than or equal to the minimum start time for IS1s
		newIS2 <- matrix(data=c(round(runif(n=1,min=IS2xmin,max=IS2xmax)),round(runif(n=1,min=IS2ymin,max=IS2ymax))),ncol=2) #A new selection for Idarnes2
			used <- rbind(IS1choice,IS2choice,pollchoice) #Bind together used oviposition sites
			usedcount <- rep(0,length(used)) #Make a vector of zeroes the length of the filled sites
			for(jj in 1:length(used[,1])){ #for every element in the used sites
				if(sum(newIS2[1,]==used[jj,])>1){ #if the new oviposition site is the same as a used
				usedcount[jj] <- 1 #add one to the point in the used count vector
				}	
			}
		if(sum(usedcount)<1){ #if there are no ones in the used count vector
		IS2choice <- rbind(IS2choice,newIS2) #Add the new site for IS1
		}
		points(IS2choice,pch=20,cex=4,col="red") #Put a plot of the new point down
		}
		##############
	}
	newsyc <- matrix(data=c(length(pollchoice[,1]),length(IS1choice[,1]),length(IS2choice[,1])),ncol=3)
	sycs <- rbind(sycs,newsyc) #Bind all of the abundances from different syconia together
}
cmatrix <- cov(sycs) #Get the covariance matrix of abundances across syconia
UG <- sinUG(cmatrix,syconia,holm=TRUE) #Run Drton & Perlman to get the simulateous p-values for partial correlations
rownames(UG) <- c("Polls","IS1","IS2"); colnames(UG) <- c("Polls","IS1","IS2")
cormat <- cov2cor(cmatrix) #Get the correlation matrix
pcormat <- cor2pcor(cormat) #Get the partial correlation matrix
rownames(pcormat) <- c("Polls","IS1","IS2"); colnames(pcormat) <- c("Polls","IS1","IS2")
apply(X=sycs,MARGIN=2,FUN=m) -> relsycs #Relativizes the abundances by the maximum abundance of a species
pregPoll <- as.matrix(summary(lm(relsycs[,1]~relsycs[,2]+relsycs[,3]))$coefficients[2:3,1]) #Partial regression on Polls
pregIS1  <- as.matrix(summary(lm(relsycs[,2]~relsycs[,1]+relsycs[,3]))$coefficients[2:3,1]) #Partial regression on IS1
pregIS2  <- as.matrix(summary(lm(relsycs[,3]~relsycs[,1]+relsycs[,2]))$coefficients[2:3,1]) #Partial regression on IS2
#The below command places the partial regressions in matrix form.
compmat  <- t(matrix(data=c(0,pregPoll[1,1],pregPoll[2,1],pregIS1[1,1],0,pregIS1[2,1],pregIS2[1,1],pregIS2[2,1],0),nrow=3,byrow=TRUE))
rownames(compmat) <- c("Polls on ->","IS1 on ->","IS2 on ->")
colnames(compmat) <- c("Polls","IS1","IS2")
abs(compmat) -> acomp #Gets the absolute values (probably not really necessary, but just in case, as we're looking at interaction strength)
fullstrength <- -1*as.matrix(apply(X=acomp,MARGIN=1,FUN=sum))
whowins <- rank(fullstrength)
topcomps <- as.matrix(whowins)
rownames(topcomps) <- c("Polls","IS1","IS2"); colnames(topcomps) <- c("Rank")
#Below returns abundances, partial correlations, significance values, partial regressions, and the rank of competitors
return(list(Syconia=sycs,pcormat=pcormat,UGmat=UG,Competitive.EFFects=compmat,Competitor.Rank=topcomps))
}





