
ANCOVABayesian <- setRefClass(
    "ANCOVABayesian",
    contains="Analysis",
    methods=list(
        run=function() {
            callSuper()
            
            dataset <- .options$dataset()
            
            dependentVar <- .options$values()$dependentVar
            factors <- .options$values()$factors
            
            
            n <- length(dataset[dependentVar])
            
            factorList <- dataset[factors]
            factorNLevels <- list()
            for (i in seq_along(factorList)) {
                
                factorNLevels[[i]] <- length(levels(factorList[[i]]))
                names(factorNLevels)[i] <- paste(factors[i],"NLevel",sep="")
                factorList[[i]] <- as.numeric(factorList[[i]])
            }
            
            
            ## Priors
            depVarMean <- mean(dataset[[dependentVar]])
            depVarSD <- sd(dataset[[dependentVar]])
            
            ## Create data list
            dataList <- list(
                dependentVar = dataset[[dependentVar]],
                n = n,
                depVarMean = depVarMean,
                depVarSD = depVarSD
                )
            names(dataList)[1] <- dependentVar
            names(dataList)[3] <- paste(dependentVar,"Mean",sep="")
            names(dataList)[4] <- paste(dependentVar,"SD",sep="")
            
            dataList <- append(dataList,append(factorList,factorNLevels))
            
            ####################################
            ## Generalised model construction ##
            ####################################
            noiseTerm <- "{dependentVar}[i] ~ dnorm( mu[i] , 1/{depVarSigma}^2 )"
            noiseSigma <- "{depVarSigma} ~ dunif( {depVarSD}/100 , {depVarSD}*10 )"
            modela0 <- "a0 ~ dnorm( {depVarMean} , 1/({depVarSD}*5)^2 )"
            
            factorCoeff <- list()
            for (k in seq_along(factors))
                factorCoeff[[k]] <- c(paste0("a",k),0)
            
            linearModelTerms <- as.matrix(expand.grid(factorCoeff))
            
            linearModelPreds <- linearModelTerms
            for (k in seq_along(factors)) {
                linearModelPreds <- gsub(paste0("a",k),paste0("{factor",k,"}[i]"),linearModelPreds,fixed=TRUE)
            }
            
            linearModelPreds <- apply(linearModelPreds,1,paste,collapse=",")
            linearModelPreds <- gsub(",0|0,","",linearModelPreds)
            linearModelPreds <- gsub("0","",linearModelPreds,fixed=TRUE)
            linearModelPreds <- linearModelPreds[linearModelPreds != ""]
            
            muModelTerms <- apply(linearModelTerms,1,paste,collapse="")
            muModelTerms <- gsub("0","", muModelTerms,fixed=TRUE)
            muModelTerms <- muModelTerms[muModelTerms != ""]
            
            modelMu <- paste(muModelTerms,"[",linearModelPreds,"]",sep="")
            modelMu <- paste(modelMu,collapse=" + ")
            modelMu <- paste("mu[i] <- a0 +",modelMu)
            print(modelMu)
            
            linearModelPos <- linearModelTerms[-dim(linearModelTerms)[1],] != "0"
            
            predictorModel <- NULL
            termNumbers <- NULL
            for (k in seq_along(muModelTerms)) {
                termNumbers <- as.numeric(which(linearModelPos[k,]))
                forLoopID <- paste0("j",termNumbers)
                innerFunction <- paste0(muModelTerms[k],"[",paste(forLoopID,collapse=","),"] ~ dnorm(0.0, 1/",muModelTerms[k],"SD^2) ")
                
                for (l in termNumbers) {
                    predictorModel[k] <- paste0("for (j",l," in 1:{Nfactor",l,"lvl}) {",innerFunction,"}")
                    innerFunction <- predictorModel[k]
                }
            predictorModel[k] <- paste0(predictorModel[k]," \n ",muModelTerms[k],"SD <- 100")
            }
            print(predictorModel)
            ## Construct model
            modelstring = "
                model {
                    for ( i in 1:n ) {
                        {dependentVar}[i] ~ dnorm( mu[i] , 1/{depVarSigma}^2 )
                        mu[i] <- a0 + a1[{factor1}[i]] + a2[{factor2}[i]] + a1a2[{factor1}[i],{factor2}[i]]
                    }
                    {depVarSigma} ~ dunif( {depVarSD}/100 , {depVarSD}*10 )
                    a0 ~ dnorm( {depVarMean} , 1/({depVarSD}*5)^2 ) 
                    
                    ## predictor distributions
                    for ( j1 in 1:{Nfactor1lvl} ) { {factor1}[j1] ~ dnorm( 0.0 , 1/a1SD^2 ) }
                    a1SD <- 100
                    for ( j2 in 1:{Nfactor2lvl} ) { {factor2}[j2] ~ dnorm( 0.0 , 1/a2SD^2 ) }
                    a2SD <- 100
                    for ( j1 in 1:{Nfactor1lvl} ) { for ( j2 in 1:{Nfactor2lvl} ) { a1a2[j1,j2] ~ dnorm( 0.0 , 1/a1a2SD^2 ) } }
                    a1a2SD <- 100
                    # Convert a0,a1[],a2[],a1a2[,] to sum-to-zero b0,b1[],b2[],b1b2[,] :
                    for ( j1 in 1:{Nfactor1lvl} ) { for ( j2 in 1:{Nfactor2lvl} ) {
                        m[j1,j2] <- a0 + a1[j1] + a2[j2] + a1a2[j1,j2] # cell means 
                    } }
                    b0 <- mean( m[1:{Nfactor1lvl},1:{Nfactor2lvl}] )
                    for ( j1 in 1:{Nfactor1lvl} ) { b1[j1] <- mean( m[j1,1:Nx2Lvl] ) - b0 }
                    for ( j2 in 1:{Nfactor2lvl} ) { b2[j2] <- mean( m[1:Nx1Lvl,j2] ) - b0 }
                    for ( j1 in 1:{Nfactor1lvl} ) { for ( j2 in 1:{Nfactor2lvl} ) {
                        b1b2[j1,j2] <- m[j1,j2] - ( b0 + b1[j1] + b2[j2] )  
                    } }
                }"
        }
    )
)
