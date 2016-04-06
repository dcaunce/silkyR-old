
ANOVA <- setRefClass(
    "ANOVA",
    contains="Analysis",
    methods=list(
        run=function() {
            callSuper()
            
            dataset <- .options$dataset()
            
            anovaRes <- .results$get("anova")
            
            dependentVar <- .options$values()$dependentVar
            factors <- .options$values()$factors
            modelTerms <- .options$values()$modelTerms
            ssMethod <- .options$values()$sumSquares
            
            modelTermsCol <- sapply(modelTerms, paste, collapse=":")
            fLHS <- paste(modelTermsCol, collapse="+")
            f <- as.formula(paste(dependentVar,"~",fLHS))
            
            ## Specify Contrasts
            base::options(contrasts=c("contr.sum", "contr.poly"))
            #add more contrast options
            
            ## Create model
            model <- aov(f, dataset)
            
            ## Apply sums of squares method
            if (ssMethod == "type1") {
                res <- anova(model)
            } else if (ssMethod == "type2") {
                res <- car::Anova(model, type=2)
            } else if (ssMethod == "type3") {
                res <- car::Anova(model, type=3, singular.ok=TRUE)
            }
            
            ## Write to tables
            for (i in seq_along(modelTermsCol)) {
                
                rowName <- modelTermsCol[i]
                anovaRes$setCell(i, "sumSquares", res[rowName,"Sum Sq"])
                anovaRes$setCell(i, "df", res[rowName,"Df"])
                anovaRes$setCell(i, "meanSquares", res[rowName,"Sum Sq"]/res[rowName,"Df"])
                anovaRes$setCell(i, "f", res[rowName,"F value"])
                anovaRes$setCell(i, "p", res[rowName,"Pr(>F)"])
            }
            ## Add Residuals to table
            anovaRes$setCell(length(modelTermsCol)+1, "sumSquares", res["Residuals","Sum Sq"])
            anovaRes$setCell(length(modelTermsCol)+1, "df", res["Residuals","Df"])
            anovaRes$setCell(length(modelTermsCol)+1, "meanSquares", res["Residuals","Sum Sq"]/res["Residuals","Df"])
            anovaRes$setCell(length(modelTermsCol)+1, "f", "")
            anovaRes$setCell(length(modelTermsCol)+1, "p", "")
        }
    )
)

