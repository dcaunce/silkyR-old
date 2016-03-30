
TTestOneS <- setRefClass(
    "TTestOneS",
    contains="Analysis",
    methods=list(
        run=function() {
            callSuper()
            
            dataset <- .options$dataset()
            naHandling <- .options$values()$miss
            
            ttest <- .results$get("ttest")
            desc <- .results$get("descriptives")
            normality <- .results$get("normality")
            
            variables <- .options$values()$vars
            
            wantsStudents <- .options$values()$student
            wantsMannWhitney <- .options$values()$mann
            
            testValue <- .options$values()$testValue
            cl <- .options$values()$ciWidth/100
            
            ## Listwise NA cleanup
            if (naHandling == "listwise")
                dataset.clean <- dataset[complete.cases(dataset[variables]),]
            
            ## Hypothesis options checking
            if (.options$values()$hypothesis == "oneGreater") {
                
                altHypothesis <- "greater"
                # Footnote message TBC
                
            } else if (.options$values()$hypothesis == "twoGreater") {
                
                altHypothesis <- "less"
                # Footnote message TBC
                
            } else 
                altHypothesis <- "two.sided"
            
            for (i in seq_along(variables)) {
                
                name <- variables[[i]]
                ## NA handling analysis by analysis
                if (naHandling == "perAnalysis")
                    dataset.clean <- dataset[complete.cases(dataset[name]),]
                
                column <- dataset.clean[[name]]
                
                n <- length(column)
                
                if (n == 0)
                    reject("Variable '{a}' only contains missing values", code="na_variable", a=name)
                
                m <- mean(column)
                stdDev <- sd(column)
                se <- stdDev/sqrt(n)
                d <- (m-testValue)/stdDev #Cohen's d
                
                ## Normality test table
                res <- NULL
                if (n < 3) {
                    normality$addFootnote(i, "name", "Too few observations (N < 3) to compute statistic")
                    res$statistic <- ""
                    res$p.value <- ""
                }
                else if (n > 5000) {
                    normality$addFootnote(i, "name", "Too many observations (N > 5000) to compute statistic")
                    res$statistic <- ""
                    res$p.value <- ""
                } else if (column[n]-column[1L] == 0) {
                    reject("Variable '{a}' has essentially constant values", code="constant_variable", a=name)
                }
                else {
                    res <- shapiro.test(column - testValue)
                }
                
                normality$setCell(i, "w", res$statistic)
                normality$setCell(i, "p", res$p.value)
                
                res<- NULL
                if (wantsStudents) {
                    
                    res <- t.test(column, mu=testValue, paired=FALSE, conf.level=cl, alternative=altHypothesis)
                    
                    ttest$setCell(i, "studT", res$statistic)
                    ttest$setCell(i, "studDf", res$parameter)
                    ttest$setCell(i, "studP", res$p.value)
                    ttest$setCell(i, "studMeanDiff", res$estimate - testValue)
                    ttest$setCell(i, "studEffectSize", d)
                    ttest$setCell(i, "studLowerCI", res$conf.int[1])
                    ttest$setCell(i, "studUpperCI", res$conf.int[2])
                    
                }
                
                res<- NULL
                if (wantsMannWhitney) {
                    
                    res <- wilcox.test(column, mu=testValue, alternative=altHypothesis, paired=FALSE, conf.int=TRUE, conf.level=cl)
                    
                    ttest$setCell(i, "mannV", res$statistic)
                    ttest$setCell(i, "mannP", res$p.value)
                    ttest$setCell(i, "mannMeanDiff", res$estimate - testValue)
                    ttest$setCell(i, "mannEffectSize", d)
                    ttest$setCell(i, "mannLowerCI", res$conf.int[1])
                    ttest$setCell(i, "mannUpperCI", res$conf.int[2])
                    
                }
                
                ## Descriptives table
                desc$setCell(i, "num", n)
                desc$setCell(i, "mean", m)
                desc$setCell(i, "sd", stdDev)
                desc$setCell(i, "se", se)
                
            }
        }
    )
)

