
Descriptives <- setRefClass(
    "Descriptives",
    contains="Analysis",
    methods=list(
        run=function() {
            callSuper()
        
            dataset <- .options$dataset()
            
            desc <- .results$get("descriptives")
            freq <- .results$get("frequencies")
            
            for (i in seq_along(.options$values()$vars)) {
                
                name   <- .options$values()$vars[[i]]
                column <- na.omit(dataset[[name]])
                
                if (is.numeric(column)) {
                    desc$setCell(i, "mean", mean(column))
                    desc$setCell(i, "median", median(column))
                    
                    mode <- as.numeric(names(table(column)[table(column)==max(table(column))]))
                    desc$setCell(i, "mode", mode[1])
                    
                    desc$setCell(i, "sum", sum(column))
                    desc$setCell(i, "sd", sd(column))
                    desc$setCell(i, "variance", var(column))
                    desc$setCell(i, "range", max(column)-min(column))
                    desc$setCell(i, "min", min(column))
                    desc$setCell(i, "max", max(column))
                    desc$setCell(i, "se", sqrt(var(column)/length(column)))
                    
                    deviation <- column-mean(column)
                    desc$setCell(i, "skew", sum(deviation^3)/(length(column)*sd(column)^3))
                    desc$setCell(i, "kurt", sum(deviation^4)/(length(column)*var(column)^2))
                    
                    desc$setCell(i, "quart1", quantile(column, c(.25)))
                    desc$setCell(i, "quart2", quantile(column, c(.5)))
                    desc$setCell(i, "quart3", quantile(column, c(.75)))
                    
                } else {
                    mode <- NULL
                    
                    desc$setCell(i, "mean", "")
                    desc$setCell(i, "median", "")
                    
                    # mode <- names(table(column)[table(column)==max(table(column))])
                    # desc$setCell(i, "mode", mode[1])
                    desc$setCell(i, "mode", "")
                    
                    desc$setCell(i, "sum", "")
                    desc$setCell(i, "sd", "")
                    desc$setCell(i, "variance", "")
                    desc$setCell(i, "range", "")
                    desc$setCell(i, "min", "")
                    desc$setCell(i, "max", "")
                    desc$setCell(i, "se", "")
                    desc$setCell(i, "skew", "")
                    desc$setCell(i, "kurt", "")
                    desc$setCell(i, "quart1", "")
                    desc$setCell(i, "quart2", "")
                    desc$setCell(i, "quart3", "")
                    
                    cumCount <- 0
                    for (k in seq_along(levels(column))) {
                        
                        count <- sum(column==levels(column)[k])
                        cumCount <- cumCount + count
                        
                        freq$get(name)$setCell(k, "counts", count)
                        freq$get(name)$setCell(k, "percentage", 100*count/length(column))
                        freq$get(name)$setCell(k, "cumpercentage", 100*cumCount/length(column))
                    }
                }
                
                if (length(mode) > 1)
                    desc$addFootnote(i, "mode", "More than one mode exists, only the first is reported")
                
            }
        }
    )
)

