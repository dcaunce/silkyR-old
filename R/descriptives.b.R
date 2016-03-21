
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
                column <- dataset[[name]]
                
                if (is.numeric(column)) {
                    desc$setCell(i, "mean", mean(column))
                    desc$setCell(i, "median", median(column))
                    
                } else {
                    desc$setCell(i, "mean", "")
                    desc$setCell(i, "median", "")
                    
                    freq$get(name)$setCell(1, "counts", "moose")
                }
            }
        }
    )
)

