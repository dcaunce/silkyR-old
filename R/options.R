
Options <- setRefClass(
    "Options",
    fields=list(
        .options="environment",
        .listeners="list"),
    methods=list(
        initialize=function(json=NULL, ...) {
            
            if ( ! is.null(json)) {
                opts <- rjson::fromJSON(json)
                for (name in names(opts))
                    .options[[name]] <<- opts[[name]]
            }
            
            opts <- list(...)
            for (name in names(opts))
                .options[[name]] <<- opts[[name]]
            
            .options[["levels"]] <<- .self$levels
        },
        dataset=function() {
            .options[["dataset"]]
        },
        values=function() {
            .options
        },
        eval=function(value, ...) {
            
            if (class(value) == "character") {
                
                vars <- list(...)
                for (name in names(vars))
                    .options[[name]] <<- vars[[name]]
                
                nch <- nchar(value)
                if ( ! is.na(suppressWarnings(as.numeric(value))))
                    value <- as.numeric(value)
                else if (nch > 0 && substring(value, 1, 1) == "(" && substring(value, nch) == ")")
                    value <- .eval(text=value)
                else
                    value <- format(value, ...)
                
                if (length(names(vars)) > 0)
                    rm(list=names(vars), envir=.options)
            }
            
            value
        },
        .eval=function(text) {
            
            value <- try(base::eval(parse(text=text), envir=.options), silent=TRUE)
            
            if (inherits(value, "try-error")) {
                reason <- extractErrorMessage(value)
                stop(format("Could not evaluate '{text}'\n    {reason}", text=text, reason=reason), call.=FALSE)
            }
            value
        },
        set=function(...) {
            
            values <- list(...)
            for (name in names(values))
                .options[[name]] <<- values[[name]]
            
            for (listener in .listeners)
                listener(names(values))
        },
        levels=function(x) {
            str <- substitute(x)
            expr <- parse(text=paste0("base::levels(dataset[[", str, "]])"))
            v = eval.parent(expr)
            v
        },
        addChangeListener=function(listener) {
            .listeners[[length(.listeners)+1]] <<- listener
        })
)
