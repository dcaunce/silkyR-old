
ResultElement <- setRefClass(
    "ResultElement",
    
    fields = c(
        .name="character",
        .title="character",
        .index="integer",
        .visible="character",
        .options="Options",
        .updated="logical"),
    methods=list(
        initialize=function(name="", index=0, options=Options()) {
            .name <<- name
            .title <<- name
            .index <<- as.integer(index)
            .options <<- options
            .visible <<- paste0(TRUE)
            .updated <<- FALSE
            
            .options$addChangeListener(.self$.optionsChanged)
        },
        name=function() {
            .name
        },
        visible=function() {
            vis <- .options$eval(.visible, name=.name, index=.index)
            print("visible")
            print(vis)
            if (is.logical(vis))
                return(vis)
            else
                return(length(vis) > 0)
        },
        .update=function() {
            .updated <<- TRUE
        },
        .optionsChanged=function(...) {
            .updated <<- FALSE
        },
        .has=function(name) {
            paste0(".", name) %in% names(.self$getRefClass()$fields())
        },
        .setDef=function(name, value) {
            if (.has(name))
                field(paste0(".", name), value)
        },
        .setup=function(def) {
            
            for (name in names(def)) {
                value <- def[[name]]
                .setDef(name, value)
            }
        },
        asString=function() {
            ""
        },
        show=function() {
            cat(.self$asString())
        }
    ))

Results <- setRefClass(
    
    "Results",
    fields = list(
        .title="character",
        .elements="list",
        .options="Options"),
    methods=list(
        initialize=function(package=NULL, name=NULL, options=NULL) {
            
            .title <<- "no title"
            
            if (is.null(options))
                options <- Options()
            .options <<- options
            
            if (is.null(package) == FALSE && is.null(name) == FALSE) {
                
                info <- loadResultsInfo(package, name)
                
                if ( ! is.null(info$title))
                    .title <<- info$title
                
                for (def in info$results) {
                    
                    element <- methods::new(def$type, def$name, options=.options)
                    element$.setup(def)
                    .self$append(element)
                }
            }
        },
        .update=function() {
            for (element in .elements)
                element$.update()
        },
        append=function(element) {
            .elements[[element$name()]] <<- element
        },
        get=function(name) {
            .elements[[name]]
        },
        show=function() {
            cat(.self$asString())
        },
        asString=function() {
            
            pieces <- character()
            
            pieces <- c(pieces, '\n\n ', toupper(.title), '\n')
            
            for (element in .elements)
                if (element$visible())
                    pieces <- c(pieces, element$asString())
                                
            pieces <- c(pieces, '\n\n')
            
            return(paste0(pieces, collapse=''))
        },
        asProtoBuf=function() {
            
            initProtoBuf()
            
            resultsBuf <- RProtoBuf::new(silkycoms.AnalysisResponse.Results)
            
            for (element in .elements) {
                
                if (element$visible() == FALSE)
                    next()
                
                elem <- RProtoBuf::new(silkycoms.ResultsElement,
                    name=element$.name,
                    title=element$.title,
                    status=silkycoms.AnalysisStatus$ANALYSIS_COMPLETE,
                    text=element$asString())
                
                resultsBuf$add("elements", elem)
                
                #resultsBuf$add("elements", element$asProtoBuf())
            }
            
            resultsBuf
        })
)
