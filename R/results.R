
ResultElement <- setRefClass(
    "ResultElement",
    
    fields = c(
        .name="character",
        .title="character",
        .index="integer",
        .visible="character",
        .options="Options"),
    methods=list(
        initialize=function(name="", index=0, options=Options()) {
            .name <<- name
            .title <<- name
            .index <<- as.integer(index)
            .options <<- options
            .visible <<- paste0(TRUE)
            
            .options$addChangeListener(.self$.optionsChanged)
        },
        name=function() {
            .name
        },
        visible=function() {
            vis <- .options$eval(.visible, name=.name, index=.index)
            if (is.logical(vis))
                return(vis)
            else
                return( ! is.null(vis))
        },
        .update=function() {
            
        },
        .optionsChanged=function(...) {
            .update()
        },
        .has=function(name) {
            paste0(".", name) %in% names(.self$getRefClass()$fields())
        },
        .setDef=function(name, value) {
            if (.has(name))
                field(paste0(".", name), value)
        },
        .init=function(def) {
            
            for (name in names(def)) {
                value <- def[[name]]
                .setDef(name, value)
            }
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
                    element$.init(def)
                    .self$append(element)
                }
            }
        },
        .update=function() {
            for (element in .elements)
                element$.update()
        },
        show=function() {

            cat("\n\n ")
            cat(toupper(.title))
            cat("\n")

            for (element in .elements)
                element$show()

            cat("\n\n")
        },
        append=function(element) {
            .elements[[element$name()]] <<- element
        },
        get=function(name) {
            .elements[[name]]
        })
)
