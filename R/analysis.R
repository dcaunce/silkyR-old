
Analysis <- setRefClass(
    "Analysis",
    fields=list(
        .id="numeric",
        .name="character",
        .package="character",
        .options="Options",
        .results="Results"),
    methods=list(
        initialize=function(id=0, options=NULL) {

            selfClass <- class(.self)
            name <- selfClass
            attributes(name) <- NULL
            
            .name    <<- name
            .package <<- attr(selfClass, 'package')
            
            .id <<- id
            
            if (is.null(options))
                options <- Options()
            .options <<- options
            
            .results <<- Results(.package, .name, .options)
            
            .options$addChangeListener(.self$.optionsChangedHandler)
        },
        check=function() {
            silkyR::check(.package, .name, .options)
        },
        init=function() {
            check()
        },
        run=function() {
            check()
        },
        dataset=function() {
            .dataset
        },
        options=function() {
            .options
        },
        results=function() {
            .results
        },
        show=function() {
            .results$show()
        },
        .optionsChangedHandler=function(optionNames) {
            check()
            .results$.update()
        },
        asProtoBuf=function() {
            response <- RProtoBuf::new(silkycoms.AnalysisResponse)
            response$id = .id
            response$results <- .results$asProtoBuf();
            response$status <- silkycoms.AnalysisStatus$ANALYSIS_COMPLETE
            response
        })
)
