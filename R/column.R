
Cell <- setRefClass(
    "Cell",
    fields=list(
        value="ANY",
        sups="integer"),
    methods=list(
        initialize=function(v=NA) {
            value <<- v
            sups <<- integer()
        },
        setValue=function(v) {
            value <<- v
        },
        addSup=function(sup) {
            sups <<- c(sups, as.integer(sup))
        },
        asProtoBuf=function() {
            initProtoBuf()
            cell <- methods::new(silky.Cell)
            
            vc <- class(value)
            
            if (vc == "integer") {
                if (.value == -2147483647)
                    cell$o <- silky.Cell.Other.MISSING
                else
                    cell$i <- value
            } else if (vc == "numeric")
                cell$f <- value
            else if (vc == "")
                cell$s <- value
            else
                cell$o <- silky.Cell.Other$NAN
            
            cell$footnotes <- sups
        })
)

Column <- setRefClass(
    "Column",
    fields = list(
        .name = "character",
        .title = "character",
        .format = "character",
        .type = "numeric",
        .cells = "list",
        .width = "numeric",
        .measures="list",
        .measured="logical",
        .contentExpr="character",
        .visibleExpr="character",
        .options="Options"),
    methods = list(
        initialize=function(name, title=name, content=".", visible=TRUE, options=Options()) {
            .name <<- name
            .title <<- title
            
            .measured <<- FALSE
            .cells <<- list()
            
            .visibleExpr <<- paste0(visible)
            .contentExpr <<- content
            
            .options <<- options
        },
        .addCell=function(value=NA, ...) {
            
            if (is.na(value))
                value <- .options$eval(.contentExpr, ...)
            
            if (inherits(value, "Cell"))
                cell <- value
            else
                cell <- Cell(value)
            
            .cells[[length(.cells)+1]] <<- cell
            .measured <<- FALSE
        },
        .setCell=function(row, value) {
            if (row > length(.cells))
                stop(format("Row '{}' does not exist in the table", row), call.=FALSE)
            .cells[[row]]$setValue(value)
            .measured <<- FALSE
        },
        .getCell=function(row) {
            if (row > length(.cells))
                stop(format("Row '{}' does not exist in the table", row), call.=FALSE)
            .cells[[row]]
        },
        .clear=function() {
            .cells <<- list()
            .measured <<- FALSE
        },
        .addSup=function(row, sup) {
            .cells[[row]]$addSup(sup)
            .measured <<- FALSE
        },
        .measure=function() {
            titleWidth <- nchar(.title)
            .measures <<- silkyMeasureElements(.cells)
            .width <<- max(.measures$width, titleWidth)
            .measured <<- TRUE
        },
        width=function() {
            if ( ! .measured)
                .measure()
            .width
        },
        visible=function() {
            .options$eval(.visibleExpr)
        },
        printTitle=function() {
            w   <- nchar(.title)
            pad <- spaces(max(0, width() - w))
            cat(paste0(.title, pad))
        },
        printCell=function(i) {
            if ( ! .measured)
                .measure()
            
            cat(silkyFormatElement(.cells[[i]], w=.width, dp=.measures$dp, sf=.measures$sf, expw=.measures$expwidth, supw=.measures$supwidth))
        },
        asProtoBuf=function() {
            initProtoBuf()
            
            column <- methods::new(silky.Column,
                name=.name,
                title=.title,
                format=.format)
            
            for (cell in .cells)
                column$add("cells", cell$asProtoBuf())
            
            column
        }
    )
)
