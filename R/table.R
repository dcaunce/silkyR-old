
Footnotes <- setRefClass(
    "Footnotes",
    fields=list(
        .notes="list"),
    methods=list(
        initialize=function() {
            .notes <<- list()
        },
        clear=function() {
            .notes <<- list()
        },
        addNote=function(message) {
            
            # adds a footnote, returns the index (0-indexed)
            
            for (i in seq_along(.notes)) {
                if (message == .notes[[i]])
                    return(i-1)
            }
            
            .notes[[length(.notes)+1]] <<- message
            return(length(.notes)-1)
        })
)

Table <- setRefClass(
    "Table",
    contains="ResultElement",
    fields=list(
        .name="character",
        .columns="list",
        .rowCount="numeric",
        .rowNames="list",
        .rowsExpr="character",
        .rowsValue="ANY",
        .margin="numeric",
        .padding="numeric",
        .marstr="character",
        .padstr="character",
        .footnotes="Footnotes",
        .swapRowsColumns="logical"),
    methods=list(
        initialize=function(name="", index=0, options=Options(), swapRowsColumns=FALSE) {
            
            callSuper(name=name, options=options)
            
            .index <<- as.integer(index)
            .swapRowsColumns <<- swapRowsColumns
            .columns <<- list()
            .rowCount <<- 0
            .rowsExpr <<- "1"
            .rowNames <<- list()
            .margin <<- 1
            .marstr <<- spaces(.margin)
            .padding <<- 2
            .padstr <<- spaces(.padding)
            .footnotes <<- Footnotes()
        },
        title=function(value) {
            .options$eval(.title)
        },
        .setDef=function(name, value) {

            if (name == "title")
                setTitle(value)
            else if (name == "columns")
                .setColumnsDef(value)
            else if (name == "rows")
                .setRowsDef(value)
            else
                callSuper(name, value)
        },
        setTitle=function(value) {
            .title <<- paste0(.options$eval(value, name=.name, index=.index))
        },
        .setRowsDef=function(value) {
            .rowsExpr <<- paste0(value)
            .update()
        },
        .setColumnsDef=function(columnDefs) {
            
            for (columnDef in columnDefs) {
                
                if (is.null(columnDef$title))
                    columnDef$title <- columnDef$name
                if (is.null(columnDef$content))
                    columnDef$content <- "."
                if (is.null(columnDef$visible))
                    columnDef$visible <- TRUE
                
                addColumn(columnDef$name, columnDef$title, columnDef$content, columnDef$visible)
            }
        },
        .update=function() {
            
            error <- NULL

            rowsValue <- try(.options$eval(.rowsExpr, name=.name, index=.index), silent=TRUE)
            
            if (inherits(rowsValue, "try-error")) {
                error <- rowsValue
                rowsValue <- 0
            }
            
            if (identical(rowsValue, .rowsValue))
                return()
            
            .rowsValue <<- rowsValue
            
            oldNames <- .rowNames
            oldRows <- getRows()
            
            if (is.numeric(.rowsValue) && .rowsValue > 0) {
                newNames <- paste(1:.rowsValue)
            } else if (is.character(.rowsValue)) {
                newNames <- .rowsValue
            } else {
                newNames <- character()
            }
            
            clearRows()
            
            for (i in seq_along(newNames)) {
                
                newName <- newNames[[i]]
                index <- which(oldNames == newName)
                
                if (length(index) > 0) {
                    
                    newRow <- oldRows[[ index[1] ]]
                    addRow(newName, newRow)
                    
                } else {
                    
                    addRow(newName)
                }
            }
            
            if ( ! is.null(error))
                rethrow(error)
        },
        clearRows=function() {
            .rowNames <<- list()
            for (column in .columns)
                column$.clear()
            .rowCount <<- 0
            .footnotes$clear()
        },
        addColumn=function(name, title=name, content=".", visible=TRUE) {
            
            column <- Column(name=name, title=title, content=content, visible, options=.options)
            i <- 1
            
            while (i <= .rowCount) {
                rowName <- .rowNames[[i]]
                column$.addCell(name=rowName, index=i)
                i <- i + 1
            }
            
            .columns[[name]] <<- column
        },
        addRow=function(name=NULL, values=NULL) {
            
            .rowNames[length(.rowNames)+1] <<- list(name)
            .rowCount <<- .rowCount + 1
            
            for (column in .columns) {
                if (column$.name %in% names(values))
                    column$.addCell(values[[column$.name]], name=name, index=.rowCount)
                else
                    column$.addCell(name=name, index=.rowCount)
            }
        },
        rowCount=function() {
            .rowCount
        },
        setCell=function(rowNo, col, value) {
            .columns[[col]]$.setCell(rowNo, value)
        },
        getCell=function(rowNo, col) {
            column <- .columns[[col]]
            if (is.null(column))
                stop(format("Column '{}' does not exist in the table", col), call.=FALSE)
            column$.getCell(rowNo)
        },
        getRows=function() {
            
            rows <- list()
            
            for (i in seq_len(.rowCount))
                rows[[i]] <- getRow(i)
            
            rows
        },
        getRow=function(row) {
            
            v <- list()
            
            if (is.character(row)) {
                rowNo <- match(row, .rowNames)
                if (is.na(index))
                    stop(format("Row '{}' does not exist in the table", row), call.=FALSE)
            } else if (is.numeric(row)) {
                rowNo <- row
            } else {
                stop(format("Table$getRow() expects a row name or a row number (character or numeric)", row), call.=FALSE)
            }
            
            if (rowNo > .rowCount)
                stop(format("Row '{}' does not exist in the table", row), call.=FALSE)
            
            for (column in .columns)
                v[[column$.name]] <- column$.getCell(rowNo)
            
            v
        },
        addFootnote=function(rowNo, colNo, note) {
            index <- .footnotes$addNote(note)
            .columns[[colNo]]$.addSup(rowNo, index)
        },
        width=function() {
            
            if ( ! .swapRowsColumns) {
                
                w <- 0
                for (column in .columns) {
                    if (column$visible())
                        w <- w + .padding + column$width() + .padding
                }
                
            } else {
                
                w <- .padding + .widthWidestHeader() + .padding
                for (i in seq_len(.rowCount))
                    w <- w + .padding + .widthWidestCellInRow(i)$width + .padding
            }
            
            max(w, nchar(.title))
        },
        .widthWidestCellInRow=function(row) {
            
            maxWidthWOSup <- 0
            maxSupInRow <- 0  # widest superscripts
            
            for (column in .columns) {
                if (column$visible()) {
                    cell <- column$.getCell(row)
                    measurements <- silkyMeasureElements(list(cell))
                    widthWOSup <- measurements$width - measurements$supwidth
                    maxWidthWOSup <- max(maxWidthWOSup, widthWOSup)
                    maxSupInRow <- max(maxSupInRow, measurements$supwidth)
                }
            }
            
            list(width=maxWidthWOSup + maxSupInRow, supwidth=maxSupInRow)
        },
        .widthWidestHeader=function() {
            width <- 0
            
            for (column in .columns) {
                if (column$visible())
                    width <- max(width, nchar(column$.title))
            }
            
            width
        },
        asString=function() {
            
            pieces <- character()
            
            pieces <- c(pieces, .titleForPrint())
            pieces <- c(pieces, .headerForPrint())
            i <- 1
            
            if ( ! .self$.swapRowsColumns) {
                
                for (i in seq_len(.rowCount))
                    pieces <- c(pieces, .rowForPrint(i))
                
            } else {
                
                for (i in seq_along(.columns)) {
                    if (i == 1)
                        next()  # the first is already printed in the header
                    if (.columns[[i]]$visible())
                        pieces <- c(pieces, .rowForPrint(i))
                }
            }
            
            pieces <- c(pieces, .footerForPrint())
            pieces <- c(pieces, '\n')
            
            paste0(pieces, collapse="")
        },
        .titleForPrint=function() {
            
            pieces <- character()
            
            w <- nchar(.title)
            wid <- width()
            padright <- repstr(' ', wid - w)
            
            pieces <- c(pieces, '\n')
            pieces <- c(pieces, .marstr, .title, padright, .marstr, '\n')
            pieces <- c(pieces, .marstr, repstr('\u2500', wid), .marstr, '\n')
            
            paste0(pieces, collapse="")
        },
        .headerForPrint=function() {
            
            pieces <- character()
            
            wid <- width()
            pieces <- c(pieces, .marstr)
            
            if ( ! .swapRowsColumns) {
            
                for (column in .columns) {
                    if (column$visible())
                        pieces <- c(pieces, .padstr, column$.titleForPrint(), .padstr)
                }
                
            } else {
                
                column <- .columns[[1]]
                
                pieces <- c(pieces, .padstr, spaces(.widthWidestHeader()), .padstr)
                
                for (i in seq_len(.rowCount)) {
                    text <- paste(column$.getCell(i)$value)
                    rowWidth <- .widthWidestCellInRow(i)$width
                    w <- nchar(text)
                    pad <- spaces(max(0, rowWidth - w))
                    
                    pieces <- c(pieces, .padstr, text, pad, .padstr)
                }
            }
            
            pieces <- c(pieces, .marstr, '\n')
            
            pieces <- c(pieces, .marstr, repstr('\u2500', wid), .marstr, '\n')
            
            paste0(pieces, collapse="")
        },
        .footerForPrint=function() {
            
            pieces <- character()
            
            wid <- width()
            
            pieces <- c(.marstr, repstr('\u2500', wid), .marstr, '\n')
            
            for (i in seq_along(.footnotes$.notes)) {
                
                # determine if the corresponding superscript is visible
                
                supVisible <- FALSE
                
                for (column in .columns) {
                    if (column$visible()) {
                        for (cell in column$.cells) {
                            if ((i-1) %in% cell$sups) {
                                supVisible <- TRUE
                                break()
                            }
                        }
                    }
                    if (supVisible)
                        break()
                }
                
                if (supVisible) {
                
                    note <- .footnotes$.notes[[i]]
                    
                    lines <- strwrap(note, width=(wid-.padding-2))
                    first <- TRUE
                    
                    for (line in lines) {
                        
                        pieces <- c(pieces, .marstr)
                        
                        if (first) {
                            pieces <- c(pieces, .SUPCHARS[i], ' ')
                            first <- FALSE
                        } else {
                            pieces <- c(pieces, '  ')
                        }
                        
                        pieces <- c(pieces, line, .marstr, '\n')
                    }
                }
            }
            
            paste0(pieces, collapse="")
        },
        .rowForPrint=function(i) {
            
            pieces <- character()
            
            pieces <- c(pieces, .marstr)
            
            if ( ! .swapRowsColumns) {
            
                for (column in .columns) {
                    if (column$visible())
                        pieces <- c(pieces, .padstr, column$.cellForPrint(i), .padstr)
                }
                
            } else {
                
                column <- .columns[[i]]
                
                width <- .widthWidestHeader()
                
                pieces <- c(pieces, .padstr, column$.titleForPrint(width), .padstr)
                
                for (j in seq_along(column$.cells)) {
                    widest <- .widthWidestCellInRow(j)
                    width <- widest$width
                    supwidth <- widest$supwidth
                    
                    cell <- column$.cells[[j]]
                    measurements <- silkyMeasureElements(list(cell))
                    measurements$width <- max(measurements$width, width)
                    measurements$supwidth  <- supwidth
                    
                    pieces <- c(pieces, .padstr, column$.cellForPrint(j, measurements), .padstr)
                }
                
            }
            
            pieces <- c(pieces, .marstr, '\n')
            
            paste0(pieces, collapse="")
        },
        asProtoBuf=function() {
            initProtoBuf()
            
            table <- RProtoBuf::new(silkycoms.ResultsTable)
            
            for (column in .columns)
                table$add("columns", column$asProtoBuf())
    
            element <- RProtoBuf::new(silkycoms.ResultsElement,
                name=.name,
                title=.title,
                table=table)
            
            element
        }
    )
)

Tables <- setRefClass(
    "Tables",
    contains="ResultElement",
    fields=c(
        .tables="list",
        .tableNames="character",
        .template="list",
        .tablesExpr="character",
        .tablesValue="ANY"),
    methods=list(
        initialize=function(name="", index=0, options=Options()) {
            callSuper(name, index, options)
            .tablesExpr <<- "1"
        },
        get=function(name) {
            
            index <- which(name == .tableNames)
            if (length(index) > 0)
                table <- .tables[[ index[1] ]]
            else
                table <- NULL
            
            table
        },
        .setDef=function(name, value) {
            if (name == "tables")
                .setTablesDef(value)
            else if (name == "template")
                .setTemplateDef(value)
            else
                callSuper(name, value)
        },
        .setTemplateDef=function(templateDef) {
            .template <<- templateDef
            .update()
        },
        .setTablesDef=function(tablesExpr) {
            .tablesExpr <<- paste0(tablesExpr)
            .update()
        },
        .update=function() {
            
            if (length(.template) == 0)
                return()
            
            error <- NULL
            
            tablesValue <- try(.options$eval(.tablesExpr, name=.name, index=.index), silent=TRUE)
            
            if (inherits(tablesValue, "try-error")) {
                error <- tablesValue
                tablesValue <- 0
            }
            
            .tablesValue <<- tablesValue
            
            oldNames <- .tableNames
            oldTables <- .tables
            
            if (is.numeric(.tablesValue) && .tablesValue > 0) {
                newNames <- paste(1:.tablesValue)
            } else if (is.character(.tablesValue)) {
                newNames <- .tablesValue
            } else {
                newNames <- character()
            }
            
            .tableNames <<- newNames
            .tables <<- list()
            
            for (i in seq_along(newNames)) {
                
                newName <- newNames[[i]]
                index <- which(oldNames == newName)
                
                if (length(index) > 0) {
                    
                    .tables[[i]] <<- oldTables[[ index[1] ]]
                    
                } else {
                    
                    table <- Table(newName, i, .options)
                    table$.init(.template)
                    .tables[[i]] <<- table
                }
            }
            
            if ( ! is.null(error))
                rethrow(error)
        },
        clear=function() {
            .tableNames <<- character()
            .tables <<- list()
        },
        asString=function() {
            
            pieces <- c(' ', .title, '\n')
            
            for (table in .tables) {
                if (table$visible())
                    pieces <- c(pieces, table$asString())
            }
            
            return(paste0(pieces, collapse=""))
        },
        asProtoBuf=function() {
            initProtoBuf()
            
            group <- RProtoBuf::new(silkycoms.ResultsGroup)
            
            for (table in .tables)
                group$add("elements", table$asProtoBuf())
            
            RProtoBuf::new(silkycoms.ResultsElement,
                name=.name,
                title=.title,
                group=group)
            
        })
)


