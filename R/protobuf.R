
initProtoBuf <- function() {

    library("RProtoBuf")
    
    resultsProtoPath <- system.file("results.proto", package="silkyR")
    if (resultsProtoPath == "")
        resultsProtoPath <- system.file("inst", "results.proto", package="silkyR")
    if (resultsProtoPath == "")
        stop("silkyR results.proto not found!", call.=FALSE)
    
    RProtoBuf::readProtoFiles(resultsProtoPath)
}
