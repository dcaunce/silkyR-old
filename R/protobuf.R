
initProtoBuf <- function() {

    library("RProtoBuf")
    
    resultsProtoPath <- system.file("silkycoms.proto", package="silkyR")
    if (resultsProtoPath == "")
        resultsProtoPath <- system.file("inst", "silkycoms.proto", package="silkyR")
    if (resultsProtoPath == "")
        stop("silkyR silkycoms.proto not found!", call.=FALSE)
    
    RProtoBuf::readProtoFiles(resultsProtoPath)
}
