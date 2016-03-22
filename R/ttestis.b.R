
# This file is an automatically generated template, it will not be subsequently
# overwritten by the compiler, and may be edited

TTestIS <- setRefClass(
  "TTestIS",
  contains="Analysis",
  methods=list(
    run=function() {
      callSuper()
        
        .results$get("ttest")$setCell(1, "t", 15)

      # `.options` contains the options
      # `.options$dataset()` contains the data
      # `.results` contains the results object (to populate)
    })
)

