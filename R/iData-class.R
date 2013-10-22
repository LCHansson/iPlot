iData <- setRefClass(
  "iData",
  fields = list(
    data = "ANY",
    categories = "character",
    numerics = "character",
    all = "character",
    levels = "list"
  ),
  methods = list(
    initialize = function(data) {
      if (!is.data.frame(data)) {
        stop("Only data.frame or data.table are supported")
      }
      data <<- data
      fix_data()
      update_fields()
    },
    update_fields = function() {
      categories <<- filter(
        function(x) is.factor(x) || is.character(x)
      )
      numerics <<- filter(is.numeric)
      levels <<- lapply(data, levels)
      all <<- names(data)
    },
    filter = function(fun) {
      names(data)[sapply(data, fun)]
    },
    fix_data = function() {
      # Fix colnames due to css error (see issue #1)
      tmp <- data
      rpl <- function(x) gsub("\\.", "_", colnames(x))
      if (is.data.table(tmp)) {
        setnames(tmp, rpl(tmp))
      } else {
        colnames(tmp) <- rpl(tmp)
      }
      data <<- tmp
    }
  )
)
