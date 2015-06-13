library(R6cpp)
path  = "inst/include/R6cpp.h"


classes = rp_parse_cpp(path)

rp_create_files(classes, path)


