library(R6cpp)
path  = "inst/include/R6cpp.h"


classes = rp_parse_cpp(path)

#rp_create_files(classes, path)

classList = rp_create_class_list(classes[[1]])

classList$constructorsSlots

classList$nameClass



 %>% cat


x = R6Test$new(1e6)
its = as.numeric(1:1e6)
Rprof()
for(i in its) x$push_back(i)
Rprof(NULL)

xx = numeric(1e6)

Rprof()
for(i in its) xx[i] = i
Rprof(NULL)

summaryRprof()
x$at(50)



