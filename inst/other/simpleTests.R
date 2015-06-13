library(R6cpp)
path  = "inst/other/R6Test.h"


classes = rp_parse_cpp(path)

class = classes[[1]]


public = rp_extract_public(class)
nameClass = rp_get_class_name(class)

slotsList = rp_split_to_slots(public, nameClass)

functionsSlots    = lapply(slotsList$functions, rp_create_function_slot, className = nameClass)
paramsSlots       = NULL
constructorsSlots = lapply(slotsList$constructors, rp_create_constructor_slot)


path  = "inst/include/R6cpp.h"

classes = rp_parse_cpp(path)
class = classes[[1]]


