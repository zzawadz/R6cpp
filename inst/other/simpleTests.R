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

rp_create_cpp_exports = function(class, path, out = "src")
{
  public = rp_extract_public(class)
  nameClass = rp_get_class_name(class)

  slotsList = rp_split_to_slots(public, nameClass)

  functionsSlots    = lapply(slotsList$functions, rp_create_function_slot, className = nameClass)
  paramsSlots       = NULL
  constructorsSlots = lapply(slotsList$constructors, rp_create_constructor_slot)


  constructorsCpp = lapply(constructorsSlots, rp_render_constructor_cpp) %>% paste(collapse = "")
  functionsCpp = lapply(functionsSlots, rp_render_function_cpp) %>% paste(collapse = "")

  header = stri_extract_last_regex(path, "\\w+\\.h")
  header = sprintf('#include "%s" \n', header)

  code = paste(header, constructorsCpp, functionsCpp)

  out = file.path(out, paste0("R6cpp",nameClass,".cpp"))
  cat(code, file = out)

}
