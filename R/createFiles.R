
rp_render_all = function(path2h = NULL, outCpp = "src", outR = "R")
{
  if(is.null(path2h))
  {
    path2h = dir("inst/include", full.names = TRUE)
  }

  classes = rp_parse_cpp(path2h)
  rp_create_files(classes, path2h, outCpp, outR)
}

rp_create_files = function(classes, path, outCpp = "src", outR = "R")
{
  lapply(classes, rp_create_files_for_class, path = path, outCpp = outCpp, outR = outR)
}

rp_create_class_list = function(class)
{
  public = rp_extract_public(class)
  nameClass = rp_get_class_name(class)

  slotsList = rp_split_to_slots(public, nameClass)

  functionsSlots    = lapply(slotsList$functions, rp_create_function_slot, nameClass = nameClass)
  paramsSlots       = NULL
  constructorsSlots = lapply(slotsList$constructors, rp_create_constructor_slot)

  classList = list(nameClass = nameClass,
                   functionsSlots = functionsSlots,
                   paramsSlots = paramsSlots,
                   constructorsSlots = constructorsSlots)
  classList

}


rp_create_files_for_class = function(class, path, outCpp = "src", outR = "R")
{
  classList = rp_create_class_list(class)
  # create cpp files
  rp_create_cpp_export_file(classList, path, outCpp)
  rp_create_r_export_file(classList, outR)
}


rp_create_r_export_file = function(classList, out = "R")
{
  template = '
{{{nameClass}}} = R6Class("{{{nameClass}}}", class = FALSE,
private = list(pointer = ""),
public = list(
{{{constructorR}}}
{{{functionsR}}}

))
'
  constructorR = rp_create_r_constructor(classList$constructorsSlots)
  functionsR = rp_render_r_functions(classList$functionsSlots)

  classRList = list(nameClass = classList$nameClass,
                    constructorR = constructorR,
                    functionsR = functionsR)

  code = whisker.render(template, classRList)
  out = file.path(out, paste0("R6cppExports",classList$nameClass,".R"))
  cat(code, file = out)
}

rp_create_cpp_export_file = function(classList, path, out = "src")
{
  constructorsCpp = lapply(classList$constructorsSlots, rp_render_constructor_cpp) %>% paste(collapse = "")
  functionsCpp = lapply(classList$functionsSlots, rp_render_function_cpp) %>% paste(collapse = "")

  header = stri_extract_last_regex(path, "\\w+\\.h")
  header = sprintf('#include "%s" \n', header)

  code = paste(header, constructorsCpp, functionsCpp)

  out = file.path(out, paste0("R6cppExports",classList$nameClass,".cpp"))
  cat(code, file = out)

}
