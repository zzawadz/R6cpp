rp_create_files = function(classes, path, out = "src")
{
  lapply(classes, rp_create_files_for_class, path = path, out = out)
}

rp_create_files_for_class = function(class, path, out = "src")
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

  # create cpp files
  rp_create_cpp_export_file(classList, path, out)

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
