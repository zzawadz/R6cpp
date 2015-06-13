rp_create_function_slot = function(slot, nameClass)
{
  attrs = rp_extract_attributes(slot)
  if(is.na(attrs)) attrs = "NOTHING"
  if(attrs == "notExport") return(NULL)

  slot = stri_replace_all_regex(slot,"//\\[\\[R6:.*\\]\\]\\s*","")
  constness = substr(slot,1,5) == "const"
  if(constness) slot = substring(slot,7)

  tmp = stri_extract_all_regex(slot, "\\w+")[[1]][1:2]

  typeReturn = tmp[1]
  returnAsPtr = if(any(attrs == "return=xptr")) TRUE else FALSE

  fncName = tmp[2]

  paramsList = rp_extract_params(slot, attrs)

  functionSlot = list(fncName = fncName,
                      typeReturn = typeReturn,
                      paramsList = paramsList,
                      constness = constness,
                      returnAsPtr = returnAsPtr,
                      nameClass = nameClass)
  functionSlot
}



rp_render_function_cpp = function(functionSlot)
{
  functionSlot$constness = if(functionSlot$constness) "const " else ""

  template = '
// [[Rcpp::export]]
{{{constness}}}{{{typeReturn}}} {{{nameClass}}}_{{{fncName}}}(const Rcpp::XPtr<{{{nameClass}}}> r6Ptr, {{{params}}})
{
  return {{{returnCode}}};
}
'

  params = functionSlot$paramsList
  functionSlot$params = paste(params$paramsType,params$paramsNames) %>% paste(collapse = ", ")
  paramsRaw = functionSlot$paramsList$paramsNames %>% paste(collapse = ", ")

  if(functionSlot$returnAsPtr)
  {
    functionSlot$returnCode = sprintf("XPtr<%s>(&r6Ptr->%s(%s))",functionSlot$typeReturn,functionSlot$fncName, paramsRaw)
  } else
  {
    functionSlot$returnCode = sprintf("r6Ptr->%s(%s)",functionSlot$fncName, paramsRaw)
  }

  whisker.render(template, functionSlot)

}


