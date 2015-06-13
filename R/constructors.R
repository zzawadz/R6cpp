rp_create_constructor_slot = function(slot)
{
  attrs = rp_extract_attributes(slot)
  if(is.na(attrs)) attrs = "NOTHING"
  if(attrs == "notExport") return(NULL)

  slot = stri_replace_all_regex(slot,"//\\[\\[R6:.*\\]\\]\\s*","")
  constness = substr(slot,1,5) == "const"
  if(constness) slot = substring(slot,7)

  className = stri_extract_all_regex(slot, "\\w+")[[1]][1]
  paramsList = rp_extract_params(slot, attrs)

  constructorSlot = list(className = className, paramsList = paramsList)
  constructorSlot
}


rp_render_constructor_cpp = function(constr)
{
  template = '
// [[Rcpp::export]]
Rcpp::XPtr<{{{className}}}> {{{className}}}_{{{className}}}({{{params}}})
{
  return Rcpp::XPtr<{{{className}}}>(new {{{className}}}({{{paramsRaw}}}));
}
  '

  params = constr$paramsList
  constr$params = paste(params$paramsType,params$paramsNames) %>% paste(collapse = ", ")
  constr$paramsRaw = constr$paramsList$paramsNames %>% paste(collapse = ", ")

  whisker.render(template, constr)

}
