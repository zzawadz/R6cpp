#' @name Extract classes from h or cpp file.
#'
rp_parse_cpp = function(path)
{
  lines = read_lines(file = path)

  begins = grep(lines, pattern = "\\[\\[R6Begin\\]\\]")
  ends = grep(lines, pattern = "\\[\\[R6End\\]\\]")
  vals = cbind(begins, ends)

  classes = lapply(1:nrow(vals), function(i) lines[(vals[i,1]+1):(vals[i,2]-1)] %>%
                     paste(collapse = "\n"))

  classes
}

rp_create_class_map = function(class)
{
  nameClass = rp_get_class_name(class)
  membershipList = rp_extract_membership(class)

}

#' @name Get class name
#' @description Extract class name from raw class definition.
rp_get_class_name = function(class)
{
  stri_extract_all_regex(class, "class \\w+", simplify = TRUE) %>% stri_split_regex(" ") %>% "[["(1) %>% "[["(2)
}


rp_extract_public = function(class)
{
  end = stri_locate_last_regex(class, "\\};")[[1]]-1
  start = stri_locate_first_fixed(class,"{")[[1]]+1

  positions = lapply(c("private\\s*:","protected\\s*:","public\\s*:"), function(membership)
  stri_locate_all_regex(class, membership)[[1]])

  names(positions) = c("private","protected","public")
  rawChangePos = c(unlist(positions) %>% na.omit %>% as.numeric, end)

  #private = c(start, positions$private[[2]]) %>% na.omit %>% as.numeric
  #private = c(private, min(rawChangePos[private < rawChangePos]))
  #private = substring(class, private[1], private[2]-1)
  #protected = NULL

  public = positions$public[[2]]
  public = c(public, min(rawChangePos[public < rawChangePos]))
  public = substring(class, public[1]+1, public[2]-1)

  #membershipList = list(private = private, protected = protected, public = public)
  #membershipList
  public
}

rp_split_to_slots = function(public, nameClass)
{
  if(is.null(public)) stop("There is no code to export!")


  # extract variables
  variables = stri_extract_all_regex(public, "(//\\[\\[R6:.*\\]\\]\\s*)?(const )?\\w+\\s+\\w+;( )*(//\\[\\[R6:.*\\]\\]\\s*)?")[[1]]

  functions = stri_extract_all_regex(public, "(//\\[\\[R6:.*\\]\\]\\s*)?(const )?\\w+\\s\\w+\\(.*\\);( )*(//\\[\\[R6:.*\\]\\]\\s*)?")[[1]]

  constructors = stri_extract_all_regex(public, "(//\\[\\[R6:.*\\]\\]\\s*)?\\w+\\(.*\\);( )*(//\\[\\[R6:.*\\]\\]\\s*)?")[[1]]
  constructors = constructors[grepl(nameClass, constructors)]

  slotsList = list(variables = variables,
                   constructors = constructors,
                   functions = functions)
  slotsList

}



rp_extract_attributes = function(slot)
{
  #slots = variables[1]
  attrs = stri_extract_all_regex(slot, "//\\[\\[R6:.*\\]\\]", simplify = TRUE)
  attrs = stri_extract_all_regex(attrs,"\\w+(=\\w+)?")[[1]]
  #attrs = lapply(attrs, function(x) if(is.na(x[1])) return(NA) else return(x[-1]))
  attrs = if(is.na(attrs[[1]])) NA else attrs[-1]
  attrs
}

rp_extract_params = function(slot, attrs)
{
  attrs = attrs[grepl(attrs, pattern = "=xptr")] %>% na.omit

  params = stri_extract_all_regex(slot, "\\(.*\\)")[[1]]
  params = substring(params,2,nchar(params)-1)

  params = stri_split_fixed(params, ",")[[1]]
  paramsNames = stri_extract_last_regex(params, "\\w+")
  paramsType = stri_extract_first_regex(params, "(const )?\\w+(::)?\\w+(<.*>)?&?")

  if(length(attrs))
  {
    ptrs = unlist(strsplit(attrs,split = "=xptr"))
    ptrsIdx = which(paramsNames %in% ptrs)

    paramsType[ptrsIdx] = rp_params2xptr(paramsType[ptrsIdx])
  }

  paramsList = list(paramsType = paramsType, paramsNames = paramsNames)
  paramsList
}

rp_params2xptr = function(ptrTypes)
{
  reference = substring(ptrTypes, nchar(ptrTypes)) == "&"
  ptrTypes[reference] = substring(ptrTypes[reference],1, nchar(ptrTypes)-1)

  constness = substr(ptrTypes,1,5) == "const"
  ptrTypes[constness] = substring(ptrTypes[constness],7)


  ptrTypes = sprintf("Rcpp::XPtr<%s >",ptrTypes)

  ptrTypes[constness] = paste("const", ptrTypes[constness])
  ptrTypes[reference] = paste0(ptrTypes[reference],"&")

  ptrTypes
}


