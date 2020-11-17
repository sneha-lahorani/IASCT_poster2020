#### ---- First and last dot in R ---- ####

# Function

dot <- function(type = "FIRST", dsetin, groupvar, arrangevar, dtype = "BC") {

  # Function to load/install packages (where needed)
  ipak <- function(pkg) {
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) {
      install.packages(new.pkg, dependencies = TRUE)
    }
    sapply(pkg, require, character.only = TRUE)
  }

  ipak(c("tidyverse", "gsktable"))

  if (toupper(type) == "FIRST") {
    one <- {{ dsetin }} %>%
      arrange_at(arrangevar) %>%
      group_by_at(groupvar) %>%
      slice(1) %>%
      ungroup()
  }
  else if (toupper(type) == "LAST") {
    one <- {{ dsetin }} %>%
      arrange_at(arrangevar) %>%
      group_by_at(groupvar) %>%
      slice(n()) %>%
      ungroup()
  }
  else {
    warning("Please input type as 'first' or 'last' only. Default has been taken as 'first'.")
  }

  if (toupper(dtype) %in% c("BC", "WC")) {
    two <- one %>%
      mutate(DTYPE = if_else(toupper(dtype) == "BC", "BC",
        if_else(toupper(dtype) == "WC", "WC", "")
      )) %>%
      bind_rows(dsetin) %>%
      mutate(DTYPE = if_else(!DTYPE %in% c("BC", "WC"), "", DTYPE))
  }
  else if (!toupper(dtype) %in% c("BC", "WC")) {
    two <- one %>%
      bind_rows(dsetin)
  }
  return(two)
}

# Subsetting adlb for 2 params, 2 visits, and a handful of variables, so that we can have a clear view of how the function truly works
adlb <- gsktable::adlb %>%
  filter(AVISITN %in% c(10, 50) & PARAMCD %in% c("LB1224", "LB1227")) %>%
  select(USUBJID, AVISITN, AVISIT, PARAMCD, PARAM, AVAL, AVALC)

# Example
example1 <- dot(
  type = "first", # Input "first" [default]/"last", else a warning is displayed
  dsetin = adlb, # Input data frame
  arrangevar = c("PARAMCD", "USUBJID", "AVAL"), # Variables to sort by
  groupvar = c("PARAMCD", "USUBJID"), # Variables to group by
  dtype = "BC" # Parameter for Worst ("WC")/Best case ("BC") [default].
)

example2 <- dot(
  type = "last", # Input "first" [default]/"last", else a warning is displayed
  dsetin = example1, # Input data frame
  arrangevar = c("PARAMCD", "USUBJID", "AVAL"), # Variables to sort by
  groupvar = c("PARAMCD", "USUBJID"), # Variables to group by
  dtype = "WC" # Parameter for Worst ("WC")/Best case ("BC") [default].
)
