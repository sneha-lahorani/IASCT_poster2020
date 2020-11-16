#### ---- First and last dot in R ---- ####

# Function

dot <- function(type = "FIRST", dsetin, groupvar, arrangevar, dtypeyn = "Y") {

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

  if (toupper(dtypeyn) == "Y") {
    two <- one %>%
      mutate(DTYPE = if_else(toupper(type) == "FIRST", "BC",
        if_else(toupper(type) == "LAST", "WC", "")
      )) %>%
      bind_rows(dsetin) %>%
      mutate(DTYPE = if_else(!DTYPE %in% c("BC", "WC"), "", DTYPE))
  }
  else if (toupper(dtypeyn) == "N") {
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
example <- dot(
  type = "first", # Input "first" or "last", else a warning is displayed
  dsetin = adlb, # Input data frame
  arrangevar = c("PARAMCD", "USUBJID", "AVAL"), # Variables to sort by
  groupvar = c("PARAMCD", "USUBJID"), # Variables to group by
  dtypeyn = "Y" # Parameter to add DTYPE. "Y" if yes (default) & "N" if no
)
