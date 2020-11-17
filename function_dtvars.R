#### ---- Function to obtain date, time, and datetime variables from character datetime string ---- ####

# Function

dtvars <- function(dsetin, datetmchar) {
  ipak <- function(pkg) {
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) {
      install.packages(new.pkg, dependencies = TRUE)
    }
    sapply(pkg, require, character.only = TRUE)
  }

  ipak(c("tidyverse", "gsktable", "chron"))

  one <- {{ dsetin }} %>%
    mutate(
      .len_ = str_length({{ datetmchar }}),
      .token_ = if_else(.len_ == 16, "FDT",
        if_else(.len_ == 10, "FD", "PD")
      ),
      chardate1 = str_replace({{ datetmchar }}, "T", " "),
      chardate2 = str_sub(chardate1, start = 1L, end = 10L),
      chartime = ifelse(.token_ == "FDT", paste0(str_sub(chardate1, start = 12L, end = 16L), ":00"), NA)
    )

  two <- one %>%
    filter(.token_ == "FDT") %>%
    mutate(
      dtm = strptime(chardate1, format = "%Y-%m-%d %H:%M"),
      dt = as.Date(chardate2, "%Y-%m-%d"),
      tm = times(chartime)
    )

  three <- one %>%
    filter(.token_ == "FD") %>%
    mutate(
      dt = as.Date(chardate2, "%Y-%m-%d"),
      dtm = as.POSIXct(NA)
    )

  four <- one %>%
    filter(.token_ == "PD") %>%
    mutate(
      dt = as.Date(NA),
      dtm = as.POSIXct(NA)
    )

  final <- bind_rows(two, three, four) %>%
    select(-.len_, -.token_, -chardate1, -chardate2, -chartime)

  if (any(is.na(final))) {
    warning("Input datetime character variable contains partial datetime values.")
  }

  return(final)
}

# Example

birthdates <- data.frame(
  BIRTHDTC = c("2018-12-31", "1997-07-15T07:55", "2007-09-19T00:02", "1995-12-09T13:30", "2020-11", "1986"),
  NAME = c("Stefan", "Ritu", "Neil", "Sneha", "Damon", "Caroline")
)

example <- dtvars(dsetin = birthdates, datetmchar = BIRTHDTC)
