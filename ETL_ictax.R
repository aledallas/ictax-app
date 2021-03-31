# Extract and trasform ICTax data
# source: https://www.ictax.admin.ch/extern/de.html#/xml

# library(XML)
library(xml2)
library(tidyverse)
library(rvest)
library(tictoc)
library(lubridate)


# define path of the file -- should be adjusted to location of the downloaded file
path <- "input/kursliste_2020_V2.0/kursliste_2020.xml"

# import file
import <- read_html(path)

# extract fund nodes
xml_extract <- import %>%
  xml_find_all(".//fund")


# retrieve fund information data
fund <- xml_extract %>% 
  xml_attrs() %>% 
  bind_rows() %>% 
  rename(fund_id = "id", 
         fund_currency = "currency")


# extract yearend object, which contains tax value
extract_yearend <- function(x) {
  
  vl <- x %>% 
    xml_attr("valornumber")
  
  isin <- x %>% 
    xml_attr("isin")
  
  a_year <- x %>% 
    xml_find_all("yearend") %>% 
    xml_attrs() %>% 
    bind_rows() %>% 
    rename(yearend_id = id)
  
  bind_cols(valorNumber = vl, isin = isin, a_year)
}

yearend <- extract_yearend(xml_extract)


# retrieve payments data for all (payment) - long process (ca. 2h)
# solution from https://community.rstudio.com/t/generate-a-data-frame-from-many-xml-files/10214/3
tic()
payments <- xml_extract %>% 
  map_dfr(~ {
    # extract the attributes from the parent tag as a data.frame
    vl <- xml_attr(.x, "valornumber")
    isin <- xml_attr(.x, "isin")
    # make a data.frame out of the attributes of the kids
    payments <- xml_find_all(.x, "payment") %>% map_dfr(~ as.list(xml_attrs(.x)))
    # combine them
    bind_cols(valorNumber = vl, isin = isin, payments) %>% set_tidy_names() %>% as_tibble()
  })
toc()

# create a new date field that considers exdate if available or paymentdate
div <- payments %>% 
  rename(valornumber = valorNumber) %>% 
  mutate(div_date = ymd(ifelse(is.na(exdate), paymentdate, exdate)),
         div_valuechf = as.numeric(paymentvaluechf)) %>% 
  select(valornumber, isin, paymentdate, exdate, withholdingtax, div_date, div_valuechf)   # eventually maintain all variables


# save the data files
saveRDS(fund, file = "data/fund_2020.rds")
saveRDS(yearend, file = "data/yearend_2020.rds")
saveRDS(div, file = "data/dividends_2020.rds")
