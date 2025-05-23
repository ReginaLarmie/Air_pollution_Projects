

```{r}
library(RAQSAPI)

ozone_data <- aqs_sampledata_by_site(
  email = "rnl34@case.edu",
  key = "copperhawk19",
  bdate = "20230701",
  edate = "20230731",
  stateFIPS = "39",       # Ohio
  countyFIPS = "035",     # Cuyahoga
  siteNumber = "0060",    # Double-check this
  param = "44201"         # Ozone
)
```
```{r}
library(RAQSAPI)

ozone_data <- aqs_sampledata_by_site(
  email = "rnl34@case.edu",
  key = "copperhawk19",
  bdate = "20230101",
  edate = "20231231",
  stateFIPS = "39",       # Ohio
  countyFIPS = "035",     # Cuyahoga
  siteNumber = "0035",    # Double-check this
  param = "44201"         # Ozone
  
```


