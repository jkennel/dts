# dts
Tools for handling borehole distributed temperature datasets (DTS)

Under construction

# installation

```{r echo = TRUE, eval = FALSE}

library(remotes)

remotes::install_github('g360codes/dts')
```

# Generate report

A report based solely on DTS data can be generated with **generate_report**.


```{r echo = TRUE, eval = FALSE}
library(dts)

# full path to DTS XML files
files_dir <- location_of_dts_directory

# full path to output directory
output_dir <- location_of_output_directory

generate_report(files_dir, output_dir)

```