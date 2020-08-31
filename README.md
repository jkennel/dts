# dts
Tools for handling borehole distributed temperature datasets (DTS)

Under construction

# installation

library(remotes)

remotes::install_github('g360codes/dts')


# Generate report

A report based solely on DTS data can be generated with **generate_report**.


```{r echo = TRUE, eval = FALSE}
library(dts)

# path to DTS XML files
files_dir <- '/gdc_05_2015-11-02/2015-11-02 Active DTS Test GDC-05 7_5 W per m/'

# output directory for report
output_dir <- '/gdc_05_2015-11-02/dts_g360_book'

generate_report(files_dir, output_dir)

```