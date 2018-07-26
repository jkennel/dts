# dts
Tools for handling borehole distributed temperature datasets (DTS)

Under construction

# installation

library(devtools)

install_github('g360group/dts')


## Write to file

in_dir = '/media/Data/DTS_Test'

out_dir = '/media/Data/'

n_cores = 4

write_dts_xml(in_dir, out_dir, n_cores)

Two files will be written meta.fst and data.fst

## Read into memory

in_dir = '/media/Data/DTS_Test'

dts <- read_dts_folder(in_dir, n_cores = 4)
