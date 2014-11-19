require(httr)
set_config(use_proxy(url="http://webproxy.its.manchester.ac.uk", port=3128))}

require(devtools)
install_github("Rdatatable/data.table",  build_vignettes = F)
