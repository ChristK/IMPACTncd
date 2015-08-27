require(httr)
set_config(use_proxy(url="http://webproxy.its.manchester.ac.uk", port=3128))

require(devtools)
install_github("Rdatatable/data.table",  build_vignettes = F)

# update.packages(.libPaths()[1], ask = F, checkBuilt = T)
# 
# install.packages("~/Dropbox/PhD/Models/packages/RAutoGenRunTime_0.3-1.tar.gz", repos = NULL, type = "source")
# install_github("omegahat/RAutoGenRunTime")
# install_github("duncantl/Rllvm")
