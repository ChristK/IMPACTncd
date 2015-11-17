## IMPACTncd: A decision support tool for primary prevention of NCDs
## Copyright (C) 2015  Chris Kypridemos
 
## IMPACTncd is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.

## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with this program; if not, see <http://www.gnu.org/licenses/>
## or write to the Free Software Foundation, Inc., 51 Franklin Street,
## Fifth Floor, Boston, MA 02110-1301  USA.


require(httr)
set_config(use_proxy(url="http://webproxy.its.manchester.ac.uk", port=3128))

require(devtools)
install_github("Rdatatable/data.table",  build_vignettes = F)

# update.packages(.libPaths()[1], ask = F, checkBuilt = T)
# 
# install.packages("~/Dropbox/PhD/Models/packages/RAutoGenRunTime_0.3-1.tar.gz", repos = NULL, type = "source")
# install_github("omegahat/RAutoGenRunTime")
# install_github("duncantl/Rllvm")
