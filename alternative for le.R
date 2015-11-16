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

setkey(chd.burden, sex, qimd, agegroup, scenario, mc, year)
setkey(stroke.burden, sex, qimd, agegroup, scenario, mc, year)
setkey(other.mortality, sex, qimd, agegroup, scenario, mc, year)
setkey(riskfactors, sex, qimd, agegroup, scenario, mc, year)
tt <- merge(riskfactors[group == "SAQ", .(pop, sex, qimd, agegroup, scenario, mc, year)],
            other.mortality[group == "SAQ", .(other.mortality, sex, qimd, agegroup, scenario, mc, year)],
            by = c("sex", "qimd", "agegroup", "scenario", "mc", "year"), 
            all.x = T)
tt <- merge(tt,
            stroke.burden[group == "SAQ", .(stroke.mortality, sex, qimd, agegroup, scenario, mc, year)],
            by = c("sex", "qimd", "agegroup", "scenario", "mc", "year"), 
            all.x = T)
tt <- merge(tt,
            chd.burden[group == "SAQ", .(chd.mortality, sex, qimd, agegroup, scenario, mc, year)],
            by = c("sex", "qimd", "agegroup", "scenario", "mc", "year"), 
            all.x = T)

tt[, mortality := (chd.mortality + stroke.mortality + other.mortality)/pop]
tt[is.na(mortality) == T, mortality := 0]
rate <- tt[, dcast(.SD, agegroup~year, value.var = "mortality"), by = .(sex, qimd, scenario, mc)]
pop  <- tt[, dcast(.SD, agegroup~year, value.var = "pop"), by = .(sex, qimd, scenario, mc)]
ttt <- merge(rate, pop,
             by = c("sex", "qimd", "agegroup", "scenario", "mc"),
             all.x = T)

ttt[, {
  rate <- paste0(2011:2035, ".x")
pop <- paste0(2011:2035, ".y")
demogdata(rate,
          pop,
          c(0, 1, seq(5, 85, 5)),
          c(2011:2035), 
          type = "mortality")
}, with = F, 
     by = .(sex, qimd, scenario, mc)]

for (k in c("Men", "Women")) { # sex
  for (l in 1:5) { # qimd
    nam <- paste0("Mortal.sex-", k, ".qimd-", l)
    nam2 <- paste0("POP.sex-", k, ".qimd-", l)
    assign(nam, demogdata(acast(tt[sex==k & qimd ==l,], agegroup~year, value.var="Mx.all"),
                          acast(tt[sex==k & qimd ==l,], agegroup~year, value.var="pop"),
                          c(0:99),
                          tt[sex==k & qimd ==l, unique(year)],
                          "mortality", 
                          paste0("England.sex-", k, ".qimd-", l), 0))
    #plot(get(nam))
    assign(nam, smooth.demogdata(get(nam), age.grid=0:100, obs.var="theoretical"))
    assign(nam2, get(nam)$pop)
    temp<- forecast.fdm(fdm(get(nam),  method = "M", max.age=100), yearstoproject)
    
    assign(nam, combine.demogdata(get(nam), temp))
    #plot(get(nam))
    assign(nam, lifetable(get(nam), type = "period"))
    
    assign(nam, as.data.table(get(nam)$qx, keep.rownames = T))[, `:=` (sex=k, qimd=l)]
    assign(nam2, as.data.table(data.frame(get(nam2)), keep.rownames = T))[, `:=` (sex=k, qimd=l)]
    setnames(get(nam), "rn", "age")
    setnames(get(nam2), "rn", "age")
    setnames(get(nam2), paste0("X0.", 2002:2013), paste0(2002:2013))
    get(nam)[, `:=` (sex = factor(sex), age = as.numeric(age), qimd = ordered(qimd))]
    get(nam2)[, `:=` (sex = factor(sex), age = as.numeric(age), qimd = ordered(qimd))]
  }
}





dt <- dt[,{
  SII <- predict(
    glm(
      eval(indicator) ~ qimd2, 
      weights = as.numeric(n),
      family="gaussian"(link="identity")),
    data.frame(qimd2 = c(0, 1)),
    type = "response",
    se = T)
  SII <- (rnorm(1000, SII[[1]][[1]], SII[[2]][[1]])-
            rnorm(1000, SII[[1]][[2]], SII[[2]][[2]]))
  list(SII = mean(SII, na.rm = T, trim = 0.05),
       lui = quantile(SII, probs = 0.025, na.rm = T),
       uui = quantile(SII, probs = 0.975, na.rm = T))
},
by = .(sex, scenario, year)
]
