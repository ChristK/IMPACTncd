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

load(file="./Models/IMPACTncd/Lagtimes/HSE.ts.RData")
HSE.ts[, porftvgCat := ordered(porftvg) ]
HSE.ts[, agegroup:=ordered(agegroup)]
HSE.ts.srv.int <- svydesign(id=~psu, strata =~cluster, weights = ~wt.int, nest=F, data=HSE.ts, check.strata = T)
HSE.ts.srv.int <- subset(HSE.ts.srv.int, age>19 & wt.int>0 & year < 1 & is.na(porftvg)==F)   

tt <- svytable(~porftvg+year+agegroup+sex+qimd, HSE.ts.srv.int)
tt <- data.table(data.frame(tt))
tt =copy(tt[Freq>0])
tt[, pct:= prop.table(Freq), by=c("year", "agegroup", "sex", "qimd")]
tt =copy(tt[pct<1])
summary(tt[, sum(pct), by=c("year", "agegroup", "sex", "qimd")])
tt[, year:= as.numeric(as.character(year))]
tt[, trials:= sum(Freq),by=c("year", "agegroup", "sex", "qimd")]
tt[, succeses:= Freq]
tt[,fails:= trials-succeses]
tt[, cumsucceses:= cumsum(Freq), by=c("year", "agegroup", "sex", "qimd")]
tt[cumsucceses>trials, cumsucceses:=trials]
tt[,cumfails:= trials-cumsucceses]

lm1<- lm(pct~porftvg+year+agegroup+sex+qimd, weights=Freq, data=tt)
predict(lm1, data.frame(porftvg="1", year=0, agegroup="30-34", sex="1", qimd="1"))
sum(predict(lm1, data.frame(porftvg=c("0","1","2","3","4","5","6","7", "8", "9"), year=0, agegroup="30-34", sex="1", qimd="1"), type="response"))

fv.svylr <- glm(cbind(cumsucceses, cumfails) ~ porftvg+year+agegroup+sex+qimd, data=tt, family = quasibinomial,  control = list(maxit = 100))

# FV rate
fvrate <- svytable(~frtpor + porftvg + agegroup + sex + qimd, HSE.ts.srv.int)
fvrate <- data.table(data.frame(tt))
fvrate[, pct:= prop.table(Freq), by=c("porftvg", "agegroup", "sex", "qimd")]
fvrate[, agegroup := ordered(agegroup)]
fvrate[, fvgroup := paste0(porftvg, agegroup , sex , qimd)]

#save(fv.svylr, file="./Models/IMPACTncd/Lagtimes/fv.svylr.rda")

predict(fv.svylr, data.frame(porftvg=c("0","1","2","3","4","5","6","7", "8", "9"), year=0, agegroup="60-64", sex="1", qimd="1"), type="response")

sum(predict(fv.svylr, data.frame(porftvg=c("0","1","2","3","4","5","6","7", "8", "9"), year=0, agegroup="30-34", sex="1", qimd="1"), type="response"))

R2 = 1-fv.svylr$deviance/fv.svylr$null.deviance

predict(fv.svylr, data.frame(porftvg=c("6"), year=c(-10:0), agegroup="30-34", sex="1", qimd="1"), type="response")


cc <- predict(fv.svylr, data.frame(porftvg=c("0","1","2","3","4","5","6","7", "8", "9"), year=rep(0:9, each=10), agegroup="60-64", sex="1", qimd="1"), type="response")

split(cc, ceiling(seq_along(cc)/10))

cc <- matrix(cc,ncol=10, byrow=T)

findInterval(c(0, 0.01, 0.9, 1), cc)
findInterval(dice(10), split(cc, row(cc)))
ccc <- apply(cc, 1, findInterval, x=dice(100))


# Define function for F&V
pred.fv <- function(year, age, sex, qimd, lag = cvd.lag) {
    agegroup2 <- agegroup.part(age, -lag)
    sex <- factor(sex, 
                  levels = c(1,2), 
                  ordered = F)
    qimd <- factor(qimd, 
                   levels = c(1,2,3,4,5), 
                   ordered = T)
    cc <- predict(fv.svylr, 
                  data.frame(porftvg=c("0","1","2","3","4","5","6","7","8"), 
                             year=rep(year-lag, each=9), 
                             agegroup=rep(agegroup2, each=9), 
                             sex=rep(sex, each=9), 
                             qimd=rep(qimd, each=9)), 
                  type="response", se.fit=T)
    cc <- rtruncnorm(n, a = 0, b = 1, mean=cc[[1]], sd=cc[[2]])
    cc <- data.table(matrix(cc,ncol=9, byrow=T))
    cc[,d:= dice(.N)]
    cols <- names(cc)[1:9]
    for (k in cols) {
        set(cc, i=NULL, j=k, ifelse(cc[["d"]] < cc[[k]], 0,1))
    }
    return(cc[,V1+V2+V3+V4+V5+V6+V7+V8+V9])
}

# Define function for Fruit consumption
pred.fvrate <- function(fvgroup1, n=.N) {
    cc <- sample_n(fvrate[fvgroup == fvgroup1, list(frtpor, pct)],n, replace = T, weight = pct)[[1]]
    return(as.numeric(as.character(cc)))
}
# Usage example of above
#POP[, fvgroup := paste0(porftvg, agegroup , sex , qimd)]
#POP[between(age, ageL, ageH), test:= pred.fvrate(.BY[[1]], .N), by= fvgroup]
