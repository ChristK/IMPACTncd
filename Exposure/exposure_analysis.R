require(data.table)
require(gamlss)
require(gamlss.tr)
require(cowplot)
require(ggridges)
require(Epi)
clonedt <-
    function(DT, times = numberofiterations) {
      xx <- key(DT)
      l <- rep(list(DT), times)
      return(setkeyv(rbindlist(l, idcol = T), xx))
    }


salt <- fread("./Exposure/24h_urine_data_extra.csv")
salt[, unique(sort(time))]
tt <- data.table(time = salt[, unique(sort(time))], year = c(1, 6, 8.5, 9.5, 10.5, 11.5, 12.5, 13.5, 14.5)) # 2000 = year 0
salt[tt, on = "time", year := i.year]
salt <- salt[incomplete == 0] # remove incomplete/extra participants
salt <- salt[rp == 0] # remove incomplete/extra participants
salt[, resp_deal := 0L]
salt[year + 2000 > 2010, resp_deal := 1L]
salt[, fsa := 0L]
salt[between(year, 3, 10), fsa := 1L]

salt[, resp_deal := factor(resp_deal)]
salt[, fsa := factor(fsa)]
# fwrite(salt, "./Lagtimes/salt_data.csv")

# ggplot(salt, aes(salt, colour = time)) +
#   geom_density() +
#   geom_vline(data = salt[, mean(salt, trim = 0.1), keyby = .(sex, time)], aes(xintercept = V1, colour = time),
#              linetype = "dashed", size = 0.5) +
#   facet_wrap(time~sex, ncol = 2) +
#   xlim(0, 20)
# salt[sex == "Female", fitDist(salt, type = "realplus")]
# salt[sex == "Female", fit.cont(salt)] # Gamma
# salt[sex == "Male", fit.cont(salt)] # Gamma
# salt[sex == "Female" & time == "2000/1", fit.cont(salt)] # Gamma
# salt[sex == "Male" & time == "2000/1", fit.cont(salt)] # Gamma
# salt[sex == "Female" & time == "2005/6", fit.cont(salt)] # Gamma
# salt[sex == "Male" & time == "2005/6", fit.cont(salt)] # Gamma

its1 <- glm(salt ~ year:resp_deal + age + sex, data = salt, family = Gamma(link = "identity")) 
ci.lin(its1)
#list(exp(coef(its1)), exp(confint(its1)))
summary(its1)
plot(its1)
saltpre <- salt[year <= 10, ]
saltpost <- salt[year >= 8, ] # not a typo we need a point before responsibility deal and all after


# return the data frame to the scenario including the intervention
datanewmen <- data.frame(year = 0:25, resp_deal = factor(c(rep(0, 11), rep(1, 15))),
                      age = mean(salt$age), sex = factor("Male", levels = c("Female", "Male")))
datanewwomen <- data.frame(year = 0:25, resp_deal = factor(c(rep(0, 11), rep(1, 15))),
                         age = mean(salt$age), sex = factor("Female", levels = c("Female", "Male")))
datanewmen2 <- data.frame(year = 0:25, resp_deal = factor(c(rep(0, 11), rep(0, 15))),
                         age = mean(salt$age), sex = factor("Male", levels = c("Female", "Male")))
datanewwomen2 <- data.frame(year = 0:25, resp_deal = factor(c(rep(0, 11), rep(0, 15))),
                           age = mean(salt$age), sex = factor("Female", levels = c("Female", "Male")))

predmen <- predict(its1, type = "response", datanewmen)
predwomen <- predict(its1, type = "response", datanewwomen)
predmen2 <- predict(its1, type = "response", datanewmen2)
predwomen2 <- predict(its1, type = "response", datanewwomen2)

plot(NULL, xlim = c(0,24), ylim = c(0,12),xlab = "Year", ylab = "Mean salt intake (g/d)",
     bty = "l", xaxt="n")
rect(11, 0, 24, 12,col = grey(0.9),border=F)
axis(1,at=0:24,labels=F)
axis(1,at=c(0, 11, 15, 24),tick=F,labels=c(2000, 2011, 2015, 2024))
points(salt[sex == "Male", mean(salt), keyby = year],cex = sqrt(salt[sex == "Male", .N, keyby = year]$N)/20, col=4)
lines(0:25,predmen,col = 4)
lines(0:25, predmen2, lty="dashed", col = 4)
points(salt[sex == "Female", mean(salt), keyby = year],cex = sqrt(salt[sex == "Female", .N, keyby = year]$N)/20, col=2)
lines(0:25,predwomen,col=2)
lines(0:25,predwomen2, lty="dashed", col=2)

# calibration for log(year + i)
newdata <- data.frame(year = 20, resp_deal = factor(0, levels = 0:1),
                            age = mean(salt$age), sex = factor(c("Female", "Male"), levels = c("Female", "Male")))
its <- glm(salt ~ year:resp_deal + age + sex, data = salt, family = Gamma("identity")) 
mean(predict(its, type = "response", newdata)) # mean salt with linear in 2020 ~6.46, 2025~~5.76

i <- 0
predmen <- 10
while (mean(predmen) > 7) {
  i <- i + 1
  its2 <- glm(salt ~ log(year + i):resp_deal + age + sex, data = salt, family = Gamma("identity")) 
  predmen <- predict(its2, type = "response", newdata)
  print(paste(i, signif(mean(predmen))))
}
mean(predict(its2, type = "response", newdata)) # mean salt with linear in 2020 ~6.99, 2025~~6.63

predmen <- predict(its1, type = "response", datanewmen)
predwomen <- predict(its1, type = "response", datanewwomen)
predmen2 <- predict(its2, type = "response", datanewmen2)
predwomen2 <- predict(its2, type = "response", datanewwomen2)
predmen3 <- predict(its1, type = "response", datanewmen2)
predwomen3 <- predict(its1, type = "response", datanewwomen2)

plot(NULL, xlim = c(0,25), ylim = c(0,12),xlab = "Year", ylab = "Mean salt intake (g/d)",
     bty = "l", xaxt="n")
rect(11, 0, 25, 12,col = grey(0.9),border=F)
axis(1,at=0:25,labels=F)
axis(1,at=c(0, 11, 20, 25),tick=F,labels=c(2000, 2011, 2020, 2025))
points(salt[sex == "Male", mean(salt), keyby = year],cex = sqrt(salt[sex == "Male", .N, keyby = year]$N)/20, col=4)
lines(0:25,predmen,col = 4)
lines(0:25, predmen2, lty="dashed", col = 4)
points(salt[sex == "Female", mean(salt), keyby = year],cex = sqrt(salt[sex == "Female", .N, keyby = year]$N)/20, col = 2)
lines(0:25,predwomen,col = 2)
lines(0:25,predwomen2, lty="dashed", col = 2)
lines(0:25, predmen3, lty="dotted", col = 4)
lines(0:25,predwomen3, lty="dotted", col = 2)


# Create truncated version of the distributions
gen.trun(par = min(salt$salt),family = "NO", type = "left")
gen.trun(par = min(salt$salt),family = "GU", type = "left")
gen.trun(par = min(salt$salt),family = "RG", type = "left")
gen.trun(par = min(salt$salt),family = "LO", type = "left")
gen.trun(par = min(salt$salt),family = "NET", type = "left")
gen.trun(par = min(salt$salt),family = "TF", type = "left")
gen.trun(par = min(salt$salt),family = "TF2", type = "left")
gen.trun(par = min(salt$salt),family = "PE", type = "left")
gen.trun(par = min(salt$salt),family = "PE2", type = "left")
gen.trun(par = min(salt$salt),family = "SN1", type = "left")
gen.trun(par = min(salt$salt),family = "SN2", type = "left")
gen.trun(par = min(salt$salt),family = "exGAUS", type = "left")
gen.trun(par = min(salt$salt),family = "SHASH", type = "left")
gen.trun(par = min(salt$salt),family = "SHASHo", type = "left")
gen.trun(par = min(salt$salt),family = "SHASHo2", type = "left")
gen.trun(par = min(salt$salt),family = "EGB2", type = "left")
gen.trun(par = min(salt$salt),family = "JSU", type = "left")
gen.trun(par = min(salt$salt),family = "JSUo", type = "left")
gen.trun(par = min(salt$salt),family = "SEP1", type = "left")
gen.trun(par = min(salt$salt),family = "SEP2", type = "left")
gen.trun(par = min(salt$salt),family = "SEP3", type = "left")
gen.trun(par = min(salt$salt),family = "SEP4", type = "left")
gen.trun(par = min(salt$salt),family = "ST1", type = "left")
gen.trun(par = min(salt$salt),family = "ST2", type = "left")
gen.trun(par = min(salt$salt),family = "ST3", type = "left")
gen.trun(par = min(salt$salt),family = "ST4", type = "left")
gen.trun(par = min(salt$salt),family = "ST5", type = "left")
gen.trun(par = min(salt$salt),family = "SST", type = "left")
gen.trun(par = min(salt$salt),family = "GT", type = "left")
gen.trun(par = min(salt$salt),family = "EXP", type = "left")
gen.trun(par = min(salt$salt),family = "GA", type = "left")
gen.trun(par = min(salt$salt),family = "IG", type = "left")
gen.trun(par = min(salt$salt),family = "LOGNO", type = "left")
gen.trun(par = min(salt$salt),family = "LOGNO2", type = "left")
gen.trun(par = min(salt$salt),family = "WEI", type = "left")
gen.trun(par = min(salt$salt),family = "WEI2", type = "left")
gen.trun(par = min(salt$salt),family = "WEI3", type = "left")
gen.trun(par = min(salt$salt),family = "IGAMMA", type = "left")
gen.trun(par = min(salt$salt),family = "PARETO2", type = "left")
gen.trun(par = min(salt$salt),family = "PARETO2o", type = "left")
gen.trun(par = min(salt$salt),family = "GP", type = "left")
gen.trun(par = min(salt$salt),family = "BCCG", type = "left")
gen.trun(par = min(salt$salt),family = "BCCGo", type = "left")
gen.trun(par = min(salt$salt),family = "exGAUS", type = "left")
gen.trun(par = min(salt$salt),family = "GIG", type = "left")
gen.trun(par = min(salt$salt),family = "LNO", type = "left")
gen.trun(par = min(salt$salt),family = "BCTo", type = "left")
gen.trun(par = min(salt$salt),family = "BCT", type = "left")
gen.trun(par = min(salt$salt),family = "BCPEo", type = "left")
gen.trun(par = min(salt$salt),family = "BCPE", type = "left")
gen.trun(par = min(salt$salt),family = "GB2", type = "left")

distr_nam <- c("NO", "GU", "RG" ,"LO", "NET", "TF", "TF2", "PE","PE2", "SN1", "SN2", "exGAUS", "SHASH", "SHASHo","SHASHo2", "EGB2", "JSU", "JSUo", "SEP1", "SEP2", "SEP3", "SEP4", "ST1", "ST2", "ST3", "ST4", "ST5", "SST", "GT", "EXP", "GA","IG","LOGNO", "LOGNO2","WEI", "WEI2", "WEI3", "IGAMMA","PARETO2", "PARETO2o", "GP", "BCCG", "BCCGo", "exGAUS", "GG", "GIG", "LNO","BCTo", "BCT", "BCPEo", "BCPE", "GB2")
distr_nam_tr <- paste0(distr_nam, "tr")
marg_distr <- fitDist(salt, data = salt, type = "realplus", extra = distr_nam_tr, trace = T, try.gamlss = T, parallel = "multicore", ncpus = 12)
marg_distr 

ttpre <- gamlss(salt~log(year + 22):resp_deal + pb(age) + sex, 
                    ~log(year + 22):resp_deal + pb(age) + sex, 
                    ~log(year + 22):resp_deal + pb(age) + sex, 
                    ~log(year + 22):resp_deal + pb(age) + sex, 
       family = SHASHtr,
       data = salt, method = mixed(5, 50))
distr_nam_tr_short <- c("SHASHtr", "RGtr") # selected based on marg_distr
t1 <- chooseDist(ttpre, type = "extra", extra = distr_nam_tr_short) #, parallel = "multicore", ncpus = 12
getOrder(t1,3) # based on GAIC & BIC RGtr fits best


itsgamlss_lin <- gamlss(salt~ year:resp_deal + pb(age) + sex, 
                        ~year:resp_deal + pb(age) + sex, 
                    family = RGtr,
                    data = salt, method = mixed(5, 50))
summary(itsgamlss_lin)
term.plot(itsgamlss_lin, pages = 1, ask = FALSE, what = c("mu"))
term.plot(itsgamlss_lin, pages = 1, ask = FALSE, what = c("sigma"))
wp(itsgamlss_lin, ylim.all = 0.5)
drop1(itsgamlss_lin)
saveRDS(itsgamlss_lin, "./Lagtimes/salt.itsgamlss_lin.rds")

itsgamlss_log <- gamlss(salt~log(year + 15):resp_deal + pb(age) + sex, 
                            ~log(year + 15):resp_deal + pb(age) + sex, 
                        family = RGtr,
                        data = salt, method = mixed(5, 50))
summary(itsgamlss_log)
term.plot(itsgamlss_log, pages = 1, ask = FALSE, what = c("mu"))
term.plot(itsgamlss_log, pages = 1, ask = FALSE, what = c("sigma"))
wp(itsgamlss_log, ylim.all = 0.5)
drop1(itsgamlss_log)
saveRDS(itsgamlss_log, "./Lagtimes/salt.itsgamlss_log.rds")

# Validation plot ---------------------------------------------
itsgamlss_lin <- readRDS("./Lagtimes/salt.itsgamlss_lin.rds")
itsgamlss_log <- readRDS("./Lagtimes/salt.itsgamlss_log.rds")
salt <- fread("./Lagtimes/salt_data.csv")
gen.trun(par = min(salt$salt),family = "RG", type = "left")
salt[, type := "Observed"]
AIC(itsgamlss_lin, itsgamlss_log)
AIC(itsgamlss_lin, itsgamlss_log, k = log(nrow(salt)))
  
salt3 <- copy(salt)
salt3[, c("y", "mu", "sigma") := predictAll(itsgamlss_lin, type = "response")]
salt3[, type := "Modelled"]
salt3 <- clonedt(salt3, 2000)
salt3[, salt := rRGtr(.N, mu, sigma)]
salt3 <- rbind(salt, salt3, use.names = TRUE, fill = TRUE)

salt4 <- copy(salt)
salt4[, c("y", "mu", "sigma") := predictAll(itsgamlss_log, type = "response")]
salt4[, type := "Modelled"]
salt4 <- clonedt(salt4, 2000)
salt4[, salt := rRGtr(.N, mu, sigma)]
salt4 <- rbind(salt, salt4, use.names = TRUE, fill = TRUE)


gg <- ggplot(salt3, aes(salt, colour = type)) +
  geom_density() +
  facet_wrap(time~sex, ncol = 2) +
  ylim(0, 0.18) +
  scale_x_continuous("Salt (g/d)", breaks = c(0, 10, 20, 30), limits = c(0, 30)) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(
          margin = margin(r = 24, unit = "pt")))
cowplot::ggsave("./Exposure/itsgamlss_lin_validation.tiff", gg, dpi = 600, width = 8, height = 6,
                units = "cm", compression = "lzw", scale = 3)

gg <- ggplot(salt4, aes(salt, colour = type)) +
  geom_density() +
  facet_wrap(time~sex, ncol = 2) +
  ylim(0, 0.18)+
  scale_x_continuous("Salt (g/d)", breaks = c(0, 10, 20, 30), limits = c(0, 30)) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(
          margin = margin(r = 24, unit = "pt")))
cowplot::ggsave("./Exposure/itsgamlss_log_validation.tiff", gg, dpi = 600, width = 8, height = 6,
                units = "cm", compression = "lzw", scale = 3)

# Joyplot -----------------------------------------------------
pop <- fread("./Population/joyplotpop.csv") # England population 2001 - 2030 ONS
pop <- melt(pop, 1:2, variable.name = "Year", value.name = "Pop")
pop[, Pop := round(Pop/1e3)]
pop[, Year := as.numeric(as.character(Year)) - 2000 + 0.5]
setnames(pop, tolower(names(pop)))
pop <- pop[year >= 3]
pop[, resp_deal := factor(0, levels = 0:1)]
pop[, c("mu", "sigma") := predictAll(itsgamlss_lin, newdata = .SD, type = "response"), .SDcols = c("age", "sex", "year", "resp_deal")]
pop[, resp_deal := factor(1, levels = 0:1)]
pop[, c("mu2", "sigma2") := predictAll(itsgamlss_lin, newdata = .SD, type = "response"), .SDcols = c("age", "sex", "year", "resp_deal")]
pop[, resp_deal := NULL]

# pop[, predictAll(itsgamlss_lin, newdata = .SD, type = "response", se.fit = T), .SDcols = c("age", "sex", "year", "resp_deal")]

pop <- pop[rep(1:.N, pop)]
pop[, Baseline := rRGtr(.N, mu, sigma)]
pop[, Responsibility_Deal := rRGtr(.N, mu2, sigma2)]
pop[(year + 2000) < 2011, Responsibility_Deal := Baseline]
pop[, c("pop", "mu", "mu2", "sigma", "sigma2") := NULL]
pop <- melt(pop, 1:3)

pop[, Year := year + 2000 - 0.5]
pop[Year > 2010, mean(value), key = .(variable)]
pop[Year >= 2010, mean(value), by = .(Year, variable)]

gg <- ggplot(pop[Year <= 2020], aes(y = Year)) +
  geom_density_ridges(aes(x = value, fill = paste(Year, variable)), 
                      alpha = .8, color = "white", from = 0, to = 25) +
  labs(x = "Salt (g/d)",
       y = "Year",
       title = "FSA led vs Responsibility Deal salt distribution projections",
       subtitle = "Standardised to age and sex ONS population estimates and projections",
       caption = "GAMLSS modelling | Source: Sodium Surveys 2001-2014") +
  scale_y_reverse(breaks = c(2003, 2010, 2020, 2030), expand = c(0.01, 0)) +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12, 15), expand = c(0.01, 0)) +
  scale_fill_cyclical(breaks = c("2003 Baseline", "2003 Responsibility_Deal"),
                      labels = c( `2003 Baseline` = "FSA Led", `2003 Responsibility_Deal` = "Responsibility Deal"),
                      values = c("#ff0000", "#0000ff", "#ff8080", "#8080ff"), 
                      name = "", guide = "legend") +
  theme_ridges(grid = FALSE)
cowplot::ggsave("./Exposure/gamlss_projections_lin.tiff", gg, dpi = 600, width = 210, height = 297,
              units = "mm", compression = "lzw")




pop <- fread("./Population/joyplotpop.csv") # England population 2001 - 2030 ONS
pop <- melt(pop, 1:2, variable.name = "Year", value.name = "Pop")
pop[, Pop := round(Pop/1e3)]
pop[, Year := as.numeric(as.character(Year)) - 2000 + 0.5]
setnames(pop, tolower(names(pop)))
pop <- pop[year >= 3]
pop[, resp_deal := factor(0, levels = 0:1)]
pop[, c("mu", "sigma") := predictAll(itsgamlss_log, newdata = .SD, type = "response"), .SDcols = c("age", "sex", "year", "resp_deal")]
pop[, resp_deal := factor(1, levels = 0:1)]
pop[, c("mu2", "sigma2") := predictAll(itsgamlss_log, newdata = .SD, type = "response"), .SDcols = c("age", "sex", "year", "resp_deal")]
pop[, resp_deal := NULL]

# pop[, predictAll(itsgamlss_log, newdata = .SD, type = "response", se.fit = T), .SDcols = c("age", "sex", "year", "resp_deal")]

pop <- pop[rep(1:.N, pop)]
pop[, Baseline := rRGtr(.N, mu, sigma)]
pop[, Responsibility_Deal := rRGtr(.N, mu2, sigma2)]
pop[(year + 2000) < 2011, Responsibility_Deal := Baseline]
pop[, c("pop", "mu", "mu2", "sigma", "sigma2") := NULL]
pop <- melt(pop, 1:3)

pop[, Year := year + 2000 - 0.5]
pop[Year > 2010, mean(value), key = .(variable)]
pop[Year >= 2010, mean(value), by = .(Year, variable)]

gg <- ggplot(pop[Year <= 2020], aes(y = Year)) +
  geom_density_ridges(aes(x = value, fill = paste(Year, variable)), 
                      alpha = .8, color = "white", from = 0, to = 25) +
  labs(x = "Salt (g/d)",
       y = "Year",
       title = "FSA led vs Responsibility Deal projections",
       subtitle = "Standardised to age and sex ONS population estimates and projections",
       caption = "GAMLSS modelling (calibrated) | Source: Sodium Surveys 2001-2014") +
  scale_y_reverse(breaks = c(2003, 2010, 2020, 2030), expand = c(0.01, 0)) +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12, 15), expand = c(0.01, 0)) +
  scale_fill_cyclical(breaks = c("2003 Baseline", "2003 Responsibility_Deal"),
                      labels = c( `2003 Baseline` = "FSA Led", `2003 Responsibility_Deal` = "Responsibility Deal"),
                      values = c("#ff0000", "#0000ff", "#ff8080", "#8080ff"), 
                      name = "", guide = "legend") +
  theme_ridges(grid = FALSE)
ggsave("./Exposure/gamlss_projections_log.tiff", gg, dpi = 600, width = 210, height = 297, units = "mm", compression = "lzw")

