length(unique(POPinit[,group]))
length(unique(POP[,group]))

dplyr::setequal(unique(POPinit[,group]), unique(POP[,group]))
outersect(unique(POP[,group]), unique(POPinit[,group]))
nrow(POP[group %in% outersect(unique(POP[,group]), unique(POPinit[,group])),])
Reduce(intersect, list(unique(POPinit[,group]),unique(POP[,group])))

length(unique(POPinit[,qimd]))
length(unique(POP[,qimd]))

length(unique(POPinit[,sex]))
length(unique(POP[,sex]))

length(unique(POPinit[,agegroup]))
length(unique(POP[,agegroup]))

length(unique(POPinit[,bmivalCat]))
length(unique(POP[,bmivalCat]))

summary(POPinit[,group])
summary(POP[,group])

summary(POPinit[,qimd])
summary(POP[,qimd])

summary(POPinit[,sex])
summary(POP[,sex])

summary(POPinit[,agegroup])
summary(POP[,agegroup])

summary(POPinit[,bmivalCat])
summary(POP[,bmivalCat])

unique(POPinit[,group])
unique(POP[,group])