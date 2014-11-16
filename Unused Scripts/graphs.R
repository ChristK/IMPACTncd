# density plots of continuous factors
qplot(x=bmival, y=NULL, col = agegroup, data=SPOP2011, geom = "density", facets = .~sex)
qplot(x=omsysval, y=NULL, col = agegroup, data=SPOP2011, geom = "density", facets = .~sex)
qplot(x=cholval, y=NULL, col = agegroup, data=SPOP2011, geom = "density", facets = .~sex)

