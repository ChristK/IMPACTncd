# INTERSALT FORMULA
require(pryr)

Na24.men <- function(n, Na, cre, K, bmi, age) {
  m.interc %<a-% rnorm(n, 25.46, 16.63)
  m.Na %<a-% rnorm(n, 0.46, 0.02) # mmol/L
  m.cre %<a-% rnorm(n, -2.75, 0.22)
  m.K %<a-% rnorm(n, -0.13, 0.04)
  m.bmi %<a-% rnorm(n, 4.10, 0.31)
  m.age %<a-% rnorm(n, 0.26, 0.78)
  m.age2 %<a-% rnorm(n, 0, 0.01)
  m.region %<a-% rnorm(n, 23.17, 4.51) # Northern Europe
  return(m.interc + m.region + 
           m.Na*Na + m.cre*cre + m.K*K + m.bmi*bmi + m.age * age + m.age2 * age^2)
}

Na24.women <- function(n, Na, cre, K, bmi, age) {
  w.interc %<a-% rnorm(n, 5.07, 13.42)
  w.Na %<a-% rnorm(n, 0.34, 0.02) # mmol/L
  w.cre %<a-% rnorm(n, -2.16, 0.20)
  w.K %<a-% rnorm(n, -0.09, 0.03)
  w.bmi %<a-% rnorm(n, 2.39, 0.20)
  w.age %<a-% rnorm(n, 2.35, 0.65)
  w.age2 %<a-% rnorm(n, -0.03, 0.01)
  w.region %<a-% rnorm(n, 15.73, 3.62) # Northern Europe
  return(w.interc + w.region + 
           w.Na*Na + w.cre*cre + w.K*K + w.bmi*bmi + w.age * age + w.age2 * age^2)
}
