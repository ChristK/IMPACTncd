#include <Rcpp.h>
#include <string>
using namespace Rcpp;

// [[Rcpp::export]]
List fvsum(
    IntegerVector porftvgcvdlag,
    IntegerVector porftvgcalag
) {
  
  int porftvgcvdlag0 = 0;
  int porftvgcvdlag1 = 0;
  int porftvgcvdlag2 = 0;
  int porftvgcvdlag3 = 0;
  int porftvgcvdlag4 = 0;
  int porftvgcvdlag5 = 0;
  int porftvgcvdlag6 = 0;
  int porftvgcvdlag7 = 0;
  int porftvgcvdlag8 = 0;
  
  int porftvgcalag0 = 0;
  int porftvgcalag1 = 0;
  int porftvgcalag2 = 0;
  int porftvgcalag3 = 0;
  int porftvgcalag4 = 0;
  int porftvgcalag5 = 0;
  int porftvgcalag6 = 0;
  int porftvgcalag7 = 0;
  int porftvgcalag8 = 0;
  
  int n1 = porftvgcvdlag.size();
  int n2 = porftvgcalag.size();
  
  for (int i = 0; i < n1; i++) {
    switch(porftvgcvdlag[i])
    {
    case 0 :
      porftvgcvdlag0 ++; 
      break;
    case 8:
      porftvgcvdlag8 ++; 
      break;
    case 1 :
      porftvgcvdlag1 ++; 
      break;
    case 2:
      porftvgcvdlag2 ++; 
      break;
    case 3:
      porftvgcvdlag3 ++; 
      break;
    case 4:
      porftvgcvdlag4 ++; 
      break;
    case 5:
      porftvgcvdlag5 ++; 
      break;
    case 6:
      porftvgcvdlag6 ++; 
      break;
    case 7:
      porftvgcvdlag7 ++; 
      break;
    }
  }
  
  for (int i = 0; i < n2; i++) {
    switch(porftvgcalag[i])
    {
    case 0 :
      porftvgcalag0 ++; 
      break;
    case 8:
      porftvgcalag8 ++; 
      break;
    case 1 :
      porftvgcalag1 ++; 
      break;
    case 2:
      porftvgcalag2 ++; 
      break;
    case 3:
      porftvgcalag3 ++; 
      break;
    case 4:
      porftvgcalag4 ++; 
      break;
    case 5:
      porftvgcalag5 ++; 
      break;
    case 6:
      porftvgcalag6 ++; 
      break;
    case 7:
      porftvgcalag7 ++; 
      break;
    }
  }
  
  return Rcpp::List::create(
    porftvgcvdlag0, porftvgcvdlag1, porftvgcvdlag2, porftvgcvdlag3,
    porftvgcvdlag4, porftvgcvdlag5, porftvgcvdlag6, porftvgcvdlag7, porftvgcvdlag8,
    porftvgcalag0, porftvgcalag1, porftvgcalag2, porftvgcalag3,
    porftvgcalag4, porftvgcalag5, porftvgcalag6, porftvgcalag7, porftvgcalag8
  );
}

// [[Rcpp::export]]
List smoksum(
    IntegerVector smokcvdlag,
    IntegerVector smokcalag,
    IntegerVector ets,
    IntegerVector diab,
    IntegerVector a30to06mcvdlag
) {
  
  int smokcvdlag1 = 0;
  int smokcvdlag4 = 0;
  
  int smokcalag1 = 0;
  int smokcalag4 = 0;
  
  int etstot = 0;
  int diabtot = 0;
  
  int pacvdlag0 = 0;
  int pacvdlag1 = 0;
  int pacvdlag2 = 0;
  int pacvdlag3 = 0;
  int pacvdlag4 = 0;
  int pacvdlag5 = 0;
  int pacvdlag6 = 0;
  int pacvdlag7 = 0;
  
  int n1 = smokcvdlag.size();
  int n2 = smokcalag.size();
  int n3 = ets.size();
  int n4 = diab.size();
  int n5 = a30to06mcvdlag.size();
  
  for (int i = 0; i < n1; i++) {
    switch(smokcvdlag[i])
    {
    case 1 :
      smokcvdlag1 ++; 
      break;
    case 4:
      smokcvdlag4 ++; 
      break;
    default :
      ;
    }
  }
  
  for (int i = 0; i < n2; i++) {
    switch(smokcalag[i])
    {
    case 1 :
      smokcalag1 ++; 
      break;
    case 4:
      smokcalag4 ++; 
      break;
    default :
      ;
    }
  }
  
  for (int i = 0; i < n3; i++) {
    if (ets[i] == 2) etstot++;
  }
  
  for (int i = 0; i < n4; i++) {
    if (diab[i] == 2) diabtot++;
  }
  
  for (int i = 0; i < n5; i++) {
    switch(a30to06mcvdlag[i])
    {
    case 0 :
      pacvdlag0 ++; 
      break;
    case 7:
      pacvdlag7 ++; 
      break;
    case 1 :
      pacvdlag1 ++; 
      break;
    case 2:
      pacvdlag2 ++; 
      break;
    case 3:
      pacvdlag3 ++; 
      break;
    case 4:
      pacvdlag4 ++; 
      break;
    case 5:
      pacvdlag5 ++; 
      break;
    case 6:
      pacvdlag6 ++; 
      break;
    }
  }
  return Rcpp::List::create(
    smokcvdlag1, smokcvdlag4, smokcalag1, smokcalag4,
    etstot, diabtot, 
    pacvdlag0, pacvdlag1, pacvdlag2, pacvdlag3, pacvdlag4, pacvdlag5,
    pacvdlag6, pacvdlag7
  );
}

// [[Rcpp::export]]
List meansd(NumericVector x) {
  x = na_omit(x);
  double meanm = 0;
  int n = x.size();
  for (int i = 0; i < n; i++) {
    meanm += x[i];
  }
  
  meanm /= n;
  
  double variance = 0;
  for(int i = 0; i < n; i++) 
  {
    variance += (x[i]-meanm)*(x[i]-meanm)/(n-1);
  }
  
  return Rcpp::List::create(meanm, sqrt(variance));
}

// [[Rcpp::export]]
double type2_raw(
    int age,
    int sex,
    int b_corticosteroids,
    int b_cvd,
    int b_treatedhyp,
    double bmi,
    int ethrisk,
    int fh_diab,
    int smoke_cat,
    double town,
    int surv
)
{
  if (sex == 1) {
    double survivor[16] = {
      0,
      0.998213708400726,
      0.996353209018707,
      0.994382798671722,
      0.992213606834412,
      0.989733397960663,
      0.987064540386200,
      0.984254062175751,
      0.981255292892456,
      0.977990627288818,
      0.974455237388611,
      0.970843732357025,
      0.967315018177032,
      0.963437378406525,
      0.959633111953735,
      0.955690681934357
    };
    
    /* The conditional arrays */
    
    double Iethrisk[10] = {
      0,
      0,
      1.2366090720913343000000000,
      1.4716746107789032000000000,
      1.8073235649498174000000000,
      1.2056055595936399000000000,
      0.6032369975938766100000000,
      0.9095436207452737300000000,
      0.9137604632927512900000000,
      0.7123719045990779500000000
    };
    double Ismoke[5] = {
      0,
      0.1618238582395977700000000,
      0.1902020385619117000000000,
      0.3210636179312467100000000,
      0.4140001301797494600000000
    };
    
    /* Applying the fractional polynomial transforms */
    /* (which includes scaling)                      */
    
    double dage = age;
    dage /= 10;
    double age_1 = log(dage);
    double age_2 = pow(dage, 3);
    double dbmi = bmi;
    dbmi /=10;
    double bmi_1 = pow(dbmi, 2);
    double bmi_2 = pow(dbmi, 3);
    
    /* Centring the continuous variables */
    
    age_1 += -1.496771812438965;
    age_2 += -89.149559020996094;
    bmi_1 += -6.832604885101318;
    bmi_2 += -17.859918594360352;
    town  += 0.132148191332817;
    
    /* Start of Sum */
    double a = 0;
    
    /* The conditional sums */
    
    a += Iethrisk[ethrisk];
    a += Ismoke[smoke_cat];
    
    /* Sum from continuous values */
    
    a += age_1 * 4.4205598323371680000000000;
    a += age_2 * -0.0041132238299394193000000;
    a += bmi_1 * 1.1169895991721528000000000;
    a += bmi_2 * -0.1793529530251269100000000;
    a += town * 0.0291530815903822650000000;
    
    /* Sum from boolean values */
    
    a += b_corticosteroids * 0.2059811979905692400000000;
    a += b_cvd * 0.3914728454990503100000000;
    a += b_treatedhyp * 0.5010787979849035100000000;
    a += fh_diab * 0.8385800403428993500000000;
    
    /* Sum from interaction terms */
    
    a += age_1 * bmi_1 * 0.5051031253768063500000000;
    a += age_1 * bmi_2 * -0.1375233635462656000000000;
    a += age_1 * fh_diab * -1.1463560542602569000000000;
    a += age_2 * bmi_1 * -0.0015800686452772700000000;
    a += age_2 * bmi_2 * 0.0003394090057824062300000;
    a += age_2 * fh_diab * 0.0018524160353981260000000;
    
    /* Calculate the score itself (0 to 1)*/
    double score = 1 - pow(survivor[surv], exp(a));
    return score;
  }
  
  if (sex == 2) {
    double survivor[16] = {
      0,
      0.998714804649353,
      0.997435748577118,
      0.996052920818329,
      0.994562506675720,
      0.992949724197388,
      0.991141080856323,
      0.989293158054352,
      0.987293541431427,
      0.985133886337280,
      0.982810735702515,
      0.980465650558472,
      0.978020071983337,
      0.975493073463440,
      0.972945988178253,
      0.970350146293640
    };
    
    /* The conditional arrays */
    
    double Iethrisk[10] = {
      0,
      0,
      1.2672136244963337000000000,
      1.4277605208830098000000000,
      1.8624060798103199000000000,
      1.2379988338989651000000000,
      0.4709034172907677900000000,
      0.3476400901703160500000000,
      1.1587283467731935000000000,
      0.7335499325010315100000000
    };
    double Ismoke[5] = {
      0,
      0.1012537024947505100000000,
      0.1915520564380613400000000,
      0.3091894136143333900000000,
      0.4646730392693820800000000
    };
    
    /* Applying the fractional polynomial transforms */
    /* (which includes scaling)                      */
    
    double dage = age;
    dage /= 10;
    double age_1 = pow(dage, 0.5);
    double age_2 = pow(dage, 3);
    double dbmi = bmi;
    dbmi /= 10;
    double bmi_1 = dbmi;
    double bmi_2 = pow(dbmi, 3);
    
    /* Centring the continuous variables */
    
    age_1 += -2.135220289230347;
    age_2 += -94.766799926757813;
    bmi_1 += -2.549620866775513;
    bmi_2 += -16.573980331420898;
    town  += 0.224075347185135;
    
    /* Start of Sum */
    double a = 0;
    
    /* The conditional sums */
    
    a += Iethrisk[ethrisk];
    a += Ismoke[smoke_cat];
    
    /* Sum from continuous values */
    
    a += age_1 * 4.3848331212989669000000000;
    a += age_2 * -0.0049763964406541149000000;
    a += bmi_1 * 3.3753336326064329000000000;
    a += bmi_2 * -0.0631628488667318330000000;
    a += town  * 0.0432726992998635970000000;
    
    /* Sum from boolean values */
    
    a += b_corticosteroids * 0.2681990966241487000000000;
    a += b_cvd * 0.3596176830984252900000000;
    a += b_treatedhyp * 0.5314598436974725700000000;
    a += fh_diab * 0.7315358845837640600000000;
    
    /* Sum from interaction terms */
    
    a += age_1 * bmi_1 * 1.3037832873997990000000000;
    a += age_1 * bmi_2 * -0.0708293717769046120000000;
    a += age_1 * fh_diab * -0.7968266815834251800000000;
    a += age_2 * bmi_1 * -0.0067725323761278549000000;
    a += age_2 * bmi_2 * 0.0002374980728666116700000;
    a += age_2 * fh_diab * 0.0017048228889394394000000;
    
    /* Calculate the score itself */
    double score = 1 - pow(survivor[surv], exp(a));
    return score;
  }
  return 0;
}

// Vectorise above
// [[Rcpp::export]]
NumericVector QDrisk(
    IntegerVector age,
    IntegerVector sex,
    IntegerVector b_corticosteroids,
    IntegerVector b_cvd,
    IntegerVector b_treatedhyp,
    NumericVector bmi,
    IntegerVector ethrisk,
    IntegerVector fh_diab,
    IntegerVector smoke_cat,
    NumericVector town,
    IntegerVector surv
)
{
  int n = age.size();
  NumericVector out(n);
  
  for (int i = 0; i < n; i++) {
    out[i] = type2_raw(age[i], sex[i], b_corticosteroids[i],
                       b_cvd[i], b_treatedhyp[i], bmi[i],
                       ethrisk[i], fh_diab[i], smoke_cat[i],
                       town[i], surv[i]);
  }
  
  return out;
}
// 
// 
// // [[Rcpp::export]]
// std::map<double, int> tableCsorted(NumericVector x) {
//   std::map<double, int> counts;
//   
//   int n = x.size();
//   for (int i = 0; i < n; i++) {
//     counts[x[i]]++;
//   }
//   
//   return counts;
// }
// 
// // [[Rcpp::plugins(cpp11)]]
// #include <unordered_map>
// // [[Rcpp::export]]
// std::unordered_map<int, int> tableCfast(IntegerVector x) {
//   std::unordered_map<int, int> counts;
//   
//   int n = x.size();
//   for (int i = 0; i < n; i++) {
//     counts[x[i]]++;
//   }
//   
//   return counts;
// }
