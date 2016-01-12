/* IMPACTncd: A decision support tool for primary prevention of NCDs
 Copyright (C) 2015  Chris Kypridemos
 
 IMPACTncd is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 3 of the License, or
 (at your option) any later version.
 
 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.
 
 You should have received a copy of the GNU General Public License
 along with this program; if not, see <http://www.gnu.org/licenses/>
 or write to the Free Software Foundation, Inc., 51 Franklin Street,
 Fifth Floor, Boston, MA 02110-1301  USA.*/

#include <Rcpp.h>
#include <string>
//#include <math.h>
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

// The following code was adapted from QDiabetes-2013 (http://qdiabetes.org, http://svn.clinrisk.co.uk/opensource/qdiabetes)
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
  if (bmi < 20) bmi = 20;
  if (bmi > 40) bmi = 40;
  
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

// The following code was adapted from QRISK2-2014 (http://qrisk.org, original sources at http://svn.clinrisk.co.uk/opensource/qrisk2).
// [[Rcpp::export]]
double cvd_raw(
    int age,             // 25:84
    int sex,             // 1, 2
    int b_AF,            // 0, 1
    int b_ra,            // 0, 1
    int b_renal,         // 0, 1
    int b_treatedhyp,    // 0, 1
    int b_type1,         // 0, 1
    int b_type2,         // 0, 1
    double bmi,          // 20:40
    int ethrisk,         // 1:9
    int fh_cvd,          // 0, 1
    double rati,         // 1 - 12
    double sbp,          // 70 - 210
    int smoke_cat,       // 0:4
    double town          // -7 to 11
)
{
  if (bmi < 20) bmi = 20;
  if (bmi > 40) bmi = 40;
  
  if (rati < 1) rati = 1;
  if (rati > 12) rati = 12;
  
  if (sbp < 70) sbp = 70;
  if (sbp > 210) sbp = 210;
  
  b_type2 -= 1; // diabtotr in my model is factor 1,2 and converted to int 1, 2. I need 0,1 
  
  if (sex == 1) {
    double survivor[16] = {
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0.977699398994446,
      0,
      0,
      0,
      0,
      0
    };
    
    /* The conditional arrays */
    
    double Iethrisk[10] = {
      0,
      0,
      0.3567133647493443400000000,
      0.5369559608176189800000000,
      0.5190878419529624300000000,
      0.2182992106490147000000000,
      -0.3474174705898491800000000,
      -0.3674730037922803700000000,
      -0.3749664891426142700000000,
      -0.1926947742531604500000000
    };
    double Ismoke[5] = {
      0,
      0.2784649664157046200000000,
      0.6067834395168959500000000,
      0.7103835060989258700000000,
      0.8626172339181202900000000
    };
    
    /* Applying the fractional polynomial transforms */
    /* (which includes scaling)                      */
    
    double dage = age;
    dage=dage/10;
    double age_1 = pow(dage,-1);
    double age_2 = pow(dage,2);
    double dbmi = bmi;
    dbmi=dbmi/10;
    double bmi_1 = pow(dbmi,-2);
    double bmi_2 = pow(dbmi,-2)*log(dbmi);
    
    /* Centring the continuous variables */
    
    age_1 = age_1 - 0.232008963823318;
    age_2 = age_2 - 18.577636718750000;
    bmi_1 = bmi_1 - 0.146408438682556;
    bmi_2 = bmi_2 - 0.140651300549507;
    rati  = rati - 4.377167701721191;
    sbp   = sbp - 131.038314819335940;
    town  = town - 0.151332527399063;
    
    /* Start of Sum */
    double a=0;
    
    /* The conditional sums */
    
    a += Iethrisk[ethrisk];
    a += Ismoke[smoke_cat];
    
    /* Sum from continuous values */
    
    a += age_1 * -17.6225543381945610000000000;
    a += age_2 * 0.0241873189298273640000000;
    a += bmi_1 * 1.7320282704272665000000000;
    a += bmi_2 * -7.2311754066699754000000000;
    a += rati * 0.1751387974012235100000000;
    a += sbp * 0.0101676305179196900000000;
    a += town * 0.0298177271496720960000000;
    
    /* Sum from boolean values */
    
    a += b_AF * 0.9890997526189402300000000;
    a += b_ra * 0.2541886209118611200000000;
    a += b_renal * 0.7949789230438320000000000;
    a += b_treatedhyp * 0.6229359479868044100000000;
    a += b_type1 * 1.3330353321463930000000000;
    a += b_type2 * 0.9372956828151940400000000;
    a += fh_cvd * 0.5923353736582422900000000;
    
    /* Sum from interaction terms */
    
    a += age_1 * (smoke_cat==1) * 0.9243747443632776000000000;
    a += age_1 * (smoke_cat==2) * 1.9597527500081284000000000;
    a += age_1 * (smoke_cat==3) * 2.9993544847631153000000000;
    a += age_1 * (smoke_cat==4) * 5.0370735254768100000000000;
    a += age_1 * b_AF * 8.2354205455482727000000000;
    a += age_1 * b_renal * -3.9747389951976779000000000;
    a += age_1 * b_treatedhyp * 7.8737743159167728000000000;
    a += age_1 * b_type1 * 5.4238504414460937000000000;
    a += age_1 * b_type2 * 5.0624161806530141000000000;
    a += age_1 * bmi_1 * 33.5437525167394240000000000;
    a += age_1 * bmi_2 * -129.9766738257203800000000000;
    a += age_1 * fh_cvd * 1.9279963874659789000000000;
    a += age_1 * sbp * 0.0523440892175620200000000;
    a += age_1 * town * -0.1730588074963540200000000;
    a += age_2 * (smoke_cat==1) * -0.0034466074038854394000000;
    a += age_2 * (smoke_cat==2) * -0.0050703431499952954000000;
    a += age_2 * (smoke_cat==3) * 0.0003216059799916440800000;
    a += age_2 * (smoke_cat==4) * 0.0031312537144240087000000;
    a += age_2 * b_AF * 0.0073291937255039966000000;
    a += age_2 * b_renal * -0.0261557073286531780000000;
    a += age_2 * b_treatedhyp * 0.0085556382622618121000000;
    a += age_2 * b_type1 * 0.0020586479482670723000000;
    a += age_2 * b_type2 * -0.0002328590770854172900000;
    a += age_2 * bmi_1 * 0.0811847212080794990000000;
    a += age_2 * bmi_2 * -0.2558919068850948300000000;
    a += age_2 * fh_cvd * -0.0056729073729663406000000;
    a += age_2 * sbp * -0.0000536584257307299330000;
    a += age_2 * town * -0.0010763305052605857000000;
    
    /* Calculate the score itself */
    double score = 1 - pow(survivor[10], exp(a));
    return score;
  }
  
  if (sex == 2) {
    double survivor[16] = {
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0.988948762416840,
      0,
      0,
      0,
      0,
      0
    };
    
    /* The conditional arrays */
    
    double Iethrisk[10] = {
      0,
      0,
      0.2671958047902151500000000,
      0.7147534261793343500000000,
      0.3702894474455115700000000,
      0.2073797362620235500000000,
      -0.1744149722741736900000000,
      -0.3271878654368842200000000,
      -0.2200617876129250500000000,
      -0.2090388032466696800000000
    };
    double Ismoke[5] = {
      0,
      0.1947480856528854800000000,
      0.6229400520450627500000000,
      0.7405819891143352600000000,
      0.9134392684576959600000000
    };
    
    /* Applying the fractional polynomial transforms */
    /* (which includes scaling)                      */
    
    double dage = age;
    dage=dage/10;
    double age_1 = pow(dage,.5);
    double age_2 = dage;
    double dbmi = bmi;
    dbmi=dbmi/10;
    double bmi_1 = pow(dbmi,-2);
    double bmi_2 = pow(dbmi,-2)*log(dbmi);
    
    /* Centring the continuous variables */
    
    age_1 = age_1 - 2.099778413772583;
    age_2 = age_2 - 4.409069538116455;
    bmi_1 = bmi_1 - 0.154046609997749;
    bmi_2 = bmi_2 - 0.144072100520134;
    rati  = rati - 3.554229259490967;
    sbp   = sbp - 125.773628234863280;
    town  = town - 0.032508373260498;
    
    /* Start of Sum */
    double a=0;
    
    /* The conditional sums */
    
    a += Iethrisk[ethrisk];
    a += Ismoke[smoke_cat];
    
    /* Sum from continuous values */
    
    a += age_1 * 3.8734583855051343000000000;
    a += age_2 * 0.1346634304478384600000000;
    a += bmi_1 * -0.1557872403333062600000000;
    a += bmi_2 * -3.7727795566691125000000000;
    a += rati * 0.1525695208919679600000000;
    a += sbp * 0.0132165300119653560000000;
    a += town * 0.0643647529864017080000000;
    
    /* Sum from boolean values */
    
    a += b_AF * 1.4235421148946676000000000;
    a += b_ra * 0.3021462511553648100000000;
    a += b_renal * 0.8614743039721416400000000;
    a += b_treatedhyp * 0.5889355458733703800000000;
    a += b_type1 * 1.6684783657502795000000000;
    a += b_type2 * 1.1350165062510138000000000;
    a += fh_cvd * 0.5133972775738673300000000;
    
    /* Sum from interaction terms */
    
    a += age_1 * (smoke_cat==1) * 0.6891139747579299000000000;
    a += age_1 * (smoke_cat==2) * 0.6942632802121626600000000;
    a += age_1 * (smoke_cat==3) * -1.6952388644218186000000000;
    a += age_1 * (smoke_cat==4) * -1.2150150940219255000000000;
    a += age_1 * b_AF * -3.5855215448190969000000000;
    a += age_1 * b_renal * -3.0766647922469192000000000;
    a += age_1 * b_treatedhyp * -4.0295302811880314000000000;
    a += age_1 * b_type1 * -0.3344110567405778600000000;
    a += age_1 * b_type2 * -3.3144806806620530000000000;
    a += age_1 * bmi_1 * -5.5933905797230006000000000;
    a += age_1 * bmi_2 * 64.3635572837688980000000000;
    a += age_1 * fh_cvd * 0.8605433761217157200000000;
    a += age_1 * sbp * -0.0509321154551188590000000;
    a += age_1 * town * 0.1518664540724453700000000;
    a += age_2 * (smoke_cat==1) * -0.1765395485882681500000000;
    a += age_2 * (smoke_cat==2) * -0.2323836483278573000000000;
    a += age_2 * (smoke_cat==3) * 0.2734395770551826300000000;
    a += age_2 * (smoke_cat==4) * 0.1432552287454152700000000;
    a += age_2 * b_AF * 0.4986871390807032200000000;
    a += age_2 * b_renal * 0.4393033615664938600000000;
    a += age_2 * b_treatedhyp * 0.6904385790303250200000000;
    a += age_2 * b_type1 * -0.1734316566060327700000000;
    a += age_2 * b_type2 * 0.4864930655867949500000000;
    a += age_2 * bmi_1 * 1.5223341309207974000000000;
    a += age_2 * bmi_2 * -12.7413436207964070000000000;
    a += age_2 * fh_cvd * -0.2756708481415109900000000;
    a += age_2 * sbp * 0.0073790750039744186000000;
    a += age_2 * town * -0.0487465462679640900000000;
    
    /* Calculate the score itself */
    double score = 1 - pow(survivor[10], exp(a)) ;
    return score;
  }
  return 0;
}

// Vectorise above
// [[Rcpp::export]]
NumericVector QRisk(
    IntegerVector age,
    IntegerVector sex,
    IntegerVector b_AF,
    IntegerVector b_ra,
    IntegerVector b_renal,
    IntegerVector b_treatedhyp,
    IntegerVector b_type1,
    IntegerVector b_type2,
    NumericVector bmi,
    IntegerVector ethrisk,
    IntegerVector fh_cvd,
    NumericVector rati,
    NumericVector sbp,
    IntegerVector smoke_cat,
    NumericVector town
)
{
  int n = age.size();
  NumericVector out(n);
  
  for (int i = 0; i < n; i++) {
    out[i] = cvd_raw(age[i], sex[i], b_AF[i], b_ra[i],
                     b_renal[i], b_treatedhyp[i], b_type1[i],
                     b_type2[i], bmi[i], ethrisk[i], fh_cvd[i],
                     rati[i], sbp[i], smoke_cat[i], town[i]);
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

// // [[Rcpp::export]]
// double test(int sex)
// {
//     if (sex == 1) return sex;
//     if (sex == 2) return sex;
//     return 0;
// }
