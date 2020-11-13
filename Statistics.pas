// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2019, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################

unit Statistics;

interface

uses MatrixConst, Matrix, BaseMathPersistence;

// ###########################################
// #### Useful functions used in for different statistics

// factorial - integer and double
function Factorial(n : integer) : Int64;
function FactorialF(n : integer) : double;   // factorial based on the gamma function
function FactorialLn(n : integer) : double;  // returns ln( n! )

// and based on the gamma function  factorial(n) = gamma(n + 1)

// gamma: gamma(x) = integral from 0 to inf of t^(x-1) exp(-t) dt.
function Gamma(x : double) : double;
// returns the value ln[GAMMA(x)] for X > 0
function GammaLN(x : Double) : double;

function beta( z, w : double ) : double;  // Result := gamma(z)*gamma(w)/gamma(z + w)
function betaI(a, b, x : double ) : double; // returns the incomplete beta function Ix(a, b)
function GammaP(a, x : double ) : double;  // returns the incomplete gamma function P(a, x);
function GammaQ(a, x : double ) : double;  // return the incomlete gamma function Q(a, x) == 1 - P(a, x)

procedure gammaCF(var gcf : double; a, x : double; var gln : double); // returns the incomplete gamma function Q(a,x) evaluated by its continued fraction representation. also returns ln (gamma(a)) as gln
procedure gammaSER(var gamser : double; a, x : double; var gln : double); // returns the incomplete gamma function P(a, x) avaluated by its series representation as gamser. also retusn ln(gamma(a)) as gln

// error function
function errorFunc( x : double ) : double;
function errorFuncC( x : double ) : double;

//
function binom(n, k : integer) : int64;
function binomf(n, k : integer) : double; // based on gamma function


procedure Covariance(dest : PDouble; DestLineWidth : TASMNativeInt; A : PDouble; LineWidthA : TASMNativeInt; width, height : TASMNativeInt;
  Unbiased : boolean = True); // writes covariance matrix of A in dest

// student t tests see Numerical recipies Chapter 14

// Student t test: Returns the probobability in prob and t = (x_A_mean - x_B_mean)/(sD) , sD the "pooled variance".
procedure StudentTTest( data1 : PDouble; n1 : integer; data2 : PDouble; n2 : integer; var t, prob : double ); overload;
procedure StudentTTest( meanData1, varData1 : double; n1 : integer; data2 : PDouble; n2 : integer; var t, prob : double ); overload;

// student t test with significantly different variances
procedure StudentTUTest( data1 : PDouble; n1 : integer; data2 : PDouble; n2 : integer; var t, prob : double ); overload;
procedure StudentTUTest( meanData1, varData1 : double; n1 : integer; data2 : PDouble; n2 : integer; var t, prob : double ); overload;

// student t test for the case of paired samples
procedure StudentTPTest( data1 : PDouble; data2 : PDouble; n : integer; var t, prob : double ); overload;
procedure StudentTPTest( data1 : PDouble; meanData1, varData1 : double; data2 : PDouble; n : integer; var t, prob : double ); overload;

// f-Test for signifacantly different variances
procedure FTest( data1 : PDouble; n1 : integer; data2 : PDouble; n2 : integer; var f, prob : double ); overload;
procedure FTest( meanData1, varData1 : double; n1 : integer; data2 : PDouble; n2 : integer; var f, prob : double ); overload;


// chi square test -> are two distributions the same.
// given observed events (bins) and expected distribution (expectedBins) the routine geturns the number of degrees of
// freedom (df) and the chi-square chsq as well as the significane prob. A small value of prob indicates a signifacant difference between the
// distrubition bins and expectBins.
procedure ChiSquareOne(bins, expectBins : PConstDoubleArr; nBins : integer; numConstraints : integer; var df, chsq, prob : double );

// case of two binned data sets (no expected data known)
procedure ChiSquareTwo(bins1, bins2 : PConstDoubleArr; nBins : integer; numConstraints : integer; var df, chsq, prob : double);

// case of unequal number of elements in both dataset
procedure ChiSquareTwoUnequal(bins1, bins2 : PConstDoubleArr; nBins : integer; numConstraints : integer; var df, chsq, prob : double);


// functions to create a histogram from data points
procedure Hist( data : PConstDoubleArr; n : integer; histogram : PConstDoubleArr; nBins : integer); overload;
procedure Hist( data : PConstDoubleArr; n : integer; histogram : PConstDoubleArr; nBins : integer; minVal, maxVal : double); overload;

// chapter 14.3 of NR
// kolmogorov smirnov test -> testing if two distributions are the same in case of unbinned distributions that are
// functions of a single independent variable.
type
  TKSTestFunc = function ( x : double ) : double;

// Returns the K-S Statistics d and the signifacance level prob. The given function "func" is a cumulative probability density function
// ranging from 0 (for smallest values of it's argument) to 1. The procedure returns the K-S statistics - D.
// the input data is expected to jump by 1/N by each "x_i". The input data is sorted in ascending order - destroying the original input!
procedure kolmogorovSmirnovOne( data : PConstDoubleArr; n : integer; func : TKSTestFunc; var d, prob : double );

// returns the K-S statisticas d and the signifacance level prob for the null hypothesis that the data sets are drawn from the same
// distribution. Small values of prob show that the cumulative distribution function of data1 is significantly different fro mthat of data2
// the arrays data1 and data2 are modified by being sorted into ascending order
procedure kolmogorovSmirnovTwo( data1 : PConstDoubleArr; n1 : integer; data2 : PConstDoubleArr; n2 : integer; var d, prob : double );


// ######################################################
// #### Class implementation of various statistic functions
type
  TStatTest = class(TBaseMathPersistence)
  private
    fN : integer;
    fMeanVar : TMeanVarRec;
    fRefData : IMatrix;
    fMinVal, fMaxVal : Double;
    fNumBins : integer;
  protected
    class function ClassIdentifier : String; override;
    procedure DefineProps; override;
    procedure OnLoadDoubleProperty(const Name : String; const Value : double); override;
    procedure OnLoadIntProperty(const Name : String; Value : integer); override;
    function OnLoadObject(const Name : String; Obj : TBaseMathPersistence) : boolean; override;
  public
    // initialize the mean var record with one db so we can compare against others more than once
    procedure InitStudentTTest( data1 : TDoubleMatrix );
    procedure InitStudentTPTest( data1 : TDoubleMatrix);
    procedure InitStudentFTest( data1 : TDoubleMatrix );
    procedure InitChisquareTest( data1 : TDoubleMatrix; nBins : integer );
    procedure InitKolmogorv2Test( data1 : TDoubleMatrix );

    // #########################################
    // #### Statistical tests available after the "init" functions (for multiple tests
    // against the same data1 source)
    procedure StudentTTest( data2 : IMatrix; var t, prob : double ); overload;
    procedure StudentTTest( data2 : TDoubleMatrix; var t, prob : double ); overload;

    // student t test with significantly different variances
    procedure StudentTUTest( data2 : IMatrix; var t, prob : double ); overload;
    procedure StudentTUTest( data2 : TDoubleMatrix; var t, prob : double ); overload;

    // student t test for the case of paired samples
    procedure StudentTPTest( data2 : IMatrix; var t, prob : double ); overload;
    procedure StudentTPTest( data2 : TDoubleMatrix; var t, prob : double ); overload;

    // f-Test for signifacntly different variances
    procedure FTest( data2 : IMatrix;  var f, prob : double ); overload;
    procedure FTest( data2 : TDoubleMatrix;  var f, prob : double ); overload;

    // chi square test
    procedure ChiSquare1( data2 : IMatrix; numConstraints : integer; var df, chsq, prob : double ); overload;
    procedure ChiSquare1( data2 : TDoubleMatrix; numConstraints : integer; var df, chsq, prob : double ); overload;

    // chi square2 test
    procedure ChiSquare2( data2 : IMatrix; numConstraints : integer; var df, chsq, prob : double ); overload;
    procedure ChiSquare2( data2 : TDoubleMatrix; numConstraints : integer; var df, chsq, prob : double ); overload;

    // kolmogroovSmirnov2 test
    procedure KolmogorovSmirnov2( data2 : IMatrix; var d, prob : double ); overload;
    procedure KolmogorovSmirnov2( data2 : TDoubleMatrix; var d, prob : double ); overload;
  public
    // note that all data is treated as all one big vector!

    // student t test with same variances
    class procedure StudentTTest( data1, data2 : IMatrix; var t, prob : double ); overload;
    class procedure StudentTTest( data1, data2 : TDoubleMatrix; var t, prob : double ); overload;

    // student t test with significantly different variances
    class procedure StudentTUTest( data1, data2 : IMatrix; var t, prob : double ); overload;
    class procedure StudentTUTest( data1, data2 : TDoubleMatrix; var t, prob : double ); overload;

    // student t test for the case of paired samples
    class procedure StudentTPTest( data1, data2 : IMatrix; var t, prob : double ); overload;
    class procedure StudentTPTest( data1, data2 : TDoubleMatrix; var t, prob : double ); overload;

    // f-Test for signifacntly different variances
    class procedure FTest( data1, data2 : IMatrix;  var f, prob : double ); overload;
    class procedure FTest( data1, data2 : TDoubleMatrix;  var f, prob : double ); overload;

    // chi square test on one dimensional datapoints,
    class procedure ChiSquare1( data1, data2 : TDoubleMatrix; nBins : integer; numConstraints : integer; var df, chsq, prob : double ); overload;
    class procedure ChiSquare1( data1, data2 : IMatrix; nBins : integer; numConstraints : integer; var df, chsq, prob : double ); overload;

    // chi square test on one dimensional datapoints -> no assumption on the expected histogram (data1 in chisquare1) is raised
    class procedure ChiSquare2( data1, data2 : TDoubleMatrix; nBins : integer; numConstraints : integer; var df, chsq, prob : double ); overload;
    class procedure ChiSquare2( data1, data2 : IMatrix; nBins : integer; numConstraints : integer; var df, chsq, prob : double ); overload;

    class procedure KolmogorovSmirnov1( data : TDoubleMatrix; func : TKSTestFunc; var d, prob : double ); overload;
    class procedure KolmogorovSmirnov1( data : IMatrix; func : TKSTestFunc; var d, prob : double ); overload;

    // returns the K-S statisticas d and the signifacance level prob for the null hypothesis that the data sets are drawn from the same
    // distribution. Small values of prob show that the cumulative distribution function of data1 is significantly different fro mthat of data2
    // the arrays data1 and data2 are modified by being sorted into ascending order
    class procedure KolmogorovSmirnov2( data1, data2 : TDoubleMatrix; var d, prob : double ); overload;
    class procedure KolmogorovSmirnov2( data1, data2 : IMatrix; var d, prob : double ); overload;

    // histogram from the input data
    // Creates a histogram from the input data between the
    // given borders. Elements >= the max value are put into the last bin
    // elements < minValue + (maxVal - minVal)/(nbins - 1) into the first bin
    // the histogram operates along the given dimension (rowWise is default)
    // returns a nbins x height (RowWise True) matrix
    class function HistogramIntf( data : IMatrix; nBins : integer; RowWise : Boolean = True ) : IMatrix; overload;
    class function HistogramIntf( data : IMatrix; nBins : integer; minVal, maxVal : double; RowWise : Boolean = True )  : IMatrix; overload;
    class function Histogram( data : TDoubleMatrix; nBins : integer; RowWise : Boolean = True ) : TDoubleMatrix; overload;
    class function Histogram( data : TDoubleMatrix; nBins : integer; minVal, maxVal : double; RowWise : Boolean = True )  : TDoubleMatrix; overload;

    // some probability density functions
    class function UniformPDF( x : TDoubleMatrix; a, b : double ) : TDoubleMatrix;
    class function NormalPDF( x : TDoubleMatrix; mu, sigma : double ) : TDoubleMatrix;
    class function LogNormalPDF( X : TDoubleMatrix; mu, sigma : double ) : TDoubleMatrix;
    class function PoissonPDF( x : TDoubleMatrix; lambda : double ) : TDoubleMatrix;
    class function RayleighPDF( x : TDoubleMatrix; sigma : double ) : TDoubleMatrix;
    class function WeibullPDF( x : TDoubleMatrix; lambda, k : double ) : TDoubleMatrix;
    class function ExpPDF( x : TDoubleMatrix; lambda : double ) : TDoubleMatrix;
    class function BinomPDF( x : TDoubleMatrix; n, p : integer ) : TDoubleMatrix;
  end;

implementation

uses Math, SysUtils, MathUtilFunc, Types, MatrixASMStubSwitch;

function Factorial(n : integer) : Int64;
begin
     Result := 1;

     while n > 1 do
     begin
          Result := Result*Int64(n);
          dec(n);
     end;
end;

function FactorialF(n : integer) : double;
begin
     assert(n >= 0, 'Error in factorial');

     if n < 32
     then
         Result := 1.0*Factorial(n)
     else
         Result := exp(gammaln(n + 1.0));
end;

function FactorialLn(n : integer) : double;  // returns ln( n! )
begin
     Result := gammaln( n + 1.0 );
end;

function Gamma(x : double) : double;
const p : Array[0..7] of double = (
                 -1.71618513886549492533811e+0,  2.47656508055759199108314e+1,
                 -3.79804256470945635097577e+2,  6.29331155312818442661052e+2,
                  8.66966202790413211295064e+2, -3.14512729688483675254357e+4,
                 -3.61444134186911729807069e+4,  6.64561438202405440627855e+4);
     q : Array[0..7] of double = (
                 -3.08402300119738975254353e+1,  3.15350626979604161529144e+2,
                 -1.01515636749021914166146e+3, -3.10777167157231109440444e+3,
                  2.25381184209801510330112e+4,  4.75584627752788110767815e+3,
                 -1.34659959864969306392456e+5, -1.15132259675553483497211e+5);
     c : Array[0..6] of double = (
                 -1.910444077728e-03,            8.4171387781295e-04,
                 -5.952379913043012e-04,         7.93650793500350248e-04,
                 -2.777777777777681622553e-03,   8.333333333333333331554247e-02,
                  5.7083835261e-03);
     spi : double = 0.9189385332046727417803297;
var xn : integer;
    y, ysq : double;
    y1 : integer;
    fact : double;
    xnum, xden : double;
    z : double;
    i : integer;
    x1 : double;
    isNeg : boolean;
    sum : double;
begin
     xn := 0;
     fact := 1;

     isNeg := x <= 0;

     // catch negative x
     if x <= 0 then
     begin
          y := -x;
          y1 := floor(y);  // nearest integer towards zero
          Result := y - y1;
          fact := -pi/sin(pi*result) * (1 - 2*(y1 mod 2));
          x := y + 1;
     end;

     // Map x in interval [0,1] to [1,2]
     x1 := 1;
     if x < 1 then
     begin
          x1 := x;
          x := x + 1;
     end;

     // Map x in interval [1,12] to [1,2]
     if x < 12 then
     begin
          xn := floor(x) - 1;
          x := x - xn;
     end;

     // Evaluate approximation for 1 < x < 2
     z := x - 1;
     xnum := 0;
     xden := xnum + 1;

     for i := 0 to High(p) do
     begin
          xnum := (xnum + p[i])*z;
          xden := xden*z + q[i];
     end;

     Result := xnum/xden + 1;

     // Adjust result for case  0.0 < x < 1.0
     Result := Result/x1;

     // Adjust result for case  2.0 < x < 12.0
     while xn > 0 do
     begin
          Result := Result*x;
          x := x + 1;
          xn := xn - 1;
     end;

     // Evaluate approximation for x >= 12
     if x >= 12 then
     begin
          y := x;
          ysq := sqr(y);
          sum := c[6];
          for i := 0 to High(c) - 1 do
              sum := sum / ysq + c[i];

          sum := sum /y - y + spi;
          sum := sum +(y - 0.5)*ln(y);
          Result := exp(sum);
     end;

     if isNeg then
        Result := fact/Result;

end;

function GammaLN(x : Double) : double;
const cof : Array[0..5] of double = (76.18009172947146, -86.50532032941677,
                                     24.01409824083091, -1.231739572450155,
                                     0.1208650973866179e-2, -0.5395239384953e-5 );
var y, tmp, ser : double;
    j : integer;
begin
     y := x;
     tmp := x + 5.5;
     tmp := tmp - (x + 0.5)*ln(tmp);
     ser := 1.000000000190015;
     for j := 0 to High(cof) do
     begin
          y := y + 1;
          ser := ser + cof[j]/y;
     end;

     result := -tmp + ln(2.5066282746310005*ser/x);
end;

function beta( z, w : double ) : double;  // Result := gamma(z)*gamma(w)/gamma(z + w)
begin
     Result := exp(gammaln(z) + gammaln(w) - gammaln(z + w));
end;


function betaCF(a, b, x : double) : double;  // evaluates the continued fraction for incomplete beat function by modified Lentz's method
const cEPS = 3.0e-7;
      cFPMIN = 1.0e-30;
      cITMax = 100;
var m, m2 : integer;
    aa, c, d, del : double;
    h, qab, qam, qap : double;
begin
     qab := a + b;
     qap := a + 1;
     qam := a - 1;
     c := 1;
     d := 1 - qab*x/qap;
     if abs(d) < cFPMIN then
        d := cFPMIN;
     d := 1/d;
     h := d;
        
     m := 1;
     while m <= cITMax do
     begin
          m2 := 2*m;
          aa := m*(b - m)*x/((qam + m2)*(a + m2));
          d := 1 + aa*d;
          if abs(d) < cFPMIN then
             d := cFPMIN;
          c := 1 + aa/c;
          if abs(c) < cFPMIN then
             c := cFPMIN;
          d := 1/d;
          h := h*d*c;
          aa := -(a + m)*(qab + m)*x/((a + m2)*(qap + m2));
          d := 1 + aa*d;
          if abs(d) < cFPMIN then
             d := cFPMIN;
          c := 1 + aa/c;
          if abs(c) < cFPMIN then
             c := cFPMIN;
          d := 1/d;
          del := d*c;
          h := h*del;
          if abs(del - 1) < cEPS then
             break;
          
          inc(m);
     end;

     if m > cITMax then
        raise Exception.Create('a or b too big or cMaxIt too small in betacf');

     Result := h;
end;

function betaI(a, b, x : double ) : double; // returns the incomplete beta function Ix(a, b)
var bt : double;
begin
     if (x < 0) or (x > 1) then
        raise Exception.Create('Bad x in beatI');

     if (x = 0) or (x = 1)
     then
         bt := 0
     else
         bt := exp(gammaln(a + b) - gammaln(a) - gammaln(b) + a*ln(x) + b*ln(1 - x));

     if x < (a + 1)/(a + b + 2) 
     then
         Result := bt*betacf(a, b, x)/a
     else
         Result := 1 - bt*betacf(b, a, 1 - x)/b;
end;

procedure gammacf(var gcf : double; a, x : double; var gln : double);
var i : integer;
    an, b, c,d, del, h : double;
const cEPS = 3.0e-7;
      cFPMIN = 1.0e-30;
      cITMax = 100;
begin
     gln := gammaln(a);
     b := x + 1 - a;
     c := 1/cFPMIN;
     d := 1/b;
     h := d;
     i := 1;
     while i <= cITMax do
     begin
          an := -i*(i - a);
          b := b + 2;
          d := an*d + b;
          if abs( d ) < cFPMin then
             d := cFPMin;
          c := b + an/c;
          if abs(c) < cFPMin then
             c := cFPMin;
          d := 1/d;
          del := d*c;
          h := h*del;
          if abs(del - 1) < cEPS then
             break;

          inc(i);
     end;

     assert(i < cITMax, 'A too large for maximum iteration');
     gcf := exp( -x + a*ln(x) - gln)*h;
end;

procedure gammaSER(var gamser : double; a, x : double; var gln : double); // returns the incomplete gamma function P(a, x) avaluated by its series representation as gamser. also retusn ln(gamma(a)) as gln
var n : integer;
    sum, del, ap : double;
const cEPS = 3.0e-7;
      cITMax = 100;
begin
     gln := gammaln(a);

     if x <= 0 then
     begin
          assert(x = 0, 'error x less than 0 in gammaser');

          gamser := 0;
          exit;
     end;

     ap := a;
     del := 1/a;
     sum := del;
     for n := 1 to cITMax do
     begin
          ap := ap + 1;
          del := del * x/ap;
          sum := sum + del;
          if abs(del) < abs(sum)*cEPS then
          begin
               gamser := sum*exp(-x + a*ln(x)-gln);
               exit;
          end;
     end;

     assert(False, 'a too large, itmax too small for gammaser');
end;

function GammaP(a, x : double ) : double;  // returns the incomplete gamma function P(a, x) = gamma_(a,x)/gamma(a)
var gln : double;
begin
     if (x < 0) or (a <= 0) then
        raise Exception.Create('Invalid arguments for gammap');

     Result := 0;
     if x < a + 1 
     then
         gammaser(Result, a, x, gln)
     else
     begin
          gammacf(Result, a, x, gln);
          Result := 1 - Result;
     end;
end;

function GammaQ(a, x : double ) : double;  // return the incomlete gamma function Q(a, x) == 1 - P(a, x)
var gln : double;
begin
     if (x < 0) or (a <= 0) then
        raise Exception.Create('Invalid arguments for gammap');

     Result := 0;
     if x < a + 1 then
     begin
          gammaser(Result, a, x, gln);
          Result := 1 - Result;
     end
     else
         gammacf(Result, a, x, gln);
end;


function errorFunc( x : double ) : double;
begin
     Result := abs(gammap(0.5, sqr(x)));
end;

function errorFuncC( x : double ) : double;
begin
     if x < 0 
     then
         Result := 1 + gammap(0.5, sqr(x))
     else
         Result := gammaq(0.5, sqr(x));
end;

// implements "n over k" -> binominal koefficients.
// see: https://en.wikipedia.org/wiki/Binomial_coefficient
// or german: http://de.wikipedia.org/wiki/Binomialkoeffizient
function binom(n, k : integer) : int64;
var tmp : integer;
    i : integer;
begin
     if k = 0 then
     begin
          Result := 1;
          exit;
     end
     else if 2*k > n
     then
         k := n - k;

     Result := n;
     tmp := n + 1;
     for i := 2 to k do
     begin
          Result := Result*(tmp - i);
          Result := Result div i;
     end;
end;

function binomf(n, k : integer) : double; // based on gamma function
begin
     Result := Round( exp( FactorialLn(n) - FactorialLn(k) - FactorialLn(n - k)));
end;

// covariance: ac = A - mean(A); Result := 1/ac.height * ac'*ac;  -> the unbiased version uses 1/(ac.height - 1)
procedure Covariance(dest : PDouble; DestLineWidth : TASMNativeInt; A : PDouble; LineWidthA : TASMNativeInt; width, height : TASMNativeInt; 
  Unbiased : boolean = True); // writes covariance matrix of A in dest
var aMean, tmp, ac : PDouble;
    aMeanLineWidth, acLineWidth, tmpLineWidth : TASMNativeInt;
    aMeanMem, acMem, tmpMem : Pointer;
    m : Integer;
begin
     if (height = 1) or (width = 1) then
     begin
          MatrixVar(dest, destLineWidth, A, LineWidthA, width, height, height = 1, Unbiased);
          exit;
     end;
     
     assert( destLineWidth >= width*sizeof(double), 'Error destlinewidth to short');
     
     aMean := MtxMallocAlign( width, 1, aMeanLineWidth, aMeanMem );
     ac := MtxMallocAlign(width, height, acLineWidth, acMem );
     tmp := MtxMallocAlign( height, width, tmpLineWidth, tmpMem);

     try
        assert( aMean <> nil, 'Memory allocation failed');
        assert( ac <> nil, 'Memory allocation failed');
        assert( tmp <> nil, 'Memory allocation failed');
     
        MatrixMean( aMean, aMeanLineWidth, A, LineWidthA, width, height, False);
        MatrixCopy(ac, acLineWidth, A, LineWidthA, width, height);
        MatrixSubVec(ac, acLineWidth, aMean, sizeof(double), width, height, True);
        MatrixTranspose( tmp, tmpLineWidth, ac, acLineWidth, width, height);
     
        m := height;
        if Unbiased then
           dec(m);

        MatrixMult( dest, DestLineWidth, tmp, ac, height, width, width, height, tmpLineWidth, acLineWidth);
        MatrixScaleAndAdd(dest, destLineWidth, width, width, 0, 1/m);
     finally
            FreeMem(aMeanMem);
            FreeMem(acMem);
            FreeMem(tmpMem);
     end;
end;

procedure InternalStudentTCalc( const meanVar1, meanVar2 : TMeanVarRec; n1, n2 : integer; var t, prob : double);
var df : double;
    sVar : double;
begin
     df := n1 + n2 - 2; // degrees of freedom
     svar := ((n1 - 1)*meanVar1.aVar + (n2 - 1)*meanVar2.aVar)/df;  // pooled variance
     t := (meanVar1.aMean - meanVar2.aMean)/sqrt(svar*(1.0/n1 + 1.0/n2));
     prob := betaI(0.5*df, 0.5, df/(df+sqr(t)));
end;

procedure StudentTTest( data1 : PDouble; n1 : integer; data2 : PDouble; n2 : integer; var t, prob : double);
var meanVar1, meanVar2 : TMeanVarRec;
begin
     MatrixMeanVar(@meanVar1, sizeof(TMeanVarRec), data1, n1*SizeOf(double), n1, 1, True, True);
     MatrixMeanVar(@meanVar2, sizeof(TMeanVarRec), data2, n2*SizeOf(double), n2, 1, True, True);

     InternalStudentTCalc(meanVar1, meanVar2, n1, n2, t, prob);
end;

procedure StudentTTest( meanData1, varData1 : double; n1 : integer; data2 : PDouble; n2 : integer; var t, prob : double ); overload;
var meanVar1, meanVar2 : TMeanVarRec;
begin
     meanVar1.aMean := meanData1;
     meanVar2.aVar := varData1;
     MatrixMeanVar(@meanVar2, sizeof(TMeanVarRec), data2, n2*SizeOf(double), n2, 1, True, True);

     InternalStudentTCalc(meanVar1, meanVar2, n1, n2, t, prob);
end;


procedure InternalStudentTUCalc( const meanVar1, meanVar2 : TMeanVarRec; n1, n2 : integer; var t, prob : double);
var df : double;
begin
     t := (meanVar1.aVar - meanVar2.aVar)/sqrt(meanVar1.aVar/n1 + meanVar2.aVar/n2);
     df := sqr(meanVar1.aVar/n1 + meanVar2.aVar/n2)/( sqr(meanVar1.aVar/n1)/(n1 - 1) + sqr(meanVar2.aVar/n2)/(n2 - 1) );
     prob := betaI(0.5*df, 0.5, df/(df+sqr(t)));
end;


procedure StudentTUTest( data1 : PDouble; n1 : integer; data2 : PDouble; n2 : integer; var t, prob : double);
var meanVar1, meanVar2 : TMeanVarRec;
begin
     MatrixMeanVar(@meanVar1, sizeof(TMeanVarRec), data1, n1*SizeOf(double), n1, 1, True, True);
     MatrixMeanVar(@meanVar2, sizeof(TMeanVarRec), data2, n2*SizeOf(double), n2, 1, True, True);

     InternalStudentTUCalc(meanVar1, meanVar2, n1, n2, t, prob);
end;

procedure StudentTUTest( meanData1, varData1 : double; n1 : integer; data2 : PDouble; n2 : integer; var t, prob : double ); overload;
var meanVar1, meanVar2 : TMeanVarRec;
begin
     meanVar1.aMean := meanData1;
     meanVar1.aVar := varData1;
     MatrixMeanVar(@meanVar2, sizeof(TMeanVarRec), data2, n2*SizeOf(double), n2, 1, True, True);

     InternalStudentTUCalc(meanVar1, meanVar2, n1, n2, t, prob);
end;

(*
procedure StudentTPTest( data1 : PDouble; data2 : PDouble; n : integer; var t, prob : double);
var meanVar1, meanVar2 : TMeanVarRec;
    df : double;
    sd, cov : double;
    j : integer;
    pD1, pD2 : PConstDoubleArr;
begin
     MatrixMeanVar(@meanVar1, sizeof(TMeanVarRec), data1, n*SizeOf(double), n, 1, True, True);
     MatrixMeanVar(@meanVar2, sizeof(TMeanVarRec), data2, n*SizeOf(double), n, 1, True, True);

     pD1 := PConstDoubleArr(data1);
     pD2 := PConstDoubleArr(data2);
     cov := 0;
     for j := 0 to n - 1 do
         cov := cov + ( pD1^[j] - meanVar1.aMean )*( pD2^[j] - meanVAr2.aMean );

     df := n - 1;
     cov := cov/df;
     sd := sqrt( (meanVar1.aVar + meanVar2.aVar - 2*cov)/n );

     t := (meanVar1.aMean - meanVar2.aMean)/sd;
     prob := betai( 0.5*df, 0.5, df/(df + sqr(t)) );
end;
*)

procedure InternalStudentTPTest(const meanVar1, meanVar2 : TMeanVarRec; data1, data2 : PDouble; n : integer; var t, prob : double);
var df : double;
    sd, cov : double;
    j : integer;
    pD1, pD2 : PConstDoubleArr;
begin
     pD1 := PConstDoubleArr(data1);
     pD2 := PConstDoubleArr(data2);
     cov := 0;
     for j := 0 to n - 1 do
         cov := cov + ( pD1^[j] - meanVar1.aMean )*( pD2^[j] - meanVAr2.aMean );

     df := n - 1;
     cov := cov/df;
     sd := sqrt( (meanVar1.aVar + meanVar2.aVar - 2*cov)/n );

     t := (meanVar1.aMean - meanVar2.aMean)/sd;
     prob := betai( 0.5*df, 0.5, df/(df + sqr(t)) );
end;

procedure StudentTPTest( data1 : PDouble; data2 : PDouble; n : integer; var t, prob : double );
var meanVar1, meanVar2 : TMeanVarRec;
begin
     MatrixMeanVar(@meanVar1, sizeof(TMeanVarRec), data1, n*SizeOf(double), n, 1, True, True);
     MatrixMeanVar(@meanVar2, sizeof(TMeanVarRec), data2, n*SizeOf(double), n, 1, True, True);

     InternalStudentTPTest(meanVar1, meanVar2, data1, data2, n, t, prob);
end;

procedure StudentTPTest( data1 : PDouble; meanData1, varData1 : double;
 data2 : PDouble; n : integer; var t, prob : double);
var meanVar1, meanVar2 : TMeanVarRec;
begin
     meanVar1.aMean := meanData1;
     meanVar1.aVar := varData1;
     MatrixMeanVar(@meanVar2, sizeof(TMeanVarRec), data2, n*SizeOf(double), n, 1, True, True);

     InternalStudentTPTest(meanVar1, meanVar2, data1, data2, n, t, prob);
end;

procedure InternalCalcFTest( const meanVAr1, meanVar2 : TMeanVarRec; n1, n2 : integer; var f, prob : double);
var df1, df2 : double;
begin
     if meanVar1.aVar > meanVar2.aVar then
     begin
          f := meanVar1.aVar/meanVar2.aVar;
          df1 := n1 - 1;
          df2 := n2 - 2;
     end
     else
     begin
          f := meanVar2.aVar/meanVar1.aVar;
          df1 := n2 - 1;
          df2 := n1 - 1;
     end;

     prob := 2*betai( 0.5*df2, 0.5*df1, df2/(df2 + df1*f) );
     if prob > 1 then
        prob := 2 - prob;
end;

procedure FTest( data1 : PDouble; n1 : integer; data2 : PDouble; n2 : integer; var f, prob : double );
var meanVar1, meanVar2 : TMeanVarRec;
begin
     MatrixMeanVar(@meanVar1, sizeof(TMeanVarRec), data1, n1*SizeOf(double), n1, 1, True, True);
     MatrixMeanVar(@meanVar2, sizeof(TMeanVarRec), data2, n2*SizeOf(double), n2, 1, True, True);

     InternalCalcFTest(meanVar1, meanVar2, n1, n2, f, prob);
end;

procedure FTest( meanData1, varData1 : double; n1 : integer; data2 : PDouble; n2 : integer; var f, prob : double );
var meanVar1, meanVar2 : TMeanVarRec;
begin
     meanVar1.aMean := meanData1;
     meanVar1.aVar := varData1;
     MatrixMeanVar(@meanVar2, sizeof(TMeanVarRec), data2, n2*SizeOf(double), n2, 1, True, True);

     InternalCalcFTest(meanVar1, meanVar2, n1, n2, f, prob);
end;

procedure ChiSquareOne(bins, expectBins : PConstDoubleArr; nBins : integer; numConstraints : integer; var df, chsq, prob : double );
var j : integer;
    temp : double;
begin
     df := nBins - numConstraints;
     chsq := 0;

     for j := 0 to nbins - 1 do
     begin
          if expectBins^[j] <= 0 then
             raise Exception.Create('Expected bins may not be zero or lower');

          temp := bins^[j] - expectBins^[j];
          chsq := chsq + sqr(temp)/expectBins^[j];
     end;

     prob := GammaQ(0.5*df, 0.5*chsq);
end;

procedure ChiSquareTwo(bins1, bins2 : PConstDoubleArr; nBins : integer; numConstraints : integer; var df, chsq, prob : double);
var j : integer;
    temp : double;
begin
     df := nBins - numConstraints;
     chsq := 0;

     for j := 0 to nbins - 1 do
     begin
          if (bins1^[j] = 0) and (bins2^[j] = 0) 
          then
              df := df - 1
          else
          begin
               temp := bins1^[j] - bins2^[j];
               chsq := chsq + sqr(temp)/(bins1^[j] + bins2^[j]);
          end;
     end;

     prob := GammaQ(0.5*df, 0.5*chsq);
end;

procedure ChiSquareTwoUnequal(bins1, bins2 : PConstDoubleArr; nBins : integer; numConstraints : integer; var df, chsq, prob : double);
var j : integer;
    temp : double;
    R, S : double;
    sqrtSR, sqrtRS : double;
begin
     df := nBins - numConstraints;
     chsq := 0;

     R := 0;
     S := 0;
     for j := 0 to nBins - 1 do
     begin
          R := R + bins1^[j];
          S := S + bins2^[j];
     end;

     sqrtSR := sqrt(S/R);
     sqrtRS := sqrt(R/S);

     for j := 0 to nbins - 1 do
     begin
          if (bins1^[j] = 0) and (bins2^[j] = 0) 
          then
              df := df - 1
          else
          begin
               temp := sqrtSR*bins1^[j] - sqrtRS*bins2^[j];
               chsq := chsq + sqr(temp)/(bins1^[j] + bins2^[j]);
          end;
     end;

     prob := GammaQ(0.5*df, 0.5*chsq);
end;

procedure Hist( data : PConstDoubleArr; n : integer; histogram : PConstDoubleArr; nBins : integer); overload;
var MinVal : double;
    MaxVal : double;
begin
     MinVal := MatrixMin(PDouble(data), n, 1, n*sizeof(double));
     MaxVal := MatrixMax(PDouble(data), n, 1, n*sizeof(double));

     Hist(data, n, histogram, nBins, minVal, maxVal);
end;

procedure Hist( data : PConstDoubleArr; n : integer; histogram : PConstDoubleArr; nBins : integer; minVal, maxVal : double); overload;
var idx : integer;
    step : double;
    i : Integer;
begin
     MtxMemInit(PDouble(histogram), nBins*sizeof(double), 0);

     step := (maxVal - minVal)/(nBins - 1);

     for i := 0 to n - 1 do
     begin
          if data^[i] >= maxVal
          then
              idx := nBins - 1
          else if data^[i] <= minVal
          then
              idx := 0
          else
              idx := Floor( (data^[i] - minVal)/step );

          histogram^[idx] := histogram^[idx] + 1;
     end;
end;

function probks(alam : double) : double;
const cEPS1 = 0.001;
      cEPS2 = 1e-8;
      
var j : integer;
    a2 : double;
    fac, termbf : double;
    term : double;
begin
     fac := 2;
     Result := 0;
     termbf := 0;

     a2 := -2*sqr(alam);
     for j := 1 to 100 do
     begin
          term := fac*exp(a2*j*j);
          Result := Result + term;
          if (abs(term) <= cEPS1*termbf) or (abs(term) <= cEPS2*Result) then
             exit;

          fac := -fac;
          termbf := abs(term);
     end;
     
     Result := 1;
end;

procedure kolmogorovSmirnovOne( data : PConstDoubleArr; n : integer; func : TKSTestFunc; var d, prob : double );
var j : integer;
    dt, en, ff, fn : double;
    fo : double;
begin
     fo := 0;

     QuickSort( data, n );
     en := n;
     d := 0;
     for j := 1 to n do
     begin
          fn := j/en;
          ff := func( data^[j - 1] );
          dt := max( abs( fo - ff ), abs( fn - ff ) );
          if dt > d then
             d := dt;
          fo := fn;
     end;

     en := sqrt(en);
     prob := probks( (en + 0.12 + 0.11/en)*d );
end;

// it is assumed that in this function data1 is already sorted
procedure InternalkolmogorovSmirnovTwo( data1 : PConstDoubleArr; n1 : integer; data2 : PConstDoubleArr; n2 : integer; var d, prob : double );
var j1, j2 : integer;
    d1, d2, dt, en1, en2, en : double;
    fn1, fn2 : double;
begin
     fn1 := 0;
     fn2 := 0;

     j1 := 1;
     j2 := 1;

     //QuickSort(data1, n1);
     QuickSort(data2, n2);

     en1 := n1;
     en2 := n2;
     d := 0;

     while (j1 <= n1) and (j2 <= n2) do
     begin
          d1 := data1^[j1 - 1];
          d2 := data2^[j2 - 1];

          if d1 <= d2 then
          begin
               fn1 := j1/en1;
               inc(j1);
          end;
          if d2 <= d1 then
          begin
               fn2 := j2/en2;
               inc(j2);
          end;

          dt := abs(fn2 - fn1);
          if dt > d then
             d := dt;
     end;

     en := sqrt(en1*en2/(en1 + en2));
     prob := probks( (en + 0.12 + 0.11/en)*d );
end;

procedure kolmogorovSmirnovTwo( data1 : PConstDoubleArr; n1 : integer; data2 : PConstDoubleArr; n2 : integer; var d, prob : double );
begin
     QuickSort(data1, n1);
     InternalkolmogorovSmirnovTwo(data1, n1, data2, n2, d, prob);
end;

{ TStatTest }

procedure TStatTest.InitStudentTTest(data1: TDoubleMatrix);
var blk : TDoubleDynArray;
begin
     fRefData := nil;
     fN := data1.Width*data1.Height;

     if (data1.Width = 1) or (data1.Height = 1) then
     begin
          MatrixMeanVar(@fMeanVar, sizeof(double)*ifThen(data1.Width = 1, 1, 2), data1.StartElement,
                        data1.LineWidth, data1.Width, data1.Height, data1.Height = 1, True);
     end
     else
     begin
          blk := data1.SubMatrix;
          MatrixMeanVar(@fMeanVar, sizeof(fMeanVar), @blk[0], Length(blk)*sizeof(double),
                        Length(blk), 1, True, True);
     end;
end;

class procedure TStatTest.StudentTTest(data1, data2: TDoubleMatrix; var t,
  prob: double);
var blk1, blk2 : TDoubleDynArray;
begin
     if (data1.Height = 1) and (data2.Height = 1) then
     begin
          Statistics.StudentTTest(data1.StartElement, data1.Width, data2.StartElement, data2.Width, t, prob);
     end
     else
     begin
          blk1 := data1.SubMatrix;
          blk2 := data2.SubMatrix;
          Statistics.StudentTTest(@blk1[0], Length(blk1), @blk2[0], Length(blk2), t, prob);
     end;
end;

class procedure TStatTest.StudentTTest(data1, data2: IMatrix; var t,
  prob: double);
begin
     StudentTTest(data1.GetObjRef, data2.GetObjRef, t, prob);
end;

class procedure TStatTest.StudentTUTest(data1, data2: TDoubleMatrix; var t,
  prob: double);
var blk1, blk2 : TDoubleDynArray;
begin
     if (data1.Height = 1) and (data2.Height = 1) then
     begin
          Statistics.StudentTUTest(data1.StartElement, data1.Width, data2.StartElement, data2.Width, t, prob);
     end
     else
     begin
          blk1 := data1.SubMatrix;
          blk2 := data2.SubMatrix;
          Statistics.StudentTUTest(@blk1[0], Length(blk1), @blk2[0], Length(blk2), t, prob);
     end;
end;


class procedure TStatTest.StudentTUTest(data1, data2: IMatrix; var t,
  prob: double);
begin
     StudentTUTest(data1.GetObjRef, data2.GetObjRef, t, prob);
end;

class procedure TStatTest.StudentTPTest(data1, data2: TDoubleMatrix; var t,
  prob: double);
var blk1, blk2 : TDoubleDynArray;
begin
     if (data1.Height = 1) and (data2.Height = 1) and (data1.Width = data2.Width) then
     begin
          Statistics.StudentTPTest(data1.StartElement, data2.StartElement, data1.Width, t, prob);
     end
     else
     begin
          blk1 := data1.SubMatrix;
          blk2 := data2.SubMatrix;
          assert(Length(blk1) = length(blk2), 'Error studentTPTest need same vector lengths');
          Statistics.StudentTPTest(@blk1[0], @blk2[0], Length(blk1), t, prob);
     end;
end;

class procedure TStatTest.StudentTPTest(data1, data2: IMatrix; var t,
  prob: double);
begin
     StudentTPTest(data1.GetObjRef, data2.GetObjRef, t, prob);
end;

class procedure TStatTest.FTest(data1, data2: TDoubleMatrix; var f, prob: double);
var blk1, blk2 : TDoubleDynArray;
begin
     if (data1.Height = 1) and (data2.Height = 1) then
     begin
          Statistics.FTest(data1.StartElement, data1.Width, data2.StartElement, data2.Width, f, prob);
     end
     else
     begin
          blk1 := data1.SubMatrix;
          blk2 := data2.SubMatrix;
          Statistics.FTest(@blk1[0], Length(blk1), @blk2[0], Length(blk2), f, prob);
     end;
end;


class procedure TStatTest.FTest(data1, data2: IMatrix; var f, prob: double);
begin
     FTest(data1.GetObjRef, data2.GetObjRef, f, prob);
end;

procedure TStatTest.InitStudentTPTest(data1: TDoubleMatrix);
begin
     fRefData := data1.AsVector(True);
     fN := fRefData.Width;

     MatrixMeanVar(@fMeanVar, sizeof(fMeanVar), fRefData.StartElement, fRefData.LineWidth, fRefData.Width,
                   1, True, True);
end;

procedure TStatTest.StudentTPTest(data2: TDoubleMatrix; var t,
  prob: double);
var aVec : IMatrix;
begin
     assert(Assigned(fRefData), 'Call InitStudentTPTest with the reference dataset first');
     assert(data2.Width*data2.Height = fRefData.Width, 'Pairwise length is wrong');

     if Data2.Height = 1
     then
         Statistics.StudentTPTest(fRefData.StartElement, fMeanVar.aMean, fMeanVar.aVar, data2.StartElement,
                                  data2.Width, t, prob)
     else
     begin
          aVec := data2.AsVector(True);
          Statistics.StudentTPTest(fRefData.StartElement, fMeanVar.aMean, fMeanVar.aVar,
                                   aVec.StartElement, aVec.VecLen,
                                   t, prob)
     end;
end;

procedure TStatTest.StudentTPTest(data2: IMatrix; var t,
  prob: double);
begin
     StudentTPTest( data2.GetObjRef, t, prob );
end;

procedure TStatTest.StudentTTest(data2: TDoubleMatrix; var t, prob: double);
var aVec : IMatrix;
begin
     assert((fN > 0) and (fRefData = nil), 'Call InitStudentTTest first');
     if Data2.Height = 1
     then
         Statistics.StudentTTest(fMeanVar.aMean, fMeanVar.aVar, fN, data2.StartElement,
                                  data2.Width, t, prob)
     else
     begin
          aVec := data2.AsVector(True);
          Statistics.StudentTTest(fMeanVar.aMean, fMeanVar.aVar, fN, aVec.StartElement,
                                  aVec.VecLen, t, prob);
     end;
end;

procedure TStatTest.StudentTTest(data2: IMatrix; var t, prob: double);
begin
     StudentTTest( data2.GetObjRef, t, prob );
end;

procedure TStatTest.StudentTUTest(data2: TDoubleMatrix; var t, prob: double);
var aVec : IMatrix;
begin
     assert((fN > 0) and (fRefData = nil), 'Call InitStudentTTest first');
     if Data2.Height = 1
     then
         Statistics.StudentTUTest(fMeanVar.aMean, fMeanVar.aVar, fN, data2.StartElement,
                                  data2.Width, t, prob)
     else
     begin
          aVec := data2.AsVector(True);
          Statistics.StudentTUTest(fMeanVar.aMean, fMeanVar.aVar, fN, aVec.StartElement,
                                   aVec.VecLen, t, prob);
     end;
end;

procedure TStatTest.StudentTUTest(data2: IMatrix; var t, prob: double);
begin
     StudentTUTest(data2.GetObjRef, t, prob);
end;

procedure TStatTest.FTest(data2: TDoubleMatrix; var f, prob: double);
var aVec : IMatrix;
begin
     assert((fN > 0) and (fRefData = nil), 'Call InitStudentTTest first');
     if Data2.Height = 1
     then
         Statistics.FTest(fMeanVar.aMean, fMeanVar.aVar, fN, data2.StartElement, data2.Width, f, prob)
     else
     begin
          aVec := data2.AsVector(True);
          Statistics.FTest(fMeanVar.aMean, fMeanVar.aVar, fN, aVec.StartElement,
                                   aVec.VecLen, f, prob);
     end;
end;

procedure TStatTest.FTest(data2: IMatrix; var f, prob: double);
begin
     FTest(data2.GetObjRef, f, prob);
end;

procedure TStatTest.InitStudentFTest(data1: TDoubleMatrix);
begin
     // uses the same pattern
     InitStudentTTest(data1);
end;

// #######################################################
// #### Persistence
// #######################################################

const  cPropMean = 'StatMean';
       cPropVar = 'StatVar';
       cPropN = 'StatN';
       cPropNBins = 'StatNBins';
       cPropMinVal = 'StatMinVal';
       cPropMaxVal = 'StatMaxVal';
       cPropData = 'StatData';

procedure TStatTest.DefineProps;
begin
     AddDoubleProperty(cPropMean, fMeanVar.aMean);
     AddDoubleProperty(cPropVar, fMeanVar.aVar);
     AddIntProperty(cPropN, fN);
     AddIntProperty(cPropNBins, fNumBins);
     AddDoubleProperty(cPropMinVal, fMinVal);
     AddDoubleProperty(cPropMaxVal, fMaxVal);
     if Assigned(fRefData) then
        AddObject(cPropData, fRefData.GetObjRef);
end;

class function TStatTest.ClassIdentifier: String;
begin
     Result := 'Statistics';
end;

procedure TStatTest.OnLoadDoubleProperty(const Name: String;
  const Value: double);
begin
     if SameText(Name, cPropMean)
     then
         fMeanVar.aMean := Value
     else if SameText(Name, cPropVar)
     then
         fMeanVar.aVar := Value
     else if SameText(Name, cPropMinVal)
     then
         fMinVal := Value
     else if SameText(Name, cPropMaxVal)
     then
         fMaxVal := Value
     else
         inherited;
end;

procedure TStatTest.OnLoadIntProperty(const Name: String; Value: integer);
begin
     if SameText(Name, cPropN)
     then
         fN := Value
     else if SameText(Name, cPropNBins)
     then
         fNumBins := Value
     else
         inherited;
end;

function TStatTest.OnLoadObject(const Name: String;
  Obj: TBaseMathPersistence): boolean;
begin
     Result := True;
     if SameText(Name, cPropData)
     then
         fRefData := (obj as TDoubleMatrix) as IMatrix
     else
         Result := Inherited OnLoadObject(Name, Obj);
end;

class function TStatTest.Histogram(data: TDoubleMatrix; nBins: integer;
  RowWise: Boolean): TDoubleMatrix;
var y: Integer;
    pData : PConstDoubleArr;
    pRes : PConstDoubleArr;
    resCol : TDoubleDynArray;
    x: Integer;
    dataCpy : IMatrix;
begin
     if RowWise then
     begin
          Result := TDoubleMatrixClass( data.ClassType ).Create(nBins, data.Height);
          pRes := PConstDoubleArr( Result.StartElement );
          pData := PConstDoubleArr( data.StartElement );

          for y := 0 to data.Height - 1 do
          begin
               Hist( pData, data.Width, pRes, nBins);
               inc(PByte(pData), data.LineWidth);
               inc(PByte(pRes), Result.LineWidth);
          end;
     end
     else
     begin
          Result := TDoubleMatrixClass( data.ClassType ).Create(data.Width, nBins );

          dataCpy := data.Transpose;
          pData := PConstDoubleArr( dataCpy.StartElement );
          SetLength(resCol, nBins);

          for x := 0 to data.Width - 1 do
          begin
               MtxMemInit(@resCol[0], Length(resCol)*sizeof(double), 0);
               Result.SetColumn(x, resCol);
               Hist( pData, data.Height, @resCol[0], nBins);
               inc(PByte(pData), data.LineWidth);
          end;
     end;
end;

class function TStatTest.Histogram(data: TDoubleMatrix; nBins: integer; minVal,
  maxVal: double; RowWise: Boolean): TDoubleMatrix;
var y: Integer;
    pData : PConstDoubleArr;
    pRes : PConstDoubleArr;
    resCol : TDoubleDynArray;
    x: Integer;
    dataCpy : IMatrix;
begin
     if RowWise then
     begin
          Result := TDoubleMatrixClass( data.ClassType ).Create(nBins, data.Height);
          pRes := PConstDoubleArr( Result.StartElement );
          pData := PConstDoubleArr( data.StartElement );

          for y := 0 to data.Height - 1 do
          begin
               Hist( pData, data.Width, pRes, nBins, minVal, maxVal);
               inc(PByte(pData), data.LineWidth);
               inc(PByte(pRes), Result.LineWidth);
          end;
     end
     else
     begin
          Result := TDoubleMatrixClass( data.ClassType ).Create(data.Width, nBins );

          dataCpy := data.Transpose;
          pData := PConstDoubleArr( dataCpy.StartElement );
          SetLength(resCol, nBins);

          for x := 0 to data.Width - 1 do
          begin
               MtxMemInit(@resCol[0], Length(resCol)*sizeof(double), 0);
               Result.SetColumn(x, resCol);
               Hist( pData, data.Height, @resCol[0], nBins, minVal, maxVal);
               inc(PByte(pData), data.LineWidth);
          end;
     end;
end;

class function TStatTest.HistogramIntf(data: IMatrix; nBins: integer; minVal,
  maxVal: double; RowWise: Boolean): IMatrix;
begin
     Result := Histogram( data.GetObjRef, nBins, minVal, maxVal, RowWise);
end;

class function TStatTest.HistogramIntf(data: IMatrix; nBins: integer;
  RowWise: Boolean): IMatrix;
begin
     Result := Histogram( data.GetObjRef, nBins, RowWise );
end;

class procedure TStatTest.ChiSquare1(data1, data2: IMatrix; nBins,
  numConstraints: integer; var df, chsq, prob: double);
begin
     ChiSquare1( data1.GetObjRef, data2.GetObjRef, nBins, numConstraints, df, chsq, prob);
end;

class procedure TStatTest.ChiSquare1(data1, data2: TDoubleMatrix; nBins,
  numConstraints: integer; var df, chsq, prob: double);
var minVal, maxVal : double;
    hist1, hist2 : IMatrix;
    vec1, vec2 : TDoubleMatrix;
    x: Integer;
begin
     minVal := data1.Min;
     maxVal := data1.Max;
     minVal := Min(minVal, data2.Min);
     maxVal := Max(maxVal, data2.Max);

     if (data1.Height = 1) and (data2.Height = 1) then
     begin
          vec1 := data1;
          vec2 := data2;
     end
     else
     begin
          vec1 := data1.AsVector(True);
          vec2 := data2.AsVector(True);
     end;

     hist1 := HistogramIntf( data1, nBins, minVal, maxVal, True);
     hist2 := HistogramIntf( data2, nBins, minVal, maxVal, True);

     if data1 <> vec1 then
        vec1.Free;
     if data2 <> vec2 then
        vec2.Free;

     // reference histogram may not have zero bins...
     minVal := hist1.Min;
     maxVal := hist1.Max;
     if minVal = 0 then
     begin
          for x := 0 to hist1.Width - 1 do
              hist1.Vec[x] := Max(eps(maxVal), hist1.Vec[x]);
     end;

     // chi square on the histograms
     ChiSquareOne(PConstDoublearr(hist1.StartElement), PConstDoublearr(hist2.StartElement), hist1.Width, numConstraints, df, chsq, prob);
end;


procedure TStatTest.InitChisquareTest(data1: TDoubleMatrix; nBins : integer);
begin
     fMinVal := data1.Min;
     fMaxVal := data1.Max;
     fNumBins := nBins;

     fRefData := data1.AsVector(True);
     fRefData := HistogramIntf(data1, nBins, fMinVal, fMaxVal);
end;

procedure TStatTest.ChiSquare1(data2: TDoubleMatrix;
  numConstraints: integer; var df, chsq, prob: double);
var hist : IMatrix;
    vec : IMatrix;
begin
     if fNumBins = 0 then
        raise Exception.Create('Error - call InitChiSquare1 first');

     vec := data2.AsVector(True);
     hist := HistogramIntf(vec, fNumBins, True);

     Statistics.ChiSquareOne(PConstDoublearr(hist.StartElement),
                             PConstDoublearr(fRefData.StartElement), fNumBins,
                             numConstraints, df, chsq, prob);
end;

procedure TStatTest.ChiSquare1(data2: IMatrix;  numConstraints: integer;
  var df, chsq, prob: double);
begin
     ChiSquare1( data2.GetObjRef, numConstraints, df, chsq, prob);
end;

class procedure TStatTest.ChiSquare2(data1, data2: IMatrix; nBins,
  numConstraints: integer; var df, chsq, prob: double);
begin
     ChiSquare2(data1.GetObjRef, data2.GetObjRef, nBins, numConstraints, df, chsq, prob);
end;

class procedure TStatTest.ChiSquare2(data1, data2: TDoubleMatrix; nBins,
  numConstraints: integer; var df, chsq, prob: double);
var minVal, maxVal : double;
    hist1, hist2 : IMatrix;
    vec1, vec2 : TDoubleMatrix;
begin
     minVal := data1.Min;
     maxVal := data1.Max;
     minVal := Min(minVal, data2.Min);
     maxVal := Max(maxVal, data2.Max);

     if (data1.Height = 1) and (data2.Height = 1) then
     begin
          vec1 := data1;
          vec2 := data2;
     end
     else
     begin
          vec1 := data1.AsVector(True);
          vec2 := data2.AsVector(True);
     end;

     hist1 := HistogramIntf( data1, nBins, minVal, maxVal, True);
     hist2 := HistogramIntf( data2, nBins, minVal, maxVal, True);

     if data1 <> vec1 then
        vec1.Free;
     if data2 <> vec2 then
        vec2.Free;

     // chi square on the histograms
     ChiSquareTwo(PConstDoublearr(hist1.StartElement),
                  PConstDoublearr(hist2.StartElement),
                  hist1.Width, numConstraints, df, chsq, prob);
end;

procedure TStatTest.ChiSquare2(data2: TDoubleMatrix; numConstraints: integer;
  var df, chsq, prob: double);
var hist : IMatrix;
    vec : IMatrix;
begin
     if fNumBins = 0 then
        raise Exception.Create('Error - call InitChiSquare1 first');

     vec := data2.AsVector(True);
     hist := HistogramIntf(vec, fNumBins, True);

     Statistics.ChiSquareTwo(PConstDoubleArr(hist.StartElement),
                             PConstDoubleArr(fRefData.StartElement),
                             fNumBins, numConstraints, df, chsq, prob);
end;

procedure TStatTest.ChiSquare2(data2: IMatrix; numConstraints: integer; var df,
  chsq, prob: double);
begin
     ChiSquare2(data2.GetObjRef, numConstraints, df, chsq, prob);
end;

class procedure TStatTest.KolmogorovSmirnov1(data: IMatrix; func: TKSTestFunc;
  var d, prob: double);
begin
     KolmogorovSmirnov1(data.GetObjRef, func, d, prob);
end;

class procedure TStatTest.KolmogorovSmirnov1(data: TDoubleMatrix;
  func: TKSTestFunc; var d, prob: double);
var vec : TDoubleMatrix;
begin
     if data.Height = 1
     then
         vec := data
     else
         vec := data.AsVector(True);

     kolmogorovSmirnovOne(PConstDoubleArr(vec.StartElement), vec.Width, func, d, prob);
     if vec <> data then
        vec.Free;
end;

class procedure TStatTest.KolmogorovSmirnov2(data1, data2: IMatrix; var d,
  prob: double);
begin
     KolmogorovSmirnov2(data1.GetObjRef, data2.GetObjRef, d, prob);
end;

class procedure TStatTest.KolmogorovSmirnov2(data1, data2: TDoubleMatrix; var d,
  prob: double);
var vec1, vec2 : TDoubleMatrix;
begin
     vec1 := data1.AsVector(True);
     vec2 := data2.AsVector(True);
     try
        Statistics.kolmogorovSmirnovTwo( PConstDoubleArr(vec1.StartElement), vec1.Width,
                                         PConstDoubleArr(vec2.StartElement), vec2.Width,
                                         d, prob );
     finally
            vec1.Free;
            vec2.Free;
     end;
end;

procedure TStatTest.InitKolmogorv2Test(data1: TDoubleMatrix);
begin
     fRefData := data1.AsVector(True);
     fN := fRefData.VecLen;
     fRefData.SortInPlace(True);
end;

procedure TStatTest.KolmogorovSmirnov2(data2: TDoubleMatrix; var d,
  prob: double);
var vec : IMatrix;
begin
     if (fN = 0) or (fRefData.VecLen <> fN) then
        raise Exception.Create('Call InitKolmogorv2Test first');

     vec := data2.AsVector(True);
     Statistics.kolmogorovSmirnovTwo( PConstDoubleArr( fRefData.StartElement ),
                                      fN, PConstDoubleArr( vec.StartElement ),
                                      vec.VecLen, d, prob);
end;

procedure TStatTest.KolmogorovSmirnov2(data2: IMatrix; var d, prob: double);
begin
     KolmogorovSmirnov2( data2.GetObjRef, d, prob );
end;

class function TStatTest.UniformPDF(x : TDoubleMatrix; a, b: double): TDoubleMatrix;
var i : integer;
begin
     Result := TDoubleMatrix.Create(x.VecLen, 1);

     for i := 0 to x.VecLen - 1 do
         if (x.Vec[i] >= a) and (x.Vec[i] < b) then
            Result.Vec[i] := 1;
end;

class function TStatTest.NormalPDF(x: TDoubleMatrix; mu,
  sigma: double): TDoubleMatrix;
var i : integer;
    normVal : double;
    scale : double;
begin
     Result := TDoubleMatrix.Create(x.VecLen, 1);
     sigma := sqr(sigma);
     scale := 1/sqrt(2*pi);

     for i := 0 to x.VecLen - 1 do
     begin
          // normalize for the standard
          normVal := (x.Vec[i] - mu)/sigma;
          Result.Vec[i] := scale * exp( - sqr(normVal)/2 );
     end;
end;

class function TStatTest.PoissonPDF(x: TDoubleMatrix;
  lambda: double): TDoubleMatrix;
var i : integer;
    val : double;
begin
     Result := TDoubleMatrix.Create(x.VecLen, 1);

     // poissoin distribution is defined for integers (occurences) -> so round to the
     // next int
     for i := 0 to x.VecLen - 1 do
     begin
          val := x.Vec[i];
          // only positive values and integers:
          if (val >= 0) and ( SameValue(val, round(val), 1e-6) ) then
          begin
               val := Round(val);
               Result.Vec[i] := exp( val * ln(lambda) - gammaln( val + 1 ) );
          end;
     end;
end;

class function TStatTest.RayleighPDF(x: TDoubleMatrix;
  sigma: double): TDoubleMatrix;
var i : integer;
    val : double;
begin
     Result := TDoubleMatrix.Create(x.VecLen, 1);

     sigma := sqr(sigma);
     // poissoin distribution is defined for integers (occurences) -> so round to the
     // next int
     for i := 0 to x.VecLen - 1 do
     begin
          val := x.Vec[i];
          // only positive values and integers:
          if (val >= 0) then
             Result.Vec[i] := val/sigma*exp(- sqr(val)/(2*sigma) );
     end;
end;


class function TStatTest.LogNormalPDF(X: TDoubleMatrix; mu,
  sigma: double): TDoubleMatrix;
var i : integer;
    normVal : double;
    scale : double;
    val : double;
begin
     Result := TDoubleMatrix.Create(x.VecLen, 1);
     sigma := sqr(sigma);
     scale := 1/sqrt(2*pi);

     for i := 0 to x.VecLen - 1 do
     begin
          // normalize for the standard
          val := x.Vec[i];
          if val > 0 then
          begin
               normVal := (ln(val) - mu)/sigma;
               Result.Vec[i] := scale * exp( - sqr(normVal)/2 )/val;
          end;
     end;
end;


class function TStatTest.WeibullPDF(x: TDoubleMatrix; lambda, k : double): TDoubleMatrix;
var i : integer;
    val : double;
begin
     assert(lambda > 0, 'lambda needs to be > 0');
     assert(k >= 0, 'k needs to be > 0');

     Result := TDoubleMatrix.Create(x.VecLen, 1);

     for i := 0 to x.VecLen - 1 do
     begin
          // normalize for the standard
          val := x.Vec[i];
          if val > 0 then
          begin
               Result.Vec[i] := lambda*k*
                                power(lambda*val, k - 1) *
                                exp( - power( lambda*val, k ) );
          end;
     end;
end;

class function TStatTest.ExpPDF(x: TDoubleMatrix;
  lambda: double): TDoubleMatrix;
var i : integer;
    val : double;
    lambdaInv : double;
begin
     assert(lambda > 0, 'lambda needs to be > 0');

     Result := TDoubleMatrix.Create(x.VecLen, 1);

     lambdaInv := 1/lambda;
     for i := 0 to x.VecLen - 1 do
     begin
          // normalize for the standard
          val := x.Vec[i];

          if val >= 0 then
             Result.Vec[i] := exp(-val*lambdaInv)*lambdaInv;
     end;
end;

class function TStatTest.BinomPDF(x: TDoubleMatrix; n,
  p: integer): TDoubleMatrix;
var i : integer;
    val : double;
begin
     assert((p >= 0) and (p <= 1), 'Error has to be in [0, 1]');
     assert( n >= 0, 'Error n needs to be >= 0');
     Result := TDoubleMatrix.Create(x.VecLen, 1);

     // poissoin distribution is defined for integers (occurences) -> so round to the
     // next int
     for i := 0 to x.VecLen - 1 do
     begin
          val := x.Vec[i];
          // only positive values and integers:
          if (val >= 0) and ( SameValue(val, round(val), 1e-6) ) then
          begin
               val := Round(val);

               Result.Vec[i] := exp( gammaln( n + 1) - gammaln( val + 1) - gammaln( n - val + 1) +
                                     val*ln(p) + (n - val)*ln(1 - p) );
          end;
     end;
end;

initialization
  RegisterMathIO(TStatTest);

end.
