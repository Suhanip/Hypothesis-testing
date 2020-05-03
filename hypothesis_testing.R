#One sample test
xbar = 130
mu0 = 100
sigma = 15
n = 25
z = (xbar - mu0)/(sigma/sqrt(n))
z
alpha = 0.05
z.alpha = qnorm(1-alpha)
z.alpha

#function for accept or reject of null hypothesis
accept_reject <- function(cal,tab)
{ if(cal < tab){ print("Accept H0")}
else { print("Reject H0")}
}
accept_reject(z,-z.alpha)

#Two-sample t-test
x <- c(3,3,12,17,19)
y <- c(20,32,30,13,13)
xbar = mean(x)
ybar = mean(y)
varx = var(x)
vary = var(y)
s = ((length(x)-1)*varx+(length(y)-1)*vary)/(length(x)+length(y)-2)
t = (xbar - ybar)/(s*sqrt(1/length(x)+1/length(y)))
t
alpha = 0.05
t.alpha = qnorm(1-alpha)
t.alpha
accept_reject(t,t.alpha)

#Paired t-test
Data = read.table(textConnection(Input),header = TRUE)
Input = ("
Bird   Typical  Odd
A     -0.255   -0.324
B     -0.213   -0.185
C     -0.190   -0.299
D     -0.185   -0.144
E     -0.045   -0.027
F     -0.025   -0.039
G     -0.015   -0.264
H      0.003   -0.077
I      0.015   -0.017
J      0.020   -0.169
K      0.023   -0.096
L      0.040   -0.330
M      0.040   -0.346
N      0.050   -0.191
O      0.055   -0.128
P      0.058   -0.182
")
Data = read.table(textConnection(Input),header=TRUE)
t.test(Data$Typical,Data$Odd,paired = TRUE)
Difference = Data$Odd - Data$Typical
plot(Difference,
pch = 16,
ylab="Difference (Odd – Typical)")
abline(0,0, col="blue", lwd=2)

#F-test
x <- rnorm(5,mean = 10.8,sd = 57.2)
y <- rnorm(5,mean = 21.6,sd = 82.3)
var.test(x,y)

#Intrnsic Chi-square test
observed       = c(1203,  2919,  1678)
expected.prop  = c(0.211, 0.497, 0.293)
expected.count = sum(observed)*expected.prop
chi2 = sum((observed- expected.count)^2/ expected.count)
chi2
pchisq(chi2,
df=1,
lower.tail=FALSE)

#Extrinsic Chi-Square test
observed = c(70, 79, 3, 4)
expected = c(0.54, 0.40, 0.05, 0.01)
chisq.test(x = observed,
p = expected)


#Chi-square test with Yates' Correction
x <- matrix(c(30,32,16,24),nc = 2)
chisq.test(x,correct = TRUE)
