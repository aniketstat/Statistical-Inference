# Example 1:-------------------------------

head(teen)
dim(teen)
mean(teen$like)

# -----------------------------------------

# Example 3:-------------------------------

mean(teen$number.orders)

#------------------------------------------

# Example 6:-------------------------------

head(breakfast)
dim(breakfast)
mean(breakfast$avg.prep.time)
var(breakfast$avg.prep.time)

#-------------------------------------------

# Example 7:--------------------------------

head(family)
prop.expense=family$avg.monthly.food.expense/family$avg.monthly.income
m1.prime=mean(prop.expense)
m2=var(prop.expense)
alpha.moment.est=m1.prime*(((m1.prime*(1-m1.prime))/m2)-1)
beta.moment.est=alpha.moment.est*(1-m1.prime)/m1.prime
alpha.moment.est
beta.moment.est

# ------------------------------------------

# Example 11:-------------------------------

prop.expense=family$avg.monthly.food.expense/family$avg.monthly.income
n=length(prop.expense)
x=prop.expense
neg.log.likelihood.beta=function(parameter)
{
alpha=parameter[1]
beta=parameter[2]
loglik=(alpha-1)*sum(log(x))+(beta-1)*sum(log(1-x))-n*(log(gamma(alpha))+log(gamma(beta))-log(gamma(alpha+beta)))
return(-loglik)
}
init.parameter=c(2.004687,4.225408)
object=optim(init.parameter,neg.log.likelihood.beta)
object$par
object$value

# --------------------------------------------

# Example 15:---------------------------------

x=breakfast$avg.prep.time
n=length(breakfast$avg.prep.time)
(n/(n-1))*var(x)

# --------------------------------------------

# Section 3.1:--------------------------------

p0=0.7
p.hat=mean(teen$like)
N=length(teen$like)
t=(p.hat-p0)/(sqrt((p.hat*(1-p.hat))/N))
alpha=0.05
cutoff=qnorm(1-alpha)
pval=1-pnorm(t)
t
cutoff
pval

# --------------------------------------------

# Section 3.2:--------------------------------

male.like=teen$like[teen$gender=="M"]
female.like=teen$like[teen$gender=="F"]
X1=sum(male.like)
X2=sum(female.like)
N1=length(male.like)
N2=length(female.like)
p1.hat=X1/N1
p2.hat=X2/N2
p.hat=(X1+X2)/(N1+N2)
t=(p1.hat-p2.hat)/(sqrt(p.hat*(1-p.hat)*((1/N1)+(1/N2))))
alpha=0.05
pos.cutoff=qnorm(1-alpha/2)
neg.cutoff=qnorm(alpha/2)
pval=2*min(1-pnorm(t),pnorm(t))
t
pos.cutoff
neg.cutoff
pval

# -------------------------------------------

# Section 3.3:-------------------------------

location.shift.variable=breakfast$avg.prep.time-13
t.test(location.shift.variable,alternative="greater")

# -------------------------------------------

# Section 3.4:-------------------------------

male.avg.prep.time=breakfast$avg.prep.time[gender=="M"]
female.avg.prep.time=breakfast$avg.prep.time[gender=="F"]
t.test(male.avg.prep.time,female.avg.prep.time)

# --------------------------------------------

# Section 3.4:-------------------------------

head(dinner)
z=dinner$after.time-dinner$before.time
t.test(z,alternative="less")

# --------------------------------------------

# Section 3.5:--------------------------------

head(beverages)
cross.table=table(beverages)
chisq.test(cross.table)

# --------------------------------------------

# Section 3.6:--------------------------------

p0=0.7
pval=0
for(i in 1:length(tabulate(teen$location)))
{
p.hat=mean(teen$like[teen$location==i])
N=length(teen$like[teen$location==i])
t=(p.hat-p0)/(sqrt((p.hat*(1-p.hat))/N))
pval[i]=1-pnorm(t)
}
alpha=0.05
which(p.adjust(pval,method="bonferroni")<alpha)
which(p.adjust(pval,method="holm")<alpha)
which(p.adjust(pval,method="BH")<alpha)

# ---------------------------------------------

# Example 17:-----------------------------------

p.hat=mean(teen$like)
N=length(teen$like)
alpha=0.05
a=p.hat-qnorm(1-alpha/2)*sqrt((p.hat*(1-p.hat))/N)
b=p.hat+qnorm(1-alpha/2)*sqrt((p.hat*(1-p.hat))/N)
CI=c(a,b)
CI

# ----------------------------------------------

# Example 18:-----------------------------------

x.bar=mean(breakfast$avg.prep.time)
s2=(n/(n-1))*var(breakfast$avg.prep.time)
n=length(breakfast$avg.prep.time)
alpha=0.05
a=x.bar-qt(1-alpha/2,n-1)*sqrt(s2/n)
b=x.bar+qt(1-alpha/2,n-1)*sqrt(s2/n)
CI=c(a,b)
CI

# ----------------------------------------------
























