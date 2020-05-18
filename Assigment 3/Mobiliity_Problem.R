#11 Stochastic process - time to work
set.seed(630829)
rep<-10 #consider also 100, 1000, 10000
d<-c(0,1,1,23.6,24) #distances from home
z<-NULL
for (i in 1:rep) {
  t<-rep(0,5) #initializing the random vector
  t[2]<-rnorm(1,8,1)  #Walking time to the bus stop
  b<-rbinom(1,1,0.03) #3% of the times there is major disturbance in the service
  t[3]<-(1-b)*5*runif(1)+b*rexp(1,0.1)  #Waiting time at the bus stop given that buses are scheduled and I aim for 5 min
  t[4]<-(1-b)*rnorm(1,31,2)+b*(31+rexp(1,0.1)) #Driving time for the bus
  #t[5]<-rnorm(1,5,2) #Walking time for bus halt to office
  t[5]<-rnorm(1,(6-(sum(t[1:4]))/50),2) #Walking time from bus halt is dependent
  z1<-c(t[1],sum(t[1:2]),sum(t[1:3]),sum(t[1:4]),sum(t[1:5])) #Summing the duration
  z<-c(z,z1)
}
plot(d,z[1:5],type="l",xlab="travel distance",ylab="time in min",xlim=c(0,25),ylim=c(0,90))
for (j in 1:(rep-1)){
  #a<-combn(40,5)   #delays the graphics if you want
  lines(d,z[(5*j+1):(5*(j+1))],col=min(j,100))
}
m<-matrix(z,5,rep) #create a column for each trip
mean(m[5,])   #average travel time
sd(m[5,])     #standard dev of travel time
hist(m[5,])   #emp pdf
plot(ecdf(m[5,]))   #emp cdf




