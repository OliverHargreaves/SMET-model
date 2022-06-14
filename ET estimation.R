# 7d average ET estimation from soil moisture data and ETr

# Packages 
library(readxl)
library(writexl)

# Load data
data=read_xlsx('Vernal 2020.xlsx')
n=length(data$Date)

# Assign alpha value
a=0.426282799121919

# Calculate daily change in SM
data$delta=c(NA)
for (i in (2:n)) {
  data$delta[i]=data$SM[i]-data$SM[i-1]
}

# Calculate daily ET estimate
data$ET_d=c(NA)

for (i in (2:n)) {
  if (data$delta[i]<0) {data$ET_d[i]=a*(data$ETr[i]+abs(data$delta[i]))}
  else {data$ET_d[i]=2*a*data$ETr[i]}}

# Plot of daily values

plot(data$Date, data$ETor, type='l', col='goldenrod', ylab='mm/day', xlab='2020', ylim=c(0,12))
lines(data$Date, data$ETcl, col='olivedrab')
lines(data$Date, data$ET_d, col='cornflowerblue')
legend('topright', inset=0.01, legend=c('EC original', 'EC closed', 'ET'), lty=1, col=c('goldenrod', 'olivedrab', 'cornflowerblue'))

# Save daily results
write_xlsx(data, 'Daily Results 2020.xlsx')

# Calculate monthly cumulative ET
data$ET_d[1]=0
ETa_month=aggregate(x=data$ETa, by=list(data$Month), FUN=sum)
ETor_month=aggregate(x=data$ETor, by=list(data$Month), FUN=sum)
ETcl_month=aggregate(x=data$ETcl, by=list(data$Month), FUN=sum)
ET_month=aggregate(x=data$ET_d, by=list(data$Month), FUN=sum)

Cumulative.df=data.frame('Month'=4:10, 'ETa'=ETa_month$x, 'ETor'=ETor_month$x, 
                         'ETcl'=ETcl_month$x, 'ET'=ET_month$x)
# Save cumulative results
write_xlsx(Cumulative.df, 'Cumulative Results 2020.xlsx')


# Monthly cumulative plots
plot(Cumulative.df$Month, Cumulative.df$ETor, type='o', ylim=c(0,220), xlab='2020', ylab='mm/month', col='goldenrod')
lines(Cumulative.df$Month, Cumulative.df$ETcl, type='o', col='olivedrab')
lines(Cumulative.df$Month, Cumulative.df$ET, type='o', col='cornflowerblue')
legend('topright', inset=0.01, legend=c('ETor', 'ETcl', 'ET'), lty=1, col=c('goldenrod', 'olivedrab', 'cornflowerblue'))

# Calculate seasonal cumulative ET
data$ETor_cum=cumsum(data$ETor)
data$ETcl_cum=cumsum(data$ETcl)
data$ETa_cum=cumsum(data$ETa)
data$ET_cum=cumsum(data$ET_d)

# Plot seasonal cumulative ET
plot(data$Date, data$ETor_cum, type='l', ylim=c(0,1000), ylab='mm', xlab='2020', col='goldenrod')
lines(data$Date, data$ETcl_cum, col='olivedrab')
lines(data$Date, data$ET_cum, col='cornflowerblue')
legend('topleft', inset=0.01, legend=c('ETor', 'ETcl', 'ET'), lty=1, col=c('goldenrod', 'olivedrab', 'cornflowerblue'))
