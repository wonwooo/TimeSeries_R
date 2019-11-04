library(fpp)
library(ggplot2)
library(forecast)
library(lubridate)
library(extrafont)
library(lmtest)
library(portes)
library(tseries)
library(zoo)
library(gridExtra)
#setwd("C:/Users/a2678/Timeseries")

them=theme(plot.title = element_text(hjust = 0.5, color = 'black',face='bold', family='Simplifica', size =30, margin = margin(0,0,20,0)))+
  theme(panel.background = element_rect(fill='ivory2'))+
  theme(panel.grid.major = element_line(size = 1, linetype = 'solid', colour = "white"), panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "white"))+
  theme(axis.title = element_text(hjust = 0.5, color = 'black', family='Iropke Batang Medium', size =15))


them2= theme(plot.title = element_text(hjust = 0.5, color = 'royalblue4',face='bold', family='Simplifica', size =30, margin = margin(0,0,20,0)))+
  theme(panel.background = element_rect(fill='azure2'))+
  theme(panel.grid.major = element_line(size = 1, linetype = 'solid', colour = "white"), panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "white"))+
  theme(axis.title = element_text(hjust = 0.5, color = 'black', family='Iropke Batang Medium', size =15))

windowsFonts()
loadfonts(device = 'win')


##############################preprocessing###########################
df = read.csv("flowers.txt")
df2 = subset(df, select = c("SALE_DATE", "GOOD_NAME", "COST"))
vitaldata = df2[df2$GOOD_NAME == '비탈',]
vital['time'] <- 1:length(vital$cost)
vital = aggregate(vitaldata['COST'], list(vitaldata$SALE_DATE), mean)
colnames(vital) = c("time", "cost")

write.csv(vital, file='vital.csv', row.names = F )

vital = read.csv('vital.csv')
vital['index'] = 1:length(vital$cost)

################plot 결과 이분산, stochastic trend 발견 #####################
ggplot(data = vital, aes(x=index, y=cost))+
  geom_line(colour='darkslategrey', size=0.75)+ggtitle('Vital Time Series plot')+them2

###매우 느리게 감소하는 acf로 인해 stochastic trend 예측####
ggAcf(vital$cost)+ggtitle("ACF on vital cost")+geom_segment(colour='darkred', size=0.75)+them+ylim(-1,1)
ggPacf(vital$cost)+ggtitle("PACF on vital cost")+geom_segment(colour='darkred', size=0.75)+them+ylim(-1,1)

######먼저 등분산성을 위해 log변환######
logvital = log(vital$cost)
vital['logvital']= logvital

ggplot(data = vital, aes(x=index, y=logvital))+
  geom_line(colour='darkslategrey', size=0.75)+ggtitle('LOG Vital Time Series plot')+them2

ggAcf(vital$logvital)+ggtitle("ACF on LOG vital cost")+geom_segment(colour='darkred', size=0.75)+them+ylim(-1,1)
ggPacf(vital$logvital)+ggtitle("PACF on LOG vital cost")+geom_segment(colour='darkred', size=0.75)+them+ylim(-1,1)
###여전히 매우 천천히 감소하는 acf를 통해 stochastic trend 예측 가능 

### 등분산은 다소 해결 되었으나 이제 stochastic trend 제거를 위해 1차 단순차분#######
diff1 = diff(logvital)

###########################stochastic trend(단위근)가 남았는지 adftest 수행##########
adf.test(diff1)####단위근검정 통과

diffdata = data.frame('diffed' = diff1, 'Time'=1:length(diff1))
ggplot(data = diffdata, aes(x=Time, y=diffed))+
  geom_line(colour='darkslategrey', size=0.75)+ggtitle('LOG&diffed vital')+them2



plot1 = ggAcf(diff1)+ggtitle("ACF on LOG&diffed vital")+geom_segment(colour='darkred', size=0.75)+them+ylim(-1,1)
plot2 = ggPacf(diff1)+ggtitle("PACF on LOG&diffed vital")+geom_segment(colour='darkred', size=0.75)+them+ylim(-1,1)
grid.arrange(plot1, plot2, nrow=1, ncol=2)
########arima(3,1,0)과 arima(1,1,1)최종 적합 고려


model1 = Arima(y=logvital, order = c(3,1,0), include.constant = F)

model2 = Arima(y=logvital, order = c(1,1,1), include.constant = F)



##########aic, bic, 계수검정 결과확인
summary(model1)
coeftest(model1)

summary(model2)
coeftest(model2)

ggtsdisplay(model2$residuals, lag.max = 100)
#############잔차의 자기상관 검정, 잔차의 acf, pacf, plot확인 ############
BoxPierce(model2$residuals, lags=seq(6,30,6))


residf1 = data.frame('residual'= model2$residuals, 'time' = 1:length(model2$residuals))
ggplot(data = residf1, aes(x=time, y=residual))+geom_line(colour='darkslategrey', size=0.75)+ggtitle("Residual plot on ARIMA(1,1,1)")+them2
r1 = ggAcf(model2$residuals, lag.max=100)+ggtitle("RACF ON ARIMA(1,1,1)")+geom_segment(colour='darkred', size=0.75)+them+ylim(-1,1)
r2 = ggPacf(model2$residuals, lag.max=100)+ggtitle("RPACF ON ARIMA(1,1,1)")+geom_segment(colour='darkred', size=0.75)+them+ylim(-1,1)
grid.arrange(r1, r2, nrow=1, ncol=2)

ggplot(data=residf1 ,aes(sample=residual))+stat_qq(colour='darkslategrey')+
  ggtitle('QQ plot on Residual')+them2


########################################forecasting####################
fcast1 = forecast(model1, h=25)

autoplot(fcast1)+
  autolayer(fitted(fcast1))+them2

#####model2. stl라이브러리로 지시변수를 이용하여 계절성분과 trend 성분 제거#######

ggtsdisplay(logvital)

dd = ts(logvital,frequency = 160)
stldata = stl(dd, s.window='periodic', t.degree = 0)
plot(stldata)
stldf =data.frame(stldata$time.series)
stldf['data'] = logvital
stldf['time'] = 1:length(logvital)

p1 = ggplot(data = stldf, aes(x=time, y=data))+
  geom_line(colour='darkslategrey', size=0.75)+ggtitle('LOG vital')+them2
p2 = ggplot(data = stldf, aes(x=time, y=seasonal))+
  geom_line(colour='darkred', size=0.75)+ggtitle('Seosonal term on LOG vital')+them
grid.arrange(p1, p2, nrow=2, ncol=1)
p3 = ggplot(data = stldf, aes(x=time, y=remainder))+
  geom_line(colour='darkslategrey', size=0.75)+ggtitle('Remainder')+them2
grid.arrange(p1, p2, p3, nrow=3, ncol=1)

adf.test(stldf$remainder)

r1 = ggAcf(stldf$remainder, lag.max=50)+ggtitle("ACF ON REMAINDER")+geom_segment(colour='darkred', size=0.75)+them+ylim(-1,1)
r2 = ggPacf(stldf$remainder, lag.max=50)+ggtitle("PACF ON REMAINDER")+geom_segment(colour='darkred', size=0.75)+them+ylim(-1,1)
grid.arrange(r1, r2, nrow=1, ncol=2)
########################계절성분과 trend 제거한 term에 ar(2) 적합####################

model3 = Arima(y = stldf$remainder, order = c(2,0,0), include.constant = F)

####계수검정 결과####
summary(model3)
coeftest(model3)

#############잔차 자기상관검정#############
BoxPierce(model3$residuals)

############################잔차 PLOT, ACF, PACF##########################
residf2 = data.frame('residual' = model3$residuals, 'time' = 1:length(model3$residuals))

ggplot(data=residf2, aes(x=time, y=residual))+geom_line(colour = 'darkslategrey', size=0.75)+ggtitle("Residual plot on ARIMA(2,0,0)")+them2
p1 = ggAcf(model3$residuals, lag.max=100)+ggtitle("RACF ON ARIMA(2,0,0)")+geom_segment(colour='darkred', size=0.75)+them+ylim(-1,1)
p2 = ggPacf(model3$residuals, lag.max=100)+ggtitle("RPACF ON ARIMA(2,0,0)")+geom_segment(colour='darkred', size=0.75)+them+ylim(-1,1)
grid.arrange(p1, p2, nrow = 1, ncol=2)

ggplot(data=residf2 ,aes(sample=residual))+stat_qq(colour='darkslategrey')+
  ggtitle('QQ plot on Residual')+them2
BoxPierce(model3$residuals)


########################forecasting########################
fcast2 = forecast(model3, h=25)

autoplot(fcast2)+
  autolayer(fitted(fcast2))+them2
