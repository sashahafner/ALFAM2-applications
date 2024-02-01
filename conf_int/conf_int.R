# ALFAM2 confidence interval based on parameter uncertainty

library(ALFAM2)


set.seed(1201)
dat7 <- data.frame(ctime = 0:84*2, TAN.app = 100, man.dm = 8, 
                   air.temp = 7 + 7*sin(0:84*2 * 2*pi/24) + rnorm(85, 0, 2), 
                   wind = 10^(0.5 + 0.4*sin(0:84*2 * 2*pi/24) + 
                              rnorm(85, 0, 0.12)), 
                   app.mthd = "bc")
plot(air.temp ~ ctime, data = dat7, type = 'o', col = 'gray45')
plot(wind ~ ctime, data = dat7, type = 'o', col = 'blue')

pred7 <- alfam2(dat7, app.name = 'TAN.app', time.name = 'ctime',
                   warn = FALSE)

