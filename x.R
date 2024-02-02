ff <- list.files('../R', full.names = TRUE)
ff <- ff[!grepl('x.R', ff)]
for (i in ff) source(i)

dat1 <- data.frame(ctime = 168, TAN.app = 50, man.dm = 8, 
                   air.temp = 20, wind.2m = 3, 
                   app.mthd.bc = TRUE)
library(ALFAM2)
pred1 <- alfam2(dat1, app.name = 'TAN.app', time.name = 'ctime', conf.int = 0.95, pars.ci = alfam2pars03var_alpha, n.ci = 10)
pred1 <- alfam2(dat1, app.name = 'TAN.app', time.name = 'ctime', conf.int = 'all', pars.ci = alfam2pars03var_alpha, n.ci = 10)
pred1 <- alfam2(dat1, app.name = 'TAN.app', time.name = 'ctime', conf.int = 0.95, pars.ci = alfam2pars03var_alpha, n.ci = 1000)
pred1 <- alfam2(dat1, app.name = 'TAN.app', time.name = 'ctime', conf.int = 0.95, pars.ci = alfam2pars03var_alpha, n.ci = 10, var.ci = c('er', 'jx'))
pred1 <- alfam2(dat1, app.name = 'TAN.app', time.name = 'ctime', conf.int = 0.95, pars.ci = alfam2pars03var_alpha, n.ci = 10, var.ci = 'jx')
Error: Expect one of the following values "f0, r1, r2, r3, r4, r5, f, s, j, ei, e, er" for argument var.ci but got "er, jx".
>Error: Expect one of the following values "f0, r1, r2, r3, r4, r5, f, s, j, ei, e, er" for argument var.ci but got "erjx".
Error: Expect one of the following values "f0, r1, r2, r3, r4, r5, f, s, j, ei, e, er" for argument var.ci but got "jx".
>>warnings()
