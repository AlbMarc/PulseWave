#install.packages("iplots",dep=TRUE)
#install.packages("ggplot2")
library(edf)
require(ggplot2)
#source("~/ASIC_Study/ASIC_vs_SphygmoCor/AdditionalR/1_Datenformatierung.R")

source("~/PulseWave/PulseWave/PulseWave/Init.R")

#library("PerformanceAnalytics")
source("~/R-Skripte/DataAnalysis/CorrelationTools.R")

source("~/R-Skripte/DataAnalysis/BasicPlotting.R")

#source("~/PulseWave/PulsWaveInit.R")



######################################## Main Script ############################


funk1 = function(x) sin(x)
funk2 = function(x) 0.3 * sin(2.1 * x - 2 * pi)
funk3 = function(x) funk1(x) + funk2(x)

num <- 2000
n <- 24
t <- 0.5

plotdata1 <- data.frame(x = c(0, n * pi))

f <- ggplot(plotdata1) + stat_function(aes(x), fun = funk1, colour = "steelblue2", n = num) +
    stat_function(aes(x), fun = funk2, colour = "coral2", n = num) +
    stat_function(aes(x), fun = funk3, colour = "springgreen3", size = 1.5, n = num) +
    geom_vline(xintercept = 4.5 * pi, size = 1, color = "steelblue3", linetype = "dashed")

plotdata2 <- data.frame(x = c(0, n * pi))

mod = function(x) t #*(1+(x/500))
funk4 = function(x) sin(x)
funk5 = function(x) sin(mod(x) * x - pi)
funk6 = function(x) 0.8 * sin(x - pi)
funk7 = function(x) 0.3 * sin(mod(x) * x - 2 * pi)
funk8 = function(x) funk4(x) + funk6(x)
funk9 = function(x) funk5(x) + funk7(x)
funk10 = function(x) funk8(x) + funk9(x)

g <- ggplot(plotdata2) + stat_function(aes(x), fun = funk4, colour = "steelblue2", n = num) +
    stat_function(aes(x), fun = funk6, colour = "coral2", n = num) +
    stat_function(aes(x), fun = funk8, colour = "springgreen3", size = 1, n = num) +
    geom_vline(xintercept = 4.5 * pi, size = 1, color = "steelblue3", linetype = "dashed")

h <- ggplot(plotdata2) + stat_function(aes(x), fun = funk5, colour = "steelblue2", n = num) +
    stat_function(aes(x), fun = funk7, colour = "coral2", n = num) +
    stat_function(aes(x), fun = funk9, colour = "springgreen3", size = 1, n = num) +
    geom_vline(xintercept = 4.5 * pi, size = 1, color = "steelblue3", linetype = "dashed")

i <- ggplot(plotdata2) + stat_function(aes(x), fun = funk8, colour = "steelblue2", n = num) +
    stat_function(aes(x), fun = funk9, colour = "coral2", n = num) +
    stat_function(aes(x), fun = funk10, colour = "springgreen3", size = 1.5, n = num) +
    geom_vline(xintercept = 4.5 * pi, size = 1, color = "steelblue3", linetype = "dashed")

funk11 = function(x) funk3(x)
funk12 = function(x) funk10(2 * (x - 3 * pi))

j <- ggplot(plotdata1) + stat_function(aes(x), fun = funk11, colour = "steelblue2", size = 1, n = num) +
    stat_function(aes(x), fun = funk12, colour = "coral2", size = 1, n = num) +
    geom_vline(xintercept = 4.5 * pi, size = 1, color = "steelblue3", linetype = "dashed")


#multiplot(g,h,i,j,cols=1)


plotdata3 <- data.frame(fulldata$signal$PLETHY)
plotdata3$x <- c(440.75, pi + 440.75)

temp <- c(11020:11120)

funk13 = function(x) 17000 * funk10(14.75 * (x - 0.75 * pi))
funk14 = function(x) 17000 * funk4(14.75 * (x - 0.75 * pi))
funk15 = function(x) 17000 * funk5(14.75 * (x - 0.75 * pi))
funk16 = function(x) 17000 * funk6(14.75 * (x - 0.75 * pi))
funk17 = function(x) 17000 * funk7(14.75 * (x - 0.75 * pi))


k <- ggplot() +
    geom_line(aes(plotdata3[temp,]$t, plotdata3[temp,]$data, colour = "graph from *.rec"), size = 1) +
    stat_function(aes(plotdata3$x, colour = "graph from sinus function"), fun = funk13, size = 1, n = num) +
    stat_function(aes(plotdata3$x, colour = "initial heart wave"), fun = funk14, size = 0.5, n = num) +
    stat_function(aes(plotdata3$x, colour = "initial valve wave"), fun = funk15, size = 0.5, n = num) +
    labs(title = "Puls Wave from t=440s to t=445s", x = "time", y = "ampl") +
    scale_colour_manual("",
                      breaks = c("graph from *.rec", "graph from sinus function", "initial heart wave", "initial valve wave"),
                      values = c("graph from *.rec" = "steelblue2", "graph from sinus function" = "coral2", "initial heart wave" = "springgreen2", "initial valve wave" = "orange2"))

l <- ggplot() +
    geom_line(aes(plotdata3[temp,]$t, plotdata3[temp,]$data, colour = "graph from *.rec"), size = 1) +
    stat_function(aes(plotdata3$x, colour = "graph from sinus function"), fun = funk13, size = 1, n = num) +
    stat_function(aes(plotdata3$x, colour = "reflected heart wave"), fun = funk16, size = 0.5, n = num) +
    stat_function(aes(plotdata3$x, colour = "reflected valve wave"), fun = funk17, size = 0.5, n = num) +
    labs(title = "Puls Wave from t=440s to t=445s", x = "time", y = "ampl") +
    scale_colour_manual("",
                      breaks = c("graph from *.rec", "graph from sinus function", "reflected heart wave", "reflected valve wave"),
                      values = c("graph from *.rec" = "steelblue2", "graph from sinus function" = "coral2", "reflected heart wave" = "springgreen2", "reflected valve wave" = "orange2"))

plotdata4 <- data.frame(fulldata$signal$PLETHY)
plotdata4$x <- c(440.75, 4 * pi + 440.75)

temp2 <- c(11020:11320)

funk18 = function(x) 15000 * funk10(14.885 * (x - 2.295 * pi))

m <- ggplot() +
    geom_line(aes(plotdata4[temp2,]$t, plotdata4[temp2,]$data, colour = "graph from *.rec"), size = 1) +
    stat_function(aes(plotdata4$x, colour = "graph from sinus function"), fun = funk18, size = 1, n = num) +
    labs(title = "Puls Wave from t=440s to t=453s", x = "time", y = "ampl") +
    scale_colour_manual("",
                      breaks = c("graph from *.rec", "graph from sinus function"),
                      values = c("graph from *.rec" = "steelblue2", "graph from sinus function" = "coral2"))

multiplot(k,l,m,cols=1)



plotdata5 <- data.frame(fulldata$signal$PLETHY)

der1 <- apply(plotdata5, 2, diff)

der1 <- rbind(c(0,0), der1)

plotdata5$derive1 <- der1[, 1]

der2 <- apply(plotdata5, 2, diff)

der2 <- rbind(c(0,0,0), der2)

plotdata5$derive2 <- der2[, 3]

der3 <- apply(plotdata5, 2, diff)

der3 <- rbind(c(0, 0, 0, 0), der3)

plotdata5$derive3 <- der3[, 4]


temp3 <- c(250000:250250)

funk19 = function(x) 15000 * funk10(20 * x)

n <- ggplot() +
    geom_line(aes(plotdata5$t[temp3], plotdata5$data[temp3]), color = "steelblue2") +
    #stat_function(aes(plotdata5[temp3,]$t), fun = funk19, color = "coral2", n = num) +
    geom_line(aes(plotdata5$t[temp3], plotdata5$derive1[temp3]), color = "coral2") +
    geom_line(aes(plotdata5$t[temp3], plotdata5$derive2[temp3]), color = "springgreen3") +
    geom_line(aes(plotdata5$t[temp3], plotdata5$derive3[temp3])) +
    geom_vline(xintercept = plotdata5$t[temp3][((c(1, plotdata5$derive1[temp3]) < 0) & (c(plotdata5$derive1[temp3], 1) > 0))], color = "coral2") +
    geom_vline(xintercept = plotdata5$t[temp3][((c(1, plotdata5$derive1[temp3]) > 0) & (c(plotdata5$derive1[temp3], 1) < 0))], color ="steelblue2")

#n

plotdata6 <- data.frame(fulldata$signal$PLETHY)

der1 <- apply(plotdata6, 2, diff)

der1 <- rbind(c(0, 0), der1)

plotdata6$derive1 <- der1[, 1]

temp4 <- (which((c(1, plotdata6$derive1) < 0) & (c(plotdata6$derive1, 1) > 0))-1)

x <-1001

o <- ggplot() +
    geom_line(aes(plotdata6$t[0:(temp4[x + 1] - temp4[x] + 1)], plotdata6$data[temp4[x]:temp4[x + 1]]), color = "steelblue2") +
    geom_line(aes(plotdata6$t[0:(temp4[x + 2] - temp4[x + 1] + 1)], plotdata6$data[temp4[x + 1]:temp4[x + 2]]), color = "steelblue2") +
    geom_line(aes(plotdata6$t[0:(temp4[x + 3] - temp4[x + 2] + 1)], plotdata6$data[temp4[x + 2]:temp4[x + 3]]), color = "steelblue2") +
    geom_line(aes(plotdata6$t[0:(temp4[x + 4] - temp4[x + 3] + 1)], plotdata6$data[temp4[x + 3]:temp4[x + 4]]), color = "steelblue2") +
    geom_line(aes(plotdata6$t[0:(temp4[x + 5] - temp4[x + 4] + 1)], plotdata6$data[temp4[x + 4]:temp4[x + 5]]), color = "steelblue2") +
    geom_line(aes(plotdata6$t[0:(temp4[x + 6] - temp4[x + 5] + 1)], plotdata6$data[temp4[x + 5]:temp4[x + 6]]), color = "steelblue2") +
    geom_line(aes(plotdata6$t[0:(temp4[x + 7] - temp4[x + 6] + 1)], plotdata6$data[temp4[x + 6]:temp4[x + 7]]), color = "steelblue2") +
    geom_line(aes(plotdata6$t[0:(temp4[x + 8] - temp4[x + 7] + 1)], plotdata6$data[temp4[x + 7]:temp4[x + 8]]), color = "steelblue2") +
    geom_line(aes(plotdata6$t[0:(temp4[x + 9] - temp4[x + 8] + 1)], plotdata6$data[temp4[x + 8]:temp4[x + 9]]), color = "steelblue2") +
    geom_line(aes(plotdata6$t[0:(temp4[x + 10] - temp4[x + 9] + 1)], plotdata6$data[temp4[x + 9]:temp4[x + 10]]), color = "steelblue2") +
    geom_line(aes(plotdata6$t[0:(temp4[x + 11] - temp4[x + 10] + 1)], plotdata6$data[temp4[x + 10]:temp4[x + 11]]), color = "steelblue2") +
    geom_line(aes(plotdata6$t[0:(temp4[x + 12] - temp4[x + 11] + 1)], plotdata6$data[temp4[x + 11]:temp4[x + 12]]), color = "steelblue2") +
    geom_line(aes(plotdata6$t[0:(temp4[x + 13] - temp4[x + 12] + 1)], plotdata6$data[temp4[x + 12]:temp4[x + 13]]), color = "steelblue2") +
    geom_line(aes(plotdata6$t[0:(temp4[x + 14] - temp4[x + 13] + 1)], plotdata6$data[temp4[x + 13]:temp4[x + 14]]), color = "steelblue2") +
    geom_line(aes(plotdata6$t[0:(temp4[x + 15] - temp4[x + 14] + 1)], plotdata6$data[temp4[x + 14]:temp4[x + 15]]), color = "steelblue2") +
    geom_line(aes(plotdata6$t[0:(temp4[x + 16] - temp4[x + 15] + 1)], plotdata6$data[temp4[x + 15]:temp4[x + 16]]), color = "steelblue2") +
    geom_line(aes(plotdata6$t[0:(temp4[x + 17] - temp4[x + 16] + 1)], plotdata6$data[temp4[x + 16]:temp4[x + 17]]), color = "steelblue2") +
    geom_line(aes(plotdata6$t[0:(temp4[x + 18] - temp4[x + 17] + 1)], plotdata6$data[temp4[x + 17]:temp4[x + 18]]), color = "steelblue2") +
    geom_line(aes(plotdata6$t[0:(temp4[x + 19] - temp4[x + 18] + 1)], plotdata6$data[temp4[x + 18]:temp4[x + 19]]), color = "steelblue2") +
    geom_line(aes(plotdata6$t[0:(temp4[x + 20] - temp4[x + 19] + 1)], plotdata6$data[temp4[x + 19]:temp4[x + 20]]), color = "steelblue2")


p <- ggplot() +
    geom_line(aes(plotdata6$t[temp4[x]:temp4[x + 20]], plotdata6$data[temp4[x]:temp4[x + 20]]), color = "steelblue2") +
    geom_vline(xintercept = plotdata6$t[temp4[x : (x + 20)]], color = "coral2")

multiplot(o, p, cols = 2)

#q <- ggplot() +
#    geom_line(aes(plotdata6$t, plotdata6$data), color = "steelblue2")

#q

#source("~/PulseWave/PulseWave/PulseWave/Pres.R")