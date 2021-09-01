library(knitr)
setwd("~/PulseWave/PulseWave/PulseWave/Presentation/")
system("cmd.exe", input="copy .\\Temp.tex .\\PWPres.rnw /Y")
knit("~/PulseWave/PulseWave/PulseWave/Presentation/PWPres.rnw", encoding = "UTF-8");
system("pdflatex PWPres.tex && pdflatex PWPres.tex")
system("cmd.exe", input=".\\PWPres.pdf")
