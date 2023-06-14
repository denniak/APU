setwd("E:\\Users\\denn\\Desktop\\apu")
install.packages("data.tree")
install.packages("formattable")
install.packages("DiagrammeR")
install.packages("E:/Users/denn/Desktop/apu/ahp_0.2.12.tar.gz",repos=NULL,type="source")


library(data.tree)
library(formattable)
library(DiagrammeR)
library(ahp)

AHPLoad<-Load("iPady.yml")
print(AHPLoad, filterFun = isNotLeaf)
Calculate(AHPLoad)
Analyze(AHPLoad)
AnalyzeTable(AHPLoad)
Visualize(AHPLoad)
