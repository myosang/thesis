##############################
#Loading Packages
#############################

library(lda)
library(slam)
#install.packages("~/stmdev/rcode/stm_0.03.07.tar.gz", type="source", repos=NULL)
library(stm)

source("~/stmdev/rcode/stm/R/ploteffect.R")
source("~/stmdev/rcode/stm/R/findthoughts.R")
source("~/stmdev/rcode/stm/R/plottopics.R")

#############################
#Cleaning Data
#############################

setwd("../Data")

dataldac <- read.ldac("gadarian_final_tdm.csv")
vocabcoarse <- read.csv("gadarian_vocab.csv")$term
meta <- read.csv("gadarian_metadata.csv")
meta$treatment <- as.numeric(meta$treat=="1. worried")

data <- prep.documents(dataldac, vocabcoarse, meta=meta)
documents <- data$documents
vocab <- data$vocab
meta <- data$meta

#####################################
#Model with fear treatment and PID interaction
#####################################

#Select Model
#set.seed(02138)
#models <- selectModel(documents, vocab, 3, prevalence=~meta$treatment + meta$pid_rep + meta$treatment*meta$pid_rep, runs=50)
#plotModels(models)
#mod <- models$runout[[7]]

#Recover selected model
#seed: 6170546
mod <- stm(documents, vocab, 3, prevalence=~meta$treatment, seed=6170546)
labeltopics(mod)

#Treatment effect plot
prep <- prep.plot(mod, meta$treatment, theta.uncertainty="Global", documents=documents, ci.level=.9)
ploteffect(prep, choosetopics=1:2,model=mod, xlab="Difference in Topic Proportions (Treated-Control)", labeltype="numbers", xlim=c(-.4,.3))
plottopics(mod, topics=1:2, width=40)

#Interaction plot
prep21 <- prep.plot(mod, meta$pid_rep, topics=1, theta.uncertainty="Global", span=5, ci.level=.9, documents=documents, subset=as.numeric(meta$treatment==1))
ploteffect(prep21, choosetopics=1,model=mod, ylab="Mean Topic Proportions", xlab="Party ID", labeltype="prob", xlim=c(0,1), printlegend=F, ylim=c(0,.7), xaxt="n")

prep20 <- prep.plot(mod, meta$pid_rep, topics=1, theta.uncertainty="Global", span=5, ci.level=.9, documents=documents, subset=as.numeric(meta$treatment==0))
ploteffect(prep20, choosetopics=1,model=mod, ylab="Mean Topic Proportions", xlab="Party ID", labeltype="prob", xlim=c(0,1), add=T, printlegend=F, linecol="blue")

axis(1, at=c(0,.5,1), c("Strong \n Democrat", "Moderate", "Strong \n Republican"), padj=.5)

text(.35, .55, "Treated", col="red")
text(.75, .15, "Control", col="blue")

#Find thoughts
thoughts <- findthoughts(mod, texts=meta$open.ended.response, topics=1:3, n=10)

meta$fear_ra1[thoughts$index[,1]]
meta$fear_ra2[thoughts$index[,1]]
plotquote(meta$open.ended.response[thoughts$index[1,1]], width=7)
plotquote(meta$open.ended.response[thoughts$index[4,1]], width=5)

meta$fear_ra1[thoughts$index[,2]]
meta$fear_ra2[thoughts$index[,2]]
plotquote(meta$open.ended.response[thoughts$index[7,2]], width=6)
plotquote(meta$open.ended.response[thoughts$index[1,2]], width=5)

#Estimating treatment effect
topic1effect <- t.test(mod$theta[,1][meta$treatment==1], mod$theta[,1][meta$treatment==0], conf.level=.9)
topic1effect$estimate[1] - topic1effect$estimate[2]
topic1effect$conf.int

#Estimating PID effect
topic1effect <- t.test(mod$theta[,1][meta$pid_rep>.5], mod$theta[,1][meta$pid_rep<.5], conf.level=.9)
topic1effect$estimate[1] - topic1effect$estimate[2]
topic1effect$conf.int


#Estimating Interaction effect
topic1effect <- t.test(mod$theta[,1][meta$pid_rep>.5 & meta$treatment==1], mod$theta[,1][meta$pid_rep<.5 & meta$treatment==0], conf.level=.9)
topic1effect$estimate[1] - topic1effect$estimate[2]
topic1effect$conf.int

#Average amont of time on each topic
apply(mod$theta[meta$pid_rep>.5 & meta$treatment==1,],2,mean)
apply(mod$theta[meta$pid_rep<.5 & meta$treatment==0,],2,mean)

