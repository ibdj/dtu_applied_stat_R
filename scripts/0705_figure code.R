

letters.to.plot<-unlist(strsplit("INTRODUCTION TO APPLIED STATISTICS WITH R",split=character(0)))
index.to.plot<-seq(-2,2,length=length(letters.to.plot))

curve(dnorm,-3,3,ylim=c(0,0.6),xlab=' ',ylab=' ',axes=F)
for(i in 1:length(index.to.plot)){
 text(index.to.plot[i],dnorm(index.to.plot[i])+0.02,letters.to.plot[i],
      srt=180*tan(-index.to.plot[i]*dnorm(index.to.plot[i])),cex=2,
      family="sans", font=2)
  }
axis(1)
