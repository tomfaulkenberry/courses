## Week 8
## PSYC 5316

# globe tossing example
# show posterior as data comes in
# adapted from R. McElreath's book

# coding the data
# 1 indicates 'water'; 0 indicates 'land'
d <- c(1,0,1,1,1,0,1,0,1)
l <- ifelse(d==1, "W", "L")

# uniform prior
a = 1
b = 1

# draw first plot
i = 0
curve(dbeta(x,a,b), 
      from=0, to=1, xlab="proportion water", ylab="", 
      col="black", ylim=c(0,2.75), lwd=2, xaxt="n", yaxt="n"
      )
axis(1, at=c(0,0.5,1), labels=c(0,0.5,1))
title(ylab="probability", mgp=c(1,1,0))
text(0.15, 2.5, paste("n =",i))
mtext(paste(l), 
      at=seq(from=0,to=1,length.out=length(l)), 
      col=c(rep("darkgray", length(l))) 
      )


# execute this code chunk once for each subsequent plot
i = i + 1
a.prior = a
b.prior = b
a = a + d[i]
b = b + 1 - d[i]

# plot prior
curve(dbeta(x,a.prior,b.prior), 
      from=0, to=1, 
      xlab="probability of water", ylab="", 
      col="red", lwd=2, lty=2, ylim=c(0,2.75), xaxt="n", yaxt="n" 
)
mtext(paste(l), 
      at=seq(from=0,to=1,length.out=length(l)), 
      col=c(rep("black",i), rep( "darkgray", length(l)-i)))
axis(1, at=c(0,0.5,1) , labels=c(0,0.5,1) )
title(ylab="probability", mgp=c(1,1,0))
text(0.15, 2.5, paste("n =", i))

# plot posterior
curve(dbeta(x,a,b), 
      from=0, to=1, 
      lwd=2,
      add=TRUE 
      )
