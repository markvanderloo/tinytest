

library(pkgsearch)

db <- tools::CRAN_package_db()

suggests <- db[db$Package=="tinytest","Reverse suggests"]
imports  <- db[db$Package=="tinytest","Reverse imports"]

suggests <- strsplit(suggests, ", ")[[1]]
imports  <- strsplit(imports, ", ")[[1]]

S <- lapply(suggests, pkgsearch::cran_package_history)
I <- lapply(imports, pkgsearch::cran_package_history)

S <- lapply(S, as.data.frame)
I <- lapply(I, as.data.frame)

first_tt <- function(df,type="Suggests"){
  for ( i in seq_len(nrow(df))){
    m <- df[,"dependencies"][[i]]
    m <- m[m$type == type,]
    if (nrow(m) == 0) next
    if ("tinytest" %in% m$package) return(substr(df[i,"Date/Publication"],1,10)) 
  }
  NA
}

s <- sapply(S, first_tt)
i <- sapply(I, first_tt, type="Imports")

sug <- data.frame(package = suggests, first_suggested=s)
imp <- data.frame(package = imports,  first_imported =s)

write.csv(sug, "suggested.csv", row.names=FALSE)
write.csv(imp, "imported.csv", row.names=FALSE)


# aggregate and make plot
sug <- read.csv("suggested.csv", colClasses=c("character","Date"))
dates <- seq(as.Date("2019-04-25"), Sys.Date(), by="day")

n <- numeric(length(dates))
i <- 0
for ( day in dates){
  i <- i+1
  n[i] <- sum(sug$first_suggested<=day)
}

d <- data.frame(date=dates, suggesting=n)

write.csv(d, file="suggesting_tinytest.csv", row.names=FALSE)

png("growth.png")
par(mar=c(5,3,4.1,1))
plot(suggesting ~ date, data=d
  , main="Packages suggesting tinytest"
  , ylab=""
  , xlab=""
  , type="l"
  , xaxt="n"
  , las=1
  , lwd=2
  ,cex.axis=1.1)
ax <- seq(as.Date("2019-03-01"), as.Date("2020-05-01"),by="month")
axis.Date(1, at=ax[c(T,F)],format="%Y-%m"
          , cex.axis=1.1,las=2)
abline(v=ax, h=seq(0,50,10),lty=3)
dev.off()








