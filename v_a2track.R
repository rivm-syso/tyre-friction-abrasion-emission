library(openxlsx)
library(pracma)

excelNames <- c("/rivm/r/E121554 LEON-T/03 - uitvoering WP3/IDIADA track data/Files sent by David Lopez/GPSExample1.xlsx",
                "/rivm/r/E121554 LEON-T/03 - uitvoering WP3/IDIADA track data/Files sent by David Lopez/GPSExample2.xlsx")

AsList <- list()
for(exnum in 1:length(excelNames)){
  exnm <- excelNames[exnum]
  sheets <- openxlsx::getSheetNames(exnm)
  for (sh in sheets){
    rt <- read.xlsx(exnm,
                    sheet = sh, startRow = 2)
    names(rt) <- c("time_s", "km_h_1", "Gx", "Gy")
    TotSec <- tail(rt$time_s, 2)[1] 
    #last may contain NA
    dist <- sum(as.numeric(rt$km_h_1[1:(nrow(rt)-1)]) * TotSec / 3600)
    accumGx <- sum(abs(rt$Gx))
    accumGyAcc <- sum(rt$Gy[rt$Gy>0])
    accumGyDec <- sum(rt$Gy[rt$Gy<0])
    accumVV <- sum((as.numeric(rt$km_h_1[1:(nrow(rt)-1)])/3.6)^2)
    meanSpeed <- mean(as.numeric(rt$km_h_1[1:(nrow(rt)-1)]))
    AsList[[past(sh)]] <- c(TotSec = TotSec,
                      Gx_km = accumGx / dist,
                      Acc_km = accumGyAcc / dist,
                      Dec_km = accumGyDec / dist,
                      AirR_km = accumVV / dist,
                      aSpeed = meanSpeed)
  }
}
df <- do.call(rbind,AsList)

Ur1$dtime <- c(0,diff(Ur1$time_s))
Ur1$v_s <- Ur1$km_h_1 * 1000 / 3600
Ur1$axAccel <- Ur1$Gx * 9.81
Ur1$angVeloc <- with(Ur1, ifelse(v_s == 0, 0, axAccel * dtime / v_s))
#Ur1$r1o <- Ur1$Gx / (Ur1$v_s)^2 
#Ur1$arcLength <- cumsum((Ur1$km_h_1 * 1000 / 3600) * Ur1$dtime + Ur1$Gx * Ur1$dtime^2)
Ur1$v_x1 <- with(Ur1, v_s * cos(angVeloc * dtime)) 
Ur1$v_y1 <- with(Ur1, v_s * sin(angVeloc * dtime)) 
Ur1$x1 = with(Ur1, cumsum(v_x1 * dtime))
Ur1$y1 = with(Ur1, cumsum(v_y1 * dtime))
plot(Ur1$x1, Ur1$y1)
plot(Ur1$v_y1, Ur1$v_x1)
plot(Ur1$angVeloc)
plot(Ur1$v_s)

head(Ur1$time_s)
