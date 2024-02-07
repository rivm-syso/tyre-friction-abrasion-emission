library(openxlsx)
tyreADAC <- read.xlsx("/rivm/r/E121554 LEON-T/03 - uitvoering WP3/Car data/AdacTyres.xlsx")
tyreADAC$SafetyRatingDry <- as.numeric(gsub(",",".",tyreADAC$Rating.dry.road.surface))
tyreADAC$SafetyRatingWet <- as.numeric(gsub(",",".",tyreADAC$Rating.wet.road.surface))
tyreADAC$Brand <- as.factor(tolower(sapply(strsplit(tyreADAC$`(Summer.Tyres.2022)`, split = " "), function(x) x[1])))
tyreADAC$Width <- as.numeric(substr(tyreADAC$Size, start = 1, stop = 3))
tyreADAC$Size <- as.factor(tyreADAC$Size)
names(tyreADAC)[names(tyreADAC) == "Tyre.abrasion.[g/1,000.km]"] <- "TyreAbbrasion_mg_km"

rf <- randomForest(TyreAbbrasion_mg_km~Width + Brand + SafetyRatingDry + SafetyRatingWet, data = tyreADAC)
varImpPlot(rf, main = "wear [mg/km] variable importance" )
