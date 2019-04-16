okdats$E <- factor(as.character(okdats$E), labels= c("I", "U"))

save(okdats, file = "img/okdats.RData")
