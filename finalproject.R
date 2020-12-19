data <- read.csv("novexdata.csv")

# Deleting Faculty Data, Blank Columns, and Cheaters
facultydata <- which(data$rep_group == 52)
df1 <- data[-facultydata,]

df <- df1[,-10]

df$rep_caseid <- as.character(df$rep_caseid)
dfrep_clickstream <- as.character((df$rep_clickstream))
df$rep_grade <- as.character((df$rep_grade))
df$rep_clickstream <- as.character(df$rep_clickstream)


# Babler Pass Rate and Answer Key
bablerdf <- df[which(df$rep_caseid == "COPDOx_Babler"),]
bablerp <- length(which(bablerdf$rep_grade == "Pass"))
bablerf <- length(which(bablerdf$rep_grade == "Fail"))
bablerse <- length(which(bablerdf$rep_grade == "Sentinel Event"))

  # List of Babler Meds
bablermeds <- vector(mode = "list", length = nrow(bablerdf))
for (i in 1:nrow(bablerdf)){
  listbabler <- as.list(strsplit(bablerdf$rep_clickstream[i], ",")[[1]])
  babmedsgiven <- Filter(function(x) any(grepl("S1MED", x)), listbabler)
  bablermeds[[i]] <- babmedsgiven
}

  # Calculating Wrong Amount of Meds
    # Answer Key
answersbabler <- list(list("S1MED1001A_b", "S1MED1001B_b", "S1MED1002A_b", "S1MED1002B_b"), list("S1MED0510A_b",
                           "S1MED0510B_b", "S1MED0503A_b", "S1MED0503B_b", "S1MED0506A_b", "S1MED0506B_b", 
                           "S1MED0506C_b"), list("S1MED0807A_b"), list("S1MED0813A_b", "S1MED00813B_b"))
processedcs <- bablermeds[[1]]
    # Scanning Function
wrongmedid <- function(answerkey, processedcs){
  matches <- processedcs
  poss_answer_len <- sapply(answerkey, length)
  answer_category <- rep(seq_along(answerkey), times=poss_answer_len)
  flat_answer <- unlist(answerkey)
  match_indices <- match(processedcs, flat_answer)
  no_match <- is.na(match_indices)
  matches[no_match] <- 0
  matches[!no_match] <- answer_category[match_indices[!no_match]]
  
  #errorcount <- vector(mode = "integer", length = length(answerkey) + 1)
  errorcount <- rep(0, length(answerkey) + 1)
  errorcount[1] <- length(which(unlist(matches) == 0))
  
  for  (i in 2:length(answerkey) + 1){
    cond <- which( unlist(matches) == (i-1) )
    if( length(cond) > 0){
      errorcount[i] <- length(cond) - 1
    }
    #else{
      #errorcount[i] <- 0
    #}
  }
  mederrors <- sum(errorcount)
  print(mederrors)
}

med1 <- wrongmedid(answerkey = answersbabler, processedcs = bablermeds[[1]])

# Aldridge Pass Rate and Answer Key
aldridgedf <- df[which(df$rep_caseid == "PeOx_Aldridge"),]
aldridgep <- length(which(aldridgedf$rep_grade == "Pass"))
aldridgef <- length(which(aldridgedf$rep_grade == "Fail"))
aldridgese <- length(which(aldridgedf$rep_grade == "Sentinel Event"))

  # List of Aldridge Meds
aldridgemeds <- vector(mode = "list", length = nrow(aldridgedf))
for (i in 1:nrow(bablerdf)){
  listaldrige <- as.list(strsplit(aldridgedf$rep_clickstream[i], ",")[[1]])
  almedsgiven <- Filter(function(x) any(grepl("S1MED", x)), listaldrige)
  aldridgemeds[[i]] <- almedsgiven
}

  # Calculating Wrong Amount of Meds
    # Answer Key
answersaldridge <- list(list("S1MED0405A_b", "S1MED0405B_b", "S1MED0416A_b", "S1MED0418A_b", 
                             "S1MED0419A_b"), list("S1MED0807A_b"), list("S1MED0413A_b", "S1MED0413B_b"),
                            list("S1MED0507C_b"))


# Bosco Pass Rate and Answer Key
boscodf <- df[which(df$rep_caseid == "CHFPe_Bosco"),]
boscop <- length(which(boscodf$rep_grade == "Pass"))
boscof <- length(which(boscodf$rep_grade == "Fail"))
boscose <- length(which(boscodf$rep_grade == "Sentinel Event"))

  # List of Bosco Meds
boscomeds <- vector(mode = "list", length = nrow(boscodf))
for (i in 1:nrow(boscodf)){
  listbosco <- as.list(strsplit(boscodf$rep_clickstream[i], ",")[[1]])
  bosmedsgiven <- Filter(function(x) any(grepl("S1MED", x)), listbosco)
  boscomeds[[i]] <- bosmedsgiven
}

  # Calculating Wrong Amount of Meds
    # Answer Key
answersbosco <- list(list("S1MED1101A_b", "S1MED1101B_b", "S1MED1101C_b"), list("S1MED0807A_b"), list("S1MED0507C_b"))


# Ogletree Pass Rate and Answer Key
ogletreedf <- df[which(df$rep_caseid == "ACSPe_Ogletree2"),]
ogletreep <- length(which(ogletreedf$rep_grade == "Pass"))
ogletreef <- length(which(ogletreedf$rep_grade == "Fail"))
ogletreese <- length(which(ogletreedf$rep_grade == "Sentinel Event"))

  # List of Ogletree Meds
ogletreemeds <- vector(mode = "list", length = nrow(ogletreedf))
for (i in 1:nrow(ogletreedf)){
  listogletree <- as.list(strsplit(ogletreedf$rep_clickstream[i], ",")[[1]])
  oglemedsgiven <- Filter(function(x) any(grepl("S1MED", x)), listogletree)
  ogletreemeds[[i]] <- oglemedsgiven
}

  # Calculating Wrong Amount of Meds
    # Answer Key
answersogletree <- list(list("S1MED0807A_b"), list("S1MED0813A_b", "S1MED00813B_b"))


# Harder Pass Rate and Answer Key
harderdf <- df[which(df$rep_caseid == "StrokeNe_Harder"),]
harderp <- length(which(harderdf$rep_grade == "Pass"))
harderf <- length(which(harderdf$rep_grade == "Fail"))
harderse <- length(which(harderdf$rep_grade == "Sentinel Event"))

  # List of Harder Meds
hardermeds <- vector(mode = "list", length = nrow(harderdf))
for (i in 1:nrow(harderdf)){
  listharder <- as.list(strsplit(harderdf$rep_clickstream[i], ",")[[1]])
  hardermedsgiven <- Filter(function(x) any(grepl("S1MED", x)), listharder)
  hardermeds[[i]] <- hardermedsgiven
}

  # Calculating Wrong Amount of Meds
    # Answer Key
answersharder <- list(list("S1MED0807A_b"), list("S1MED0813A_b", "S1MED00813B_b"))


# Taylor Pass Rate and Answer Key
taylordf <- df[which(df$rep_caseid == "StrokeNe_Traylor"),]
taylorp <- length(which(taylordf$rep_grade == "Pass"))
taylorf <- length(which(taylordf$rep_grade == "Fail"))
taylorse <- length(which(taylordf$rep_grade == "Sentinel Event"))

  # List of Taylor Meds
taylormeds <- vector(mode = "list", length = nrow(taylordf))
for (i in 1:nrow(taylordf)){
  listtaylor <- as.list(strsplit(taylordf$rep_clickstream[i], ",")[[1]])
  taylormedsgiven <- Filter(function(x) any(grepl("S1MED", x)), listtaylor)
  taylormeds[[i]] <- taylormedsgiven
}

  # Calculating Wrong Amount of Meds
    # Answer Key
answerstaylor <- list(list("S1MED0402A_b"), list("S1MED0507A_b", "S1MED0507C_b"), list("S1MED0807A_b"))


# Manson Pass Rate and Answer Key
mansondf <- df[which(df$rep_caseid == "DMEndo_Manson"),]
mansonp <- length(which(mansondf$rep_grade == "Pass"))
mansonf <- length(which(mansondf$rep_grade == "Fail"))
mansonse <- length(which(mansondf$rep_grade == "Sentinel Event"))

  # List of Manson Meds
mansonmeds <- vector(mode = "list", length = nrow(mansondf))
for (i in 1:nrow(mansondf)){
  listmanson <- as.list(strsplit(mansondf$rep_clickstream[i], ",")[[1]])
  mansonmedsgiven <- Filter(function(x) any(grepl("S1MED", x)), listmanson)
  mansonmeds[[i]] <- mansonmedsgiven
}

  # Calculating Wrong Amount of Meds
    # Answer Key
answersmanson <- list(list("S1MED0507C_b", "S1MED0202B_b", "S1MED0202C_b"), list("S1MED0507C_b", 
                            "S1MED0202B_b", "S1MED0202C_b"), list("S1MED0807A_b"))


# Arles Pass Rate and Answer Key
arlesdf <- df[which(df$rep_caseid == "AnaphSh_Arles_CC"),]
arlesp <- length(which(arlesdf$rep_grade == "Pass"))
arlesf <- length(which(arlesdf$rep_grade == "Fail"))
arlesse <- length(which(arlesdf$rep_grade == "Sentinel Event"))

  # List of Arles Meds
arlesmeds <- vector(mode = "list", length = nrow(arlesdf))
for (i in 1:nrow(arlesdf)){
  listarles <- as.list(strsplit(arlesdf$rep_clickstream[i], ",")[[1]])
  arlesmedsgiven <- Filter(function(x) any(grepl("S1MED", x)), listarles)
  arlesmeds[[i]] <- arlesmedsgiven
}

  # Calculating Wrong Amount of Meds
    # Answer Key
answersarles <- list(list("S1MED0311A_b"), list("S1MED0807A_b"))


# Wesson Pass Rate and Answer Key
wessondf <- df[which(df$rep_caseid == "PneumoOx_Wesson"),]
wessonp <- length(which(wessondf$rep_grade == "Pass"))
wessonf <- length(which(wessondf$rep_grade == "Fail"))
wessonse <- length(which(wessondf$rep_grade == "Sentinel Event"))

  # List of Wesson Meds
wessonmeds <- vector(mode = "list", length = nrow(wessondf))
for (i in 1:nrow(wessondf)){
  listwesson <- as.list(strsplit(wessondf$rep_clickstream[i], ",")[[1]])
  wessonmedsgiven <- Filter(function(x) any(grepl("S1MED", x)), listwesson)
  wessonmeds[[i]] <- wessonmedsgiven
}

  # Calculating Wrong Amount of Meds
    # Answer Key
answerswesson <- list(list("S1MED0202A_b", "S1MED0202B_b", "S1MED0202C_b"), list("S1MED0807A_b"), 
                      list("S1MED0813A_b", "S1MED00813B_b"))


# Michaels Pass Rate and Answer Key
michaelsdf <- df[which(df$rep_caseid == "SepsisSe_Michaels"),]
michaelsp <- length(which(michaelsdf$rep_grade == "Pass"))
michaelsf <- length(which(michaelsdf$rep_grade == "Fail"))
michaelsse <- length(which(michaelsdf$rep_grade == "Sentinel Event"))

  # List of Michaels Meds
michaelsmeds <- vector(mode = "list", length = nrow(michaelsdf))
for (i in 1:nrow(michaelsdf)){
  listmichaels <- as.list(strsplit(michaelsdf$rep_clickstream[i], ",")[[1]])
  michaelsmedsgiven <- Filter(function(x) any(grepl("S1MED", x)), listmichaels)
  michaelsmeds[[i]] <- michaelsmedsgiven
}

  # Calculating Wrong Amount of Meds
    # Answer Key
answersmichael <- list(list("S1MED0202B_b"), list("S1MED0807A_b"), list("S1MED0507C_b"))

# Visualizing Pass, Fail, and SEs
passes <- c(bablerp, aldridgep, boscop, ogletreep, harderp, taylorp, mansonp, arlesp, wessonp, michaelsp)
fails <- c(bablerf, aldridgef, boscof, ogletreef, harderf, taylorf, mansonf, arlesf, wessonf, michaelsf)
ses <- c(bablerse, aldridgese, boscose, ogletreese, harderse, taylorse, mansonse, arlesse, wessonse, michaelsse)
courses <- c(1, seq(5, 45, 5))
plot(courses, passes, type = "l", ylim = c(0, 80), col = "springgreen4", xlab = "Case Number", ylab = "Number of Students", main = "Visiualizing Outcomes by Number of Completed Cases")
lines(courses, fails, col = "navyblue")
lines(courses, ses, col = "red4")
legend(1, 80, legend = c("Passes", "Fails", "SEs"), lty=1,col = c("springgreen4", "navyblue", "red4"), title= "Outcome")

