

###Travis Asher
###This code is how I keep track of the current equipment on my Animated Guardian in POE





## Name vectors for the stats we keep track of and the pieces of armor + total:
names_stats <- c("arm","ev","es","fire","cold","light","chaos","hp_reg_val","hp_reg_pc","max_hp")
names_eq <- c("helm", "body", "hand", "foot", "wep", "shld", "total")



## We create a blank matrix with these names as eq X stats respectively:
mat <- matrix(nrow = 7,ncol = 10,dimnames = list(names_eq,names_stats))



## We convert our matrix to a data.frame and set the default element value to 0, then adjust resistances to base for guardian:
eq_ref <- as.data.frame(x = mat)
eq_ref[1:7,1:10] <- 0
eq_ref["total",c("fire","cold","light","chaos")] <- c(40,40,40,20)



#We create an indicator to indicate whether the last equipped item b/w shld and wep was a shld or a wep:
last_eq <- c("shield","weapon")










###################
#### BEGIN LOG ####





####  5:30, 4_29_19 stats:

# eq_ref["body",c("arm","es","max_hp","fire","cold","light","hp_reg_pc")] <- c(711,141,75,15,15,15,2)
# eq_ref["shld",c("ev","fire","cold","light","max_hp","hp_reg_val")] <- c(572,42,12,12,26,6)
# eq_ref["foot",c("arm","es","max_hp","hp_reg_val")] <- c(142,24,104,10.9)
# eq_ref["helm",c('arm','ev','max_hp','light')] <- c(346,381,105,37)
# eq_ref["hand",c('arm','es','light')] <- c(218,88,20)
# eq_ref["wep",] <- eq_ref["wep",]
#
# eq_ref["total",names_stats] <- eq_ref["total",] + eq_ref["body",] + eq_ref["shld",] + eq_ref["foot",] + eq_ref["helm",] +
#   eq_ref["hand",] + eq_ref["wep",]
#
#
# animate_guardian_1 <- eq_ref
# animate_guardian_1
# print(paste("Last equipped b/w shield and weapon was: ",last_eq[1]))






####  6:00, 4_29_19 stats:

eq_ref["body",c("arm","es","max_hp","fire","cold","light","hp_reg_pc")] <- c(711,141,75,15,15,15,2)
eq_ref["shld",c("ev","fire","cold","light","max_hp","hp_reg_val")] <- c(572,42,12,12,26,6)
eq_ref["foot",c("arm","es","max_hp","hp_reg_val")] <- c(142,24,104,10.9)
eq_ref["helm",c('arm','ev','max_hp','light')] <- c(346,381,105,37)
eq_ref["hand",c('arm','es','max_hp','hp_reg_val','fire')] <- c(185,36,26,9.6,28) #New addition for this version of A.G.
eq_ref["wep",] <- eq_ref["wep",]

eq_ref["total",names_stats] <- eq_ref["total",] + eq_ref["body",] + eq_ref["shld",] + eq_ref["foot",] + eq_ref["helm",] +
  eq_ref["hand",] + eq_ref["wep",]


animate_guardian_2 <- eq_ref
animate_guardian_2
print(paste("Last equipped b/w shield and weapon was: ",last_eq[1]))
