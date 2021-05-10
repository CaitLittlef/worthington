
## Load data

plot <- read.csv(paste0(data.dir, "LS_Worthington_Data_2018_plot.csv"))
cwd <- read.csv(paste0(data.dir, "LS_Worthington_Data_2018_CWD.csv"))
seedlings <- read.csv(paste0(data.dir, "LS_Worthington_Data_2018_seedlings.csv"))
saplings <- read.csv(paste0(data.dir, "LS_Worthington_Data_2018_saplings.csv"))
trees <- read.csv(paste0(data.dir, "LS_Worthington_Data_2018_Trees.csv"))
legacy <- read.csv(paste0(data.dir, "legacy.growth.csv"))


## Set plot area, expansion factors
# Ellipse area = pi * a * b; a & b are the pseudo radii.
# IN ORIGINAL CODE BELOW, FULL AXIS LENGTH WAS (INACCURATELY) USED. 

head(plot,5)
colnames(plot)
colnames(plot) <- c("plot", "northing", "easting", "tx", "date", "invasives", "sp", "ht_m", "NE", "SE", "SW", "NW", "axis1", "axis2", "notes")


plot$area_m2 <- pi * (plot$axis1/2) * (plot$axis2/2)
plot$area_ha <- plot$area_m2 * 0.0001
plot$area_ac <- plot$area_ha * 2.47105
plot$ef_ha <- 1/plot$area_ha # expansion factor

plot[,c(1,16:19)]


## CODE BELOW CORRESPONDS TO ANALYSES & FIGURES ON EMMA SASS FEMC POSTER
# 
# 
# #analyses:
# #0: plot area - calculate from transects of different sizes
# #1: CWD
# #     volume per treatment; anova, t-tests, graph
# #     Size distribution; Intercept diameter by treatment
# #2: Legacy trees
# #     Mortality rate
# #     Individual tree diameter growth rate
# #     Gap-level legacy tree basal area growth
# #3: regeneration
# #     Seedling density by treatment; by species
# #     Sapling density by treatment; by species
# #     Tree regeneration assemblages: MRPP, NMS
# #     species diversity
# #4: gap winners
# 
# ##########################################################################
# #0: plot area
# # A = pi * a * b
# ellipse = function(a,b){ #calculate an ellipse from twploto axes
#   area = pi*a*b
#   return(area)
# }
# plot<-read.csv(paste0(data.dir,"LS_Worthington_Data_2018_plot.csv")) 
# str(plot)
# plot$area = NA
# plot$area = ellipse(a = plot$axis1, b = plot$axis2) #length in meters, area in m^2
# plot$ha = NA
# plot$ha = plot$area*0.0001 #per ha
# plot$expansion = NA
# plot$expansion = 1/plot$ha #expansion factor for each plot
# plot[,c(1,16:18)]
# #   Plot     area        ha expansion
# #     1 5384.564 0.5384564  1.857161
# #     2 5384.564 0.5384564  1.857161
# #     3 4327.827 0.4327827  2.310629
# #     4 4405.895 0.4405895  2.269686
# #     5 5384.564 0.5384564  1.857161
# #     6 5072.415 0.5072415  1.971447
# #     7 4620.026 0.4620026  2.164490
# #     8 5384.564 0.5384564  1.857161
# #     9 4988.849 0.4988849  2.004470
# #    10 4968.366 0.4968366  2.012734
# #    11 4903.335 0.4903335  2.039428
# #    12 5384.564 0.5384564  1.857161
# #    13 4048.885 0.4048885  2.469816
# #    14 4404.324 0.4404324  2.270496
# #    15 5384.564 0.5384564  1.857161
# #    16 4911.817 0.4911817  2.035906
# 
# ##########################################################################
# 
# #1: CWD
# #     volume per treatment
# #     Size distribution; Intercept diameter by treatment
# cwd<-read.csv("LS_Worthington_Data_2018_CWD.csv") 
# str(cwd)
# 
# #create a function to calculate reduction factor, based on decay class (4 or 5) and if the species is white pine or not
# reduction.factor<-function(decay, species){
#   red.factor=1
#   if(decay==4 & species == "PIST"){red.factor=0.586}
#   if(decay==4 & species !="PIST"){red.factor=0.519}
#   if(decay==5 & species == "PIST"){red.factor=0.377}
#   if(decay==5 & species != "PIST"){red.factor=0.381}
#   return(red.factor)
# }
# #add reduction factor as new column
# cwd$red.fac = NA
# for (i in 1:nrow(cwd)){
#   cwd$red.fac[i] = reduction.factor(decay = cwd$Decay_class[i], species = cwd$Species[i])
# }
# 
# #calculate volume in m^3 PER PIECE per ha (m3/ha)
# # sum of { pi^2 / 8L * 10,000 *  diameters(m)^2 * reduction factor }
# #diameter needs to be in meters
# cwd$dia.m <- cwd$Diameter*0.01 #diameter in meters
# cwd$volume <- NA
# dim(cwd) #273*10
# for (i in 1:273){
#   cwd$volume[i] = ((pi^2/(8*cwd$Total_length[i]))*10000)*(cwd$dia.m[i]^2)*cwd$red.fac[i] #calculate volume of each piece
# }
# cwd$vol.cm <- NA #volume per piece per ha in cm3
# cwd$vol.cm <- cwd$volume*1000000 #times a million because it's m^3 to cm^3
# tail(cwd)
# 
# #sum CWD volumes by plot
# library(plyr)
# cwd.vol.df<-ddply(cwd, .(Plot), .fun = summarize, 
#                   total.vol = sum(volume)) #aggregate the data, sum of CWD volume by plot
# treat.vec<-c("CONTROL","LEAVE","LEGACY","GAP","LEGACY","LEGACY","LEAVE","CONTROL","GAP","GAP","LEGACY","CONTROL","LEAVE","GAP","CONTROL","LEAVE") #plots 1:16
# cwd.vol.df$treatment<-treat.vec
# cwd.vol.df
# #write.csv(cwd.vol.df,"cwd_volume_by_plot.csv")
# 
# #summarize by treatment
# cwd.agg <- NULL
# cwd.agg<-ddply(cwd.vol.df, .(treatment), .fun = summarize, 
#                mean.vol = mean(total.vol), 
#                se.vol = sd(total.vol)/sqrt(4)) #aggregate the data, sum of CWD+SE by treatment
# cwd.agg
# 
# #analyze: ANOVA
# m1 = aov(total.vol ~ treatment, data = cwd.vol.df)
# summary(m1)
# #             Df Sum Sq Mean Sq F value   Pr(>F)    
# #treatment    3  82170   27390   14.44 0.000276 ***
# 
# #separate treatment data to run t.test
# con = cwd.vol.df[cwd.vol.df$treatment=="CONTROL",]
# leave = cwd.vol.df[cwd.vol.df$treatment=="LEAVE",]
# leg = cwd.vol.df[cwd.vol.df$treatment=="LEGACY",]
# gap = cwd.vol.df[cwd.vol.df$treatment=="GAP",]
# 
# t.test(con$total.vol,leave$total.vol) #t = -4.2925, df = 5.423, p-value = 0.006468****
# t.test(con$total.vol,leg$total.vol) #t = -2.0061, df = 3.924, p-value = 0.1167
# t.test(con$total.vol,gap$total.vol) #t = -0.5194, df = 3.207, p-value = 0.6372
# t.test(leave$total.vol,leg$total.vol) #t = 3.5916, df = 3.478, p-value = 0.02899**
# t.test(leave$total.vol,gap$total.vol) #t = 4.8524, df = 3.105, p-value = 0.01542**
# t.test(leg$total.vol,gap$total.vol) #t = 3.7172, df = 4.254, p-value = 0.01839**
# 
# #plot the total CWD volume per treatment
# library(ggplot2)
# ggplot(data = cwd.agg, aes(x=treatment, y=mean.vol))+
#   geom_bar(stat = "identity", colour="black")+
#   geom_errorbar(aes(ymin=mean.vol-se.vol, ymax=mean.vol+se.vol),width=.1,position=position_dodge(0.9))+
#   theme_bw() +
#   labs(y="CWD volume (m3/ha)", x="Treatment", title = "CWD volume") +
#   theme(title = element_text(size=16),
#         axis.title.x = element_text(size=16),
#         axis.text.x = element_text(size=14),
#         axis.title.y = element_text(size=16),
#         axis.text.y = element_text(size=14))
# 
# #intercept diameter
# #head(cwd)
# #library(plyr)
# cwd$treatment <- NA #add treatment designation to each cwd line
# for(i in 1:nrow(cwd)){
#   if (cwd$Plot[i] == 1 | cwd$Plot[i] == 8 | cwd$Plot[i] == 12 | cwd$Plot[i] == 15){
#     cwd$treatment[i] = "CONTROL"
#   }
#   if (cwd$Plot[i] == 2 | cwd$Plot[i] == 7 | cwd$Plot[i] == 13 | cwd$Plot[i] == 16){
#     cwd$treatment[i] = "LEAVE"
#   }
#   if (cwd$Plot[i] == 3 | cwd$Plot[i] == 5 | cwd$Plot[i] == 6 | cwd$Plot[i] == 11){
#     cwd$treatment[i] = "LEGACY"
#   }
#   if (cwd$Plot[i] == 4 | cwd$Plot[i] == 9 | cwd$Plot[i] == 10 | cwd$Plot[i] == 14){
#     cwd$treatment[i] = "GAP"
#   }
# }
# tail(cwd)
# 
# #analyze intercept diameter across treatments: ANOVA
# m2 = aov(Diameter ~ treatment, data=cwd) 
# summary(m2)
# # 3    512  170.67   1.999  p = 0.114 not significant
# 
# 
# #changes in decay class over time
# getmode <- function(v) {
#   uniqv <- unique(v)
#   uniqv[which.max(tabulate(match(v, uniqv)))]
# } 
# 
# cwd.plot.dec<-ddply(cwd, .(Plot, Decay_class), .fun = summarize, 
#                     total.vol = sum(volume),
#                     treatment = getmode(treatment))#aggregate the data, sum of CWD volume by plot
# cwd.plot.dec
# cwd.trt.agg<-ddply(cwd.plot.dec, .(treatment,Decay_class), .fun = summarize, 
#                    mean.vol = mean(total.vol), 
#                    sd.vol = sd(total.vol),
#                    treatment = getmode(treatment)) 
# cwd.trt.agg
# write.csv(cwd.plot.dec, "CWD_2018_vol_by_decay_class_and_plot.csv")
# 
# #plot cwd by decay class (for poster)
# #need matrix of treatment across and decay classes down
# decay.mat = matrix(nrow = 4,ncol = 4, data = NA)
# colnames(decay.mat) = c("CONTROL","LEAVE","LEGACY","GAP")
# rownames(decay.mat) = c("decay1","decay2","decay3","decay4")
# decay.mat[2:4,1] = cwd.dec.agg[1:3,2] #control
# decay.mat[2:4,4] = cwd.dec.agg[4:6,2] #gap
# decay.mat[2:4,2] = cwd.dec.agg[7:9,2] #leave
# decay.mat[2:4,3] = cwd.dec.agg[10:12,2] #legacy
# decay.mat[1,] = 0
# colSums(decay.mat)
# 
# #plot black+white
# ylim <- c(0,350) #take the higher value from 2011 data
# angle1 <- c(0,45,45,0)
# density1 <- c(0,99,20,99)
# col <- c("gray99","gray10","gray40","gray80") 
# density1.1 <- c(0,99,20,0)
# #gray100 is white, gray0 is black
# 
# # plot --------------------------------------------------------------------
# barplot(decay.mat, beside=FALSE, ylim=ylim, col=col, angle=angle1, density=density1,ylab="CWD M2/ha",xlab="Treatment", main = "CWD 2018")
# legend("topright",inset = 0.02,legend=c("Decay class I","Decay class II","Decay class III","Decay class IV"), ncol=1, fill=c("white","white","white","gray"))
# par(bg="transparent")
# legend("topright", inset = 0.02, legend=c("Decay class I","Decay class II","Decay class III","Decay class IV"), ncol=1, fill=TRUE, col=col, angle=angle1, density=density1.1)
# 
# #plot in color for poster
# col <- c("oldlace","wheat1","tan3","saddlebrown")
# barplot(decay.mat, beside=FALSE, ylim=ylim, col=col, ,ylab="CWD M2/ha",xlab="Treatment", main = "CWD 2018")
# legend("topright",inset = 0.02,legend=c("Decay class I","Decay class II","Decay class III","Decay class IV"), ncol=1, fill=col)
# 
# #end 2018 CWD code
# 
# #changes in mass over time: 2011-2018 #no cwd data from 2007
# 
# #2011
# cwd.11 <- read.csv("LS_Worthington_Data_2011_cwd.csv")
# head(cwd.11)
# transect.plot = ddply(cwd.11, .(Plot_num,Transect), .fun = summarize,
#                       transect.length = mean(Transect_length))
# transect.lengths = ddply(transect.plot, .(Plot_num), .fun = summarize, 
#                          trans.length = sum(transect.length))
# transect.lengths
# cwd.11$red.fac = NA
# for (i in 1:nrow(cwd.11)){
#   cwd.11$red.fac[i] = reduction.factor(decay = cwd.11$Decay_class[i], species = cwd.11$Species[i])
# }
# 
# #calculate volume in m^2 PER PIECE per ha (m3/ha)
# # sum of { pi^2 / 8L * 10,000 *  diameters(m)^2 * reduction factor }
# #diameter needs to be in meters
# cwd.11$dia.m <- cwd.11$Diameter*0.01 #diameter in meters
# cwd.11$volume <- NA
# dim(cwd.11) #351 rows
# #add transect length to each plot 
# cwd.11$total.length = NA
# for (i in 1:351){
#   for (j in 1:16){
#     if(cwd.11$Plot_num[i]==j){
#       cwd.11$total.length[i] = transect.lengths[j,2]
#     }
#   }
# }
# 
# for (i in 1:351){
#   cwd.11$volume[i] = ((pi^2/(8*cwd.11$total.length[i]))*10000)*(cwd.11$dia.m[i]^2)*cwd.11$red.fac[i] #calculate volume of each piece
# }
# cwd.11$vol.cm <- NA #volume per piece per ha in cm3
# cwd.11$vol.cm <- cwd.11$volume*1000000 #times a million because it's m^3 to cm^3
# tail(cwd.11)
# cwd.11.dc = cwd.11[cwd.11$Decay_class==1|cwd.11$Decay_class==2|cwd.11$Decay_class==3|cwd.11$Decay_class==4|cwd.11$Decay_class==5,] #cut out the zeros for ks.test
# ks1 = ks.test(cwd.11$Decay_class,cwd$Decay_class)
# ks1 #D = 0.8673, p-value < 2.2e-16
# 
# #aggregate by plot
# #library(plyr)
# cwd.11.vol.df<-ddply(cwd.11, .(Plot_num), .fun = summarize, 
#                      total.vol = sum(volume)) #aggregate the data, sum of CWD volume by plot
# treat.vec<-c("CONTROL","LEAVE","LEGACY","GAP","LEGACY","LEGACY","LEAVE","CONTROL","GAP","GAP","LEGACY","CONTROL","LEAVE","GAP","CONTROL","LEAVE") #plots 1:16
# cwd.11.vol.df$treatment<-treat.vec
# cwd.11.vol.df
# write.csv(cwd.11.vol.df,"CWD_2011_vol_by_decay_class_and_plot.csv")
# 
# #summarize by treatment
# cwd.11.agg <- NULL
# cwd.11.agg<-ddply(cwd.vol.df, .(treatment), .fun = summarize, 
#                   mean.vol = mean(total.vol), 
#                   se.vol = sd(total.vol)/sqrt(4)) #aggregate the data, sum of CWD+SE by treatment
# cwd.11.agg
# 
# cwd.11$treatment <- NA #add treatment designation to each cwd line
# for(i in 1:nrow(cwd.11)){
#   if (cwd.11$Plot[i] == 1 | cwd.11$Plot[i] == 8 | cwd.11$Plot[i] == 12 | cwd.11$Plot[i] == 15){
#     cwd.11$treatment[i] = "CONTROL"
#   }
#   if (cwd.11$Plot[i] == 2 | cwd.11$Plot[i] == 7 | cwd.11$Plot[i] == 13 | cwd.11$Plot[i] == 16){
#     cwd.11$treatment[i] = "LEAVE"
#   }
#   if (cwd.11$Plot[i] == 3 | cwd.11$Plot[i] == 5 | cwd.11$Plot[i] == 6 | cwd.11$Plot[i] == 11){
#     cwd.11$treatment[i] = "LEGACY"
#   }
#   if (cwd.11$Plot[i] == 4 | cwd.11$Plot[i] == 9 | cwd.11$Plot[i] == 10 | cwd.11$Plot[i] == 14){
#     cwd.11$treatment[i] = "GAP"
#   }
# }
# tail(cwd.11)
# 
# #changes in decay class over time
# getmode <- function(v) {
#   uniqv <- unique(v)
#   uniqv[which.max(tabulate(match(v, uniqv)))]
# }
# cwd.11.vol.dec<-ddply(cwd.11, .(Plot_num, Decay_class), .fun = summarize, 
#                       total.vol = sum(volume),
#                       treatment = getmode(treatment))#aggregate the data, sum of CWD volume by plot
# cwd.11.vol.dec
# cwd.11.dec.agg<-ddply(cwd.11.vol.dec, .(treatment,Decay_class), .fun = summarize, 
#                       mean.vol = mean(total.vol), 
#                       sd.vol = sd(total.vol),
#                       treatment = getmode(treatment)) 
# cwd.11.dec.agg
# #get rid of decay class 0
# cwd.11.dec.agg<-cwd.11.dec.agg[c(2:5,7:18),]
# 
# #need matrix of treatment across and decay classes down
# decay.mat.11 = matrix(nrow = 4,ncol = 4, data = NA)
# colnames(decay.mat.11) = c("CONTROL","LEAVE","LEGACY","GAP")
# rownames(decay.mat.11) = c("decay1","decay2","decay3","decay4")
# decay.mat.11[,1] = cwd.11.dec.agg[1:4,2] #control
# decay.mat.11[,4] = cwd.11.dec.agg[5:8,2] #gap
# decay.mat.11[,2] = cwd.11.dec.agg[9:12,2] #leave
# decay.mat.11[,3] = cwd.11.dec.agg[13:16,2] #legacy
# colSums(decay.mat.11) #up to 350
# 
# #plot black+white
# ylim <- c(0,350)
# angle1 <- c(0,45,45,0)
# density1 <- c(0,99,20,99)
# col <- c("gray99","gray10","gray40","gray80") 
# density1.1 <- c(0,99,20,0)
# #gray100 is white, gray0 is black
# 
# # plot --------------------------------------------------------------------
# barplot(decay.mat.11, beside=FALSE, ylim=ylim, col=col, angle=angle1, density=density1,ylab="CWD M2/ha",xlab="Treatment", main = "CWD 2011")
# legend("topright",inset = 0.02,legend=c("Decay class I","Decay class II","Decay class III","Decay class IV"), ncol=1, fill=c("white","white","white","gray"))
# par(bg="transparent")
# legend("topright", inset = 0.02, legend=c("Decay class I","Decay class II","Decay class III","Decay class IV"), ncol=1, fill=TRUE, col=col, angle=angle1, density=density1.1)
# 
# col <- c("oldlace","wheat1","tan3","saddlebrown")
# #plot with color for poster
# barplot(decay.mat.11, beside=FALSE, ylim=ylim, col=col,ylab="CWD M2/ha",xlab="Treatment", main = "CWD 2011")
# legend("topright",inset = 0.02,legend=c("Decay class I","Decay class II","Decay class III","Decay class IV"), ncol=1, fill=col)
# 
# #compare total cwd volume between years
# cwd.11.vol.df #total volume by plot in 2011
# cwd.vol.df #total volume by plot in 2018
# 
# hist(sqrt(cwd.11.vol.df$total.vol)) #normal
# hist(sqrt(cwd.vol.df$total.vol))
# cwd1 <- t.test(sqrt(cwd.11.vol.df$total.vol),sqrt(cwd.vol.df$total.vol))
# cwd1 #no difference between years t = 1.3256, df = 27.885, p-value = 0.1957
# 
# #########################################################
# 
# #2: Legacy Trees
# #plots 3, 5, 6, 11
# #     Mortality rate
# #     Individual tree diameter growth rate
# #     Gap-level legacy tree basal area growth
# 
# #Legacy-tree mortality rates were calculated as: 1 - [1 - (M1/N0)]^1/t
# #where M1 is the total number of trees that died during the sampling period, N0 is the total number of live legacy trees at the beginning of
# #the sampling period, and t is the number of years between sampling periods (Sheil and May 1996).
# 
# mortality = function(M1,N0,t){
#   mort = 1-(1-(M1/N0))^(1/t)
#   return(mort)
# }
# 
# #start with 15 trees
# #one died 2007-2011
# #two died 2011-2018
# 
# #from 2007 - 2011:
# mortality(M1 = 1, N0 = 15, t = 4) #mortality rate = 0.0171
# #from 2011-2018:
# mortality(M1=2,N0=14,t=7) #mortality rate = 0.0218
# #overall:
# mortality(M1=3,N0=15,t=11) #0.0201
# 
# 
# #individual tree diameter growth rate
# legacy = read.csv("legacy_trees.csv")
# legacy
# basal.area = function(dbh){ #basal area for each tree diameter
#   ba = 0.00007854*dbh^2
#   return(ba)
# }
# basal.area(100)
# 
# legacy$ba07 <- basal.area(as.numeric(as.character(legacy$dbh_2007)))
# legacy$ba11 <- basal.area(as.numeric(as.character(legacy$dbh_2011)))
# legacy$ba18 <- basal.area(as.numeric(as.character(legacy$dbh_2018)))
# 
# legacy$increment07.11 = (legacy$ba11-legacy$ba07)/4
# legacy$increment11.18 = (legacy$ba18-legacy$ba11)/7
# legacy$increment07.18 = (legacy$ba18-legacy$ba07)/11
# 
# legacy$dbh.inc <- (as.numeric(as.character(legacy$dbh_2018))-as.numeric(as.character(legacy$dbh_2007)))/11
# mean(legacy$dbh.inc,na.rm=T) #0.5558442
# sd(legacy$dbh.inc,na.rm=T)/(sqrt(14)) #0.1396112
# 
# legacy
# t.test(legacy$increment07.11,legacy$increment11.18) #no difference between first years and second set
# mean(legacy$increment07.11, na.rm = T) #0.004110426 m2
# mean(legacy$increment11.18, na.rm = T) #0.003810929
# mean(legacy$increment07.18, na.rm = T) #0.004082264
# head(legacy)
# #per ha values
# #the four legacy plots cover what area?
# leg.plot<-plot[c(3,5,6,11),c(1,16:18)]
# sum(leg.plot$ha) #the four plots cover 1.9688 ha
# sum(legacy$increment07.18, na.rm = T) #0.0571517
# #total legacy trees divided by total area (to get total growth/ha)
# sum(legacy$increment07.18, na.rm = T)/sum(leg.plot$ha) #0.02902849 m2/ha
# 
# ##################
# #plot-level bai - basal area increment
# #including ingrowth into tree category
# trees<-read.csv("LS_Worthington_Data_2018_trees.csv") 
# head(trees)
# livetrees = trees[trees$Condition==1,]
# dim(trees) #348 trees total
# dim(livetrees) #306 trees that are alive
# 
# #calculate basal area of each tree
# basal.area = function(dbh){ #dbh in cm
#   ba = 0.00007854*dbh^2
#   return(ba) #ba in m^2
# }
# trees$BA = basal.area(trees$DBH)
# tail(trees)
# 
# #sum to calculate BA of each plot: 2011 to 2018
# #for 2011
# trees11 = read.csv("LS_Worthington_Data_2011.csv")
# head(trees11)
# trees11$BA = basal.area(trees11$DBH)
# plot11.sum = as.data.frame(matrix(nrow = 16,ncol = 5,data = NA))
# colnames(plot11.sum) = c("plot","BA11sum","expansion","BA11ha","treatment")
# plot11.sum$plot = 1:16
# for (i in 1:16){ #sum the basal area for each plot #in the future, use ddply
#   tempdat = trees11[trees11$Plot==i,]
#   tempsum = sum(tempdat$BA)
#   plot11.sum$BA11sum[i] = tempsum
# }
# plot11.sum$expansion = plot$expansion #see above (section 1)
# plot11.sum$BA11ha = plot11.sum$BA11sum*plot11.sum$expansion #convert per plot to per ha
# plot11.sum$treatment = plot$Treatment
# 
# #for 2018
# plot.sum = as.data.frame(matrix(nrow = 16,ncol = 5,data = NA))
# colnames(plot.sum) = c("plot","BA18sum","expansion","BA18ha","treatment")
# plot.sum$plot = 1:16
# for (i in 1:16){ #sum the basal area for each plot
#   tempdat = trees[trees$Plot==i,]
#   tempsum = sum(tempdat$BA)
#   plot.sum$BA18sum[i] = tempsum
# }
# plot.sum$expansion = plot$expansion #see above (section 1)
# plot.sum$BA18ha = plot.sum$BA18sum*plot.sum$expansion #convert per plot to per ha
# plot.sum$treatment = plot$Treatment
# plot.sum
# 
# bai <- cbind(plot11.sum,plot.sum)
# bai
# bai$bai = (bai$BA18ha - bai$BA11ha)/7
# write.csv(bai,"BA_and_BAI_2011_2018.csv")
# 
# m3 <- lm(bai$bai ~ bai$treatment) #does bai vary by treatment?
# summary(m3)
# a1<-aov(m3)
# library(car)
# Anova(a1,type="III")
# #              Sum Sq   Df  F value   Pr(>F)   
# #(Intercept)   0.088083  1  11.554 0.005278 **
# #bai$treatment 0.031766  3   1.389 0.293678   
# #Residuals     0.091480 12   
# TukeyHSD(a1, which = "bai$treatment")
# leveneTest(m2) #not significant, so passes assumptions test (variances don't have sig diff)
# range(bai$bai)
# 
# plot(bai$bai ~ bai$treatment, ylab = "BAI (m2/year/ha)", xlab = "treatment")
# 
# #compare BA between treatments
# m4 <- lm(plot.sum$BA18ha ~ plot.sum$treatment) #compare groups, none set as default
# summary(m4)
# a4<-aov(m4)
# Anova(a4,type="III")
# TukeyHSD(a4, which = "plot.sum$treatment")
# leveneTest(m4) #not significant, so passes assumptions test (variances don't have sig diff)
# # 
# #$`plot.sum$treatment`
# #                    diff          lwr       upr     p adj
# #gap-control    -8.4274862 -9.891880794 -6.963092 0.0000000
# #leave-control  -8.0670883 -9.531482885 -6.602694 0.0000000
# #legacy-control -6.9584835 -8.422878085 -5.494089 0.0000000
# #leave-gap       0.3603979 -1.103996717  1.824793 0.8829462
# #legacy-gap      1.4690027  0.004608083  2.933397 0.0491998
# #legacy-leave    1.1086048 -0.355789827  2.572999 0.1656669
# 
# 
# #########################################################
# 
# #3: regeneration
# #     Seedling density by treatment
# seed <- read.csv("LS_Worthington_Data_2018_seedlings.csv")
# seedling.total<-ddply(seed, .(Plot), .fun = summarize, 
#                       seed.m2 = sum(Tally)/36, #aggregate the data by plot PER M2
#                       seed.ha = sum(Tally)/36*10000) #and per ha
# #add treatment
# seedling.total$treatment <- c("CONTROL","LEAVE","LEGACY","GAP","LEGACY","LEGACY","LEAVE","CONTROL","GAP","GAP","LEGACY","CONTROL","LEAVE","GAP","CONTROL","LEAVE") #aka treat.vec from above
# seedling.total
# 
# #aggregate seedling density by treatment
# seedling.agg = ddply(seedling.total, .(treatment), .fun = summarize,
#                      mean.seed = mean(seed.m2),
#                      sd.seed = sd(seed.m2)) #or divide by sqrt(4) for SE
# seedling.agg
# 
# #test differences
# m3 = aov(seed.m2 ~ treatment, data = seedling.total)
# summary(m3) #treatment    3  0.333  0.1112   0.185  0.904 no treatment effect
# 
# ####by SPECIES
# #agg by treatment and species
# unique(seed$Species) #ACPE ACRU ACSA APCE BEAL BELE FAGR FRAM OSVI PIST POGR POTR PRSE TSCA
# seedling.sp<-ddply(seed, .(Plot,Species), .fun = summarize, 
#                    seed.m2 = sum(Tally)/36, #aggregate the data by plot PER M2
#                    seed.ha = sum(Tally)/36*10000) #and per ha
# 
# #add treatment
# seedling.sp$treatment <- NA
# for(i in 1:nrow(seedling.sp)){
#   if (seedling.sp$Plot[i] == 1 | seedling.sp$Plot[i] == 8 | seedling.sp$Plot[i] == 12 | seedling.sp$Plot[i] == 15){
#     seedling.sp$treatment[i] = "CONTROL"
#   }
#   if (seedling.sp$Plot[i] == 2 | seedling.sp$Plot[i] == 7 | seedling.sp$Plot[i] == 13 | seedling.sp$Plot[i] == 16){
#     seedling.sp$treatment[i] = "LEAVE"
#   }
#   if (seedling.sp$Plot[i] == 3 | seedling.sp$Plot[i] == 5 | seedling.sp$Plot[i] == 6 | seedling.sp$Plot[i] == 11){
#     seedling.sp$treatment[i] = "LEGACY"
#   }
#   if (seedling.sp$Plot[i] == 4 | seedling.sp$Plot[i] == 9 | seedling.sp$Plot[i] == 10 | seedling.sp$Plot[i] == 14){
#     seedling.sp$treatment[i] = "GAP"
#   }
# }
# 
# 
# #aggregate by species at the plot level, to get accurate SE (among plots)
# seed.sp.mat = matrix(nrow = 5,ncol = 16,data = 0)
# seed.sp.vec <- c("ACPE","ACRU","Betula","FAGR","other")
# rownames(seed.sp.mat)<-seed.sp.vec
# colnames(seed.sp.mat)<-c(1:16)
# for(i in 1:16){ #for each plot
#   tempdat = seedling.sp[seedling.sp$Plot == i,] #per plot
#   for (j in 1:nrow(tempdat)){ #extract species values for each plot
#     if(tempdat$Species[j]=="ACPE"){seed.sp.mat[1,i]=tempdat$seed.ha[j]}
#     if(tempdat$Species[j]=="ACRU"){seed.sp.mat[2,i]=tempdat$seed.ha[j]}
#     if(tempdat$Species[j]=="BEAL" | tempdat$Species[j]=="BELE"){seed.sp.mat[3,i]=tempdat$seed.ha[j]+seed.sp.mat[3,i]}
#     if(tempdat$Species[j]=="FAGR"){seed.sp.mat[4,i]=tempdat$seed.ha[j]}
#     if(tempdat$Species[j]==" ACRU"|tempdat$Species[j]=="ACSA"|tempdat$Species[j]=="FRAM"|tempdat$Species[j]=="OSVI"|tempdat$Species[j]=="PIST"|tempdat$Species[j]=="POGR"|tempdat$Species[j]=="POTR"|tempdat$Species[j]=="PRSE"|tempdat$Species[j]=="TSCA"){seed.sp.mat[5,i]=tempdat$seed.ha[j]+seed.sp.mat[5,i]}
#   }
# }
# seed.sp.mat
# #melt it back to long form
# library(reshape2)
# seed.sp.mat.long <- melt(seed.sp.mat)
# tail(seed.sp.mat.long)
# colnames(seed.sp.mat.long)<-c("species","Plot","seed.ha")
# 
# #add treatment
# seed.sp.mat.long$treatment <- NA
# for(i in 1:nrow(seed.sp.mat.long)){
#   if (seed.sp.mat.long$Plot[i] == 1 | seed.sp.mat.long$Plot[i] == 8 | seed.sp.mat.long$Plot[i] == 12 | seed.sp.mat.long$Plot[i] == 15){
#     seed.sp.mat.long$treatment[i] = "CONTROL"
#   }
#   if (seed.sp.mat.long$Plot[i] == 2 | seed.sp.mat.long$Plot[i] == 7 | seed.sp.mat.long$Plot[i] == 13 | seed.sp.mat.long$Plot[i] == 16){
#     seed.sp.mat.long$treatment[i] = "LEAVE"
#   }
#   if (seed.sp.mat.long$Plot[i] == 3 | seed.sp.mat.long$Plot[i] == 5 | seed.sp.mat.long$Plot[i] == 6 | seed.sp.mat.long$Plot[i] == 11){
#     seed.sp.mat.long$treatment[i] = "LEGACY"
#   }
#   if (seed.sp.mat.long$Plot[i] == 4 | seed.sp.mat.long$Plot[i] == 9 | seed.sp.mat.long$Plot[i] == 10 | seed.sp.mat.long$Plot[i] == 14){
#     seed.sp.mat.long$treatment[i] = "GAP"
#   }
# }
# write.csv(seed.sp.mat.long,"seedling_species_plot.csv")
# 
# #NOW aggregate by treatment
# seed.sp.total = ddply(seed.sp.mat.long, .(treatment,species), .fun = summarize,
#                       tot.seed.ha = mean(seed.ha),
#                       se.seed.ha = sd(seed.ha)/sqrt(4))
# 
# #restructure to have rows of species and columns of treatments
# 
# library(reshape)
# #for the means
# seed.means = seed.sp.total[,1:3]
# bar.seed <- cast(seed.means, species ~ treatment) #it worked!
# bar.seed = as.matrix(bar.seed)
# bar.seed1 = bar.seed[c(1,3,4,2,5),] #change order so colors match: ACPE,Betula,FAGR,ACRU,other
# #for the errors
# seed.se = seed.sp.total[,c(1,2,4)]
# bar.se <- cast(seed.se, species ~ treatment) #it worked!
# bar.se = as.matrix(bar.se)
# bar.se1 = bar.se[c(1,3,4,2,5),] #change order so colors match: ACPE,Betula,FAGR,ACRU,other
# #orders match
# 
# #graph it pretty!
# range(seed.sp.total$tot.seed.ha) #0-9444
# ylim <- c(0,14000) #to account for SE
# angle1 <- c(0,45,45,0,45)
# angle2 <- c(0,0,0,0,135)
# density1 <- c(0,99,20,99,15)
# density2 <- c(0,0,0,0,15)
# col <- c("gray99","gray10","gray40","gray80","gray25")
# col1 <- c("gray99","gray10","gray40","gray99","gray25")
# density1.1 <- c(0,99,20,0,15)
# #gray100 is white, gray0 is black
# 
# # plot --------------------------------------------------------------------
# barplot(bar.seed1, beside=TRUE, ylim=ylim, col=col, angle=angle1, density=density1,ylab="Seedlings/ha",xlab="Treatment")
# seed1<-barplot(bar.seed1, add=TRUE, beside=TRUE, ylim=ylim, col=col, angle=angle2, density=density2)
# legend("topright",legend=c(expression(italic("Acer pennsylvanica"),italic("Betula spp."),italic("Fagus grandifolia"),italic("Acer rubrum")),"Other"), ncol=1, fill=c("white","white","white","gray","white"))
# par(bg="transparent")
# legend("topright", legend=c(expression(italic("Acer pennsylvanica"),italic("Betula spp."),italic("Fagus grandifolia"),italic("Acer rubrum")),"Other"), ncol=1, fill=TRUE, col=col1, angle=angle1, density=density1.1)
# par(bg="transparent")
# legend("topright", legend=c(expression(italic("Acer pennsylvanica"),italic("Betula spp."),italic("Fagus grandifolia"),italic("Acer rubrum")),"Other"), ncol=1, fill=TRUE, col=col, angle=angle2, density=density2)
# #add error bars:
# segments(seed1,bar.seed1,seed1,bar.seed1+bar.se1,lwd=1)
# arrows(seed1, bar.seed1, seed1, bar.seed1+bar.se1, lwd = 1, angle = 90,
#        code = 3, length = 0.03)
# 
# #plot in color for the poster
# col = c("olivedrab4","peru","gold","brown3","gray55")
# seed1<-barplot(bar.seed1, beside=TRUE, ylim=ylim, col=col,ylab="Seedlings/ha",xlab="Treatment")
# legend("topright",inset=0.02,legend=c(expression(italic("Acer pennsylvanica"),italic("Betula spp."),italic("Fagus grandifolia"),italic("Acer rubrum")),"Other"), ncol=1, fill=col)
# segments(seed1,bar.seed1,seed1,bar.seed1+bar.se1,lwd=1)
# arrows(seed1, bar.seed1, seed1, bar.seed1+bar.se1, lwd = 1, angle = 90,
#        code = 3, length = 0.03)
# 
# 
# #test differences
# #ACPE
# acpe = seed.sp.mat.long[seed.sp.mat.long$species=="ACPE",]
# hist(acpe$seed.ha) #NOT normally distributed
# acpe$ln.density = log(1+acpe$seed.ha)
# hist(acpe$ln.density) #natural log, not great still
# acpe.con = acpe[acpe$treatment=="CONTROL",]
# acpe.gap = acpe[acpe$treatment=="GAP",]
# acpe.leave = acpe[acpe$treatment=="LEAVE",]
# acpe.leg = acpe[acpe$treatment=="LEGACY",]
# 
# #data not normally distributed
# m12 = aov(ln.density ~ treatment, data = acpe)
# summary(m12)#treatment    3 0.02033 0.006778   0.667  0.588
# 
# #ACRU
# acru = seed.sp.mat.long[seed.sp.mat.long$species=="ACRU",]
# hist(acru$density) #NOT normally distributed
# acru$ln.density = log(1+log(1+acru$density))
# hist(acru$ln.density) #natural log of natural log, not great still
# acru.con = acru[acru$treatment=="CONTROL",]
# acru.gap = acru[acru$treatment=="GAP",]
# acru.leave = acru[acru$treatment=="LEAVE",]
# acru.leg = acru[acru$treatment=="LEGACY",]
# 
# #data not normally distributed
# m13 = aov(ln.density ~ treatment, data = acru)
# summary(m13)#treatment    3 0.0516  0.0172   0.353  0.788
# 
# #BEAL
# beal = seed.sp.mat.long[seed.sp.mat.long$species=="Betula",]
# hist(beal$density) #NOT normally distributed
# beal$ln.density = sqrt(beal$density)
# hist(beal$ln.density) #square root, better
# beal.con = beal[beal$treatment=="CONTROL",]
# beal.gap = beal[beal$treatment=="GAP",]
# beal.leave = beal[beal$treatment=="LEAVE",]
# beal.leg = beal[beal$treatment=="LEGACY",]
# 
# #data not normally distributed
# m14 = aov(ln.density ~ treatment, data = beal)
# summary(m14)#treatment    3 0.0942 0.03140   0.823  0.506 #still no differences
# 
# #BELE
# bele = seed.sp.mat.long[seed.sp.mat.long$species=="BELE",]
# hist(bele$density) #NOT normally distributed
# bele$ln.density = log(1+sqrt(bele$density))
# hist(bele$ln.density) #log of square root, not great
# bele.con = bele[bele$treatment=="CONTROL",]
# bele.gap = bele[bele$treatment=="GAP",]
# bele.leave = bele[bele$treatment=="LEAVE",]
# bele.leg = bele[bele$treatment=="LEGACY",]
# 
# #data not normally distributed
# m15 = aov(ln.density ~ treatment, data = bele)
# summary(m15)#treatment    3 0.0522 0.01740   0.632  0.608
# 
# 
# #FAGR
# fagr = seed.sp.mat.long[seed.sp.mat.long$species=="FAGR",]
# hist(fagr$density) #NOT normally distributed
# fagr$ln.density = sqrt(fagr$density)
# hist(fagr$ln.density) #square root, good
# fagr.con = fagr[fagr$treatment=="CONTROL",]
# fagr.gap = fagr[fagr$treatment=="GAP",]
# fagr.leave = fagr[fagr$treatment=="LEAVE",]
# fagr.leg = fagr[fagr$treatment=="LEGACY",]
# 
# #data not normally distributed
# m15 = aov(ln.density ~ treatment, data = fagr)
# summary(m15)#treatment    3  0.262 0.08733   0.971  0.439
# 
# #NMS ##
# #     Tree regeneration assemblages: MRPP, NMS, indicator species analysis
# seed.main = matrix(nrow = 16,ncol = 5,data = 0)
# seed.sp.vec <- c("ACPE","Betula","FAGR","ACRU","other")
# colnames(seed.main)<-seed.sp.vec
# rownames(seed.main)<-c(1:16)
# unique(seedling.sp$Species) #ACPE ACRU ACSA BEAL BELE FAGR FRAM OSVI PIST POGR POTR PRSE TSCA
# for(i in 1:16){ #for each plot
#   tempdat = seedling.sp[seedling.sp$Plot == i,] #per plot
#   for (j in 1:nrow(tempdat)){ #extract species values for each plot
#     if(tempdat$Species[j]=="ACPE"){seed.main[i,1]=tempdat$seed.ha[j]}
#     if(tempdat$Species[j]=="BEAL" | tempdat$Species[j]=="BELE"){seed.main[i,2]=tempdat$seed.ha[j]+seed.main[i,2]}
#     if(tempdat$Species[j]=="FAGR"){seed.main[i,3]=tempdat$seed.ha[j]}
#     if(tempdat$Species[j]=="ACRU"){seed.main[i,4]=tempdat$seed.ha[j]}  
#     if(tempdat$Species[j]=="ACSA"|tempdat$Species[j]=="BEPA"|tempdat$Species[j]=="OSVI"|tempdat$Species[j]=="PIST"|tempdat$Species[j]=="QURU"|tempdat$Species[j]=="TIAM"|tempdat$Species[j]=="FRAM"|tempdat$Species[j]=="OSVI"|tempdat$Species[j]=="POGR"|tempdat$Species[j]=="POTR"|tempdat$Species[j]=="TSCA"|tempdat$Species[j]=="PRSE"){seed.main[i,5]=tempdat$seed.ha[j]+seed.main[i,5]}
#   }
# }
# write.csv(seed.main,"seed.main.betula.comb.csv")
# #results from PC-ORD: still no solution
# 
# 
# #compare densities of commercial to non-commercial species
# seedling.sp$commercial = NA
# unique(seedling.sp$Species) #ACPE ACRU ACSA BEAL BELE FAGR FRAM OSVI PIST POGR POTR PRSE TSCA
# for(i in 1:nrow(seedling.sp)){
#   if(seedling.sp$Species[i]=="ACPE"|seedling.sp$Species[i]=="FAGR"|seedling.sp$Species[i]=="OSVI"){
#     seedling.sp$commercial[i] = 0
#   }
#   if(seedling.sp$Species[i]=="ACRU"|seedling.sp$Species[i]=="ACSA"|seedling.sp$Species[i]=="BEAL"|seedling.sp$Species[i]=="BELE"|seedling.sp$Species[i]=="FRAM"|seedling.sp$Species[i]=="PIST"|seedling.sp$Species[i]=="POGR"|seedling.sp$Species[i]=="POTR"|seedling.sp$Species[i]=="PRSE"|seedling.sp$Species[i]=="TSCA"){
#     seedling.sp$commercial[i] = 1
#   }
# }
# tail(seedling.sp)
# #analyze
# seed.com.plot = ddply(seedling.sp, .(Plot,commercial), .fun = summarize,
#                       tot.seed.ha = sum(seed.ha),
#                       treatment = getmode(treatment))
# comm.plot = seed.com.plot[seed.com.plot$commercial == 1,]
# noncomm.plot = seed.com.plot[seed.com.plot$commercial == 0,]
# hist(log(comm.plot$tot.seed.ha)) #normal
# hist(sqrt(noncomm.plot$tot.seed.ha)) #normal
# 
# m.com <- aov(log(comm.plot$tot.seed.ha) ~ comm.plot$treatment) #no difference between treatments
# summary(m.com) #comm.plot$treatment  3  1.859  0.6198   0.395  0.759
# 
# m.noncom <- aov(sqrt(noncomm.plot$tot.seed.ha) ~ noncomm.plot$treatment) #no difference either
# summary(m.noncom) #noncomm.plot$treatment  3   2462   820.5   1.628  0.235
# 
# #plot
# seed.com.agg = ddply(seed.com.plot, .(treatment,commercial), .fun = summarize,
#                      total.seed.ha = mean(tot.seed.ha),
#                      se.seed.ha = sd(tot.seed.ha)/sqrt(4))
# seed.com.agg
# 
# seed.com.mat = matrix(nrow = 2, ncol = 4, data = NA)
# colnames(seed.com.mat) = c("CONTROL","LEAVE","LEGACY","GAP")
# rownames(seed.com.mat) = c("noncommercial","commercial")
# seed.com.mat[,1]=seed.com.agg[1:2,3]
# seed.com.mat[,2]=seed.com.agg[5:6,3]
# seed.com.mat[,3]=seed.com.agg[7:8,3]
# seed.com.mat[,4]=seed.com.agg[3:4,3]
# colSums(seed.com.mat) #total is 13300
# 
# #graph it 
# ylim <- c(0,14000) #to account for SE
# 
# # plot --------------------------------------------------------------------
# barplot(seed.com.mat, beside=FALSE, ylim=ylim, ylab="Seedlings/ha",xlab="Treatment", main = "Commerical seedling densities")
# legend("topright",inset = 0.02,legend=c("Non-commercial","Commercial"), ncol=1, fill=c("gray30","gray80"))
# #plot in color for the poster
# barplot(seed.com.mat, beside=FALSE, ylim=ylim, ylab="Seedlings/ha",xlab="Treatment", main = "Commerical seedling densities",col = c("pink4","wheat2"))
# legend("topright",inset = 0.02,legend=c("Non-commercial","Commercial"), ncol=1, fill=c("pink4","wheat2"))
# 
# ################################
# 
# #     Sapling density by treatment; by species
# sap <- read.csv("LS_Worthington_Data_2018_saplings.csv")
# tail(sap)
# #calculate per ha values for saplings
# #
# expansion.vec = c(1.857161, 1.857161 ,2.310629, 2.269686, 1.857161, 1.971447, 2.164490, 1.857161, 2.004470, 2.012734 ,2.039428, 1.857161, 2.469816, 2.270496, 1.857161, 2.035906) #aka plot$expansion
# sap$expansion = NA
# for(j in 1:nrow(sap)){
#   for (i in 1:16){
#     if(sap$Plot[j]==i){
#       sap$expansion[j]=expansion.vec[i]}}}
# sap
# sap$density.ha = NA
# sap$density.ha = sap$Sapling_tally*sap$expansion
# 
# sap$treatment <- NA
# for(i in 1:nrow(sap)){
#   if (sap$Plot[i] == 1 | sap$Plot[i] == 8 | sap$Plot[i] == 12 | sap$Plot[i] == 15){
#     sap$treatment[i] = "CONTROL"
#   }
#   if (sap$Plot[i] == 2 | sap$Plot[i] == 7 | sap$Plot[i] == 13 | sap$Plot[i] == 16){
#     sap$treatment[i] = "LEAVE"
#   }
#   if (sap$Plot[i] == 3 | sap$Plot[i] == 5 | sap$Plot[i] == 6 | sap$Plot[i] == 11){
#     sap$treatment[i] = "LEGACY"
#   }
#   if (sap$Plot[i] == 4 | sap$Plot[i] == 9 | sap$Plot[i] == 10 | sap$Plot[i] == 14){
#     sap$treatment[i] = "GAP"
#   }
# }
# 
# #aggregate sapling density by plot
# library(plyr)
# sap.total = ddply(sap, .(Plot), .fun = summarize,
#                   tot.dens.ha = sum(density.ha))
# sap.total$treatment <- NA
# for(i in 1:nrow(sap.total)){
#   if (sap.total$Plot[i] == 1 | sap.total$Plot[i] == 8 | sap.total$Plot[i] == 12 | sap.total$Plot[i] == 15){
#     sap.total$treatment[i] = "CONTROL"
#   }
#   if (sap.total$Plot[i] == 2 | sap.total$Plot[i] == 7 | sap.total$Plot[i] == 13 | sap.total$Plot[i] == 16){
#     sap.total$treatment[i] = "LEAVE"
#   }
#   if (sap.total$Plot[i] == 3 | sap.total$Plot[i] == 5 | sap.total$Plot[i] == 6 | sap.total$Plot[i] == 11){
#     sap.total$treatment[i] = "LEGACY"
#   }
#   if (sap.total$Plot[i] == 4 | sap.total$Plot[i] == 9 | sap.total$Plot[i] == 10 | sap.total$Plot[i] == 14){
#     sap.total$treatment[i] = "GAP"
#   }
# }
# sap.total=sap.total[1:16,]
# write.csv(sap.total,"sapling_density_by_plot.csv")
# #aggregate sapling density by treatment
# sap.agg = ddply(sap.total, .(treatment), .fun = summarize,
#                 sap.density = mean(tot.dens.ha),
#                 sap.dens.se = sd(tot.dens.ha)/sqrt(4))
# sap.agg = sap.agg[1:4,]
# 
# #analyze sapling data
# m4 = aov(tot.dens.ha ~ treatment, data = sap.total)
# summary(m4) #treatment    3 3625802 1208601   11.38  8e-04 ***
# 
# #separate treatment data to run t.test
# con = sap.total[sap.total$treatment=="CONTROL",]
# leave = sap.total[sap.total$treatment=="LEAVE",]
# leg = sap.total[sap.total$treatment=="LEGACY",]
# gap = sap.total[sap.total$treatment=="GAP",]
# 
# t.test(con$tot.dens.ha,leave$tot.dens.ha) #t = -5.9528, df = 4.005, p-value = 0.00398****
# t.test(con$tot.dens.ha,leg$tot.dens.ha) #t = -5.328, df = 3.856, p-value = 0.006618****
# t.test(con$tot.dens.ha,gap$tot.dens.ha) #t = -5.2928, df = 3.413, p-value = 0.009425*
# t.test(leave$tot.dens.ha,leg$tot.dens.ha) #t = 0.163, df = 5.958, p-value = 0.8759
# t.test(leave$tot.dens.ha,gap$tot.dens.ha) #t = -1.1767, df = 5.071, p-value = 0.2916
# t.test(leg$tot.dens.ha,gap$sap.total) #t = -1.2725, df = 5.323, p-value = 0.2559
# 
# ###
# #breakdown by species
# head(sap)
# 
# #aggregate saplings by species at the plot level, to get accurate SE (among plots)
# #sap is equivalent in structure to seedling.total
# sap.sp.mat = matrix(nrow = 5,ncol = 16,data = 0)
# sap.sp.vec <- c("ACPE","Betula","FAGR","Popul","other")
# rownames(sap.sp.mat)<-sap.sp.vec
# colnames(sap.sp.mat)<-c(1:16)
# unique(sap$Species)
# for(i in 1:16){ #for each plot
#   tempdat = sap[sap$Plot == i,] #per plot
#   for (j in 1:nrow(tempdat)){ #extract species values for each plot
#     if(tempdat$Species[j]=="ACPE"){sap.sp.mat[1,i]=tempdat$density.ha[j]}#for each species, replace value with density
#     if(tempdat$Species[j]=="BEAL"|tempdat$Species[j]=="BELE"){sap.sp.mat[2,i]=tempdat$density.ha[j]+sap.sp.mat[2,i]}
#     if(tempdat$Species[j]=="FAGR"){sap.sp.mat[3,i]=tempdat$density.ha[j]}
#     if(tempdat$Species[j]=="POGR"|tempdat$Species[j]=="POTR"){sap.sp.mat[4,i]=tempdat$density.ha[j]+sap.sp.mat[4,i]}  
#     if(tempdat$Species[j]=="ACRU"|tempdat$Species[j]=="ACSA"|tempdat$Species[j]=="BEPA"|tempdat$Species[j]=="CADE"|tempdat$Species[j]=="OSVI"|tempdat$Species[j]=="PIST"|tempdat$Species[j]=="QURU"|tempdat$Species[j]=="TIAM"|tempdat$Species[j]=="TSCA"){sap.sp.mat[5,i]=tempdat$density.ha[j]+sap.sp.mat[5,i]} #add all others together
#   }
# }
# sap.sp.mat
# #melt it back to long form
# library(reshape2)
# sap.sp.mat.long <- melt(sap.sp.mat)
# tail(sap.sp.mat.long)
# colnames(sap.sp.mat.long)<-c("species","Plot","sap.ha")
# 
# #add treatment
# sap.sp.mat.long$treatment <- NA
# for(i in 1:nrow(sap.sp.mat.long)){
#   if (sap.sp.mat.long$Plot[i] == 1 | sap.sp.mat.long$Plot[i] == 8 | sap.sp.mat.long$Plot[i] == 12 | sap.sp.mat.long$Plot[i] == 15){
#     sap.sp.mat.long$treatment[i] = "CONTROL"
#   }
#   if (sap.sp.mat.long$Plot[i] == 2 | sap.sp.mat.long$Plot[i] == 7 | sap.sp.mat.long$Plot[i] == 13 | sap.sp.mat.long$Plot[i] == 16){
#     sap.sp.mat.long$treatment[i] = "LEAVE"
#   }
#   if (sap.sp.mat.long$Plot[i] == 3 | sap.sp.mat.long$Plot[i] == 5 | sap.sp.mat.long$Plot[i] == 6 | sap.sp.mat.long$Plot[i] == 11){
#     sap.sp.mat.long$treatment[i] = "LEGACY"
#   }
#   if (sap.sp.mat.long$Plot[i] == 4 | sap.sp.mat.long$Plot[i] == 9 | sap.sp.mat.long$Plot[i] == 10 | sap.sp.mat.long$Plot[i] == 14){
#     sap.sp.mat.long$treatment[i] = "GAP"
#   }
# }
# write.csv(sap.sp.mat.long,"sapling_species_plot.csv")
# #NOW aggregate by treatment
# head(sap.sp.mat.long)
# sap.sp.total = ddply(sap.sp.mat.long, .(treatment,species), .fun = summarize,
#                      tot.sap.ha = mean(sap.ha),
#                      se.sap.ha = sd(sap.ha)/sqrt(4))
# 
# #restructure to have rows of species and columns of treatments
# 
# library(reshape)
# #for the means
# sap.means = sap.sp.total[,1:3]
# bar.sap <- cast(sap.means, species ~ treatment) #it worked!
# bar.sap = as.matrix(bar.sap)
# bar.sap1 = bar.sap[c(1,2,3,5,4),] #change order so colors match seedling graph #ACPE, Betula, FAGR, Popul, other
# #for the errors
# sap.se = sap.sp.total[,c(1,2,4)]
# bar.se <- cast(sap.se, species ~ treatment) #it worked!
# bar.se = as.matrix(bar.se)
# bar.se1 = bar.se[c(1,2,3,5,4),]
# #orders match
# 
# 
# #now graph it
# ## plotting settings -------------------------------------------------------
# ylim <- c(0,1200)
# angle1 <- c(0,45,45,0,45)
# angle2 <- c(0,0,0,0,135)
# density1 <- c(0,99,20,99,15)
# density2 <- c(0,0,0,0,15)
# col <- c("gray99","gray10","gray40","gray80","gray25")
# col1 <- c("gray99","gray10","gray40","gray99","gray25")
# density1.1 <- c(0,99,20,0,15)
# #gray100 is white, gray0 is black
# 
# # plot --------------------------------------------------------------------
# barplot(bar.sap1, beside=TRUE, ylim=ylim, col=col, angle=angle1, density=density1,ylab="Saplings/ha",xlab="Treatment")
# sap1<-barplot(bar.sap1, add=TRUE, beside=TRUE, ylim=ylim, col=col, angle=angle2, density=density2)
# 
# legend("topleft",legend=c(expression(italic("Acer pennsylvanica"),italic("Betula spp."),italic("Fagus grandifolia"),italic("Populus spp.")),"Other"), ncol=1, fill=c("white","white","white","gray","white"))
# par(bg="transparent")
# legend("topleft", legend=c(expression(italic("Acer pennsylvanica"),italic("Betula spp."),italic("Fagus grandifolia"),italic("Populus spp.")),"Other"), ncol=1, fill=TRUE, col=col1, angle=angle1, density=density1.1)
# par(bg="transparent")
# legend("topleft", legend=c(expression(italic("Acer pennsylvanica"),italic("Betula spp."),italic("Fagus grandifolia"),italic("Populus spp.")),"Other"), ncol=1, fill=TRUE, col=col, angle=angle2, density=density2)
# 
# #to add error bars:
# #make new matrix of errors = semat (above) and add as "segments"
# segments(sap1,bar.sap1,sap1,bar.sap1+bar.se1,lwd=1)
# arrows(sap1, bar.sap1, sap1, bar.sap1+bar.se1, lwd = 1, angle = 90,
#        code = 3, length = 0.03)
# 
# 
# #plot in color for the poster
# ylim <- c(0,1200)
# col = c("olivedrab4","peru","gold","slateblue","gray55")
# sap1<-barplot(bar.sap1, beside=TRUE, ylim=ylim, col=col,ylab="Saplings/ha",xlab="Treatment")
# legend("topleft", inset = 0.02, legend=c(expression(italic("Acer pennsylvanica"),italic("Betula spp."),italic("Fagus grandifolia"),italic("Populus spp.")),"Other"), ncol=1, fill=col)
# segments(sap1,bar.sap1,sap1,bar.sap1+bar.se1,lwd=1)
# arrows(sap1, bar.sap1, sap1, bar.sap1+bar.se1, lwd = 1, angle = 90,
#        code = 3, length = 0.03)
# 
# #test differences
# #ACPE
# acpe = sap.sp.mat.long[sap.sp.mat.long$species=="ACPE",]
# hist(acpe$sap.ha) #NOT normally distributed
# acpe$ln.sap.ha = log(1+acpe$sap.ha)
# acpe$ln.density = log(1+acpe$sap.ha)
# hist(acpe$ln.sap.ha) #natural log, better
# acpe.con = acpe[acpe$treatment=="CONTROL",]
# acpe.gap = acpe[acpe$treatment=="GAP",]
# acpe.leave = acpe[acpe$treatment=="LEAVE",]
# acpe.leg = acpe[acpe$treatment=="LEGACY",]
# 
# #data not normally distributed
# m6 = aov(ln.density ~ treatment, data = acpe)
# summary(m6)#treatment  3  34.18  11.394   9.706 0.00157 **
# t.test(acpe.con$ln.density,acpe.gap$ln.density) #t = -4.4234, df = 4.227, p-value = 0.01013**
# t.test(acpe.con$ln.density,acpe.leave$ln.density) #t = -3.0237, df = 5.29, p-value = 0.02727**
# t.test(acpe.con$ln.density,acpe.leg$ln.density) #t = -2.3792, df = 3.61, p-value = 0.08305
# t.test(acpe.gap$ln.density,acpe.leave$ln.density) #t = 1.4765, df = 5.282, p-value = 0.1968
# t.test(acpe.gap$ln.density,acpe.leg$ln.density) #t = 4.2209, df = 5.341, p-value = 0.007194***
# t.test(acpe.leave$ln.density,acpe.leg$ln.density) #t = 1.5409, df = 4.267, p-value = 0.1938
# 
# 
# #Betula
# beal = sap.sp.mat.long[sap.sp.mat.long$species=="Betula",]
# hist(beal$sap.ha) #NOT normally distributed
# beal$ln.sap.ha = log(1+beal$sap.ha)
# hist(beal$ln.sap.ha) #natural log, better
# beal.con = beal[beal$treatment=="CONTROL",]
# beal.gap = beal[beal$treatment=="GAP",]
# beal.leave = beal[beal$treatment=="LEAVE",]
# beal.leg = beal[beal$treatment=="LEGACY",]
# 
# #data not normally distributed
# m7 = aov(ln.sap.ha ~ treatment, data = beal)
# summary(m7)#treatment    3  5.466  1.8221   18.09 9.51e-05 ***
# 
# t.test(beal.con$ln.density,beal.gap$ln.density) #t = -8.9239, df = 5.309, p-value = 0.0002151***
# t.test(beal.con$ln.density,beal.leave$ln.density) #t = -5.6168, df = 5.968, p-value = 0.001385**
# t.test(beal.con$ln.density,beal.leg$ln.density)  #t = -4.8038, df = 4.902, p-value = 0.005127**
# t.test(beal.gap$ln.density,beal.leave$ln.density)  #t = 2.0116, df = 5.09, p-value = 0.09943
# t.test(beal.gap$ln.density,beal.leg$ln.density) #t = 0.807, df = 3.98, p-value = 0.4651
# t.test(beal.leave$ln.density,beal.leg$ln.density) #t = -0.5573, df = 5.121, p-value = 0.6008
# 
# #FAGR
# fagr = sap.sp.mat.long[sap.sp.mat.long$species=="FAGR",]
# hist(fagr$sap.ha) #looks okay
# fagr.con = fagr[fagr$treatment=="CONTROL",]
# fagr.gap = fagr[fagr$treatment=="GAP",]
# fagr.leave = fagr[fagr$treatment=="LEAVE",]
# fagr.leg = fagr[fagr$treatment=="LEGACY",]
# 
# #
# m8 = aov(sap.ha ~ treatment, data = fagr)
# summary(m8)#treatment    3 948655  316218   4.128 0.0316 *
# t.test(fagr.con$sap.ha,fagr.gap$sap.ha) #t = -1.9057, df = 4.27, p-value = 0.1249
# t.test(fagr.con$sap.ha,fagr.leave$sap.ha) #t = -3.6577, df = 4.415, p-value = 0.0182**
# t.test(fagr.con$sap.ha,fagr.leg$sap.ha) #t = -3.3146, df = 4.165, p-value = 0.02779**
# t.test(fagr.gap$sap.ha,fagr.leave$sap.ha) #t = -1.2697, df = 5.978, p-value = 0.2514
# t.test(fagr.gap$sap.ha,fagr.leg$sap.ha) #t = -1.1753, df = 5.987, p-value = 0.2845
# t.test(fagr.leave$sap.ha,fagr.leg$sap.ha) #t = 0.0292, df = 5.932, p-value = 0.9776
# 
# #POPULUS
# popul = sap.sp.mat.long[sap.sp.mat.long$species=="Popul",]
# hist(popul$sap.ha) #NOT normally distributed
# popul$ln.sap.ha = log(1+log(1+popul$sap.ha))
# hist(popul$ln.sap.ha) #natural log of natural log, not great
# popul.con = popul[popul$treatment=="CONTROL",]
# popul.gap = popul[popul$treatment=="GAP",]
# popul.leave = popul[popul$treatment=="LEAVE",]
# popul.leg = popul[popul$treatment=="LEGACY",]
# 
# #data still not normally distributed
# m10 = aov(ln.sap.ha ~ treatment, data = popul)
# summary(m10)#treatment    3  2.042  0.6805   1.791  0.202 #no differences
# 
# ################
# #compare betula lenta vs betula alleghaniensis
# sap.bet.mat = as.data.frame(matrix(nrow = 16,ncol = 2,data = 0))
# sap.bet.vec <- c("BEAL","BELE")
# colnames(sap.bet.mat)<-sap.bet.vec
# rownames(sap.bet.mat)<-c(1:16)
# for(i in 1:16){ #for each plot
#   tempdat = sap[sap$Plot == i,] #per plot
#   for (j in 1:nrow(tempdat)){ #extract species values for each plot
#     if(tempdat$Species[j]=="BEAL"){sap.bet.mat[i,1]=tempdat$density.ha[j]}#for each species, replace value with density
#     if(tempdat$Species[j]=="BELE"){sap.bet.mat[i,2]=tempdat$density.ha[j]}
#   }
# }
# sap.bet.mat$perc.BEAL = sap.bet.mat$BEAL/(sap.bet.mat$BEAL+sap.bet.mat$BELE)
# sap.bet.mat$perc.BELE = sap.bet.mat$BELE/(sap.bet.mat$BEAL+sap.bet.mat$BELE)
# hist(sap.bet.mat$perc.BEAL)
# hist(sap.bet.mat$perc.BELE)
# #add treatment
# sap.bet.mat$treatment <- c("CONTROL","LEAVE","LEGACY","GAP","LEGACY","LEGACY","LEAVE","CONTROL","GAP","GAP","LEGACY","CONTROL","LEAVE","GAP","CONTROL","LEAVE")
# bet.m1 <- aov(sap.bet.mat$perc.BELE ~ sap.bet.mat$treatment) #no difference
# summary(bet.m1)
# #                      Df Sum Sq Mean Sq F value Pr(>F)
# #sap.bet.mat$treatment  3 0.5849  0.1950   1.243  0.345
# #Residuals             10 1.5682  0.1568      
# mean(sap.bet.mat$perc.BEAL, na.rm = T) #33.49%
# mean(sap.bet.mat$perc.BELE, na.rm = T) #66.51%
# 
# #################
# 
# 
# #     Tree regeneration assemblages: MRPP, NMS, indicator species analysis
# sap.main = matrix(nrow = 16,ncol = 5,data = 0)
# sap.sp.vec <- c("ACPE","Betula","FAGR","Popul","other")
# colnames(sap.main)<-sap.sp.vec
# rownames(sap.main)<-c(1:16)
# unique(sap$Species)
# for(i in 1:16){ #for each plot
#   tempdat = sap[sap$Plot == i,] #per plot
#   for (j in 1:nrow(tempdat)){ #extract species values for each plot
#     if(tempdat$Species[j]=="ACPE"){sap.main[i,1]=tempdat$density.ha[j]}
#     if(tempdat$Species[j]=="BEAL"|tempdat$Species[j]=="BELE"){sap.main[i,2]=tempdat$density.ha[j]+sap.main[i,2]}
#     if(tempdat$Species[j]=="FAGR"){sap.main[i,3]=tempdat$density.ha[j]}
#     if(tempdat$Species[j]=="POGR"|tempdat$Species[j]=="POTR"){sap.main[i,4]=tempdat$density.ha[j]+sap.main[i,4]}  
#     if(tempdat$Species[j]=="ACRU"|tempdat$Species[j]=="ACSA"|tempdat$Species[j]=="BEPA"|tempdat$Species[j]=="CADE"|tempdat$Species[j]=="OSVI"|tempdat$Species[j]=="PIST"|tempdat$Species[j]=="QURU"|tempdat$Species[j]=="TIAM"|tempdat$Species[j]=="TSCA"){sap.main[i,5]=tempdat$density.ha[j]+sap.main[i,5]}
#   }
# }
# write.csv(sap.main,"sap.main.betula.comb.csv")
# 
# #calculate tau scores #from 2016_5_30_NMS_tau_calcs.R
# #!!! rotated graph 90° !!!
# #the following are correlation coeffs and kendall's tau from PC-ORD:
# #ACPE: 
# 
# #install.packages("Kendall")
# library(Kendall)
# #make matrix of plots, row axis scores, and relativized species scores
# row = read.csv("sap_nms2_betula.comb_graphrow.csv")
# sap.rel = sap.main
# sap.rel[,1] = sap.main[,1]/max(sap.main[,1])
# sap.rel[,2] = sap.main[,2]/max(sap.main[,2])
# sap.rel[,3] = sap.main[,3]/max(sap.main[,3])
# sap.rel[,4] = sap.main[,4]/max(sap.main[,4])
# sap.rel[,5] = sap.main[,5]/max(sap.main[,5])
# sap.rel #same as relativized by max in PC-ORD
# 
# sap.mat = cbind(row[1:16,],sap.rel)
# sap.mat$plot = 1:16
# sap.mat
# 
# #from http://statistical-research.com/wp-content/uploads/2012/09/kendall-tau1.pdf
# #calculate tau values from NMS axis scores and relativized species abundances 
# taumat <- matrix(nrow=5,ncol=2,data=NA) #6 species, 2 axes
# colnames(taumat)=c("axis1tau","axis2tau")
# rownames(taumat)=c(names(sap.mat[,3:7]))
# for (i in 1:2){ # number of axes
#   for (j in 1:5){ # number of variables
#     taumat[j,i] = cor(sap.mat[,(i)],sap.mat[,(j+2)],method="kendall")
#   }
# }
# taumat
# 
# pvalmat <- matrix(nrow=5,ncol=2,data=NA)
# colnames(pvalmat)=c("axis1p","axis2p")
# for (i in 1:2){
#   for (j in 1:5){
#     temp1 = Kendall(sap.mat[,(i)],sap.mat[,(2+j)])
#     pvalmat[j,i] = temp1$sl[1]
#   }
# }
# pvalmat
# 
# sap.nms.values = cbind(taumat,pvalmat)
# sap.nms.values #aligns with Sap_NMS_2axis.jpg graph
# #          axis1tau   axis2tau       axis1p      axis2p
# #ACPE   -0.33333333 -0.5000000 0.0791092217 0.007899663 *
# #Betula -0.51046472 -0.1924703 0.0068491111* 0.321441114 
# #FAGR   -0.63333333  0.2000000 0.0007336401* 0.300427198
# #Popul   0.05634362 -0.3662335 0.8413646221 0.095335074
# #other   0.45769286 -0.4407413 0.0165046453 0.021050012
# 
# 0.05/10 #0.005 bonferroni cutoff
# 0.05/5 #0.01 for each column
# 
# ################################
# #MRPP
# #Groups (identifiers) #1 = CONTROL, 2 = LEAVE, 3 = LEGACY, 4 = GAP
# #Compared             T             A             p
# #1  vs.      2     -3.32218367    0.23060124    0.00833933 
# #1  vs.      3     -3.40696515    0.22423694    0.00932007
# #1  vs.      4     -3.47445365    0.24651570    0.00858561
# #2  vs.      3      0.14897586   -0.00687514    0.49950368
# #2  vs.      4     -0.47724525    0.02338075    0.28648748
# #3  vs.      4     -2.60837480    0.11050066    0.00917158
# 
# #
# #sapling density by commercial
# sap$commercial = NA
# for (i in 1:nrow(sap)){
#   if(sap$Species[i]=="ACPE"|sap$Species[i]=="FAGR"|sap$Species[i]=="OSVI"){
#     sap$commercial[i] = 0
#   }
#   if(sap$Species[i]=="ACRU"|sap$Species[i]=="ACSA"|sap$Species[i]=="BEAL"|sap$Species[i]=="BELE"|sap$Species[i]=="BEPA"|sap$Species[i]=="CADE"|sap$Species[i]=="PIST"|sap$Species[i]=="POGR"|sap$Species[i]=="POTR"|sap$Species[i]=="PRSE"|sap$Species[i]=="QURU"|sap$Species[i]=="TIAM"|sap$Species[i]=="TSCA"){
#     sap$commercial[i] = 1
#   }
# }
# 
# sap.agg.plot = ddply(sap, .(Plot,commercial), .fun = summarize,
#                      tot.saps.ha = sum(density.ha),
#                      treatment = getmode(treatment))
# sap.agg.plot = sap.agg.plot[1:32,]
# sap.agg.tot = ddply(sap.agg.plot, .(treatment,commercial), .fun = summarize,
#                     mean.sap.ha = mean(tot.saps.ha),
#                     se.sap.ha = sd(tot.saps.ha)/sqrt(4),
#                     treatment = getmode(treatment))
# 
# sap.com.mat = matrix(nrow = 2, ncol = 4, data = NA)
# colnames(sap.com.mat) = c("CONTROL","LEAVE","LEGACY","GAP")
# rownames(sap.com.mat) = c("noncommercial","commercial")
# sap.com.mat[,1]=sap.agg.tot[1:2,2] #mean.sap.ha of controls
# sap.com.mat[,2]=sap.agg.tot[5:6,2] #mean.sap.ha of leave
# sap.com.mat[,3]=sap.agg.tot[7:8,2] #mean.sap.ha of legacy
# sap.com.mat[,4]=sap.agg.tot[3:4,2] #mean.sap.ha of gap
# colSums(sap.com.mat) #total is 1550 
# 
# #graph it 
# ylim <- c(0,1600) #to account for SE
# 
# # plot --------------------------------------------------------------------
# barplot(sap.com.mat, beside=FALSE, ylim=ylim, ylab="Saplings/ha",xlab="Treatment", main = "Commerical sapling densities")
# legend("topleft",inset = 0.02,legend=c("Non-commercial","Commercial"), ncol=1, fill=c("gray30","gray80"))
# #graph in color for poster
# col = c("pink4","wheat2")
# barplot(sap.com.mat, beside=FALSE, ylim=ylim, ylab="Saplings/ha",xlab="Treatment", main = "Commerical sapling densities", col = col)
# legend("topleft",inset = 0.02,legend=c("Non-commercial","Commercial"), ncol=1, fill=col)
# 
# #analyze 
# sap.agg.plot
# comm.plot = sap.agg.plot[sap.agg.plot$commercial == 1,]
# noncomm.plot = sap.agg.plot[sap.agg.plot$commercial == 0,]
# hist(log(comm.plot$tot.saps.ha)) #normal
# hist(sqrt(noncomm.plot$tot.saps.ha)) #normal
# 
# m.com <- aov(log(comm.plot$tot.saps.ha) ~ comm.plot$treatment) #difference!
# summary(m.com) # 3  23.49   7.831   6.507 0.00733 **
# 
# m.noncom <- aov(sqrt(noncomm.plot$tot.saps.ha) ~ noncomm.plot$treatment) #difference!
# summary(m.noncom) #3  333.7  111.24    5.69 0.0117 *
# 
# #t-tests
# head(comm.plot)
# comm.con = comm.plot[comm.plot$treatment=="CONTROL",]
# comm.leave = comm.plot[comm.plot$treatment=="LEAVE",]
# comm.leg = comm.plot[comm.plot$treatment=="LEGACY",]
# comm.gap = comm.plot[comm.plot$treatment=="GAP",]
# hist(comm.leave$tot.saps.ha) #all pretty normal
# #bonferroni: 6 tests: p > 0.0083
# #no treatments are significantly different with that cutoff
# t.test(comm.con$tot.saps.ha,comm.leave$tot.saps.ha) #t = -3.2534, df = 3.156, p-value = 0.04407
# t.test(comm.con$tot.saps.ha,comm.leg$tot.saps.ha) #t = -1.6766, df = 3.01, p-value = 0.1919
# t.test(comm.con$tot.saps.ha,comm.gap$tot.saps.ha) #t = -3.5761, df = 3.019, p-value = 0.03701
# t.test(comm.leave$tot.saps.ha,comm.leg$tot.saps.ha) #t = -0.829, df = 3.373, p-value = 0.4617
# t.test(comm.leave$tot.saps.ha,comm.gap$tot.saps.ha) #t = -2.2936, df = 3.723, p-value = 0.08847
# t.test(comm.leg$tot.saps.ha,comm.gap$tot.saps.ha) #t = -0.7178, df = 5.431, p-value = 0.5026
# 
# head(noncomm.plot)
# noncomm.con = noncomm.plot[noncomm.plot$treatment=="CONTROL",]
# noncomm.leave = noncomm.plot[noncomm.plot$treatment=="LEAVE",]
# noncomm.leg = noncomm.plot[noncomm.plot$treatment=="LEGACY",]
# noncomm.gap = noncomm.plot[noncomm.plot$treatment=="GAP",]
# hist(noncomm.leave$tot.saps.ha) #all pretty normal
# #bonferroni: 6 tests: p > 0.0083
# #no treatments are significantly different with that cutoff
# t.test(noncomm.con$tot.saps.ha,noncomm.leave$tot.saps.ha) #t = -6.8893, df = 5.932, p-value = 0.0004849*****
# t.test(noncomm.con$tot.saps.ha,noncomm.leg$tot.saps.ha) #t = -3.3283, df = 3.956, p-value = 0.02964
# t.test(noncomm.con$tot.saps.ha,noncomm.gap$tot.saps.ha) #t = -2.797, df = 3.443, p-value = 0.05809
# t.test(noncomm.leave$tot.saps.ha,noncomm.leg$tot.saps.ha) #t = 0.5298, df = 4.17, p-value = 0.6232
# t.test(noncomm.leave$tot.saps.ha,noncomm.gap$tot.saps.ha) #t = -0.0857, df = 3.548, p-value = 0.9363
# t.test(noncomm.leg$tot.saps.ha,noncomm.gap$tot.saps.ha) #t = -0.3989, df = 5.257, p-value = 0.7057
# 
# 
# 
# 
# ############     species diversity
# 
# #the following is copied from another code
# #shannon's diversity index
# 
# # H = ∑(over S, i=1) - (Pi * ln Pi)
# #make a matrix: /microsite/ | N.indvs | Prop.indvs | ln(Prop.indvs) | -(Pi * lnPi) ; H=sum of -Pi*lnPi
# #run this on microsite2 data: collapsed categories, without rock
# 
# sap.div <- as.data.frame(matrix(ncol=2, nrow=16, data = NA)) #16 plots
# sap.div[,1]<-1:16
# colnames(sap.div)<-c("plot","H")
# head(sap.div)
# species.vec = c( "ACPE", "ACRU", "ACSA", "BEAL" ,"BELE" ,"BEPA" ,"CADE" ,"FAGR", "OSVI", "PIST" ,"POGR", "POTR", "PRSE" ,"QURU", "TIAM", "TSCA")
# 
# head(sap)
# for(i in 1:16){
#   temp.mat = matrix(ncol = 4, nrow = 16, data=0) #to get variables to calculate H for each plot # number of rows = number of species # ACPE ACRU ACSA BEAL BELE BEPA CADE FAGR OSVI PIST POGR POTR PRSE QURU TIAM TSCA = 16
#   for (j in 1:16){
#     tempdat = sap[sap$Plot == i & sap$Species == species.vec[j],] #use only data from the single plot and species
#     tempplot = sap[sap$Plot == i,]
#     temp.mat[j,1]=tempdat$density.ha[1] #n individuals of species j
#     temp.mat[j,2]=temp.mat[j,1]/sum(tempplot$density.ha) #proportion of species j, compared to ALL species in plot
#     temp.mat[j,3]=log(temp.mat[j,2]) #ln proportion of individuals
#     temp.mat[j,4]=-(temp.mat[j,2]*temp.mat[j,3]) #-(Pi * lnPi)
#   }
#   pilnpi.temp <- temp.mat[,4]
#   sap.div[i,2]=sum(pilnpi.temp, na.rm = T)
# }
# 
# sap.div #looks like it works!
# range(sap.div[,2]) # 0.03201816 1.71994806
# sap.div$treatment = treat.vec
# 
# #aggregate
# sap.sp.agg <- NULL
# sap.sp.agg <- ddply(sap.div, .(treatment), .fun = summarize, 
#                     diversity.mean = mean(H),
#                     diversity.se = sd(H)/sqrt(4)) #aggregate the data by plot and species)
# sap.sp.agg
# 
# #test
# hist(sap.div$H, breaks = 7) #pretty normal
# m5 <- aov(H ~ treatment, data = sap.div)
# summary(m5) #treatment    3  1.083  0.3609   3.121 0.0662 .#not significant
# 
# #######################
# 
# #######################
# #4 gap winners
# plot<-read.csv("LS_Worthington_Data_2018_plot.csv")
# head(plot)
# plot<-plot[,c(1,4,7,8)]
# plot.gap = plot[plot$Treatment=="gap",]
# plot.leg = plot[plot$Treatment=="legacy",]
# plot.leave = plot[plot$Treatment=="leave",]
# plotplot = as.data.frame(matrix(nrow = 3,ncol = 3,data = NA))
# colnames(plotplot) = c("treatment","height","SE")
# plotplot[,1] = c("gap","legacy","leave")
# 
# plotplot[1,2] = mean(plot.gap$Height.m) #11.0 +1.6 (SE)
# plotplot[1,3] = sd(plot.gap$Height.m)/sqrt(4)
# plotplot[2,2] = mean(plot.leg$Height.m) #8.5 +0.33 (SE)
# plotplot[2,3] = sd(plot.leg$Height.m)/sqrt(4)
# plotplot[3,2] = mean(plot.leave$Height.m) #11.0 + 0.95 (SE)
# plotplot[3,3] = sd(plot.leave$Height.m)/sqrt(4)
# 
# hist(plot$Height.m) #pretty normal
# h1 <- aov(plot$Height.m ~ plot$Treatment)
# summary(h1) #    Df Sum Sq Mean Sq F value Pr(>F)
# #plot$Treatment  2  17.00   8.501   1.726  0.232
# mean(plot$Height.m, na.rm = T)
# sd(plot$Height.m, na.rm = T)/sqrt(12)
# 
# #library(ggplot2)
# ggplot(data = plotplot, aes(y=height, x = treatment))+
#   geom_bar(stat = "identity", color = "black", position = position_dodge())+
#   geom_errorbar(aes(ymin=height, ymax=height+SE),width=.2,position=position_dodge(0.9))+
#   theme_bw() +
#   labs(y="Height (m)", x="Treatment", title = "Gap winner height") +
#   theme(title = element_text(size=16),
#         axis.title.x = element_text(size=16),
#         axis.text.x = element_text(size=14),
#         axis.title.y = element_text(size=16),
#         axis.text.y = element_text(size=14))
# 
# #make a graph
# plotmat = matrix(nrow = 3,ncol = 2,data = NA)
# rownames(plotmat) = c("gap","legacy","leave")
# colnames(plotmat) = c("height","SE")
# plotmat[,1]=plotplot[,2]
# plotmat[,2]=plotplot[,3]
# 
# # plotting settings -------------------------------------------------------
# ylim <- c(0,13)
# 
# # plot --------------------------------------------------------------------
# library(Hmisc)
# barplot(plotmat[,1], ylim=ylim, ylab="Gap winner height (m)",xlab="Treatment")
# errbar(x=c(.7,1.9,3.1),y=plotmat[,1],yplus=(plotmat[,1]+plotmat[,2]),yminus=(plotmat[,1]-plotmat[,2]),add=TRUE)
# x=c("gap","legacy","leave")
# 
# #species!
# plot.gap$Species #POGR ACRU BEAL ACPE
# plot.leg$Species #BELE BEAL BELE BELE
# plot.leave$Species #PRSE BELE BELE PRSE
# 
# # plot pretty
# win.sp.mat = matrix(nrow = 6,ncol = 3,data = NA)
# colnames(win.sp.mat) =c("gap","legacy","leave")
# rownames(win.sp.mat) =c("ACPE","ACRU","BEAL","BELE","POGR","PRSE")
# win.sp.mat[,1]=c(1,1,1,0,1,0)
# win.sp.mat[,2]=c(0,0,1,3,0,0)
# win.sp.mat[,3]=c(0,0,0,2,0,2)
# #acpe = 1
# #acru = 2
# #beal = 3
# #bele = 4
# #pogr = 5
# #prse = 6
# 
# # plotting settings -------------------------------------------------------
# ylim <- c(0,4.97)
# angle1 <- c(45,45,135,0,45,45) #
# angle2 <- c(0,0,0,0,135,0)
# density1 <- c(90,90,20,0,20,10)
# density2 <- c(0,0,0,0,20,0)
# col <- gray(level=c(0,.5,0,1,.75,.5)) #zero is black, one is white
# 
# # plot --------------------------------------------------------------------
# barplot(win.sp.mat, beside=FALSE, ylim=ylim, col=col, angle=angle1, density=density1)
# barplot(win.sp.mat, add=TRUE, beside=FALSE, ylim=ylim, col=col, angle=angle2, density=density2)
# legend("top", legend=c(expression(italic("Acer pennsylvanica"),italic("Acer rubrum"),italic("Betula alleghaniensis"),italic("Betula lenta"), italic("Populus grandifolia"), italic("Prunus serotina"))), ncol=2, fill=c("white","gray","white","white","white")) 
# par(bg="transparent")
# legend("top", legend=c(expression(italic("Acer pennsylvanica"),italic("Acer rubrum"),italic("Betula alleghaniensis"),italic("Betula lenta"), italic("Populus grandifolia"), italic("Prunus serotina"))), ncol=2, fill=TRUE, col=col, angle=angle1, density=c(90,0,20,0,20,10))
# par(bg="transparent")
# legend("top", legend=c(expression(italic("Acer pennsylvanica"),italic("Acer rubrum"),italic("Betula alleghaniensis"),italic("Betula lenta"), italic("Populus grandifolia"), italic("Prunus serotina"))), ncol=2, fill=TRUE, col=col, angle=angle2, density=density2)
