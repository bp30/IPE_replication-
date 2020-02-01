#Generate 4 plots to look at the random slope effect of linear mixed model when participant as the random effect 
#plot display option 1= random effect plot ref_group vs dummy1, 2=random slope plot ref_group vs dummy1, 3= individual random slope plot ref_group vs dummy1, 
#4= individual random slope plot ref_group vs dummy2
Plot_LMM<- function (LMM_model, ref_group,dummy1, dummy2, plot_display, DV){
  #extracts varaince parameters from LMM including intercept and random slope values for each participant
  u0_u1 <-coef(LMM_model)[[1]]
  #Obtain the intercept values for participants 
  intercept<- u0_u1[1][,1,]
  #add random slope parameter to each intercept value for each participant, control should be the reference group
  random_slope<- data.frame ('Participant'=rownames(u0_u1) , ref_group= intercept, dummy1= intercept+u0_u1[[2]], dummy2= intercept+u0_u1[[3]])
  names(random_slope)[names(random_slope) == "ref_group"] <- ref_group
  names(random_slope)[names(random_slope) == "dummy1"] <- dummy1
  names(random_slope)[names(random_slope) == "dummy2"] <- dummy2
  #Create individual slope plots
  random_slope2 <- melt(random_slope, id= 'Participant')
  colnames(random_slope2) <- c('Participants','Condition', 'Mean')
  plot_0v1.df<- random_slope2[random_slope2$Condition!=dummy2,]
  plot_0v2.df <-random_slope2[random_slope2$Condition!=dummy1,]
  #Generate random effect and individual random slope plots for each condition of interest relative to the reference group
  if (plot_display==1){
    plot(random_slope$Participant,u0_u1[[2]],xlab='Participants', ylab=paste(dummy1,'condition random slopes'));abline(0,0, col='red');abline(LMM_model@beta[2],0, col='blue')
  } else if (plot_display==2){
    plot(random_slope$Participant,u0_u1[[3]],xlab='Participants', ylab=paste(dummy2,'condition random slopes'));abline(0,0, col='red');abline(LMM_model@beta[3],0, col='blue')
  } else if (plot_display==3){
    xyplot (Mean~Condition, data = plot_0v1.df,ylab=DV, groups= Participants, type = c("p","l"),auto.key=list(space="left", columns=4,title="Participants", cex.title=1),ylim=c(1,7))
  } else if (plot_display==4){
    xyplot (Mean~Condition, data = plot_0v2.df,ylab=DV, groups= Participants, type = c("p","l"),auto.key=list(space="left", columns=4,title="Participants", cex.title=1), ylim=c(1,7))
  }
}
