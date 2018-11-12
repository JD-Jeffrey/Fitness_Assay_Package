get_some_stderror<- function(FC_data){

## Jasper's tasks: 
  
  #1) include a boolean somewhere to tell the script whether or not to plot the data. 
  #2) go down to the plot section and make the plots pretty! 
  
  
  destroy_baddies<- as.logical(readline(prompt="Eliminate poor samples? (TRUE/FALSE): "))

  FC_data$"Std.Error" <- NA #Creates a new column
  c <- ncol(FC_data)

  time_points<- (c(0:((ncol(FC_data)-4)/2)))*10 #finds the number of time points in dataset 
  

  
  #Goes through dataframe and calculates Std. Error for each sample
  list_for_plots<- list()
  
  for (i in 1:nrow(FC_data)) {
    count<- FC_data[i, seq(2,ncol(FC_data)-1,2)] #-1 so we don't count Std. Error col
    refcount<- FC_data[i, seq(3,ncol(FC_data)-1,2)]
    diff<- count-refcount
    natlog<- log(diff/refcount)
    natlog<-as.numeric(natlog[1,])
    list_for_plots<- append(list_for_plots, natlog) # storing the natlog data for later
    regress<- lm(natlog ~ time_points)
    slope<- as.numeric(coef(regress)[2])
    stderror<- as.numeric(coef(summary(regress))[2,2])
    FC_data[i,c] <- stderror
    
    }

  ## this section plots raw data and stores it to a pdf file in the directory the script is run in
  
  if (TRUE){ #fix with boolean for user would like to plot
      samples<- seq(from=1, to=length(list_for_plots), by = length(time_points)) #a list to interate by
      pdf("ln(exp/ref)_plots.pdf", height = (ncol(FC_data)*5.3), width = 16)
      par(mfrow=c(nrow(FC_data)/4, 4)) #control the margins of the plots
      par(mar=c(2,3,3,2))
    for (i in 1:length(samples)) {
      start<- samples[i]
      end<- samples[i]+(length(time_points)-1)
      plot(time_points, list_for_plots[start:end]) #Jasper - make plots pretty here!! 
    }
     dev.off()
  }
  
  
  # this section asks a user for a standard error cuttoff value 
  # and removes samples that don't meet the cuttoff
  new.frame<- FC_data #copies FC dataframe to new frame 
  for (i in 1:ncol(new.frame)){
    new.frame[,i]=NA
  }
  new.frame<-na.omit(new.frame)
  if (destroy_baddies){
    cutoff<-as.numeric(readline(prompt="Insert cutoff threshold: "))
    for (i in 1:nrow(FC_data)){
      if(FC_data[i,c] < cutoff){
        new.frame <- rbind(new.frame, FC_data[i, ])
        }
      }
    }


  return(FC_data)
}

