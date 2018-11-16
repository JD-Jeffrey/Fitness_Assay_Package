get_some_stderror<- function(FC_data){

## Jasper's tasks: 
  
  #1) include a boolean somewhere to tell the script whether or not to plot the data. 
  #2) go down to the plot section and make the plots pretty! 
  
  
  remove_samp<- as.logical(readline(prompt="Eliminate poor samples? (TRUE/FALSE): "))

  FC_data$"Std.Error" <- NA #Creates a new column
  c <- ncol(FC_data)
  make_plots <- FALSE

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
  
  
  # this section asks a user for a standard error cuttoff value 
  # and removes samples that don't meet the cuttoff
  good_samples.frame<- FC_data #copies FC dataframe to new frame 
  for (i in 1:ncol(good_samples.frame)){
    good_samples.frame[,i]=NA
  }
  good_samples.frame<-na.omit(good_samples.frame)
  if (remove_samp){
    cutoff<-as.numeric(readline(prompt="Insert cutoff threshold: "))
    make_plots<- as.logical(readline(prompt="Plot poor samples? (TRUE/FALSE): "))
    
    for (i in 1:nrow(FC_data)){
      if(FC_data[i,c] < cutoff){
        good_samples.frame <- rbind(good_samples.frame, FC_data[i, ])
        }
    }
    FC_data<- good_samples.frame
  }
  
  ## this section plots raw data and stores it to a pdf file in the directory the script is run in
  if (make_plots){
    samples<- seq(from=1, to=length(list_for_plots), by = length(time_points)) #a list to interate by
    pdf("raw_data_plots.pdf", height = (ncol(FC_data)*5.3), width = 16)
    par(mfrow=c(nrow(FC_data)/4, 4)) #control the margins of the plots
    par(mar=c(2,3,3,2))
    for (i in 1:length(samples)) {
      start<- samples[i]
      end<- samples[i]+(length(time_points)-1)
      plot(time_points, list_for_plots[start:end], pch= 16, cex= 2, type = "o", 
           col= sample(1:600,1,replace = T), ylim = c(-1,1))#generates plots
      legend("topleft", c("Natlog"), bg="azure2", fill = "coral1")
      title(main = FC_data[i,1], xlab = "Generation", ylab = "NatLog(diff/refcount)")
    }
    dev.off()
  }
  return(FC_data)
}

