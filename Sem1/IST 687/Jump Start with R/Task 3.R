len.inches <- readline(prompt="Enter inches: ")
len.inches <- as.integer(len.inches)
len.cm <- len.inches * 2.54
sprintf("Length in cm is %s",len.cm)
newLen.inches <- len.cm / 2.54
if(len.inches == newLen.inches){
  print("yes")
  } else 
  print("no")
