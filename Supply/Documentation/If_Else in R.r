#if else if else Syntax
if (condition1){
	Output1
} else if (condition2) {
	Output2
} else {
	Output3
}

#Example
  if (colnames(DAT_Shell[i]) %>% str_detect("PREC")){
    DAT_Shell[i] = ""
  } else if (colnames(DAT_Shell[i]) %>% str_detect("TM")){
    DAT_Shell[i] = ""
  }else {
    DAT_Shell[i] = DAT_Shell[i]
  }
 
 #For Loop Syntax
 for (variable in 1:last_iteration) {
		if (condition1){
		Output1
	} else if (condition2) {
		Output2
	} else {
		Output3
	}
}

#Example
for (i in 1:length(DAT_Shell)){
  if (colnames(DAT_Shell[i]) %>% str_detect("PREC")){
    DAT_Shell[i] = ""
  } else if (colnames(DAT_Shell[i]) %>% str_detect("TM")){
    DAT_Shell[i] = ""
  }else {
    DAT_Shell[i] = DAT_Shell[i]
  }
}
