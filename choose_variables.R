# Ipomoea paper
# Variable selection for modeling



# Read data with climatic and soils information




# ----------------------------------------------------------------------------------- #
# Nonlinear iterative partial least square algorithm
# ----------------------------------------------------------------------------------- #

# By specie


nipals_by_specie =function(x){
  library(raster)
  library(usdm)
  library(plsdepot)
  
  
y<-data.frame(x)
y<- y[,paste0("bio",1:19)]
  
cat("processing NIPALS ","\n")
  if(nrow(y)>4){
    cat("processing NIPALS (MORE THAN 4 OCCURENCES","\n")
    
    z = nipals(Data=y, comps=5, scaled=T)
    vars1 = z$cor.xt[,1] > 0.7 | z$cor.xt[,1] < -0.7
    vars2 = z$cor.xt[,2] > 0.7 | z$cor.xt[,2] < -0.7
    vars = c(vars1, vars2)
    vars = names(vars[which(vars==TRUE)])
  }else{
    cat("ommiting NIPALS, USING ALL VALUES","\n")
    
    vars = colnames(y)
  }

nipals_by_specie<-y[,vars]


if(nrow(nipals_by_specie)<10){
  vif_res<-colnames(y)
#  return(vif_res)
}else{
  #print(z)
  vif_res = vifstep(nipals_by_specie, th=10)
  vif_res = sort(as.character(vif_res@results$Variables))
  #return(vif_res)
}

nipals_by_specie<-vif_res

return(nipals_by_specie)

}
#x<-spData_presence
#x<-nipals_by_specie(spData_presence)
