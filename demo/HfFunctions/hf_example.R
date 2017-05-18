

hf_example = function(bl, history, t){
  r_sr<-0.0000000123 - bl["Age"]/1000000*(2-(bl["Sex"]))
  res<-r_sr+0*t
  p<--0.3*(sum(history)+t)+0.3
  res[sum(history)+t<1]<- max(-log(1-p)/(sum(history)+t), r_sr)
  return(res)
}



