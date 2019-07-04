#Returns returns scatter plot with log regression results
#Maintainer: Mauricio Arango
#plot_lreg(x,y,titl,xlab,ylab,scx,scy,w,names,sepa,ini,font)
#@x: variable on x axis
#@y: variable on y axis
#@titl: tittle
#@xlab: lable x axis
#@ylab: lable y axisa
#@scx: scale x variable. E.g: 10s,100s, 1000s,...
#@scy: scale y variable. E.g: 10s,100s, 1000s,...
#@w: regression weights
#@names: name of observations. E.g. City, State, Country
#@sepa: higher numbers reduce the frequency at which observation names apper
#@ini: number beyond which the frequency of names is reduced
#@font: plot font
#example 
#poti<-plot_lreg(x=trade$Distance_from_Largest_City_to_NYC_miles,y=trade$Total_Trade_Amount_2015_Thousand_USD,titl='trade',xlab = 'trade',ylab='distance',w=trade$GDP_2015_USD,names=trade$Name_country)
#poti

plot_lreg<-function(x,y,titl,xlab,ylab,scx,scy,w,names,sepa,ini,font){
  if(!require(ggplot2, quietly = TRUE)){install.packages('ggplot',dep = TRUE,quietly = TRUE)}
  if(missing(ini)){ini<-1}
  if(missing(sepa)){sepa<-1}
  if(missing(scx)){scx<-1}
  if(missing(scy)){scy<-1}
  if(missing(names)){names<-rep(' ',length(x))}
  if(missing(font)){font<-"serif"}
  names<-as.character(names)
  seq<-summary(lm(log(y)~log(x),weights=w))
  eql<-paste0(ylab,'=',round(seq$coefficients[1],1),'+',round(seq$coefficients[2],1),'*',xlab,' R2=',round(seq$r.squared,2))
  xma<-max(x,na.rm=TRUE)
  yma<-max(y,na.rm=TRUE)
  xf<-x/xma
  yf<-y/yma
  tpos<-xf+yf
  df_names<-data.frame(cbind(names,tpos),stringsAsFactors=FALSE)
  rtpos<--sort(-tpos)
  diftpos<-NULL
  for(i in 1:(length(rtpos)-1)){diftpos<-c(diftpos,(rtpos[i]-rtpos[i+1]))}
  diftpos_m<-mean(diftpos)*sepa
  df_names$short_name<-' '
  for (i in 1:ini){df_names$short_name[df_names$tpos==as.character(rtpos[i])]<-df_names$names[df_names$tpos==as.character(rtpos[i])&!is.na(df_names$tpos)]}
  ini_tpos<-rtpos[ini]
  while(ini<=(length(rtpos)-1)){
    if((ini_tpos-rtpos[ini+1])>=diftpos_m){
      df_names$short_name[df_names$tpos==as.character(rtpos[ini+1])]<-df_names$names[df_names$tpos==as.character(rtpos[ini+1])&!is.na(df_names$tpos)]
      ini_tpos<-rtpos[ini+1]}
    ini=ini+1}
  names_sh<-df_names$short_name
  maxx<-log((ceiling(xma/(scx*10))*10))
  minx<-log((floor(min(x,na.rm=TRUE)/(scx*10))*10))
  maxy<-log((ceiling(yma/(scy*10))*10))
  miny<-log((floor(min(y,na.rm=TRUE)/(scy*10))*10))
  stepx<-seq(from =minx, to = maxx, by =((maxx-minx)/5))
  stepy<-seq(from =miny, to = maxy, by =((maxy-miny)/5))
  stepx<-exp(stepx)
  stepy<-exp(stepy)
  for (i in 1:length(stepx)){stepx[i]<-round(stepx[i],digits = 0)
  stepy[i]<-round(stepy[i],digits = 0)}
  dataf<-data.frame(cbind(x,y,w))
  plot <- ggplot(dataf, aes(x=x/scx,y=y/scy))+geom_point(aes(size = w),show.legend = FALSE,alpha = 1/10)+
    ggtitle(titl)+
    xlab(xlab)+ 
    ylab(ylab)+
    scale_y_continuous(trans = 'log2',breaks=stepy)+ 
    scale_x_continuous(trans = 'log2',breaks=stepx)+
    geom_smooth(method='lm', se = FALSE,aes(colour=eql))+
    theme_classic(base_family = font)+
    theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5))+
    scale_colour_manual(name='reg',values="black")+
    geom_text(aes(label=names_sh),size = 3,family = font,hjust=minx*0.1, vjust=-miny*0.5)}

#Returns returns scatter plot with log regression results
#Maintainer: Mauricio Arango
#plot_lreg(x,y,titl,xlab,ylab,scx,scy,w,names,sepa,ini,font)
#@data: data set
#@var: variable name string
#@name: lable observation
#@namevar: name string to equation
#@sepa: higher numbers reduce the frequency at which observation names apper
#example 
#zipfslaw(data=trade,var='GDP_2015_USD',name='Name_country',namevar = 'GDP',sepa=3)
zipfslaw<-function(data,var,name, namevar,sepa,color,width,pointsize,height,adress){
  if(missing(sepa)){sepa=1}
  if(missing(color)){color<-'gray'}
  if(missing(width)){width=1200}
  if(missing(pointsize)){pointsize=25}
  if(missing(height)){height=1200}
  if(missing(adress)){adress=getwd()}
  namv<-c(1:9,seq(from = 10, to = nrow(data), by =10))
  data$name_full<-data[,names(data)==name]
  data$var<-data[,names(data)==var]
  rtpos<--sort(-data$var)
  diftpos<-NULL
  for(i in 1:(length(rtpos)-1)){diftpos<-c(diftpos,(rtpos[i]-rtpos[i+1]))}
  diftpos_m<-abs(mean(diftpos)*sepa)
  data$name_short<-' '
  data$rank<-rank(-data$var)
  data$name_short[1]<-as.character(data$name_full[1])
  for (i in 2:nrow(data)){
    if(abs(data$var[i]-data$var[i-1])>=diftpos_m){data$name_short[i]<-as.character(data$name_full[i])}}
  abl<-lm(log(data$rank)~log(data$var))
  abls<-summary(abl)
  png(filename=paste0(getwd(),"/",name,".png"),
      width=width,
      pointsize=pointsize,
      height=height)
  plot(log(data$var[data$rank>0]),log(data$rank[data$rank>0]),pch=16,main=paste0("Zipf Regression: ",namevar),xlab=paste0("Log(",namevar,")"),ylab=paste0("Log(Rank ",namevar,")"),xlim=c(min(log(data$var[data$rank>0]),na.rm=TRUE)-1,max(log(data$var[data$rank>0]),na.rm=TRUE)+1),family="serif")
  text(log(data$var),log(data$rank),data$name_short,pos=4,col=color,family="serif")
  abline(abl)
  text(min(log(data$var[data$rank>0]),na.rm=TRUE)*1,1,paste0('Log(Rank ',namevar,')= ',round(summary(abl)$coefficients[1],2),round(summary(abl)$coefficients[2],2),"(Log(",namevar,"))","  R2=",round(abls$r.squared,2)),family="serif")
  dev.off()}
