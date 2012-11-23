parseTS<-function(
  ### Function creates time series object from PostgreSQL ts object
  string,##<< string representation returned by PostgreSQL
  t_init=-540##<< initial measurement time
){
  duration<- -1.0;
  if((regexpr('/[0-9\\.]+$',string)->n)>-1){# there are duration value
    duration<-as.double(substr(string,n+1,n+attr(n,'match.length')-1))
    string<-substr(string,0,n-1)
  }
  val<-as.double(unlist(strsplit(string,';')));
  if(duration<0) duration<-(length(val)-1);
  t<-ts(val,start=t_init,end=duration+t_init,frequency=(length(val)-1)/duration)
  ### time series of type ts
}

parseSAX<-function(
  ### Function creates time series object from PostgreSQL ts object
  sstring,##<< string representation of SAX returned by PostgreSQL
  t_init=-540##<< initial measurement time
){
  tmps<-unlist(strsplit(sstring,split='/'))
  val<-c();
  ppl<- as.double(tmps[2]);
  sym<-strsplit(unlist(strsplit(tmps[1],split='; *')),split='\\|')
  val<-sapply(sym,.parseSAX.symbol,ppl)
  return(as.vector(val))
  ### time series of type ts
}

.parseSAX.symbol<-function(symStr,ppl){
  sym<-unlist(strsplit(symStr,split="\\|"))
  i<-as.numeric(paste('0x',toupper(sym[1]),sep=''))
  num<-2^as.numeric(sym[2])
  bp <- qnorm(1:(num-1)/num);
  return(rep(bp[i],ppl))
}

wrapSignal<-function(
  ### Funcion converts numerical vector into database TS string representation. Signas assume to be regulaly spaced.
  sig ##<< signal to convert
){
  dur<-length(sig-1)
  ts.string<-paste(paste(sig,collapse=';'),dur,sep='/')
  return(ts.string)
}
