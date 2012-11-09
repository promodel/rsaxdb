# Set of functions to process DB output from TS, SAX and float4[] datatypes
# 
# Author: asorokin
###############################################################################

connect.rdm<-function(
### Open connection to RobustDM database
  dbname = 'robustdm',##<< name of the database! Should be changed before distribution!!!!
  port='10000',##<< port at which database is listening! Should be changed before distribution, standard PostgreSQL port is 56!!!!
  host =if(!is.null(Sys.getenv("PGHOST"))) Sys.getenv("PGHOST") else "localhost",##<< database server name, URL or IP
  ...##<< other authorization arguments needed by the DBMS instance; these typically include user, password, dbname, host, port, etc. For details see the PostgreSQL DBIDriver.
  ){
  if(!require(RPostgreSQL)){
    stop("Method requires library 'RPostgreSQL'. Please install and relaunch.\n")
  }
	drv <- dbDriver("PostgreSQL")
	con <- dbConnect(drv, dbname = dbname,port=port,host=host)
	return(con)
  ### connection object see 
}

clean.con.rdm<-function(
  ### Clear resources and close DB connection
  con ##<< a connection object as produced by dbConnect.
  ){
	dbDisconnect(con)
	
}

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

#' 
#' 
#' @param string 
#' @returnType 
#' @return 
#' @author asorokin
#' @export
parseArray<-function(
  ### Converts string of numbers similar to C static arrays to vector of doubles
  string##<< string of numbers similar to C static array definition
  ){
	if((regexpr('\\}+$',string)->n)>-1){# there are duration value
		string<-substr(string,2,n-1)
	}
	val<-as.double(unlist(strsplit(string,',')));
  ### vector of doubles
}

shiftTSwindow<-function(
  ### Shifts time series to new window position
  ts,##<< time series to shift
  offset=0,##<< new start position
  length=-1##<< length of new window
  ){
  ##note<< if length of new window is negative, then new window will last
  ### from the offset value till the end of the ts. 
		if(length<=0){
			tsz<-window(ts,start=offset);
		}else{
			tsz<-window(ts,start=offset,end=offset+length);
		}
		tsp(tsz)->tszp;
		tszp[1:2]<-tszp[1:2]-tszp[1];
		tsp(tsz)<-tszp;
	return(tsz)
    ### shifted time series
}

plotTS<-function(
  ###Function to plot time-serieses from DB
  ts,##<< time serieses in database string form as character vector
  genes=1:length(ts),##<< number of individual lines
  light=c(),##<< decoration of graph 
  title='Time series',##<< title of the plot
  norm=FALSE,##<< logical to use normalisation or not
  shift=rep(0,length(ts)),##<< offsets to be added to the time serieces
  first.is.query=FALSE##<< logical should first line be decorated with bolder line as original query
  ){
	lts<-lapply(ts,parseTS);
	if(norm){
		lts<-lapply(lts,function(.x) (.x-mean(.x))/sd(.x));
	}
	if(any(shift!=0)){
		lts<-mapply(shiftTSwindow,lts,shift)
	}
	low<-min(unlist(lts));
	hi<-max(unlist(lts));
	ltsp<-sapply(lts,tsp);
	lf<-min(ltsp[1,]);
	rt<-max(ltsp[2,]);
	fI=2;
	plot(lts[[2]],main=title,xlab='Time',ylab='activity',xlim=c(lf,rt),ylim=c(low,hi),col=1,type='n');
	if(length(light)>0){
		region<-par('usr');
		if(length(light)%%2 > 0){
			light<-c(light,region[2]);
		}
		for(i in seq(from=1,to=length(light),by=2)){
			rect(light[i],region[3],light[i+1],region[4],col='grey',border="transparent");
		}
	}
	for(i in fI:length(lts)){
		lines(lts[[i]],col=i);
	}
	if(length(genes)>0){
		legend('topleft',legend=genes,col=1:length(lts),lwd=1);
	}
	if(first.is.query){
		lines(lts[[1]],col=1,type='l',lwd=3);
		fI=2;
	}else{
		lines(lts[[1]],col=1,type='l');
	}
}

getexp<-function(
  ### Helper function for mapply on regexp
  str,##<< string to analyse
  greg##<< result of grexpr invocation
  ){
	substring(str,greg,greg+attr(greg,'match.length')-1)
  ### list of matches found by regular expression in the string
}

tmp_tab_name<-function(
  ### Generates unique name for temporary table to load data to
  ){
	s<-format(Sys.time(),"%Y%m%dT%H%M%OS4");
	ttn<-paste('temp_ts',paste(unlist(strsplit(s,'\\.')),collapse='',sep=''),sep='');
	return(ttn);
  ### name of the table
}

