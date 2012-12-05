# Set of functions to process DB output from TS, SAX and float4[] datatypes
# 
# Author: asorokin
###############################################################################

connect.rdm<-function(
### Open connection to RobustDM database
  dbname = 'eldb',##<< name of the database! Should be changed before distribution!!!!
  port='5432',##<< port at which database is listening! Should be changed before distribution, standard PostgreSQL port is 56!!!!
  host =if(!is.null(Sys.getenv("PGHOST"))) Sys.getenv("PGHOST") else "localhost",##<< database server name, URL or IP
  ...##<< other authorization arguments needed by the DBMS instance; these typically include user, password, dbname, host, port, etc. For details see the PostgreSQL DBIDriver.
  ){
  if(!require(RPostgreSQL)){
    stop("Method requires library 'RPostgreSQL'. Please install and relaunch.\n")
  }
	drv <- dbDriver("PostgreSQL")
	con <- dbConnect(drv, dbname = dbname,port=port,host=host,...)
	return(con)
  ### connection object see 
}

clean.con.rdm<-function(
  ### Clear resources and close DB connection
  con ##<< a connection object as produced by dbConnect.
  ){
	dbDisconnect(con)
	
}

findElSeq<-function(
### main search function: it calculates the potential profile, wrap it into Decima TS string and submit query to the database 
  seq##<< sequence to be processed
  ,ref=271##<< reference point to align with promoter TSS
  ){
  if(!require(reldna)){
    stop('Required library "reldna" is missing')
  }
  pot<-sseqspline1D(seq,ref)
  sub('\\$\\$\\$\\$1',wrapSignal(pot),.getQuery()$closest10.05t)->q10.05t
  con<-connect.rdm()
  ec<-dbGetQuery(con,q10.05t)
  if(dim(ec)[1]<=0){
    sub('\\$\\$\\$\\$1',wrapSignal(pot),.getQuery()$closest10.1t)->q10.1t
    ec<-dbGetQuery(con,q10.1t)
  }
  clean.con.rdm(con)
  res<-data.frame(id=ec$id,nm=ec$nm,regulonid=ec$regulonid,pot=ec$pot,sax_distance=ec$sax_distance,p_distance=unlist(lapply(ec$pot,.potDist,pot)),stringsAsFactors=FALSE)
  attr(res,'query.seq')<-seq
  attr(res,'query.pot')<-pot
 return(res)
}

.potDist<-function(query,pot){
  return(sqrt(mean((scale(pot)-scale(parseTS(query)))^2)))
}

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

plotRes<-function(
  ###Function to plot results of DB quering
  res##<< result of 'findElSeq' call
){
  plotTS(c(wrapSignal(attr(res,'query.pot')),res$pot),
         norm=TRUE,first.is.query=TRUE,
         genes=c('query',res$nm),title='Electrostatic Potential profile'
         ) 
}

plotTS<-function(
  ###Function to plot time-serieses from DB
  ts,##<< time serieses in database string form as character vector
  genes=1:length(ts),##<< number of individual lines
  light=c(),##<< decoration of graph 
  title='Electrostatic potential',##<< title of the plot
  norm=FALSE,##<< logical to use normalisation or not
  shift=rep(0,length(ts)),##<< offsets to be added to the time serieces
  first.is.query=FALSE##<< logical should first line be decorated with bolder line as original query
  ){
  old<-par(mar=c(5.1, 4.1, 4.1, 8.1),xpd=TRUE)
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
  col<-rainbow(length(ts)*2)[as.vector(rbind(1:length(ts)*2,2*length(ts):1))[1:length(ts)]]
	fI=2;
	plot(lts[[2]],main=title,xlab='Z, A',ylab='phi',xlim=c(lf,rt),ylim=c(low,hi),col=col[1],type='n');
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
		lines(lts[[i]],col=col[i-fI+1]);
	}
	if(first.is.query){
	  lines(lts[[1]],col=1,type='l',lwd=3);
	  fI=2;
	}else{
	  lines(lts[[1]],col=1,type='l');
	}
	if(length(genes)>0){
    if(first.is.query){
      legend("topright", inset=c(-0.23,0),legend=genes,col=c(1,col[2:length(lts)-1]),lwd=c(3,rep(1,length(lts)-1)),bty='n');
#      legend('topleft',legend=genes,col=c(1,col[2:length(lts)-1],lwd=c(3,rep(1,length(lts)-1)),);
    }else{
		  legend('topleft',legend=genes,col=c(1,col[2:length(lts)-1]),lwd=1);
    }
	}
  par(old)
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

