.getQuery<-function(
  ### main function creates and return SQL queries by their name
  name##<< name of the query to be returned
  ){
  ###TODO: to change to the environment
  queryList<-list( 
    ten.closest.18a="select e1.id,p2.nm,w1.sax05t,sax_tstring(w1.sax05t) c1,
       sax_tstring(w2.sax05t) c2,
       sax_distance(w1.sax05t,w2.sax05t) d , e1.pot
       from elpot e1,elpot e2,promoter p, tot_win w1,tot_win w2,promoter p2
       where   p.nm like '$$$$1' and p.potid=e2.id
       and sax_nar_50p(w1.sax05t,w2.sax05t) and e1.id=w1.potid and e2.id=w2.potid
       and p2.potid=e1.id
       order by 6 limit 10;", 
    ten.closest.144a="select e1.id,e1.sax144,sax_tstring(e1.sax144) c1,
        sax_tstring(e2.sax144) c2,
        sax_distance(e1.sax144,e2.sax144) d  
        from elpot e1,elpot e2,promoter p
        where   p.nm like '$$$$1' and p.potid=e2.id
        and sax_nar_1p(e1.sax144,e2.sax144) 
        and e1.id <> e2.id
        order by 5 limit 30;"
    )
  return(queryList[name])
  ### list of strings, representing parts of the query to be concatenated with 
}