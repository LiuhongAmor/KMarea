#'KMA
#'
#'画出KM曲线下区域
#'@param fit 用survfit函数建立的模型
#'@param start 表示开始时间点
#'@param stop 表示结束时间点
#'@param col 表示颜色
#'@example KMA(fit,0,100,col=1)
#'
KMA<-function(fit.A,fit.B,start,stop,col,between=FALSE){
  t1<-start;t2<-stop
  time.A<-fit.A$time;surv.A<-fit.A$surv
  num.A<-c(1:length(time.A))
  ALL.A<-data.frame(num.A,time.A,surv.A)
  all.A<-ALL.A[ALL.A[2]>=t1 & ALL.A[2]<=t2 ,]
  time.A<-c(t1,all.A$time.A,t2);time.A<-rep(time.A,each=2)
  num.A1<-all.A$num.A[1]-1
  if(num.A1!=0){
    surv1.A<-ALL.A$surv.A[all.A$num.A[1]-1]
  } else  {
    surv1.A<-1}
  surv.A<-c(surv1.A,all.A$surv.A,all.A$surv.A[nrow(all.A)])
  surv.A<-rep(surv.A,each=2);surv.A<-c(surv.A[1],surv.A)
  surv.A<-surv.A[1:length(time.A)]
  #
  if(between){
    time.B<-fit.B$time;surv.B<-fit.B$surv
    num.B<-c(1:length(time.B))
    ALL.B<-data.frame(num.B,time.B,surv.B)
    all.B<-ALL.B[ALL.B[2]>=t1 & ALL.B[2]<=t2 ,]
    time.B<-c(t1,all.B$time.B,t2) ##
    num.B1<-all.B$num.B[1]-1
    if(num.B1!=0){
      surv1.B<-ALL.B$surv.B[all.A$num.A[1]-1]
    } else  {
      surv1.B<-1}
    surv.B<-c(surv1.B,all.B$surv.B,all.B$surv.B[nrow(all.B)])
    time.B<-rep(time.B,each=2);surv.B<-rep(surv.B,each=2)
    surv.B<-c(surv.B[1],surv.B)
    surv.B<-surv.B[1:length(time.B)]
    #
    time.B<-rev(time.B);surv.B<-rev(surv.B)
    #
    tt<-c(time.A,time.B);ss<-c(surv.A,surv.B)
    polygon(tt,ss,col =col,border = NA,lwd=1)
  }  else {
    tt<-c(time.A,rev(time.A));ss<-c(surv.A,rep(0,length(surv.A)))
    polygon(tt,ss,col =col,border = NA,lwd=1)
  }
}
