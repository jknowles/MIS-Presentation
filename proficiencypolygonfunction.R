#' Creates a proficiency polygon in ggplot2 for showing assessment categories
#'
#' @param grades a vector of tested grades in sequential order
#' @param LOSS is a vector of the lowest obtainable scale score on 
#'        an assessment by grade
#' @param minimal is a vector of the floor of the minimal assessment category by grade
#' @param basic is a vector of the floor of the basic assessment category by grade
#' @param proficient is a vector of the floor of the proficient assessment category by grade
#' @param advanced is a vectof of the floor of the advanced assessment category by grade
#' @param HOSS is a vector of the highest obtainable scale score by grade
#' @keywords ggplot2
#' @keywords polygon
#' @return a ggplot2 object that can be printed or saved
#' @seealso
#'  \code{\link{geom_polygon}} which this function wraps
#'
#' @export
#' @examples
#'grades<-c(3,4,5,6,7,8)
#' g<-length(grades)
#' LOSS<-rep(200,6)
#' HOSS<-rep(650,6)
#' basic<-c(320,350,370,390,420,440)
#' minimal<-basic-30
#' prof<-c(380,410,430,450,480,500)
#' adv<-c(480,510,530,550,580,600)
#' 
#' z<-profpoly(grades,LOSS,minimal,basic,proficient,advanced,HOSS)
#' z


# basic, proficient, and advanced need to be the bottom threshold
# minimal is the top threshold for the minimal category
# HOSS and LOSS are highest and lowest obtainable scale score vectors
# grades is the vector of tested grades to draw polygon for
# All vectors are grade ordered and same length
# Returns a ggplot object

profpoly<-function(grades,LOSS,minimal,basic,proficient,advanced,HOSS...){
  require(ggplot2)
  g<-length(grades)
  #
  rep.invert<-function(x){
    c(x,x[order(-x)])
  }
  #
  grades<-rep.invert(grades)
  len<-length(grades)
  minimala<-c(LOSS,minimal[order(-minimal)])
  basica<-c(minimal+1,prof[order(-prof)])
  profa<-c(prof+1,adv[order(-adv)])
  adva<-c(adv+1,HOSS)
  prof<-c(rep(1,len),rep(2,len),rep(3,len),rep(4,len))
  vals<-c(minimala,basica,profa,adva)
  gradeP<-rep(grades,4)
  profpoly<-cbind(gradeP,prof,vals)
  profpoly<-as.data.frame(profpoly)
  profpoly$vals<-as.character(profpoly$vals)
  profpoly$vals<-as.numeric(profpoly$vals)
  profpoly$prof<-factor(profpoly$prof,levels=unique(as.numeric(prof)))
  p<-ggplot(profpoly,aes(x=gradeP,y=vals))
  p+geom_polygon(aes(fill=prof,group=prof))+scale_fill_brewer('Proficiency',type='seq')
}

##########################################################
# More flexible output from profpoly
##########################################################

#' Creates a data frame suitable for building custom polygon layers in ggplot2 objects
#'
#' @param grades a vector of tested grades in sequential order
#' @param LOSS is a vector of the lowest obtainable scale score on 
#'        an assessment by grade
#' @param minimal is a vector of the floor of the minimal assessment category by grade
#' @param basic is a vector of the floor of the basic assessment category by grade
#' @param proficient is a vector of the floor of the proficient assessment category by grade
#' @param advanced is a vectof of the floor of the advanced assessment category by grade
#' @param HOSS is a vector of the highest obtainable scale score by grade
#' @keywords ggplot2
#' @keywords polygon
#' @return a dataframe for adding a polygon to layers in other ggplot2 plots
#' @seealso
#'  \code{\link{geom_polygon}} which this function assists
#'
#' @export
#' @examples
#'grades<-c(3,4,5,6,7,8)
#' g<-length(grades)
#' LOSS<-rep(200,6)
#' HOSS<-rep(650,6)
#' basic<-c(320,350,370,390,420,440)
#' minimal<-basic-30
#' prof<-c(380,410,430,450,480,500)
#' adv<-c(480,510,530,550,580,600)
#' 
#' z<-profpoly.df(grades,LOSS,minimal,basic,proficient,advanced,HOSS)
#' z

profpoly.df<-function(grades,LOSS,minimal,basic,proficient,advanced,HOSS...){
  require(ggplot2)
  g<-length(grades)
  #
  rep.invert<-function(x){
    c(x,x[order(-x)])
  }
  #
  grades<-rep.invert(grades)
  len<-length(grades)
  minimala<-c(LOSS,minimal[order(-minimal)])
  basica<-c(minimal+1,prof[order(-prof)])
  profa<-c(prof+1,adv[order(-adv)])
  adva<-c(adv+1,HOSS)
  prof<-c(rep(1,len),rep(2,len),rep(3,len),rep(4,len))
  vals<-c(minimala,basica,profa,adva)
  gradeP<-rep(grades,4)
  profpoly<-cbind(gradeP,prof,vals)
  profpoly<-as.data.frame(profpoly)
  profpoly$vals<-as.character(profpoly$vals)
  profpoly$vals<-as.numeric(profpoly$vals)
  profpoly$prof<-factor(profpoly$prof,levels=unique(as.numeric(prof)))
  return(profpoly)
}
