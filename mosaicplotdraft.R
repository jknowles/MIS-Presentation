#' Draw a visual crosstab (mosaid plot) with shading for correlations 
#' and labels in each cell.
#'
#' Improves labeling of mosaic plots over \code{mosaic} from the vcd package
#'
#' @param data a data object, matrix or dataframe, that contains the categorical 
#'    variables to compose the crosstab
#' @param rowvar a reference to the column in data that will be displayed on the rows
#'    of the crosstab, expressed as data$rowvar
#' @param colvar a reference to the column in data that will be displayed in columns of the 
#'    crosstab
#' @param varnames a character vector of length two with the labels for rowvar and colvar
#'    respectively
#' @param title a character vector of length one that contains the main title for the plot
#' @param subtitle a character vector of length one that contains the subtitle displayed 
#'    beneath the plot
#' @keywords mosaic
#' @keywords crosstabs
#' @keywords vcd
#' @return A mosaic plot
#' @seealso
#'  \code{\link{mosaic}} which this function wraps
#'
#'  \code{\link{structure}} which does the data manipulation for the crosstab
#' 
#' @export
#' @examples
#' df<-data.frame(cbind(x=seq(1,3,by=1), y=sample(LETTERS[6:8],60,replace=TRUE)),
#' fac=sample(LETTERS[1:4], 60, replace=TRUE))
#' varnames<-c('Quality','Grade')
#' mosaictabs.label(df,df$y,df$fac,varnames,'My Plot','Foo')

mosaictabs.label<-function(data,rowvar,colvar,varnames,title,subtitle){
    require(vcd)
mosaictabs <-function(rowvar,colvar,varnames){
  crosstab<-table(rowvar,colvar)
  rowvarcat<-levels(as.factor(rowvar))
  colvarcat<-levels(as.factor(colvar))
  proportions<-round(prop.table(crosstab , 2)*100)
  values<-c(crosstab)
  names=varnames
  dims<-c(length((rowvarcat)),length(colvarcat))
  dimnames<-structure( list(rowvarcat,colvarcat ),.Names = c(names) )
  TABS <<- structure( c(values), .Dim = as.integer(dims), .Dimnames = dimnames, class = "table") 
  PROPORTIONS <- structure( c(proportions), .Dim = as.integer(dims), .Dimnames = dimnames, class = "table") 
  TABSPROPORTIONS <<- structure( c(paste(PROPORTIONS,"%","\n", "(",values,")",sep="")), .Dim = as.integer(dims), 
      .Dimnames = dimnames, class = "table") 
  #grid.newpage()
 # pushViewport(viewport(x=6,y=6,w=unit(1,'inches'),h=unit(1,'inches'),clip='off'))
  mosaic(TABS,pop=FALSE, shade=TRUE, main=title, sub=subtitle)
 #labeling_cells(text=TABSPROPORTIONS , clip_cells=FALSE)(TABS, prefix="plot1")
  }
with(data,mosaictabs(rowvar,colvar,varnames))
labeling_cells(text=TABSPROPORTIONS , clip_cells=FALSE)(TABS)
    }


##Mosaic function with labels
# http://www.rexdouglass.com/blog:3

#TODO: Add better handling of inputs
#TODO: Shading on and off
#TODO: generalize to multivariate
#Correct call still requires data elements to be specified using $var


