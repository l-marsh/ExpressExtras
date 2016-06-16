
#' Checks the sampleinfo file
#' Reads the excelsheet containing sample information, performs sanity checks and generates pData
#' @param file : path to the inputfile
#' @param type : Input type. Can be a dataframe or a csv file. Options are df and csv
#' @return Dataframe with pData
#' @export
#' @examples
#' checkpData(file="/home/user/file.csv",type="csv")
#' checkpData(file=dataframe,type="df")

checkpData <- function(file,type)
{
  if (missing(type))
    stop("Need to specify input type")

  if(type=="csv"){
    if(file.exists(file)==TRUE){
      sampledata=read.csv(file)}
    else{
    print("The file does not exist. Please check path")}
  }
  else if(type=="df"){
      sampledata=file
  }

  colnames_pdat=colnames(sampledata)
  if("sample_name" %in% colnames_pdat){
    if("maineffect" %in% colnames_pdat){
      pdata=data.frame(sampledata)
    }
  }
  else{print("sample_name/maineffect column(s) does not exist. Please check the file.")}

  return(sampledata)
}

