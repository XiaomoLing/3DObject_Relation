
#' A 3D Object relationship detection function
#'
#' This function allows you to compare objects saved in two list
#' @param TableFrom, TableTo are tables with; ClosedFrom, ClosedTo
#' @keywords 3D
#' @return A table of combined the input tables with result
#' @export
#' Detecte3DRelation()


Detecte3DRelation <- function (TableFrom, TableTo, ClosedFrom, ClosedTo ) {

   #### TableFrom is Check Against List, with 6 column, with column names:
   #### c ("MIN.X","MAX.X", "")
   #### ClosedFrom is a vector, c(T,T) for example
  TableTo$RNO <- seq.int(nrow(TableTo))

  # INCLUDED Interval method

  ## X-axis
  ### From is Check Against List
  TableFrom.X <- as.matrix(dplyr::select(TableFrom,MIN.X,MAX.X))

  ### convert to intervals and name each row
  From.X <- intervals::Intervals_full(TableFrom.X, closed = ClosedFrom, type = "R")
  rownames(From.X) <- TableFrom$RNO

  ### To is Checking List
  TableTo.X <- as.matrix(dplyr::select(TableTo,MIN.X,MAX.X))
  To.X <- intervals::Intervals_full(TableTo.X,closed = ClosedTo, type = "R")

  ### Result List of X included
  list.X <- intervals::interval_included(From.X, To.X)
  df.list.X <- plyr::ldply(list.X, as.data.frame)
  colnames(df.list.X) <- c("WithinRNO","TableToRNO")

  df.list.X <- tibble::as_tibble(df.list.X)

  ## Y-axis
  ### From is Check Against List
  TableFrom.Y <- as.matrix(dplyr::select(TableFrom,MIN.Y,MAX.Y))

  ### convert to intervals and name each row
  From.Y <- intervals::Intervals_full(TableFrom.Y, closed = ClosedFrom, type = "R")
  rownames(From.Y) <- TableFrom$RNO

  ### To is Checking List
  TableTo.Y <- as.matrix(dplyr::select(TableTo,MIN.Y,MAX.Y))
  To.Y <- intervals::Intervals_full(TableTo.Y,closed = ClosedTo, type = "R")

  ### Result List of Y included
  list.Y <- intervals::interval_included(From.Y, To.Y)
  df.list.Y <- plyr::ldply(list.Y, as.data.frame)
  colnames(df.list.Y) <- c("WithinRNO","TableToRNO")

  df.list.Y <- tibble::as_tibble(df.list.Y)

  ## Z-axis
  ### From is Check Against List
  TableFrom.Z <- as.matrix(dplyr::select(TableFrom,MIN.Z,MAX.Z))
  From.Z <- intervals::Intervals_full(TableFrom.Z, closed = ClosedFrom, type = "R")
  rownames(From.Z) <- TableFrom$RNO

  ### To is Checking List
  TableTo.Z <-  as.matrix(dplyr::select(TableTo,MIN.Z,MAX.Z))
  To.Z <- intervals::Intervals_full(TableTo.Z,closed = ClosedTo, type = "R")

  ### Result List of Z included
  list.Z <- intervals::interval_included(From.Z, To.Z)
  df.list.Z <- plyr::ldply(list.Z, as.data.frame)
  colnames(df.list.Z) <- c("WithinRNO","TableToRNO")

  df.list.Z <- tibble::as_tibble(df.list.Z)

  ## Bind the all result table of XYZ, then filter for count = 3
  df.list.XYZ <- dplyr::bind_rows(df.list.X,df.list.Y,df.list.Z)
  df.list.XYZ <- dplyr::group_by(df.list.XYZ, TableToRNO, WithinRNO)
  df.list.XYZ <- dplyr::summarise(df.list.XYZ, n=n())
  Within <- dplyr::filter(df.list.XYZ, n >2)


  # OVERLAP Interval method

  TableTo.rest <- dplyr::filter(TableTo,!RNO %in% Within$TableToRNO)
  TableTo.rest$RRNO <- seq(1:nrow(TableTo.rest))

  ## X-axis
  ### To is the rest of the checkList
  TableTo.rest.X <- dplyr::select(TableTo.rest,MIN.X,MAX.X)
  To.rest.X <- intervals::Intervals(TableTo.rest.X,closed = ClosedTo, type = "R")

  ### List of X overlapped
  list.rest.X <- intervals::interval_overlap(From.X, To.rest.X)
  df.list.rest.X <- plyr::ldply(list.rest.X, as.data.frame)
  colnames(df.list.rest.X) <- c("OverlapRNO","TableToRRNO")

  df.list.rest.X <- tibble::as_tibble(df.list.rest.X)

  ## Y-axis
  ### To is the rest of the checkList
  TableTo.rest.Y <- dplyr::select(TableTo.rest,MIN.Y,MAX.Y)
  To.rest.Y <- intervals::Intervals(TableTo.rest.Y,closed = ClosedTo, type = "R")

  #### List of Y overlapped
  list.rest.Y <- intervals::interval_overlap(From.Y, To.rest.Y)
  df.list.rest.Y <- plyr::ldply(list.rest.Y, as.data.frame)
  colnames(df.list.rest.Y) <- c("OverlapRNO","TableToRRNO")

  df.list.rest.Y <- tibble::as_tibble(df.list.rest.Y)

  ## Z-axis
  ### To is the rest of the checkList
  TableTo.rest.Z <- dplyr::select(TableTo.rest,MIN.Z,MAX.Z)
  To.rest.Z <- intervals::Intervals(TableTo.rest.Z,closed = ClosedTo, type = "R")

  ### List of Z overlapped
  list.rest.Z <- intervals::interval_overlap(From.Z, To.rest.Z)
  df.list.rest.Z <- plyr::ldply(list.rest.Z, as.data.frame)
  colnames(df.list.rest.Z) <- c("OverlapRNO","TableToRRNO")

  df.list.rest.Z <- tibble::as_tibble(df.list.rest.Z)

  ### Bind the rows of the result table of XYZ, then filter for count = 3
  df.list.rest.XYZ <-  dplyr::bind_rows(df.list.rest.X,df.list.rest.Y,df.list.rest.Z)

  df.list.rest.XYZ <- dplyr::group_by(df.list.rest.XYZ, TableToRRNO, OverlapRNO)
  df.list.rest.XYZ <- dplyr::summarise(df.list.rest.XYZ, n=n())
  Overlap <- dplyr::filter(df.list.rest.XYZ, n >2)

  ## Combine overlap and included results
  Within.Result <- dplyr::left_join(TableTo, Within,by = c("RNO" = "TableToRNO") )

  Overlap.R <- dplyr::left_join(TableTo.rest, Overlap,by = c("RRNO" = "TableToRRNO") )
  Overlap.R <- dplyr::select(Overlap.R, 8:11)
  Overlap.Result <- dplyr::left_join(TableTo,Overlap.R, by = "RNO" )

  Result <- dplyr::left_join(Within.Result, Overlap.Result, by = c("MIN.X", "MAX.X", "MIN.Y", "MAX.Y", "MIN.Z", "MAX.Z", "Manual", "RNO"))

  return(Result)

}









