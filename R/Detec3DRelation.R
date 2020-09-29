
#' A 3D Object relationship detection function
#'
#' This function allows you to compare objects saved in two list
#'
#' @param tablefrom, tableto are tables with; closedfrom, closedto
#' @keywords 3D
#' @return A table of combined the input tables with result
#' @export
detecte3Dr <- function (tablefrom, tableto, closedfrom, closedto ) {

   #### tablefrom is Check Against List, with 6 column, with column names:
   #### c ("MIN.X","MAX.X", "")
   #### closedfrom is a vector, c(T,T) for example
  tableto$RNO <- seq.int(nrow(tableto))

  # INCLUDED Interval method

  ## X-axis
  ### From is Check Against List
  tablefrom.X <- as.matrix(dplyr::select(tablefrom,MIN.X,MAX.X))

  ### convert to intervals and name each row
  From.X <- intervals::Intervals_full(tablefrom.X, closed = closedfrom, type = "R")
  rownames(From.X) <- tablefrom$RNO

  ### To is Checking List
  tableto.X <- as.matrix(dplyr::select(tableto,MIN.X,MAX.X))
  To.X <- intervals::Intervals_full(tableto.X,closed = closedto, type = "R")

  ### Result List of X included
  list.X <- intervals::interval_included(From.X, To.X)
  names(list.X) <- c(seq.int(nrow(tablefrom.X)))
  df.list.X <- data.frame(WithinRNO = rep(names(list.X), sapply(list.X, length)),
                          tabletoRNO = unlist(list.X))
  df.list.X <- tibble::as_tibble(df.list.X)

  ## Y-axis
  ### From is Check Against List
  tablefrom.Y <- as.matrix(dplyr::select(tablefrom,MIN.Y,MAX.Y))

  ### convert to intervals and name each row
  From.Y <- intervals::Intervals_full(tablefrom.Y, closed = closedfrom, type = "R")
  rownames(From.Y) <- tablefrom$RNO

  ### To is Checking List
  tableto.Y <- as.matrix(dplyr::select(tableto,MIN.Y,MAX.Y))
  To.Y <- intervals::Intervals_full(tableto.Y,closed = closedto, type = "R")

  ### Result List of Y included
  list.Y <- intervals::interval_included(From.Y, To.Y)
  names(list.Y) <- c(seq.int(nrow(tablefrom.X)))
  df.list.Y <- data.frame(WithinRNO = rep(names(list.Y), sapply(list.Y, length)),
                          tabletoRNO = unlist(list.Y))

  df.list.Y <- tibble::as_tibble(df.list.Y)
  
  ## Z-axis
  ### From is Check Against List
  tablefrom.Z <- as.matrix(dplyr::select(tablefrom,MIN.Z,MAX.Z))
  From.Z <- intervals::Intervals_full(tablefrom.Z, closed = closedfrom, type = "R")
  rownames(From.Z) <- tablefrom$RNO

  ### To is Checking List
  tableto.Z <-  as.matrix(dplyr::select(tableto,MIN.Z,MAX.Z))
  To.Z <- intervals::Intervals_full(tableto.Z,closed = closedto, type = "R")

  ### Result List of Z included
  list.Z <- intervals::interval_included(From.Z, To.Z)
  names(list.Z) <- c(seq.int(nrow(tablefrom.X)))
  df.list.Z <- data.frame(WithinRNO = rep(names(list.Z), sapply(list.Z, length)),
                          tabletoRNO = unlist(list.Z))
  df.list.Z <- tibble::as_tibble(df.list.Z)
  
  ## Bind the all result table of XYZ, then filter for count = 3
  df.list.XYZ <- dplyr::bind_rows(df.list.X,df.list.Y,df.list.Z)
  df.list.XYZ <- dplyr::group_by(df.list.XYZ, tabletoRNO, WithinRNO)
  df.list.XYZ <- dplyr::summarise(df.list.XYZ, n=n())
  Within <- dplyr::filter(df.list.XYZ, n >2)


  # OVERLAP Interval method

  tableto.rest <- dplyr::filter(tableto,!RNO %in% Within$tabletoRNO)
  tableto.rest$RRNO <- seq(1:nrow(tableto.rest))

  ## X-axis
  ### To is the rest of the checkList
  tableto.rest.X <- dplyr::select(tableto.rest,MIN.X,MAX.X)
  To.rest.X <- intervals::Intervals(tableto.rest.X,closed = closedto, type = "R")

  ### List of X overlapped
  list.rest.X <- intervals::interval_overlap(From.X, To.rest.X)
  
  names(list.rest.X) <- c(seq.int(nrow(tablefrom.X)))
  df.list.rest.X <- data.frame(OverlapRNO = rep(names(list.rest.X), sapply(list.rest.X, length)),
                               tabletoRRNO = unlist(list.rest.X))

  df.list.rest.X <- tibble::as_tibble(df.list.rest.X)

  ## Y-axis
  ### To is the rest of the checkList
  tableto.rest.Y <- dplyr::select(tableto.rest,MIN.Y,MAX.Y)
  To.rest.Y <- intervals::Intervals(tableto.rest.Y,closed = closedto, type = "R")

  #### List of Y overlapped
  list.rest.Y <- intervals::interval_overlap(From.Y, To.rest.Y)

  names(list.rest.Y) <- c(seq.int(nrow(tablefrom.X)))
  df.list.rest.Y <- data.frame(OverlapRNO = rep(names(list.rest.Y), sapply(list.rest.Y, length)),
                               tabletoRRNO = unlist(list.rest.Y))
  
  df.list.rest.Y <- tibble::as_tibble(df.list.rest.Y)

  ## Z-axis
  ### To is the rest of the checkList
  tableto.rest.Z <- dplyr::select(tableto.rest,MIN.Z,MAX.Z)
  To.rest.Z <- intervals::Intervals(tableto.rest.Z,closed = closedto, type = "R")

  ### List of Z overlapped
  list.rest.Z <- intervals::interval_overlap(From.Z, To.rest.Z)
  names(list.rest.Z) <- c(seq.int(nrow(tablefrom.X)))
  df.list.rest.Z <- data.frame(OverlapRNO = rep(names(list.rest.Z), sapply(list.rest.Z, length)),
                               tabletoRRNO = unlist(list.rest.Z))
  
  df.list.rest.Z <- tibble::as_tibble(df.list.rest.Z)

  ### Bind the rows of the result table of XYZ, then filter for count = 3
  df.list.rest.XYZ <-  dplyr::bind_rows(df.list.rest.X,df.list.rest.Y,df.list.rest.Z)

  df.list.rest.XYZ <- dplyr::group_by(df.list.rest.XYZ, tabletoRRNO, OverlapRNO)
  df.list.rest.XYZ <- dplyr::summarise(df.list.rest.XYZ, n = dplyr::n())
  Overlap <- dplyr::filter(df.list.rest.XYZ, n >2)

  ## Combine overlap and included results
  Within.Result <- dplyr::left_join(tableto, Within,by = c("RNO" = "tabletoRNO") )

  Overlap.R <- dplyr::left_join(tableto.rest, Overlap,by = c("RRNO" = "tabletoRRNO") )
  Overlap.R <- dplyr::select(Overlap.R, 8:11)
  Overlap.Result <- dplyr::left_join(tableto,Overlap.R, by = "RNO" )

  Result <- dplyr::left_join(Within.Result, Overlap.Result, by = c("MIN.X", "MAX.X", "MIN.Y", "MAX.Y", "MIN.Z", "MAX.Z", "Manual", "RNO"))

  return(Result)

}









