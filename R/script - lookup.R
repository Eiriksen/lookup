
#' Look up individual-associated data from one dataset and insert into another.
#'
#' Similar to merge(), but more flexible (and slower). Supports renaming of columns and overwrite-controll.
#' Returns the "to" dataframe with new columns from "from" added.
#'
#' @param to A data frame (x) you want to add data from another set into
#' @param from The data (y) frame you want to obtain this data from
#' @param what Which columns from y to look up into x. Single chr value or vector with multiple values. Will take all columns from y if unspecified.
#' @param by Which column to use for identifying individuals. The function only adds data to columns with matching identifiers.
#' @param by.x If the identifier has a different column name in the "to" dataset, specify it with by.x
#' @param by.y If the identifier has a different column name in the "from" dataset, specify it with by.y
#' @param new_name If you want to rename some columns from y before inserting into x. Must be same length as "what". Use an empty "" for columns to stay the same.
#' @param default If inserted columns doesn't exist yet in x, use this to specify the default value in case some rows can't be looked up
#' @param overwrite If there is already a value in x where one is to be inserted from y, use this to specify if it should be overwritten or not
#' @export
#' @examples all_people = lookup(to=all_people, from=phone_book, what=c("phone_number","adress"), new_name=("mobile", ""), by="name", by.x="Name", default="not found")
lookup = function(to, from, what, by, by.x, by.y, new_name, default=NA, overwrite=F, overwriteNA=F){
  require(tidyverse)

  # translate simple parameters to specific ones
  df_x    <- to
  df_y    <- from
  if (missing(by.x)) by.x = by
  if (missing(by.y)) by.y = by

  # make sure by.x and by.y is of the same length
  if (length(by.x) != length(by.y)) stop("by.x and by.y must have the same amount of column names")
  # make sure columns mentioned in by.x actually exist in df_x (same for by.y)
  if (sum(names(df_x) %in% by.x) != length(by.x)) stop("Not all columns in by.x exist in x dataframe")
  if (sum(names(df_y) %in% by.y) != length(by.y)) stop("Not all columns in by.y exist in y dataframe")
  # same with "what", make sure all the columns exist in df_y
  if (!missing(what)){
    if(sum(names(df_y) %in% what) != length(what)) stop("Not all columns in 'what' exist in 'from' dataframe")
  }
  # if "what" is not defined, make it all the columns in Y (from) except the by.y columns
  if(missing(what)) what=names(from)[!names(from) %in% by.y]
  columns <- what

  # if new name is being used, check that it has the same length as "what"
  if (!missing(new_name)) if (length(new_name) != length(columns)) stop("'new_name' and 'what' must be of same length if 'new_name' is used")

  # if new name is not being used, keep the same names as have been used in "what"/"columns
  # and if it is being used, go over it and make all names not specified by the user (denoted "") keep the same name as in "what"/"columns"
  if (missing(new_name)) new_name = columns
  else
  {
    for (i in 1:length(new_name))
    {
      if (new_name[i]=="") new_name[i]=columns[i]
    }
  }

  # columns_n  (new) are the names of the new columns where to checkup values go
  columns_n <- new_name
  # cokumns_o  (old) are the  names of the columns where the values were gotten from
  columns_o <- columns

  # variables in use now, and checked:
  # df_x, df_y, by.x, by.y, columns_n, column_o


  # first do a column check, create those that doesn't exist
  for (i in 1:length(columns_n))
  {
    if(!columns_n[i] %in% colnames(df_x))
    {
      df_x[[columns_n[i]]] = default
    }
  }

  # iterate over all rows in x
  for (r in 1:nrow(df_x))
  {
    # currently in a row r
    # check if by.x of this rows matches by.y of any row in y
    rows_y = df_y

    for (i in 1:length(by.x))
    {

      col_x = by.x[i]
      col_y = by.y[i]
      rows_y = rows_y[!is.na(rows_y[[col_y]]) & rows_y[[col_y]] == df_x[[col_x]][r],]
    }


    if (nrow(rows_y) != 0)
    {
      # a match was found!
      # Iterate over all columns_o in y and assign them to column_n in x
      for (clm in 1:length(columns_o) )
      {
        # currently looking at column clm
        # put in df_x, at this column, at the current row, the corresponding value in the matching rows in y
        # but only if overwrite is turned on, or the value in x is NA
        if (overwrite == T || is.na(df_x[[columns_n[clm]]][r]))
        {
          # only overwrite values in X with NA from y if overwriteNA is T
          if(!is.na(rows_y[[columns_o[clm]]][1]) | overwriteNA==T)
          {
          df_x[[columns_n[clm]]][r] = rows_y[[columns_o[clm]]][1]
          }
          else if(is.na(rows_y[[columns_o[clm]]][1]))
          {
          # if y value is NA and overwriteNA is set to F, DO NOTHING //prev:overwrite with the default value//
          #df_x[[columns_n[clm]]][r] = default
          }
        }
      }

    }
    # if not, leave be and continue to next

  }
    # check if its by.x matches any by.y
      # if so, take the values from y (depending on setting of overwrite)
      # if not, leave be
return(df_x)

}
