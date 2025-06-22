# 讀取標籤資訊
get_useful_labels <- function(con) {
  # 檢查是否為 RSQLite::SQLiteConnection 類別
  if (!inherits(con, "SQLiteConnection") || attr(class(con), "package") != "RSQLite") {
    stop("Error: Please provide a SQLiteConnection object from the RSQLite package.")
  }
  
  # 抓取資料並回傳有用欄位
  labels <- dbGetQuery(con, "SELECT * FROM labels")
  labels[c("LabelID", "LabelName", "LabelColorName", "GroupId",
           "Guid", "LastModified", "AutoSyncGenerated")]
}

get_useful_files <- function(con) {
  if (!inherits(con, "SQLiteConnection") || attr(class(con), "package") != "RSQLite") {
    stop("Error: Please provide a SQLiteConnection object from the RSQLite package.")
  }
  sql <- paste(
    "SELECT FileID, FileName, FolderId, Extension, FileSize, Guid,",
    "Comment, Star,",
    "LastModified, AddDate, IsFolder, CreationDate, MediaDuration",
    "FROM files"
  )
  dbGetQuery(con, sql)
}


# 傳入標籤名稱 給出所有GUID 
get_col_ignore_case <- function(df, colname) {
  col <- names(df)[tolower(names(df)) == tolower(colname)]
  if (length(col) == 0) stop("No such column found: ", colname)
  return(df[[col]])
}

fetch_guid <- function(labelnames, con) {
  #檢查大小寫轉換函數是否存在
  if (!exists("get_col_ignore_case", mode = "function")) {
    stop("Error: helper function 'get_col_ignore_case' is not defined in the environment.")
  }
  # 驗證資料庫連線物件
  if (!inherits(con, "SQLiteConnection") || attr(class(con), "package") != "RSQLite") {
    stop("Error: Please provide a SQLiteConnection object from the RSQLite package.")
  }
  # 檢查輸入類型
  if (!is.character(labelnames) || length(labelnames) == 0) {
    stop("Error: labelnames must be a non-empty character vector.")
  }
  
  # SQL 組合 抓標籤
  labelnames_sql <- paste0("'", labelnames, "'", collapse = ", ")
  query_label_ids <- sprintf(
    "SELECT LabelID FROM labels WHERE LabelName IN (%s)",
    labelnames_sql
  )
  
  res_label_ids <- dbGetQuery(con, query_label_ids)
  label_ids <- get_col_ignore_case(res_label_ids, "LabelID")
  
  if (length(label_ids) == 0) {
    return(integer(0))  # 找不到對應標籤，回傳空向量
  }
  
  # 查詢對應的 FileID
  label_ids_sql <- paste(label_ids, collapse = ", ")
  query_file_ids <- sprintf(
    "SELECT DISTINCT FileID FROM labeledfiles WHERE LabelID IN (%s)",
    label_ids_sql
  )
  res_file_ids <- dbGetQuery(con, query_file_ids)
  file_ids <- get_col_ignore_case(res_file_ids, "FileID")
  #----------------------------------------------------
  
  # 查找 GUIDs
  file_ids_sql <- paste(file_ids, collapse = ", ")
  query_guids <- sprintf(
    "SELECT DISTINCT Guid FROM files WHERE FileID IN (%s)",
    file_ids_sql
  )
  guids <- dbGetQuery(con, query_guids)
  guids_out <- get_col_ignore_case(guids, "guid")
  
  return(guids_out)
}
