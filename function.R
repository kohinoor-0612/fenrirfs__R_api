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