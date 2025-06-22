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


get_col_ignore_case <- function(df, colname) {
  col <- names(df)[tolower(names(df)) == tolower(colname)]
  if (length(col) == 0) stop("No such column found: ", colname)
  return(df[[col]])
}

# 傳入標籤名稱 給出所有GUID 
fetch_guid_bylabel <- function(labelnames, con) {
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
    return(character(0))  # 找不到對應標籤，回傳空向量
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
  sql_injection_warning()
  
  return(guids_out)
}


# 傳入GUID  給檔案路徑
#代辦 alliaes支援

fetch_path_byguid <- function(guids, con) {
  #檢查大小寫轉換函數是否存在
  if (!exists("get_col_ignore_case", mode = "function")) {
    stop("Error: helper function 'get_col_ignore_case' is not defined in the environment.")
  }
  # 驗證輸入
  if (!inherits(con, "SQLiteConnection") || attr(class(con), "package") != "RSQLite") {
    stop("Error: Please provide a SQLiteConnection object from the RSQLite package.")
  }
  if (!is.character(guids) || length(guids) == 0) {
    stop("Error: guids must be a non-empty character vector.")
  }
  #檢查是否為假名fenrirs
  
  #組合 db_path
  
  db_path <- sub("\\\\db\\\\FenrirFS\\.db$", "", con@dbname)
  
  # 組合SQL的引號包裹GUID
  guids_sql <- paste0("'", guids, "'", collapse = ", ")
  
  query <- sprintf(
    "SELECT Guid, FileName FROM files WHERE Guid IN (%s)",
    guids_sql
  )
  
  res <- dbGetQuery(con, query)
  
  # 如果沒找到任何資料回傳空字串向量
  if (nrow(res) == 0) return(character(0))
  
  # 取出Guid跟FileName欄位
  guids_found <- get_col_ignore_case(res, "Guid")
  filenames <- get_col_ignore_case(res, "FileName")
  
  # 建立命名向量：以 GUID 為名稱，路徑為值
  paths <- file.path(db_path, "files", filenames)
  names(paths) <- guids_found
  sql_injection_warning()
  return(paths)
}





sql_injection_warning <- function() {
  lang <- tolower(Sys.getlocale())
  is_chinese <- grepl("zh|cn|tw|chinese ", lang)
  
  if (is_chinese) {
    warning_msg <- paste0(
      "\n",
      "##########################################################\n",
      "# 警告：偵測到可能的 SQL 注入風險！                   #\n",
      "# 請務必使用參數化查詢，避免直接拼接使用者輸入到 SQL。 #\n",
      "# 無法使用參數化時，請嚴格驗證和過濾所有輸入。          #\n",
      "##########################################################\n"
    )
  } else {
    warning_msg <- paste0(
      "\n",
      "##########################################################\n",
      "# WARNING: Potential SQL Injection Risk Detected!       #\n",
      "# Please ensure all SQL queries use parameterized inputs.#\n",
      "# Avoid directly concatenating user input into SQL strings.#\n",
      "# If you cannot use parameterized queries, at least     #\n",
      "# validate and sanitize all inputs strictly.             #\n",
      "##########################################################\n"
    )
  }
  
  message(warning_msg)
}
