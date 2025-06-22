install.packages("RSQLite")


library(RSQLite)

# 替換成你實際的資料庫路徑
db_path <- "C:/Users/User/FenrirFS Storage/fenrirfs api test.profile"
# 連接資料庫
con <- dbConnect(SQLite(), dbname = paste0(db_path,"/db/FenrirFS.db"))

#資料夾沒問題 這無法處理allias
list.files(paste0(db_path,"/files"))


tables <- dbListTables(con)


files <- dbGetQuery(con, "SELECT * FROM files")


labeledfiles <- dbGetQuery(con, "SELECT * FROM labeledfiles")


#資料夾分類
smartfolders <- dbGetQuery(con, "SELECT * FROM smartfolders")


# 關閉連線
dbDisconnect(con)


