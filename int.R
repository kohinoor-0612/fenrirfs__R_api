install.packages("RSQLite")


library(RSQLite)

# 替換成你實際的資料庫路徑
db_path <- "C:/Users/User/FenrirFS Storage/fenrirfs api test.profile/db/FenrirFS.db"

# 連接資料庫
con <- dbConnect(SQLite(), dbname = db_path)

tables <- dbListTables(con)



# 讀取標籤資訊
tags <- dbGetQuery(con, "SELECT ID, Name, Color, Type FROM Tags")
print(tags)

# 關閉連線
dbDisconnect(con)
