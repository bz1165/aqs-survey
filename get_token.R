# ══════════════════════════════════════════════════════════════════════
# AQS Survey — 一次性配置脚本
# 在 RStudio 中运行此脚本，完成 Microsoft Graph 授权并获取所有必要的配置值。
# 运行前请确认已完成 Azure AD 应用注册（见下方说明）。
# ══════════════════════════════════════════════════════════════════════

# ── 第一步：填写你的应用信息 ──────────────────────────────────────────
# 在 https://aad.portal.azure.com → App registrations 注册应用后填写：
CLIENT_ID     <- "在此填写 Application (client) ID"
CLIENT_SECRET <- "在此填写 Client Secret 的 Value"
TENANT_ID     <- "在此填写 Directory (tenant) ID"

# ── 第二步：运行以下代码，在浏览器完成授权 ───────────────────────────
library(httr)

# 请求设备码
resp <- POST(
  paste0("https://login.microsoftonline.com/", TENANT_ID, "/oauth2/v2.0/devicecode"),
  body   = list(client_id = CLIENT_ID,
                scope     = "Files.ReadWrite offline_access"),
  encode = "form"
)
info <- content(resp)

# 打印授权指引
cat("\n", strrep("=", 60), "\n")
cat(info$message, "\n")
cat(strrep("=", 60), "\n\n")
cat("在浏览器中完成授权后，回到 RStudio 按 Enter 继续...\n")
readline()

# 轮询获取 token
for (i in 1:20) {
  Sys.sleep(info$interval %||% 5)
  tok_resp <- POST(
    paste0("https://login.microsoftonline.com/", TENANT_ID, "/oauth2/v2.0/token"),
    body = list(
      client_id     = CLIENT_ID,
      client_secret = CLIENT_SECRET,
      grant_type    = "urn:ietf:params:oauth:grant-type:device_code",
      device_code   = info$device_code
    ), encode = "form"
  )
  tok <- content(tok_resp)
  if (!is.null(tok$access_token)) break
  if (!is.null(tok$error) && tok$error != "authorization_pending") {
    stop("授权失败：", tok$error_description)
  }
}
if (is.null(tok$access_token)) stop("授权超时，请重新运行脚本。")
cat("✓ 授权成功！\n\n")

# ── 第三步：在 OneDrive 创建 CSV 文件并获取 File ID ─────────────────
# 在 OneDrive 根目录创建空白 CSV 文件（如已存在会覆盖）
FILE_NAME <- "AQS_Survey_Responses_2026.csv"
COLUMNS   <- paste(
  "timestamp,user_id,q0,q1,q1_other,q2,q2_other,q3_ranking,q3_other,",
  "q4,q5,q5_other_text,q5_text,q6,q6_other,q7,q7_other,q8,",
  "q9_ranking,q9_other,q10,q10_other,q11,q12", sep = "")

# 写入带 BOM 的 CSV 标题行
csv_bytes <- c(as.raw(c(0xEF, 0xBB, 0xBF)),
               charToRaw(paste0(COLUMNS, "\r\n")))

upload_resp <- PUT(
  paste0("https://graph.microsoft.com/v1.0/me/drive/root:/",
         FILE_NAME, ":/content"),
  add_headers(Authorization  = paste("Bearer", tok$access_token),
              "Content-Type" = "text/csv; charset=utf-8"),
  body   = csv_bytes,
  encode = "raw"
)

file_info <- content(upload_resp)
FILE_ID   <- file_info$id

if (is.null(FILE_ID)) {
  cat("⚠ 文件创建失败，请检查权限设置。\n")
  print(file_info)
} else {
  cat("✓ OneDrive 文件已创建：", FILE_NAME, "\n\n")
}

# ── 输出：复制以下内容到 Shinyapps.io 环境变量 ──────────────────────
cat(strrep("=", 60), "\n")
cat("请将以下内容逐条添加到 Shinyapps.io → Settings → Environment Variables\n")
cat(strrep("=", 60), "\n\n")
cat(sprintf("MS_TENANT_ID     = %s\n", TENANT_ID))
cat(sprintf("MS_CLIENT_ID     = %s\n", CLIENT_ID))
cat(sprintf("MS_CLIENT_SECRET = %s\n", CLIENT_SECRET))
cat(sprintf("MS_REFRESH_TOKEN = %s\n", tok$refresh_token))
cat(sprintf("MS_FILE_ID       = %s\n", FILE_ID))
cat(sprintf("ADMIN_KEY        = aqs2026admin\n"))
cat(strrep("=", 60), "\n\n")

# ── 额外：生成 OneDrive 文件的共享链接 ─────────────────────────────
share_resp <- POST(
  paste0("https://graph.microsoft.com/v1.0/me/drive/items/",
         FILE_ID, "/createLink"),
  add_headers(Authorization  = paste("Bearer", tok$access_token),
              "Content-Type" = "application/json"),
  body   = '{"type":"view","scope":"organization"}',
  encode = "raw"
)
share_info <- content(share_resp)
if (!is.null(share_info$link$webUrl)) {
  cat("📎 OneDrive 文件共享链接（组织内可查看）：\n")
  cat(share_info$link$webUrl, "\n\n")
  cat("将上面 6 个环境变量配置好并 Republish app 后，\n")
  cat("每次有人提交问卷，数据会自动出现在上面这个链接的 Excel 文件里。\n")
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
