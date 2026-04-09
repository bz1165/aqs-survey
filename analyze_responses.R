# ═══════════════════════════════════════════════════════════════════════════════
# AQS AI Survey — Response Analysis Helper
# Run this script from the aqs-survey/ directory.
# ═══════════════════════════════════════════════════════════════════════════════

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(purrr)

RESPONSES_DIR <- "responses"

# ── 1. Load all responses ──────────────────────────────────────────────────────
load_responses <- function(dir = RESPONSES_DIR) {
  files <- list.files(dir, pattern = "\\.rds$", full.names = TRUE)
  if (length(files) == 0) { message("No responses found in: ", dir); return(NULL) }
  df <- map_dfr(files, readRDS)
  message(sprintf("✓ Loaded %d response(s).", nrow(df)))
  df
}

df <- load_responses()
if (is.null(df)) stop("No data to analyse.")

# ── 2. Export combined CSV (for Excel / other tools) ──────────────────────────
write_csv(df, "responses_combined.csv")
message("✓ Exported → responses_combined.csv")

# ── Helper: expand pipe-separated columns ──────────────────────────────────────
expand_multi <- function(df, col) {
  df |>
    select(timestamp, value = all_of(col)) |>
    filter(!is.na(value)) |>
    mutate(value = str_split(value, " \\| ")) |>
    unnest(value) |>
    filter(nchar(trimws(value)) > 0)
}

cat("\n", strrep("═", 60), "\n")
cat("  AQS AI Survey — Summary Report\n")
cat("  Responses:", nrow(df), "  |  Generated:", format(Sys.time()), "\n")
cat(strrep("═", 60), "\n\n")

# ── Q0: Role ──────────────────────────────────────────────────────────────────
cat("── Q0: Role ──────────────────────────────────────────────\n")
labels_q0 <- c(A="统计师", B="统计程序员", C="组长/管理者", D="其他")
q0_tbl <- df |> count(q0) |> mutate(label = labels_q0[q0], pct = round(n/sum(n)*100,1))
print(q0_tbl)

# ── Q1: Tools used (multi) ────────────────────────────────────────────────────
cat("\n── Q1: Tools Used (multiple choice) ─────────────────────\n")
labels_q1 <- c(A="Internal ChatGPT", B="M365 Copilot", C="Microsoft Agent",
                D="Coach Mira", E="GitHub Copilot/Cursor", F="Other", G="None")
expand_multi(df, "q1") |>
  count(value, sort = TRUE) |>
  mutate(label = labels_q1[value], pct = round(n/nrow(df)*100,1)) |>
  print()

# ── Q2: Most frequent tool ────────────────────────────────────────────────────
cat("\n── Q2: Most Frequent Tool ────────────────────────────────\n")
labels_q2 <- c(A="Internal ChatGPT", B="M365 Copilot", C="Microsoft Agent",
                D="Coach Mira", E="GitHub Copilot/Cursor", F="Other", G="None/No AI")
df |> count(q2) |> mutate(label = labels_q2[q2], pct = round(n/sum(n)*100,1)) |> print()

# ── Q3: Task ranking (aggregate mean rank) ────────────────────────────────────
cat("\n── Q3: Task Ranking (mean rank, 1 = most frequent) ──────\n")
q3_df <- df |>
  filter(!is.na(q3_ranking)) |>
  mutate(ranked = str_split(q3_ranking, " \\| ")) |>
  select(timestamp, ranked) |>
  unnest(ranked) |>
  group_by(timestamp) |>
  mutate(rank = row_number()) |>
  ungroup() |>
  group_by(item = ranked) |>
  summarise(mean_rank = round(mean(rank), 2), n = n(), .groups = "drop") |>
  arrange(mean_rank)
print(q3_df)

# ── Q4: Usage frequency ───────────────────────────────────────────────────────
cat("\n── Q4: AI Usage Frequency ────────────────────────────────\n")
labels_q4 <- c(A="远超每日一次", B="至少每日一次", C="至少每周一次",
                D="每月或更少", E="视具体情况")
df |> filter(!is.na(q4)) |> count(q4) |>
  mutate(label = labels_q4[q4], pct = round(n/sum(n)*100,1)) |> print()

# ── Q5: Helpful moment ────────────────────────────────────────────────────────
cat("\n── Q5: Most Helpful AI Moment ───────────────────────────\n")
labels_q5 <- c(A="文献查询/综述", B="代码写作/调试", C="专业文档撰写",
                D="数据结果解读", E="学习新工具/方法", G="其他", H="想不起来")
df |> filter(!is.na(q5)) |> count(q5) |>
  mutate(label = labels_q5[q5], pct = round(n/sum(n)*100,1)) |> print()

cat("\n  Open-ended Q5 descriptions:\n")
df |> filter(!is.na(q5_text)) |> pull(q5_text) |> walk(~cat(" •", .x, "\n"))

# ── Q6: Failed tasks (multi) ──────────────────────────────────────────────────
cat("\n── Q6: Unsatisfactory Tasks (multiple choice, max 3) ────\n")
labels_q6 <- c(A="需要文献支持的写作", B="专业知识选择判断",
                D="复杂格式文件起草", E="实时数据任务", F="其他", G="无")
expand_multi(df, "q6") |>
  count(value, sort = TRUE) |>
  mutate(label = labels_q6[value], pct = round(n/nrow(df)*100,1)) |> print()

# ── Q7: Challenge ranking ─────────────────────────────────────────────────────
cat("\n── Q7: AI Challenges (mean rank, 1 = most impactful) ────\n")
q7_df <- df |>
  filter(!is.na(q7_ranking)) |>
  mutate(ranked = str_split(q7_ranking, " \\| ")) |>
  select(timestamp, ranked) |>
  unnest(ranked) |>
  group_by(timestamp) |>
  mutate(rank = row_number()) |>
  ungroup() |>
  group_by(item = ranked) |>
  summarise(mean_rank = round(mean(rank), 2), n = n(), .groups = "drop") |>
  arrange(mean_rank)
print(q7_df)

# ── Q8: Agreement statement ───────────────────────────────────────────────────
cat("\n── Q8: Agreement — AI costs more time? ──────────────────\n")
labels_q8 <- c(A="强烈同意", B="同意", C="中立", D="不同意", E="强烈不同意")
df |> filter(!is.na(q8)) |> count(q8) |>
  mutate(label = labels_q8[q8], pct = round(n/sum(n)*100,1)) |> print()

# ── Q9: Desired tools ranking ─────────────────────────────────────────────────
cat("\n── Q9: Desired AI Tools (mean rank, 1 = most wanted) ────\n")
q9_df <- df |>
  filter(!is.na(q9_ranking)) |>
  mutate(ranked = str_split(q9_ranking, " \\| ")) |>
  select(timestamp, ranked) |>
  unnest(ranked) |>
  group_by(timestamp) |>
  mutate(rank = row_number()) |>
  ungroup() |>
  group_by(item = ranked) |>
  summarise(mean_rank = round(mean(rank), 2), n = n(), .groups = "drop") |>
  arrange(mean_rank)
print(q9_df)

# ── Q10: Open responses ───────────────────────────────────────────────────────
cat("\n── Q10: Open Responses ───────────────────────────────────\n")
open <- df |> filter(!is.na(q10) & nchar(q10) > 0) |> pull(q10)
if (length(open) == 0) {
  cat("  (no open responses)\n")
} else {
  walk(seq_along(open), ~cat(sprintf("  [%d] %s\n", .x, open[[.x]])))
}

cat("\n", strrep("═", 60), "\n")
cat("  Analysis complete.\n\n")
