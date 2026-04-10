# ══════════════════════════════════════════════════════
# AQS AI 问卷数据分析脚本
# 在 aqs-survey/ 目录下运行此脚本
# ══════════════════════════════════════════════════════

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(purrr)

RESPONSES_DIR <- "responses"

# ── 1. 加载所有回收数据 ────────────────────────────────
load_responses <- function(dir = RESPONSES_DIR) {
  files <- list.files(dir, pattern = "\\.rds$", full.names = TRUE)
  if (length(files) == 0) { message("未找到任何回答文件：", dir); return(NULL) }
  df <- map_dfr(files, readRDS)
  message(sprintf("✓ 已加载 %d 份回答。", nrow(df)))
  df
}

df <- load_responses()
if (is.null(df)) stop("无数据可分析。")

# ── 2. 导出合并 CSV ────────────────────────────────────
write_csv(df, "responses_combined.csv")
message("✓ 已导出 → responses_combined.csv")

# ── 辅助：展开多选 / 排序列 ────────────────────────────
expand_multi <- function(df, col) {
  df |> select(timestamp, value = all_of(col)) |>
    filter(!is.na(value)) |>
    mutate(value = str_split(value, " \\| ")) |>
    unnest(value) |>
    filter(nchar(trimws(value)) > 0)
}

mean_rank <- function(df, col) {
  df |> filter(!is.na(.data[[col]])) |>
    mutate(ranked = str_split(.data[[col]], " \\| ")) |>
    select(timestamp, ranked) |>
    unnest(ranked) |>
    group_by(timestamp) |> mutate(rank = row_number()) |> ungroup() |>
    group_by(item = ranked) |>
    summarise(平均名次 = round(mean(rank), 2), 填答人数 = n(), .groups = "drop") |>
    arrange(平均名次)
}

divider <- function(title) cat("\n──", title, strrep("─", max(0, 50 - nchar(title))), "\n")

cat("\n", strrep("═", 55), "\n")
cat("  AQS AI 问卷汇总报告\n")
cat("  回收份数：", nrow(df), "  |  生成时间：", format(Sys.time()), "\n")
cat(strrep("═", 55), "\n")

# Q0
divider("Q0 角色分布")
lbl_q0 <- c(A="统计师", B="统计程序员", C="组长/管理者", D="其他")
df |> count(q0) |> mutate(角色 = lbl_q0[q0], 占比 = paste0(round(n/sum(n)*100,1),"%")) |> print()

# Q1
divider("Q1 使用过的 AI 工具（多选）")
lbl_q1 <- c(A="Internal ChatGPT", B="M365 Copilot", C="Microsoft Agent",
             D="Coach Mira", E="GitHub Copilot/Cursor", F="其他", G="几乎不用")
expand_multi(df, "q1") |> count(value, sort=TRUE) |>
  mutate(工具 = lbl_q1[value], 占比 = paste0(round(n/nrow(df)*100,1),"%")) |> print()

# Q2
divider("Q2 最常用工具")
lbl_q2 <- c(A="Internal ChatGPT", B="M365 Copilot", C="Microsoft Agent",
             D="Coach Mira", E="GitHub Copilot/Cursor", F="其他", G="几乎不用")
df |> count(q2) |> mutate(工具 = lbl_q2[q2], 占比 = paste0(round(n/sum(n)*100,1),"%")) |> print()

# Q3
divider("Q3 任务使用频率排序（平均名次，1=最常用）")
mean_rank(df, "q3_ranking") |> print()

# Q4
divider("Q4 使用频率")
lbl_q4 <- c(A="远超每日一次", B="至少每日一次", C="至少每周一次", D="每月或更少", E="视情况而定")
df |> filter(!is.na(q4)) |> count(q4) |>
  mutate(频率 = lbl_q4[q4], 占比 = paste0(round(n/sum(n)*100,1),"%")) |> print()

# Q5
divider("Q5 AI 帮上大忙的场景")
lbl_q5 <- c(A="文献查询/综述", B="代码写作/调试", C="专业文档撰写",
             D="数据结果解读", E="学习新工具/方法", G="其他", H="想不起来")
df |> filter(!is.na(q5)) |> count(q5) |>
  mutate(场景 = lbl_q5[q5], 占比 = paste0(round(n/sum(n)*100,1),"%")) |> print()
cat("\n  开放描述：\n")
df |> filter(!is.na(q5_text)) |> pull(q5_text) |> walk(~cat(" •", .x, "\n"))

# Q6
divider("Q6 AI 效果不满意的任务（多选，最多3项）")
lbl_q6 <- c(A="需要文献支持的写作", B="专业知识选择判断",
             D="复杂格式文件起草", E="实时数据任务", F="其他", G="无")
expand_multi(df, "q6") |> count(value, sort=TRUE) |>
  mutate(任务 = lbl_q6[value], 占比 = paste0(round(n/nrow(df)*100,1),"%")) |> print()

# Q7
divider("Q7 使用 AI 遇到的挑战（多选，最多3项）")
lbl_q7 <- c(A="输出错误/幻觉", B="缺乏专业知识", C="工具功能受限",
             D="难融入workflow", E="SAS支持弱", F="可复现性低",
             G="记忆丢失", H="其他")
expand_multi(df, "q7") |> count(value, sort=TRUE) |>
  mutate(挑战 = lbl_q7[value], 占比 = paste0(round(n/nrow(df)*100,1),"%")) |> print()
cat("\n  其他补充：\n")
df |> filter(!is.na(q7_other)) |> pull(q7_other) |> walk(~cat(" •", .x, "\n"))

# Q8
divider("Q8 使用 AI 是否花更多时间")
lbl_q8 <- c(A="强烈同意", B="同意", C="中立", D="不同意", E="强烈不同意")
df |> filter(!is.na(q8)) |> count(q8) |>
  mutate(态度 = lbl_q8[q8], 占比 = paste0(round(n/sum(n)*100,1),"%")) |> print()

# Q9
divider("Q9 最希望被加速的任务排序（平均名次，1=最期待）")
mean_rank(df, "q9_ranking") |> print()

# Q10
divider("Q10 公司 AI 工具实际使用经验（多选）")
lbl_q10 <- c(A="通用任务用ChatGPT/Copilot", B="文档/邮件场景用M365 Copilot",
              C="区分Copilot Chat与Pre-built Agents", D="用过GitHub Copilot不同AI模式",
              E="通过instruction/MCP固化编码规范", F="用过Tools/MCP执行开发动作",
              G="了解Copilot Studio/Agent Builder", H="用过data42 Foundry AIP功能",
              I="了解/试用GAIN/Unify+AI", J="用过特定AI工具(CLIP/YSEOP等)",
              K="参与AQS部门AI工具开发")
expand_multi(df, "q10") |> count(value, sort=TRUE) |>
  mutate(选项 = lbl_q10[value], 占比 = paste0(round(n/nrow(df)*100,1),"%")) |> print()
cat("\n  其他补充：\n")
df |> filter(!is.na(q10_other)) |> pull(q10_other) |> walk(~cat(" •", .x, "\n"))

# Q11
divider("Q11 结构固定任务的工作方式偏好")
lbl_q11 <- c(A="按需局部使用", B="交互式生成", C="复用提示模板", D="固化为Agent/流程")
df |> filter(!is.na(q11)) |> count(q11) |>
  mutate(方式 = lbl_q11[q11], 占比 = paste0(round(n/sum(n)*100,1),"%")) |> print()

# Q12
divider("Q12 开放回答")
open <- df |> filter(!is.na(q12) & nchar(q12) > 0) |> pull(q12)
if (length(open) == 0) cat("  （无开放回答）\n")
else walk(seq_along(open), ~cat(sprintf("  [%d] %s\n", .x, open[[.x]])))

cat("\n", strrep("═", 55), "\n分析完成。\n\n")
