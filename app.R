# AQS AI 使用现状摸底问卷 — Posit Connect 测试版（修复 clean + 改数据库路径）
try(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"), silent = TRUE)

library(shiny)
library(bslib)
library(shinyjs)
library(sortable)
library(DBI)
library(RSQLite)

ADMIN_KEY     <- "aqs2026admin"
LAST_Q_PAGE   <- 13
THANKYOU_PAGE <- 14

DATA_DIR <- file.path(path.expand("~"), "aqs_survey_data")
dir.create(DATA_DIR, recursive = TRUE, showWarnings = FALSE)

DB_PATH <- file.path(DATA_DIR, "responses.sqlite")

message("=== APP START ===")
message("getwd(): ", getwd())
message("HOME: ", path.expand("~"))
message("DATA_DIR: ", DATA_DIR)
message("DATA_DIR exists: ", dir.exists(DATA_DIR))
message("DATA_DIR writable: ", file.access(DATA_DIR, 2) == 0)
message("DB_PATH: ", DB_PATH)
message("tempdir(): ", tempdir())
message("files in getwd(): ", paste(list.files(".", all.files = TRUE), collapse = ", "))

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

sv <- function(saved, key, default = NULL) {
  val <- saved[[key]]
  if (is.null(val) || length(val) == 0) return(default)
  if (is.list(val)) unlist(val) else val
}

restore_rank <- function(saved_order, all_items) {
  if (is.null(saved_order) || length(saved_order) == 0) return(all_items)
  s <- as.character(unlist(saved_order))
  c(s[s %in% all_items], all_items[!all_items %in% s])
}

clear_rv <- function(rv) {
  nm <- c(
    "q0","q1","q1_other","q2","q2_other","q3","q3_other","q4",
    "q5","q5_other_text","q5_text","q6","q6_other","q7","q7_other",
    "q8","q9","q9_other","q10","q10_other","q11","q12"
  )
  for (x in nm) rv[[x]] <- NULL
}

init_db <- function() {
  con <- dbConnect(SQLite(), DB_PATH)
  on.exit(dbDisconnect(con), add = TRUE)

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS responses (
      response_id INTEGER PRIMARY KEY AUTOINCREMENT,
      submitted_at TEXT,
      q0 TEXT,
      q1 TEXT,
      q1_other TEXT,
      q2 TEXT,
      q2_other TEXT,
      q3_ranking TEXT,
      q3_other TEXT,
      q4 TEXT,
      q5 TEXT,
      q5_other_text TEXT,
      q5_text TEXT,
      q6 TEXT,
      q6_other TEXT,
      q7 TEXT,
      q7_other TEXT,
      q8 TEXT,
      q9_ranking TEXT,
      q9_other TEXT,
      q10 TEXT,
      q10_other TEXT,
      q11 TEXT,
      q12 TEXT
    )
  ")
}

load_all_responses <- function() {
  if (!file.exists(DB_PATH)) return(data.frame())
  con <- dbConnect(SQLite(), DB_PATH)
  on.exit(dbDisconnect(con), add = TRUE)
  tryCatch(
    dbReadTable(con, "responses"),
    error = function(e) {
      message("load_all_responses() failed: ", e$message)
      data.frame()
    }
  )
}

write_response <- function(rv) {
  scalar <- function(x) {
    if (is.null(x) || length(x) == 0) return(NA_character_)
    as.character(x[[1]])
  }

  collapse <- function(x) {
    if (is.null(x) || length(x) == 0) return(NA_character_)
    paste(x, collapse = " | ")
  }

  clean <- function(x) {
    v <- scalar(x)
    if (is.null(v) || length(v) == 0 || is.na(v)) return(NA_character_)
    v <- trimws(v)
    if (!nzchar(v)) NA_character_ else v
  }

  df <- data.frame(
    submitted_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    q0 = scalar(rv$q0),
    q1 = collapse(rv$q1),
    q1_other = clean(rv$q1_other),
    q2 = scalar(rv$q2),
    q2_other = clean(rv$q2_other),
    q3_ranking = collapse(rv$q3),
    q3_other = clean(rv$q3_other),
    q4 = scalar(rv$q4),
    q5 = scalar(rv$q5),
    q5_other_text = clean(rv$q5_other_text),
    q5_text = clean(rv$q5_text),
    q6 = collapse(rv$q6),
    q6_other = clean(rv$q6_other),
    q7 = collapse(rv$q7),
    q7_other = clean(rv$q7_other),
    q8 = scalar(rv$q8),
    q9_ranking = collapse(rv$q9),
    q9_other = clean(rv$q9_other),
    q10 = collapse(rv$q10),
    q10_other = clean(rv$q10_other),
    q11 = scalar(rv$q11),
    q12 = clean(rv$q12),
    stringsAsFactors = FALSE
  )

  tryCatch({
    init_db()
    con <- dbConnect(SQLite(), DB_PATH)
    on.exit(dbDisconnect(con), add = TRUE)
    dbWriteTable(con, "responses", df, append = TRUE, row.names = FALSE)
    message("✓ SQLite write success: ", DB_PATH)
    TRUE
  }, error = function(e) {
    message("✗ SQLite write failed: ", e$message)
    FALSE
  })
}

radio_hc <- function(inputId, choices_cn, selected = character(0)) {
  labels <- enc2utf8(names(choices_cn))
  values <- unname(choices_cn)

  items <- Map(function(lbl, val) {
    chk <- if (val %in% selected) list(checked = NA) else list()
    tags$div(
      class = "radio",
      tags$label(
        do.call(tags$input, c(list(type = "radio", name = inputId, value = val), chk)),
        tags$span(lbl)
      )
    )
  }, labels, values)

  div(
    id = inputId,
    class = "shiny-input-radiogroup form-group shiny-input-container",
    tags$label(class = "control-label", id = paste0(inputId, "-label"), `for` = inputId),
    div(class = "shiny-options-group", tagList(items))
  )
}

check_hc <- function(inputId, choices_cn, checked = character(0)) {
  labels <- enc2utf8(names(choices_cn))
  values <- unname(choices_cn)

  items <- Map(function(lbl, val) {
    is_chk <- val %in% checked
    tags$div(
      class = "checkbox",
      tags$label(
        do.call(tags$input, c(list(type = "checkbox", name = inputId, value = val),
                              if (is_chk) list(checked = NA) else list())),
        tags$span(lbl)
      )
    )
  }, labels, values)

  div(
    id = inputId,
    class = "shiny-input-checkboxgroup form-group shiny-input-container",
    tags$label(class = "control-label", id = paste0(inputId, "-label"), `for` = inputId),
    div(class = "shiny-options-group", tagList(items))
  )
}

rank_ui <- function(input_id, items, other_lbl = NULL, other_val = "") {
  other_id <- paste0(input_id, "_other")
  tagList(
    div(class = "rank-instruction", tags$span("⇅"), " 拖拽排序，最常用 / 最优先排最前"),
    rank_list(
      text = NULL,
      labels = items,
      input_id = input_id,
      options = sortable_options(animation = 150)
    ),
    if (!is.null(other_lbl)) {
      div(
        class = "other-field",
        tags$label(class = "other-lbl", other_lbl),
        textInput(other_id, label = NULL, value = other_val, placeholder = "请输入…")
      )
    }
  )
}

Q3_ITEMS <- enc2utf8(c(
  "统计方法 / 行业知识查询与解释",
  "代码写作或调试 (R / Python / SAS)",
  "文献检索与摘要",
  "邮件 / 会议纪要总结",
  "临床或分析报告起草",
  "演示材料 (PPT) 准备",
  "数据分析结果解读",
  "翻译与语言润色",
  "其他（默认无，如有请注明）"
))

Q7_CHOICES <- enc2utf8(c(
  "A. 输出存在错误或幻觉，需要人工核查（例如：AI 引用的文献不存在或信息不准确）" = "A",
  "B. AI 缺乏统计和制药领域专业知识，输出过于泛化" = "B",
  "C. 内部 AI 工具功能受限，无法满足技术需求" = "C",
  "D. 难以融入现有 workflow" = "D",
  "E. AI 对 SAS 语言的支持远不如对 R / Python 的支持" = "E",
  "F. AI 输出结果的可复现性和可追溯性较低（例如：重复提问时，会收到不同的答案）" = "F",
  "G. 当前对话次数过多，AI 会出现记忆丢失" = "G",
  "H. 其他" = "H"
))

Q9_ITEMS <- enc2utf8(c(
  "一个面向 AQS 的知识库，针对实时更新的内部文档（如标准、spec、编程规范等）和外部行业指导原则回答相关问题",
  "基于文献的阅读，生成辅助学习的思维导图",
  "基于 SAP，生成 TFL shells 的候选列表",
  "SAS / R 程序注释生成与高效编码支持"
))

section_for_page <- function(p) {
  if (p == 1) return("你的角色")
  if (p >= 2 && p <= 5) return("A · AI 工具使用情况")
  if (p >= 6 && p <= 13) return("B · 挑战、期待与失败案例")
  NULL
}

qcard <- function(qnum, qtype, question, content, hint = NULL) {
  div(
    class = "qcard",
    div(class = "qcard-header", span(class = "qnum-pill", qnum), span(class = "qtype-tag", qtype)),
    h2(class = "qcard-title", question),
    if (!is.null(hint)) div(class = "qcard-hint", hint),
    div(class = "qcard-body", content)
  )
}

intro_page <- function() {
  div(
    class = "intro-wrap",
    div(class = "intro-icon", "📊"),
    h1(class = "intro-h1", "AQS AI 使用现状摸底问卷"),
    div(
      class = "info-card",
      p("作为 AQS 部门的 AI Fluency Champion，我们正在为部门设计 2026 年的 AI 工具的学习和应用推广计划。这份问卷的目的是摸清部门当前的 AI 使用现状（Section A），让接下来的推广方案有真实数据支撑、有的放矢（Section B）。"),
      p("预计耗时：6–8 分钟。共 12 道题目，以选择题和排序题为主。问卷中所有 AI 使用问题默认为基于数据安全和合规的考量的公司内部 AI 工具。"),
      div(
        class = "info-meta",
        div(class = "meta-row", span("🔒"), "100% 匿名 — 不收集姓名、邮箱、IP 等任何身份信息"),
        div(class = "meta-row", span("📅"), "回收截止：请在截止日期前完成")
      )
    )
  )
}

q0_page <- function(s = list()) {
  qcard("Q0","单选","你的当前角色是？",
        radio_hc("q0", enc2utf8(c(
          "A. 统计师"="A",
          "B. 统计程序员"="B",
          "C. 组长 / 部门管理者"="C",
          "D. 其他角色"="D"
        )), selected=sv(s,"q0",character(0))))
}

q1_page <- function(s = list()) {
  qcard("Q1","多选","你使用过下列哪些 AI 工具？",
        tagList(
          check_hc("q1", enc2utf8(c(
            "A. Internal ChatGPT（含嵌入的 Claude）"="A",
            "B. M365 Copilot（Teams / Word / PowerPoint 中嵌入的 Copilot）"="B",
            "C. Microsoft Agent（Researcher, Analyst 等）"="C",
            "D. Coach Mira"="D",
            "E. GitHub Copilot / Cursor / 其他编程类 AI"="E",
            "F. 其他"="F",
            "G. 我目前几乎不使用任何 AI 工具"="G"
          )), checked=sv(s,"q1",character(0))),
          conditionalPanel("input.q1 !== null && input.q1.includes('F')",
                           div(class="cond-field",
                               textInput("q1_other","请注明：",value=sv(s,"q1_other",""),placeholder="请输入…")))
        ))
}

q2_page <- function(s = list()) {
  qcard("Q2","单选","你使用最频繁的 AI 工具是？",
        tagList(
          radio_hc("q2", enc2utf8(c(
            "A. Internal ChatGPT（含嵌入的 Claude）"="A",
            "B. M365 Copilot"="B",
            "C. Microsoft Agent"="C",
            "D. Coach Mira"="D",
            "E. GitHub Copilot / Cursor / 其他编程类 AI"="E",
            "F. 其他"="F",
            "G. 无 — 我目前几乎不使用 AI 工具（选此项将跳过 Q3、Q4）"="G"
          )), selected=sv(s,"q2",character(0))),
          conditionalPanel("input.q2 === 'F'",
                           div(class="cond-field",
                               textInput("q2_other","请注明：",value=sv(s,"q2_other",""),placeholder="请输入…")))
        ),
        hint="💡 如选 G，将自动跳过 Q3 和 Q4")
}

q3_page <- function(s=list()) {
  items <- restore_rank(sv(s,"q3",NULL), Q3_ITEMS)
  qcard("Q3","排序","针对你最常用的 AI 工具，请按使用频率排列下列任务（最常用排最前）",
        rank_ui("q3_ranking", items, "如有其他，请注明内容：", sv(s,"q3_other","")))
}

q4_page <- function(s=list()) {
  qcard("Q4","单选","你使用 AI 工具的总体频率是？",
        radio_hc("q4", enc2utf8(c(
          "A. 远超每日一次（没有 AI 几乎工作不下去）"="A",
          "B. 至少每日一次"="B",
          "C. 至少每周一次"="C",
          "D. 每月或更少"="D",
          "E. 视具体情况而定"="E"
        )), selected=sv(s,"q4",character(0))),
        hint="请按照平均使用习惯预估，向下选择。例如每日一次也可以是每日 2–3 次，但这种情况不属于「远超每日一次」。")
}

q5_page <- function(s=list()) {
  qcard("Q5","单选 + 可选补充",
        "回想一下，最近一次你用 AI 完成工作任务时真正觉得它帮上了大忙（不是 refine wording 等简单工作）",
        tagList(
          radio_hc("q5", enc2utf8(c(
            "A. 文献（例如：统计方法，行业标准等）的查询和综述"="A",
            "B. 代码写作和调试"="B",
            "C. 专业文档撰写（例如：CSR / SAP 等）"="C",
            "D. 数据结果解读"="D",
            "E. 学习新工具 / 新方法 / 新 SOP 等"="E",
            "G. 其他"="G",
            "H. 想不起来有这种时刻"="H"
          )), selected=sv(s,"q5",character(0))),
          conditionalPanel("input.q5 === 'G'",
                           div(class="cond-field",
                               textInput("q5_other_text","请注明：",value=sv(s,"q5_other_text",""),placeholder="请输入…"))),
          div(class="other-field",
              tags$label(class="other-lbl","可选 — 用一句话描述这个任务（≤30 字）："),
              textInput("q5_text",NULL,value=sv(s,"q5_text",""),placeholder="请输入…"))
        ))
}

q6_page <- function(s=list()) {
  qcard("Q6","多选（最多 3 项）","你曾尝试用 AI 完成、但效果不满意的任务是？",
        tagList(
          check_hc("q6", enc2utf8(c(
            "A. 需要文献（例如：统计方法，行业标准等）支持的写作（例如：publication、PPT 等）"="A",
            "B. 需要专业领域知识的选择判断（例如：统计学方法选择，estimand 制定，统计编程模型设计等）"="B",
            "D. 复杂的且有格式要求的文件起草（例如：CSR、SAP、TFL shell 等）"="D",
            "E. 依赖实时和最新数据的任务（例如：行业指南更新对现存工作模式的影响）"="E",
            "F. 其他"="F",
            "G. 无 — 没有遇到效果不满意的情况"="G"
          )), checked=sv(s,"q6",character(0))),
          conditionalPanel("input.q6 !== null && input.q6.includes('F')",
                           div(class="cond-field",
                               textInput("q6_other","请注明：",value=sv(s,"q6_other",""),placeholder="请输入…"))),
          uiOutput("q6_warn")
        ))
}

q7_page <- function(s=list()) {
  qcard("Q7","多选（最多 3 项）","你在使用 AI 过程中遇到过哪些挑战？（最多选 3 项）",
        tagList(
          check_hc("q7", Q7_CHOICES, checked=sv(s,"q7",character(0))),
          conditionalPanel("input.q7 !== null && input.q7.includes('H')",
                           div(class="cond-field",
                               textInput("q7_other","请注明：",value=sv(s,"q7_other",""),placeholder="请输入…"))),
          uiOutput("q7_warn")
        ))
}

q8_page <- function(s=list()) {
  qcard("Q8","单选",
        tags$span("你是否同意以下说法：", tags$br(),
                  tags$em("「当使用 AI 替代我的日常工作时，我经常需要花更多的时间（例如，和 AI 沟通需求，调整 AI 输出的结果，做比人工产出的初稿更多的核查等）。」")),
        radio_hc("q8", enc2utf8(c(
          "A. 强烈同意"="A",
          "B. 同意"="B",
          "C. 中立"="C",
          "D. 不同意"="D",
          "E. 强烈不同意"="E"
        )), selected=sv(s,"q8",character(0))))
}

q9_page <- function(s=list()) {
  items <- restore_rank(sv(s,"q9",NULL), Q9_ITEMS)
  qcard("Q9","排序",
        "在你的日常工作中，如果有一个具有针对性的 AI 新工具，最希望被加速的任务是？请按优先级排列（最期待排最前）",
        rank_ui("q9_ranking", items, "其他（请注明，也可以是对上面选项的细化）：",
                sv(s,"q9_other","")))
}

q10_page <- function(s=list()) {
  qcard("Q10","多选","以下哪些说法符合你对公司 AI 工具的实际使用经验？",
        tagList(
          check_hc("q10", enc2utf8(c(
            "A. 日常翻译、润色、起草初稿等通用任务，我通常优先使用 Internal ChatGPT 和 Copilot 对话"="A",
            "B. 当任务用到我自己的文档 / 邮件 / Teams 上下文时，我更倾向调用内嵌的 M365 Copilot 而不是把内容复制到聊天机器人"="B",
            "C. 我区分 Copilot Chat 与 Pre-built Agents 的使用场景，并实际用过其中至少一个：Researcher / Analyst / Facilitator / Prompt Coach"="C",
            "D. 我用过 VS Code 中 GitHub Copilot 的不同 AI 模式（例如：Edit / Agent / Plan）"="D",
            "E. 我曾通过 instruction、skill、hook、MCP 或类似技术固定编码规范、步骤与约束，并集成到 VS Code Copilot 的 prompts 目录，实现跨任务的统一行为与可复现输出"="E",
            "F. 我使用过 Tools / MCP 在开发环境中执行动作（例如：读取 / 修改文件、运行命令、搜索代码库等）"="F",
            "G. 我了解且尝试过 Copilot Studio / Agent Builder 的功能（例如 connectors、workflow 等），并知道它适合哪些业务场景"="G",
            "H. 我在 data42 Foundry 平台内使用过 AIP Assist 和 AIP developer capabilities 的不同功能（例如：completion model、embedding model、vision model）"="H",
            "I. 我了解且试用过公司内部用于 GenAI 落地的平台（例如 GAIN、Unify+AI）"="I",
            "J. 我使用（或参与测试）过具有特定功能的 AI Tools（如 CLIP / Protocol / YSEOP / QC Programmer / Novatron 等）"="J",
            "K. 我了解且参与过 AQS 部门的 AI 工具开发（如 AI QC Programmer / QuantaBot / AI-assisted Modelling & Simulation 等）"="K"
          )), checked=sv(s,"q10",character(0))),
          div(class="other-field",
              tags$label(class="other-lbl","其他（请注明，可以是从未使用过任何一种，也可以是对上面选项的细化）："),
              textInput("q10_other",NULL,value=sv(s,"q10_other",""),placeholder="请输入…"))
        ),
        hint="可多选，若以上均未使用，请在「其他」中注明。")
}

q11_page <- function(s=list()) {
  qcard("Q11","单选",
        tags$span(
          "你需要定期产出一类「结构固定」的工作产物：",tags$br(),tags$br(),
          "例如每周发一封项目更新邮件（固定包含：本周所有的项目进展、风险、下周计划、需要支持），信息来源主要是近期的邮件 / 会议纪要 / 个人项目跟踪表。",
          tags$br(),tags$br(),"面对这种任务，你会考虑采用以下哪种做法？"),
        radio_hc("q11", enc2utf8(c(
          "A. 按需局部使用：通常以人工方式完成整体结构与内容组织，仅在需要的环节调用 AI（例如用来设计汇总格式、润色语气、压缩字数、或把已写好的内容改成更清晰的表达）"="A",
          "B. 交互式生成：在 M365 Copilot / Internal ChatGPT 中根据材料写一段 prompt 生成初稿，并在同一轮对话中迭代修改"="B",
          "C. 复用「提示模板」：设计并保存可复用模板，以后每次直接调用同一模板，只替换本周输入材料与关键要点"="C",
          "D. 固化为 Agent / 流程：将该任务固化为更可复用的功能（例如使用 Pre-built Agents 做「检索 + 总结」，或用 Copilot Studio / 工作流把步骤串起来），按预设步骤完成「汇集信息 → 生成初稿 → 提取行动项 / 待决策点」"="D"
        )), selected=sv(s,"q11",character(0))))
}

q12_page <- function(s=list()) {
  qcard("Q12","开放问题（选填）",
        "欢迎您在此分享最感兴趣、最希望学习的 AI 使用相关知识，或提出你对 AI 工具使用与推广方面的任何想法和建议。",
        textAreaInput("q12",NULL,value=sv(s,"q12",""),
                      placeholder="请在此输入…",rows=5,width="100%"))
}

thankyou_page <- function() {
  div(class="ty-wrap",
      div(class="ty-icon","🎉"),
      h2(class="ty-h2","感谢你的填写！"),
      div(class="info-card",
          p("回收的结果将在 4 月底以匿名汇总的形式与全部门同步。"),
          tags$hr(),
          tags$strong("📢 关于后续参与机会（与本问卷无关）"),
          p("这份问卷是 100% 匿名的，我们无法从问卷里直接联系任何同事。如果你对以下任何一件事感兴趣，欢迎直接私信 Mingmei 或 Beichen："),
          tags$ul(
            tags$li("① 成为 AQS Use Case 的贡献者（4–6 月持续收集，每位贡献者署名并参与公司评奖）"),
            tags$li("② 参加 AQS AI Workshop"),
            tags$li("③ 加入 Pilot 项目的技术小组（5 月起，需有一定编程经验）")),
          p(style="margin-top:12px;",
            "📅 从下周开始每周三 11:30 在 Teams 频道发布 AQS AI Weekly Tip，欢迎留意。")))
}

validate_page <- function(p, input) {
  need <- "此题为必答题，请选择后继续。"
  ok   <- function() list(ok=TRUE, msg="")
  fail <- function(m) list(ok=FALSE, msg=m)
  if      (p== 1){if(is.null(input$q0) ||input$q0 =="")           return(fail(need))}
  else if (p== 2){if(is.null(input$q1) ||length(input$q1)==0)     return(fail(need))}
  else if (p== 3){if(is.null(input$q2) ||input$q2 =="")           return(fail(need))}
  else if (p== 5){if(is.null(input$q4) ||input$q4 =="")           return(fail(need))}
  else if (p== 6){if(is.null(input$q5) ||input$q5 =="")           return(fail(need))}
  else if (p== 7){
    if(is.null(input$q6)||length(input$q6)==0) return(fail(need))
    if(length(input$q6)>3) return(fail("Q6 最多选择 3 项，请重新选择。"))}
  else if (p== 8){
    if(is.null(input$q7)||length(input$q7)==0) return(fail(need))
    if(length(input$q7)>3) return(fail("Q7 最多选择 3 项，请重新选择。"))}
  else if (p== 9){if(is.null(input$q8) ||input$q8 =="")           return(fail(need))}
  else if (p==12){if(is.null(input$q11)||input$q11=="")           return(fail(need))}
  ok()
}

save_page <- function(p, input, rv) {
  if (p== 1){rv$q0 <- input$q0}
  if (p== 2){rv$q1 <- input$q1;          rv$q1_other      <- input$q1_other}
  if (p== 3){rv$q2 <- input$q2;          rv$q2_other      <- input$q2_other}
  if (p== 4){rv$q3 <- input$q3_ranking;  rv$q3_other      <- input$q3_ranking_other}
  if (p== 5){rv$q4 <- input$q4}
  if (p== 6){rv$q5 <- input$q5;          rv$q5_other_text <- input$q5_other_text; rv$q5_text <- input$q5_text}
  if (p== 7){rv$q6 <- input$q6;          rv$q6_other      <- input$q6_other}
  if (p== 8){rv$q7 <- input$q7;          rv$q7_other      <- input$q7_other}
  if (p== 9){rv$q8 <- input$q8}
  if (p==10){rv$q9 <- input$q9_ranking;  rv$q9_other      <- input$q9_ranking_other}
  if (p==11){rv$q10 <- input$q10;        rv$q10_other     <- input$q10_other}
  if (p==12){rv$q11 <- input$q11}
  if (p==13){rv$q12 <- input$q12}
}

ui <- fluidPage(
  title="AQS AI 使用现状摸底问卷",
  useShinyjs(),
  tags$head(
    tags$link(rel="stylesheet", href="style.css"),
    tags$meta(name="viewport", content="width=device-width, initial-scale=1"),
    tags$meta(charset="UTF-8"))
)

server <- function(input, output, session) {}

shinyApp(ui = ui, server = server)
