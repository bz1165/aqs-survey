# AQS AI 使用现状摸底问卷 — Shiny App
# 依赖: shiny, bslib, shinyjs, sortable

Sys.setlocale("LC_ALL", "en_US.UTF-8")

library(shiny)
library(bslib)
library(shinyjs)
library(sortable)

# ── 目录初始化 ─────────────────────────────────────────────────────────────────
RESPONSES_DIR <- "responses"
if (!dir.exists(RESPONSES_DIR)) dir.create(RESPONSES_DIR)

# 页面编号
# 0=封面 1=Q0 2=Q1 3=Q2 4=Q3 5=Q4 6=Q5 7=Q6 8=Q7 9=Q8 10=Q9 11=Q10 12=Q11 13=Q12 14=感谢
LAST_Q_PAGE   <- 13
THANKYOU_PAGE <- 14

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

# ── 自定义选择题控件（确保中文正确传入 DOM）────────────────────────────────────
# 通过 tags$span(lbl) 路径写入标签文本，与 h2() 等走相同的 htmltools 编码逻辑。
radio_hc <- function(inputId, choices_cn, selected = character(0)) {
  # choices_cn: 命名字符向量，name=显示文字，value=存储值
  labels <- enc2utf8(names(choices_cn))
  values <- unname(choices_cn)
  items  <- Map(function(lbl, val) {
    chk <- if (val %in% selected) list(checked = NA) else list()
    tags$div(class = "radio",
      tags$label(
        do.call(tags$input, c(list(type = "radio", name = inputId, value = val), chk)),
        tags$span(lbl)
      )
    )
  }, labels, values)
  div(id    = inputId,
      class = "shiny-input-radiogroup form-group shiny-input-container",
      tags$label(class = "control-label", id = paste0(inputId, "-label"), `for` = inputId),
      div(class = "shiny-options-group", tagList(items)))
}

check_hc <- function(inputId, choices_cn) {
  labels <- enc2utf8(names(choices_cn))
  values <- unname(choices_cn)
  items  <- Map(function(lbl, val) {
    tags$div(class = "checkbox",
      tags$label(
        tags$input(type = "checkbox", name = inputId, value = val),
        tags$span(lbl)
      )
    )
  }, labels, values)
  div(id    = inputId,
      class = "shiny-input-checkboxgroup form-group shiny-input-container",
      tags$label(class = "control-label", id = paste0(inputId, "-label"), `for` = inputId),
      div(class = "shiny-options-group", tagList(items)))
}

# ── 问卷内容常量 ───────────────────────────────────────────────────────────────
Q3_ITEMS <- enc2utf8(c(
  "统计方法 / 行业知识查询与解释",
  "代码写作或调试 (R / Python / SAS)",
  "文献检索与摘要",
  "邮件 / 会议纪要总结",
  "临床或分析报告起草",
  "演示材料 (PPT) 准备",
  "数据分析结果解读",
  "翻译与语言润色"
))

Q7_ITEMS <- enc2utf8(c(
  "输出存在错误或幻觉，需要人工核查（例如：AI 引用的文献不存在或信息不准确）",
  "AI 检索的文献不是最新的，存在知识截止日期问题",
  "AI 缺乏统计和制药领域专业知识，输出过于泛化",
  "内部 AI 工具功能受限，无法满足技术需求",
  "难以融入现有 workflow",
  "AI 对 SAS 语言的支持远不如对 R / Python 的支持",
  "图像识别准确度不够",
  "AI 输出结果的可复现性和可追溯性较低（例如：重复提问时，会收到不同的答案）",
  "当前对话次数过多，AI 会出现记忆丢失"
))

Q9_ITEMS <- enc2utf8(c(
  "一个面向 AQS 的知识库，针对实时更新的内部文档（如标准、spec、编程规范等）和外部行业指导原则回答相关问题",
  "基于文献的阅读，生成辅助学习的思维导图",
  "基于 SAP，生成 TFL shells 的候选列表",
  "SAS / R 程序注释生成与高效编码支持"
))

section_for_page <- function(p) {
  if (p == 1)              return("你的角色")
  if (p >= 2 && p <= 5)   return("A · AI 工具使用情况")
  if (p >= 6 && p <= 13)  return("B · 挑战、期待与失败案例")
  NULL
}

# ── UI 组件 ────────────────────────────────────────────────────────────────────
qcard <- function(qnum, qtype, question, content, hint = NULL) {
  div(class = "qcard",
    div(class = "qcard-header",
      span(class = "qnum-pill", qnum),
      span(class = "qtype-tag", qtype)
    ),
    h2(class = "qcard-title", question),
    if (!is.null(hint)) div(class = "qcard-hint", hint),
    div(class = "qcard-body", content)
  )
}

rank_ui <- function(input_id, items, other_lbl = "其他（请注明）：") {
  other_id <- paste0(input_id, "_other")
  tagList(
    div(class = "rank-instruction", tags$span("⇅"), "拖拽排序，最常用 / 最优先排最前"),
    rank_list(text = NULL, labels = items, input_id = input_id,
              options = sortable_options(animation = 150)),
    div(class = "other-field",
      tags$label(class = "other-lbl", other_lbl),
      textInput(other_id, label = NULL, placeholder = "请输入…")
    )
  )
}

# ── 各页内容 ───────────────────────────────────────────────────────────────────
intro_page <- function() {
  div(class = "intro-wrap",
    div(class = "intro-icon", "📊"),
    h1(class = "intro-h1", "AQS AI 使用现状摸底问卷"),
    div(class = "info-card",
      p("作为 AQS 部门的 AI Fluency Champion，我们正在为部门设计 2026 年的 AI 工具推广计划。这份问卷的目的是摸清部门当前的 AI 使用现状（Section A），让接下来的推广方案有真实数据支撑、有的放矢（Section B）。"),
      p("题目以选择题和排序题为主。问卷中的所有 AI 使用问题默认基于数据安全和合规的考量。"),
      div(class = "info-meta",
        div(class = "meta-row", span("⏱"), "预计耗时：6–8 分钟"),
        div(class = "meta-row", span("🔒"), "100% 匿名 — 不收集姓名、邮箱、IP 等任何身份信息"),
        div(class = "meta-row", span("📅"), "回收截止：请在截止日期前完成")
      )
    )
  )
}

q0_page <- function() {
  qcard("Q0", "单选",
    "你的当前角色是？",
    radio_hc("q0", enc2utf8(c(
      "A. 统计师"         = "A",
      "B. 统计程序员"     = "B",
      "C. 组长 / 部门管理者" = "C",
      "D. 其他角色"       = "D"
    )))
  )
}

q1_page <- function() {
  qcard("Q1", "多选",
    "你使用过下列哪些 AI 工具？",
    tagList(
      check_hc("q1", enc2utf8(c(
        "A. Internal ChatGPT（含嵌入的 Claude）"               = "A",
        "B. M365 Copilot（Teams / Word / PowerPoint 中嵌入的 Copilot）" = "B",
        "C. Microsoft Agent（Researcher, Analyst 等）"          = "C",
        "D. Coach Mira"                                        = "D",
        "E. GitHub Copilot / Cursor / 其他编程类 AI"            = "E",
        "F. 其他"                                              = "F",
        "G. 我目前几乎不使用任何 AI 工具"                        = "G"
      ))),
      conditionalPanel(
        "input.q1 !== null && input.q1.includes('F')",
        div(class = "cond-field",
          textInput("q1_other", "请注明：", placeholder = "请输入…")
        )
      )
    )
  )
}

q2_page <- function() {
  qcard("Q2", "单选",
    "你使用最频繁的 AI 工具是？",
    tagList(
      radio_hc("q2", enc2utf8(c(
        "A. Internal ChatGPT（含嵌入的 Claude）" = "A",
        "B. M365 Copilot"                       = "B",
        "C. Microsoft Agent"                    = "C",
        "D. Coach Mira"                         = "D",
        "E. GitHub Copilot / Cursor / 其他编程类 AI" = "E",
        "F. 其他"                               = "F",
        "G. 无 — 我目前几乎不使用 AI 工具（选此项将跳过 Q3、Q4）" = "G"
      ))),
      conditionalPanel(
        "input.q2 === 'F'",
        div(class = "cond-field",
          textInput("q2_other", "请注明：", placeholder = "请输入…")
        )
      )
    ),
    hint = "💡 如选 G，将自动跳过 Q3 和 Q4"
  )
}

q3_page <- function() {
  qcard("Q3", "排序",
    "针对你最常用的 AI 工具，请按使用频率排列下列任务（最常用排最前）",
    rank_ui("q3_ranking", Q3_ITEMS, "其他任务（请注明）：")
  )
}

q4_page <- function() {
  qcard("Q4", "单选",
    "你使用 AI 工具的总体频率是？",
    radio_hc("q4", enc2utf8(c(
      "A. 远超每日一次（没有 AI 几乎工作不下去）" = "A",
      "B. 至少每日一次"                         = "B",
      "C. 至少每周一次"                         = "C",
      "D. 每月或更少"                           = "D",
      "E. 视具体情况而定"                       = "E"
    ))),
    hint = "请按照平均使用习惯预估，向下选择。例如每日一次也可以是每日 2–3 次，但这种情况不属于"远超每日一次"。"
  )
}

q5_page <- function() {
  qcard("Q5", "单选 + 可选补充",
    "回想一下，最近一次你用 AI 完成工作任务时真正觉得它帮上了大忙（不是 refine wording 等简单工作）",
    tagList(
      radio_hc("q5", enc2utf8(c(
        "A. 文献（例如：统计方法，行业标准等）的查询和综述" = "A",
        "B. 代码写作和调试"                              = "B",
        "C. 专业文档撰写（例如：CSR / SAP 等）"          = "C",
        "D. 数据结果解读"                               = "D",
        "E. 学习新工具 / 新方法 / 新 SOP 等"             = "E",
        "G. 其他"                                      = "G",
        "H. 想不起来有这种时刻"                          = "H"
      ))),
      div(class = "other-field",
        tags$label(class = "other-lbl", "可选 — 用一句话描述这个任务（≤30 字）："),
        textInput("q5_text", NULL, placeholder = "请输入…")
      )
    )
  )
}

q6_page <- function() {
  qcard("Q6", "多选（最多 3 项）",
    "你曾尝试用 AI 完成、但效果不满意的任务是？",
    tagList(
      check_hc("q6", enc2utf8(c(
        "A. 需要文献（例如：统计方法，行业标准等）支持的写作（例如：publication、PPT 等）" = "A",
        "B. 需要专业领域知识的选择判断（例如：统计学方法选择，estimand 制定，统计编程模型设计等）" = "B",
        "D. 复杂的且有格式要求的文件起草（例如：CSR、SAP、TFL shell 等）" = "D",
        "E. 依赖实时和最新数据的任务（例如：行业指南更新对现存工作模式的影响）" = "E",
        "F. 其他"                                       = "F",
        "G. 无 — 没有遇到效果不满意的情况"               = "G"
      ))),
      conditionalPanel(
        "input.q6 !== null && input.q6.includes('F')",
        div(class = "cond-field",
          textInput("q6_other", "请注明：", placeholder = "请输入…")
        )
      ),
      uiOutput("q6_warn")
    )
  )
}

q7_page <- function() {
  qcard("Q7", "排序",
    "你在使用 AI 过程中遇到过哪些挑战？请按影响程度排列（影响最大排最前）",
    rank_ui("q7_ranking", Q7_ITEMS, "其他挑战（请注明）：")
  )
}

q8_page <- function() {
  qcard("Q8", "单选",
    tags$span(
      "你是否同意以下说法：",
      tags$br(),
      tags$em("「当使用 AI 替代我的日常工作时，我经常需要花更多的时间（例如，和 AI 沟通需求，调整 AI 输出的结果，做比人工产出的初稿更多的核查等）。」")
    ),
    radio_hc("q8", enc2utf8(c(
      "A. 强烈同意" = "A",
      "B. 同意"     = "B",
      "C. 中立"     = "C",
      "D. 不同意"   = "D",
      "E. 强烈不同意" = "E"
    )))
  )
}

q9_page <- function() {
  qcard("Q9", "排序",
    "在你的日常工作中，如果有一个具有针对性的 AI 新工具，最希望被加速的任务是？请按优先级排列（最期待排最前）",
    rank_ui("q9_ranking", Q9_ITEMS, "其他（请注明，也可以是对上面选项的细化）：")
  )
}

q10_page <- function() {
  qcard("Q10", "多选",
    "以下哪些说法符合你对公司 AI 工具的实际使用经验？",
    tagList(
      check_hc("q10", enc2utf8(c(
        "A. 日常翻译、润色、起草初稿等通用任务，我通常优先使用 Internal ChatGPT 和 Copilot 对话" = "A",
        "B. 当任务用到我自己的文档 / 邮件 / Teams 上下文时，我更倾向调用内嵌的 M365 Copilot 而不是把内容复制到聊天机器人" = "B",
        "C. 我区分 Copilot Chat 与 Pre-built Agents 的使用场景，并实际用过其中至少一个：Researcher / Analyst / Facilitator / Prompt Coach" = "C",
        "D. 我用过 VS Code 中 GitHub Copilot 的不同 AI 模式（例如：Edit / Agent / Plan）" = "D",
        "E. 我曾通过 instruction、skill、hook、MCP 或类似技术固定编码规范、步骤与约束，并集成到 VS Code Copilot 的 prompts 目录，实现跨任务的统一行为与可复现输出" = "E",
        "F. 我使用过 Tools / MCP 在开发环境中执行动作（例如：读取 / 修改文件、运行命令、搜索代码库等）" = "F",
        "G. 我了解或尝试过 Copilot Studio / Agent Builder 的功能（例如 connectors、workflow 等），并知道它适合哪些业务场景" = "G",
        "H. 我在 data42 Foundry 平台内使用过 AIP Assist 和 AIP developer capabilities 的不同功能（例如：completion model、embedding model、vision model）" = "H",
        "I. 我了解或试用过公司内部用于 GenAI 落地的平台（例如 GAIN、Unify+AI）" = "I",
        "J. 我使用（或参与测试）过具有特定功能的 AI Tools（如 CLIP / Protocol / YSEOP / QC Programmer / Novatron 等）" = "J",
        "K. 我了解或参与过 AQS 部门的 AI 工具开发（如 AI QC Programmer / QuantaBot / AI-assisted Modelling & Simulation 等）" = "K"
      ))),
      div(class = "other-field",
        tags$label(class = "other-lbl",
          "其他（请注明，可以是从未使用过任何一种，也可以是对上面选项的细化）："),
        textInput("q10_other", NULL, placeholder = "请输入…")
      )
    ),
    hint = "可多选，若以上均未使用，请在「其他」中注明。"
  )
}

q11_page <- function() {
  qcard("Q11", "单选",
    tags$span(
      "你需要定期产出一类「结构固定」的工作产物：",
      tags$br(), tags$br(),
      "例如每周发一封项目更新邮件（固定包含：本周所有的项目进展、风险、下周计划、需要支持），信息来源主要是近期的邮件 / 会议纪要 / 个人项目跟踪表。",
      tags$br(), tags$br(),
      "面对这种任务，你会考虑采用以下哪种做法？"
    ),
    radio_hc("q11", enc2utf8(c(
      "A. 按需局部使用：通常以人工方式完成整体结构与内容组织，仅在需要的环节调用 AI（例如用来设计汇总格式、润色语气、压缩字数、或把已写好的内容改成更清晰的表达）" = "A",
      "B. 交互式生成：在 M365 Copilot / Internal ChatGPT 中根据材料写一段 prompt 生成初稿，并在同一轮对话中迭代修改" = "B",
      "C. 复用「提示模板」：设计并保存可复用模板，以后每次直接调用同一模板，只替换本周输入材料与关键要点" = "C",
      "D. 固化为 Agent / 流程：将该任务固化为更可复用的功能（例如使用 Pre-built Agents 做「检索 + 总结」，或用 Copilot Studio / 工作流把步骤串起来），按预设步骤完成「汇集信息 → 生成初稿 → 提取行动项 / 待决策点」" = "D"
    )))
  )
}

q12_page <- function() {
  qcard("Q12", "开放问题（选填）",
    "你任何关于 AI 使用的相关想法，没有被前面的问题所覆盖，请在这里留下：",
    textAreaInput("q12", NULL,
      placeholder = "请在此输入…",
      rows = 5, width = "100%"
    )
  )
}

thankyou_page <- function() {
  div(class = "ty-wrap",
    div(class = "ty-icon", "🎉"),
    h2(class = "ty-h2", "感谢你的填写！"),
    div(class = "info-card",
      p("回收的结果将在 4 月底以匿名汇总的形式与全部门同步。"),
      tags$hr(),
      tags$strong("📢 关于后续参与机会（与本问卷无关）"),
      p("这份问卷是 100% 匿名的，我们无法从问卷里直接联系任何同事。如果你对以下任何一件事感兴趣，欢迎直接私信 Mingmei 或 Beichen（私信不会与你的问卷答案以任何方式关联）："),
      tags$ul(
        tags$li("① 成为 AQS Use Case 的贡献者（4–6 月持续收集，每位贡献者署名并参与公司评奖）"),
        tags$li("② 参加 AQS AI Workshop"),
        tags$li("③ 加入 Pilot 项目的技术小组（5 月起，需有一定编程经验）")
      ),
      p(style = "margin-top:12px;",
        "📅 从下周开始每周三 11:30 在 Teams 频道发布 AQS AI Weekly Tip，欢迎留意。")
    )
  )
}

# ── 验证 ───────────────────────────────────────────────────────────────────────
validate_page <- function(p, input) {
  need <- "此题为必答题，请选择后继续。"
  ok   <- function() list(ok = TRUE,  msg = "")
  fail <- function(m) list(ok = FALSE, msg = m)

  if      (p == 1) { if (is.null(input$q0)  || input$q0 == "")            return(fail(need)) }
  else if (p == 2) { if (is.null(input$q1)  || length(input$q1) == 0)     return(fail(need)) }
  else if (p == 3) { if (is.null(input$q2)  || input$q2 == "")            return(fail(need)) }
  else if (p == 5) { if (is.null(input$q4)  || input$q4 == "")            return(fail(need)) }
  else if (p == 6) { if (is.null(input$q5)  || input$q5 == "")            return(fail(need)) }
  else if (p == 7) {
    if (is.null(input$q6) || length(input$q6) == 0) return(fail(need))
    if (length(input$q6) > 3) return(fail("Q6 最多选择 3 项，请重新选择。"))
  }
  else if (p == 9)  { if (is.null(input$q8)  || input$q8 == "")           return(fail(need)) }
  else if (p == 12) { if (is.null(input$q11) || input$q11 == "")          return(fail(need)) }
  ok()
}

# ── 数据保存 ───────────────────────────────────────────────────────────────────
save_page <- function(p, input, rv) {
  if (p == 1)  { rv$q0  <- input$q0 }
  if (p == 2)  { rv$q1  <- input$q1;           rv$q1_other  <- input$q1_other }
  if (p == 3)  { rv$q2  <- input$q2;           rv$q2_other  <- input$q2_other }
  if (p == 4)  { rv$q3  <- input$q3_ranking;   rv$q3_other  <- input$q3_ranking_other }
  if (p == 5)  { rv$q4  <- input$q4 }
  if (p == 6)  { rv$q5  <- input$q5;           rv$q5_text   <- input$q5_text }
  if (p == 7)  { rv$q6  <- input$q6;           rv$q6_other  <- input$q6_other }
  if (p == 8)  { rv$q7  <- input$q7_ranking;   rv$q7_other  <- input$q7_ranking_other }
  if (p == 9)  { rv$q8  <- input$q8 }
  if (p == 10) { rv$q9  <- input$q9_ranking;   rv$q9_other  <- input$q9_ranking_other }
  if (p == 11) { rv$q10 <- input$q10;          rv$q10_other <- input$q10_other }
  if (p == 12) { rv$q11 <- input$q11 }
  if (p == 13) { rv$q12 <- input$q12 }
}

write_response <- function(rv) {
  scalar   <- function(x) if (is.null(x) || length(x) == 0) NA_character_ else as.character(x[[1]])
  collapse <- function(x) if (is.null(x) || length(x) == 0) NA_character_ else paste(x, collapse = " | ")
  clean    <- function(x) { v <- trimws(scalar(x)); if (nchar(v) == 0) NA_character_ else v }

  df <- data.frame(
    timestamp   = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    q0          = scalar(rv$q0),
    q1          = collapse(rv$q1),
    q1_other    = clean(rv$q1_other),
    q2          = scalar(rv$q2),
    q2_other    = clean(rv$q2_other),
    q3_ranking  = collapse(rv$q3),
    q3_other    = clean(rv$q3_other),
    q4          = scalar(rv$q4),
    q5          = scalar(rv$q5),
    q5_text     = clean(rv$q5_text),
    q6          = collapse(rv$q6),
    q6_other    = clean(rv$q6_other),
    q7_ranking  = collapse(rv$q7),
    q7_other    = clean(rv$q7_other),
    q8          = scalar(rv$q8),
    q9_ranking  = collapse(rv$q9),
    q9_other    = clean(rv$q9_other),
    q10         = collapse(rv$q10),
    q10_other   = clean(rv$q10_other),
    q11         = scalar(rv$q11),
    q12         = clean(rv$q12),
    stringsAsFactors = FALSE
  )
  fn <- file.path(RESPONSES_DIR,
                  paste0("resp_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))
  saveRDS(df, fn)
}

# ── UI ─────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  title = "AQS AI 使用现状摸底问卷",
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", href = "style.css"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$meta(charset = "UTF-8")
  ),
  div(class = "page-shell",
    div(class = "top-bar",
      div(class = "top-bar-left",
        span(class = "aqs-badge", "AQS"),
        span(class = "top-bar-title", "AI 使用现状摸底问卷 2026")
      ),
      uiOutput("top_bar_right")
    ),
    div(class = "survey-body",
      uiOutput("progress_ui"),
      uiOutput("section_ui"),
      div(id = "q-area", uiOutput("question_ui")),
      uiOutput("nav_ui")
    )
  )
)

# ── Server ─────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  page <- reactiveVal(0)
  rv   <- reactiveValues()

  output$top_bar_right <- renderUI({
    p <- page()
    if (p %in% c(0, THANKYOU_PAGE)) return(NULL)
    span(class = "top-bar-counter", paste0("Q", p - 1, " / Q12"))
  })

  output$progress_ui <- renderUI({
    p <- page()
    if (p %in% c(0, THANKYOU_PAGE)) return(NULL)
    pct <- round((p - 1) / LAST_Q_PAGE * 100)
    div(class = "prog-wrap",
      div(class = "prog-track",
        div(class = "prog-fill", style = paste0("width:", pct, "%;"))
      ),
      span(class = "prog-pct", paste0(pct, "%"))
    )
  })

  output$section_ui <- renderUI({
    lbl <- section_for_page(page())
    if (is.null(lbl)) return(NULL)
    div(class = "section-badge", lbl)
  })

  output$question_ui <- renderUI({
    switch(as.character(page()),
      "0"  = intro_page(),
      "1"  = q0_page(),
      "2"  = q1_page(),
      "3"  = q2_page(),
      "4"  = q3_page(),
      "5"  = q4_page(),
      "6"  = q5_page(),
      "7"  = q6_page(),
      "8"  = q7_page(),
      "9"  = q8_page(),
      "10" = q9_page(),
      "11" = q10_page(),
      "12" = q11_page(),
      "13" = q12_page(),
      "14" = thankyou_page()
    )
  })

  output$q6_warn <- renderUI({
    if (!is.null(input$q6) && length(input$q6) > 3)
      div(class = "warn-msg", "⚠️ 最多选择 3 项，请重新选择。")
  })

  output$nav_ui <- renderUI({
    p <- page()
    if (p == THANKYOU_PAGE) return(NULL)
    div(class = "nav-row",
      if (p > 1) actionButton("btn_back", "← 上一题", class = "btn-back"),
      div(class = "nav-spacer"),
      actionButton("btn_next",
        label = if (p == 0) "开始填写 →"
                else if (p == LAST_Q_PAGE) "提交 ✓"
                else "下一题 →",
        class = if (p == LAST_Q_PAGE) "btn-submit" else "btn-next"
      )
    )
  })

  observeEvent(input$btn_next, {
    p <- page()
    if (p == 0) { page(1); scroll_top(); return() }

    v <- validate_page(p, input)
    if (!v$ok) { showNotification(v$msg, type = "warning", duration = 4); return() }

    save_page(p, input, rv)

    next_p <- if (p == 3 && isTRUE(input$q2 == "G")) {
      6
    } else if (p == LAST_Q_PAGE) {
      write_response(rv); THANKYOU_PAGE
    } else {
      p + 1
    }

    page(next_p)
    scroll_top()
  })

  observeEvent(input$btn_back, {
    p <- page()
    prev_p <- if (p == 6 && isTRUE(rv$q2 == "G")) 3 else p - 1
    if (prev_p >= 1) { page(prev_p); scroll_top() }
  })
}

scroll_top <- function() runjs("window.scrollTo({top:0,behavior:'smooth'});")

shinyApp(ui = ui, server = server)
