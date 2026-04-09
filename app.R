# AQS AI Usage Baseline Survey - Shiny App
# Dependencies: shiny, bslib, shinyjs, sortable

Sys.setlocale("LC_ALL", "en_US.UTF-8")

library(shiny)
library(bslib)
library(shinyjs)
library(sortable)

# ── Setup ─────────────────────────────────────────────────────────────────────
RESPONSES_DIR <- "responses"
if (!dir.exists(RESPONSES_DIR)) dir.create(RESPONSES_DIR)

LAST_Q_PAGE   <- 11   # Q0=page1 … Q10=page11
THANKYOU_PAGE <- 12

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

# ── Custom radio / checkbox builders ──────────────────────────────────────────
# Build input HTML manually so label text goes through htmltools' htmlEscape()
# (the same path used by h2(), p(), etc.) — which correctly preserves UTF-8/CJK.
# Shiny's JS bindings recognise the class names and read input values normally.

radio_hc <- function(inputId, ..., selected = character(0)) {
  pairs  <- c(...)                        # named vec: "label" = "value"
  labels <- names(pairs)
  values <- unname(pairs)
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

check_hc <- function(inputId, ...) {
  pairs  <- c(...)
  labels <- names(pairs)
  values <- unname(pairs)
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

# ── Survey constants ───────────────────────────────────────────────────────────
Q3_ITEMS <- c(
  "统计方法 / 行业知识查询与解释",
  "代码写作或调试 (R / Python / SAS)",
  "文献检索与摘要",
  "邮件 / 会议纪要总结",
  "临床或分析报告起草",
  "演示材料 (PPT) 准备",
  "数据分析结果解读",
  "翻译与语言润色"
)

Q7_ITEMS <- c(
  "输出存在错误或幻觉，需要人工核查",
  "AI 检索的文献不是最新的，存在知识截止日期问题",
  "AI 缺乏统计和制药领域专业知识，输出过于泛化",
  "内部 AI 工具功能受限，无法满足技术需求",
  "难以融入现有 workflow",
  "AI 对 SAS 语言的支持远不如对 R / Python 的支持",
  "图像识别准确度不够",
  "AI 输出结果的可复现性和可追溯性较低",
  "当前对话次数过多，AI 会出现记忆丢失"
)

Q9_ITEMS <- c(
  "AQS 知识库：针对实时更新的内部文档和外部行业指导原则",
  "基于文献的阅读，生成辅助学习的思维导图",
  "基于 SAP，生成 TFL shells 的候选列表",
  "SAS / R 程序注释生成与高效编码支持"
)

section_for_page <- function(p) {
  if (p == 1)             return("Your Role / 你的角色")
  if (p >= 2 && p <= 5)  return("Section A  ·  AI 工具使用情况")
  if (p >= 6 && p <= 11) return("Section B  ·  挑战、期待与失败案例")
  NULL
}

# ── Reusable UI helpers ────────────────────────────────────────────────────────
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

rank_ui <- function(input_id, items, other_lbl = "其他 / Other (optional):") {
  other_id <- paste0(input_id, "_other")
  tagList(
    div(class = "rank-instruction",
      tags$span("⇅"), " 拖拽排序，最优先 / 最常用排最前"
    ),
    rank_list(
      text     = NULL,
      labels   = items,
      input_id = input_id,
      options  = sortable_options(animation = 150)
    ),
    div(class = "other-field",
      tags$label(class = "other-lbl", other_lbl),
      textInput(other_id, label = NULL, placeholder = "请输入 / Type here…")
    )
  )
}

# ── Page content functions ─────────────────────────────────────────────────────
intro_page <- function() {
  div(class = "intro-wrap",
    div(class = "intro-icon", "📊"),
    h1(class = "intro-h1", "AQS AI 使用现状摸底问卷"),
    p(class  = "intro-sub", "AQS AI Usage Baseline Survey · 2026"),
    div(class = "info-card",
      p("作为 AQS 部门的 AI Fluency Champion，我们正在为部门设计 2026 年的 AI 工具推广计划。这份问卷旨在摸清部门当前的 AI 使用现状，让推广方案有真实数据支撑。"),
      div(class = "info-meta",
        div(class = "meta-row", span("⏱"), "预计耗时：6–8 分钟"),
        div(class = "meta-row", span("🔒"), "100% 匿名 — 不收集任何身份信息"),
        div(class = "meta-row", span("📅"), "结果将于 4 月底匿名汇总同步")
      )
    )
  )
}

q0_page <- function() {
  qcard("Q0", "单选 / Single Choice",
    "你的当前角色是？ / What is your current role?",
    radio_hc("q0",
      "A. 统计师 / Statistician"                  = "A",
      "B. 统计程序员 / Statistical Programmer"     = "B",
      "C. 组长 / 部门管理者 / Team Lead & Manager" = "C",
      "D. 其他角色 / Other"                        = "D"
    )
  )
}

q1_page <- function() {
  qcard("Q1", "多选 / Multiple Choice",
    "你使用过下列哪些 AI 工具？ / Which AI tools have you used?",
    tagList(
      check_hc("q1",
        "A. Internal ChatGPT (含嵌入的 Claude)"                        = "A",
        "B. M365 Copilot (Teams / Word / PowerPoint 中嵌入的 Copilot)" = "B",
        "C. Microsoft Agent (Researcher, Analyst 等)"                  = "C",
        "D. Coach Mira"                                                = "D",
        "E. GitHub Copilot / Cursor / 其他编程类 AI"                   = "E",
        "F. 其他 / Other"                                              = "F",
        "G. 我目前几乎不使用任何 AI 工具 / I barely use AI tools"       = "G"
      ),
      conditionalPanel(
        "input.q1 !== null && input.q1.includes('F')",
        div(class = "cond-field",
          textInput("q1_other", "请注明 / Please specify:", placeholder = "请输入…")
        )
      )
    )
  )
}

q2_page <- function() {
  qcard("Q2", "单选 / Single Choice",
    "你使用最频繁的 AI 工具是？ / Which AI tool do you use most frequently?",
    tagList(
      radio_hc("q2",
        "A. Internal ChatGPT (含嵌入的 Claude)"            = "A",
        "B. M365 Copilot"                                  = "B",
        "C. Microsoft Agent"                               = "C",
        "D. Coach Mira"                                    = "D",
        "E. GitHub Copilot / Cursor / 其他编程类 AI"        = "E",
        "F. 其他 / Other"                                  = "F",
        "G. 无 — 我目前几乎不使用 AI（跳过 Q3、Q4）"        = "G"
      ),
      conditionalPanel(
        "input.q2 === 'F'",
        div(class = "cond-field",
          textInput("q2_other", "请注明 / Please specify:", placeholder = "请输入…")
        )
      )
    ),
    hint = "💡 如选 G，将自动跳过 Q3 和 Q4 / Selecting G will skip Q3 and Q4."
  )
}

q3_page <- function() {
  qcard("Q3", "排序 / Ranking",
    "针对你最常用的 AI 工具，请按使用频率排列下列任务（最常用排最前）",
    rank_ui("q3_ranking", Q3_ITEMS, "其他任务 / Other tasks (optional):"),
    hint = "For your most-used AI tool, drag tasks from most to least frequent."
  )
}

q4_page <- function() {
  qcard("Q4", "单选 / Single Choice",
    "你使用 AI 工具的总体频率是？ / How often do you use AI tools overall?",
    radio_hc("q4",
      "A. 远超每日一次（没有 AI 几乎工作不下去）"       = "A",
      "B. 至少每日一次 / At least once a day"          = "B",
      "C. 至少每周一次 / At least once a week"         = "C",
      "D. 每月或更少 / Monthly or less"                = "D",
      "E. 视具体情况而定 / Depends on situation"       = "E"
    ),
    hint = "请按照平均使用习惯预估，向下选择。"
  )
}

q5_page <- function() {
  qcard("Q5", "单选 + 可选补充 / Single Choice + Optional Text",
    "最近一次你用 AI 完成工作任务时，真正觉得它帮上了大忙（不是 refine wording 等简单工作）",
    tagList(
      radio_hc("q5",
        "A. 文献（统计方法，行业标准等）的查询和综述"                   = "A",
        "B. 代码写作和调试 / Code writing & debugging"               = "B",
        "C. 专业文档撰写 (CSR / SAP 等) / Professional doc drafting" = "C",
        "D. 数据结果解读 / Data results interpretation"              = "D",
        "E. 学习新工具 / 新方法 / 新 SOP 等 / Learning new tools"    = "E",
        "G. 其他 / Other"                                           = "G",
        "H. 想不起来有这种时刻 / Cannot recall such a moment"        = "H"
      ),
      div(class = "other-field",
        tags$label(class = "other-lbl",
          "可选 — 用一句话描述这个任务 (≤30 字) / Optional — describe in one sentence:"),
        textInput("q5_text", NULL, placeholder = "请输入…")
      )
    ),
    hint = "Think of the last time AI truly made a significant difference (not just simple editing)."
  )
}

q6_page <- function() {
  qcard("Q6", "多选（最多 3 项）/ Multiple Choice (max 3)",
    "你曾尝试用 AI 完成、但效果不满意的任务是？ / Tasks where AI results were unsatisfactory?",
    tagList(
      check_hc("q6",
        "A. 需要文献支持的写作 (publication, PPT 等)"                    = "A",
        "B. 需要专业领域知识的选择判断（统计方法选择、estimand 制定等）"   = "B",
        "D. 复杂且有格式要求的文件起草 (CSR、SAP、TFL shell 等)"          = "D",
        "E. 依赖实时和最新数据的任务（行业指南更新对现存工作模式的影响等）" = "E",
        "F. 其他 / Other"                                               = "F",
        "G. 无 — 没有遇到效果不满意的情况 / None"                        = "G"
      ),
      conditionalPanel(
        "input.q6 !== null && input.q6.includes('F')",
        div(class = "cond-field",
          textInput("q6_other", "请注明 / Please specify:", placeholder = "请输入…")
        )
      ),
      uiOutput("q6_warn")
    )
  )
}

q7_page <- function() {
  qcard("Q7", "排序 / Ranking",
    "你在使用 AI 过程中遇到过哪些挑战？请按影响程度排列（影响最大排最前）",
    rank_ui("q7_ranking", Q7_ITEMS, "其他挑战 / Other challenges (optional):"),
    hint = "Drag to rank AI challenges from most to least impactful."
  )
}

q8_page <- function() {
  qcard("Q8", "单选 / Single Choice",
    tags$span(
      '"当使用 AI 替代我的日常工作时，我经常需要花更多的时间',
      tags$em("（例如，和 AI 沟通需求，调整 AI 输出，做比人工初稿更多的核查等）"),
      '。"'
    ),
    radio_hc("q8",
      "A. 强烈同意 / Strongly Agree"     = "A",
      "B. 同意 / Agree"                  = "B",
      "C. 中立 / Neutral"                = "C",
      "D. 不同意 / Disagree"             = "D",
      "E. 强烈不同意 / Strongly Disagree" = "E"
    ),
    hint = '"When using AI to replace my daily work, I often spend more time (communicating, adjusting output, extra verification)."'
  )
}

q9_page <- function() {
  qcard("Q9", "排序 / Ranking",
    "如果有一个具有针对性的 AI 新工具，最希望被加速的任务是？请按优先级排列（最期待排最前）",
    rank_ui("q9_ranking", Q9_ITEMS, "其他 / Other（可以是对上面选项的细化）(optional):"),
    hint = "Drag to rank from most to least desired AI feature."
  )
}

q10_page <- function() {
  qcard("Q10", "开放问题 / Open Question（选填）",
    "你任何关于 AI 使用的相关想法，没有被前面的问题所覆盖，请在这里留下。",
    textAreaInput("q10", NULL,
      placeholder = "请在此输入 / Type your thoughts here…",
      rows = 5, width = "100%"
    ),
    hint = "Any thoughts on AI usage not covered by the previous questions. This question is optional."
  )
}

thankyou_page <- function() {
  div(class = "ty-wrap",
    div(class = "ty-icon", "🎉"),
    h2(class = "ty-h2", "感谢你的填写！"),
    p(class  = "ty-sub", "Thank you for completing the survey!"),
    div(class = "info-card",
      p("回收的结果将在 4 月底以匿名汇总的形式与全部门同步。"),
      tags$hr(),
      tags$strong("📢 关于后续参与机会"),
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

# ── Validation ─────────────────────────────────────────────────────────────────
validate_page <- function(p, input) {
  need <- "此题为必答题，请选择后继续。/ This question is required."
  ok   <- function() list(ok = TRUE,  msg = "")
  fail <- function(m) list(ok = FALSE, msg = m)

  if (p == 1) {
    if (is.null(input$q0) || input$q0 == "") return(fail(need))
  } else if (p == 2) {
    if (is.null(input$q1) || length(input$q1) == 0) return(fail(need))
  } else if (p == 3) {
    if (is.null(input$q2) || input$q2 == "") return(fail(need))
  } else if (p == 5) {
    if (is.null(input$q4) || input$q4 == "") return(fail(need))
  } else if (p == 6) {
    if (is.null(input$q5) || input$q5 == "") return(fail(need))
  } else if (p == 7) {
    if (is.null(input$q6) || length(input$q6) == 0) return(fail(need))
    if (length(input$q6) > 3)
      return(fail("Q6：最多选择 3 项 / Please select at most 3 options."))
  } else if (p == 9) {
    if (is.null(input$q8) || input$q8 == "") return(fail(need))
  }
  ok()
}

# ── Response helpers ───────────────────────────────────────────────────────────
save_page <- function(p, input, rv) {
  if (p == 1)  { rv$q0 <- input$q0 }
  if (p == 2)  { rv$q1 <- input$q1;          rv$q1_other <- input$q1_other }
  if (p == 3)  { rv$q2 <- input$q2;          rv$q2_other <- input$q2_other }
  if (p == 4)  { rv$q3 <- input$q3_ranking;  rv$q3_other <- input$q3_ranking_other }
  if (p == 5)  { rv$q4 <- input$q4 }
  if (p == 6)  { rv$q5 <- input$q5;          rv$q5_text  <- input$q5_text }
  if (p == 7)  { rv$q6 <- input$q6;          rv$q6_other <- input$q6_other }
  if (p == 8)  { rv$q7 <- input$q7_ranking;  rv$q7_other <- input$q7_ranking_other }
  if (p == 9)  { rv$q8 <- input$q8 }
  if (p == 10) { rv$q9 <- input$q9_ranking;  rv$q9_other <- input$q9_ranking_other }
  if (p == 11) { rv$q10 <- input$q10 }
}

write_response <- function(rv) {
  scalar   <- function(x) if (is.null(x) || length(x) == 0) NA_character_ else as.character(x[[1]])
  collapse <- function(x) if (is.null(x) || length(x) == 0) NA_character_ else paste(x, collapse = " | ")
  clean    <- function(x) { x <- trimws(scalar(x)); if (nchar(x) == 0) NA_character_ else x }

  df <- data.frame(
    timestamp  = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    q0         = scalar(rv$q0),
    q1         = collapse(rv$q1),
    q1_other   = clean(rv$q1_other),
    q2         = scalar(rv$q2),
    q2_other   = clean(rv$q2_other),
    q3_ranking = collapse(rv$q3),
    q3_other   = clean(rv$q3_other),
    q4         = scalar(rv$q4),
    q5         = scalar(rv$q5),
    q5_text    = clean(rv$q5_text),
    q6         = collapse(rv$q6),
    q6_other   = clean(rv$q6_other),
    q7_ranking = collapse(rv$q7),
    q7_other   = clean(rv$q7_other),
    q8         = scalar(rv$q8),
    q9_ranking = collapse(rv$q9),
    q9_other   = clean(rv$q9_other),
    q10        = clean(rv$q10),
    stringsAsFactors = FALSE
  )

  fn <- file.path(RESPONSES_DIR,
                  paste0("resp_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))
  saveRDS(df, fn)
}

# ── UI ─────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  title = "AQS AI Survey 2026",
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
        span(class = "top-bar-title", "AI Usage Survey 2026")
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
    span(class = "top-bar-counter", paste0("Q", p - 1, " / Q10"))
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
      "12" = thankyou_page()
    )
  })

  output$q6_warn <- renderUI({
    if (!is.null(input$q6) && length(input$q6) > 3)
      div(class = "warn-msg", "⚠️ 最多选择 3 项 / Please select at most 3 options.")
  })

  output$nav_ui <- renderUI({
    p <- page()
    if (p == THANKYOU_PAGE) return(NULL)
    div(class = "nav-row",
      if (p > 1)
        actionButton("btn_back", "← 上一题", class = "btn-back"),
      div(class = "nav-spacer"),
      actionButton("btn_next",
        label = if (p == 0)            "开始填写 / Start →"
                else if (p == LAST_Q_PAGE) "提交 / Submit ✓"
                else                   "下一题 →",
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
      write_response(rv)
      THANKYOU_PAGE
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
