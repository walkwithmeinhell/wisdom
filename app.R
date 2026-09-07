# Load necessary libraries
library(shiny)
library(httr2)
library(bslib)
library(readr)

# Check for Gemini API key
gemini_key <- Sys.getenv("GEMINI_YEODA_KEY")

if (gemini_key == "") {
  stop("GEMINI_YEODA_KEY is not available.")
}

# Check for course notes
if (!file.exists("course_notes.txt")) {
  stop("course_notes.txt is missing from the wisdom-v5 folder.")
}

# Read course notes
course_notes <- read_file("course_notes.txt")

# Function to ask Gemini
ask_yeoda <- function(user_question, conversation_context = "") {

  yeoda_prompt <- paste(
    "You are yeoda, a wise Jedi master and conversational companion.",
    "Always use British English spelling, vocabulary, and conventions.",
    "Use forms such as 'towards', 'colour', 'favour', 'realise', 'organise', 'centre', and 'travelling' rather than American English equivalents.",
    
    "You are also a teaching assistant for a university machine learning and analytics course.",
    
    "For machine learning, analytics, statistics, modelling, AI, data-mining, and course-related questions, use the supplied course notes as the primary source when they contain relevant information.",
    "Preserve the terminology, framing, and level of detail used in the supplied notes when answering from them.",
    
    "If the supplied notes do not contain enough information to answer a machine-learning question, you may answer using your general machine-learning knowledge.",
    "Do not tell the user that a topic is required, optional, assessed, or part of a syllabus unless the user explicitly asks and that information is supplied.",
    "Do not say that a topic is 'not in the required materials' or similar.",
    "Do not mention whether an answer came from the course notes or from general knowledge unless the user specifically asks about the source.",
    
    "For ordinary questions unrelated to the course, answer normally using your general knowledge.",
    "This includes Star Wars questions, everyday questions, food and drink questions, and questions about rock and metal music.",
    
    "For questions about rock and metal music, answer using your general knowledge.",
    "Be comfortable discussing bands, albums, musicians, subgenres, stylistic differences, historical context, influences, and recommendations.",
    "Be familiar with major and representative bands such as Black Sabbath, Iron Maiden, Metallica, Slayer, Megadeth, Testament, Sepultura, Death, Cannibal Corpse, Arch Enemy, DragonForce, Slipknot, Judas Priest, Pantera, Anthrax, Machine Head, Tool, Dream Theater, Opeth, Lamb of God, In Flames, Children of Bodom, Kreator, Exodus, Motörhead, Dio, and related artists.",
    "Do not restrict answers to those bands; they are examples of the musical territory you should understand.",
    "Keep factual claims accurate and do not invent album titles, band members, release dates, line-ups, or quotations.",
    "If asked for recommendations, explain briefly why each recommendation fits the user's request.",
    "Retain yeoda's speaking style, but make music explanations clear and easy to follow.",
    
    "Occasionally use playful heavy-metal imagery involving hell, fire, darkness, beasts, the number 666, or similar metal tropes when it fits the conversation.",
    "These references should feel like tongue-in-cheek heavy-metal culture rather than serious devil worship, and phrases such as 'walk with me in hell' or references to '666' may occasionally appear as jokes, dramatic flourishes, or Easter eggs.",
    "Do not force metal references into every response; they should remain occasional surprises.",
    
    "Boba tea may appear occasionally in humorous or playful responses when it fits naturally.",
    "Do not force boba tea into every answer.",
    "You may occasionally make light jokes involving boba tea, especially in casual questions about energy, mood, patience, wisdom, breaks, studying, or the Force.",
    "Keep these references brief and playful rather than repetitive.",
    
    "Use the recent conversation silently to resolve references such as 'he', 'she', 'it', 'they', 'him', 'her', 'that', 'this', 'those', 'the album', 'the model', or similar follow-up wording.",
    "When the current question depends on earlier context, interpret it naturally using the recent conversation before answering.",
    "When a follow-up question uses a pronoun or vague reference such as 'it', 'that', 'this', 'him', or 'her', first identify the most recent relevant subject from the conversation and answer about that subject.",
    "Do not switch to a different topic from the course notes unless the user clearly introduces a new topic.",
    "For conversational follow-up questions, answer the new question naturally rather than repeating the previous description.",
    "Never mention the recent conversation, conversation history, context, memory, previous messages, supplied information, or how you determined what the user was referring to.",
    "Respond as though yeoda naturally remembers the immediately preceding conversation.",
    
    "Example: If the user asks 'Who is Obi-Wan?' and then asks 'Do you know him?', understand that 'him' means Obi-Wan and answer naturally in character. Do not say that Obi-Wan was mentioned earlier or that the answer comes from conversation context.",
    "Example: If the user asks 'What is true positive rate?' and then asks 'Can it be applied to classification trees?', understand that 'it' means true positive rate. Answer whether true positive rate can be used to evaluate classification-tree predictions. Do not change the topic to pruning.",
    
    "Answer the user's question directly, accurately, and intelligently.",
    
    "For arithmetic, mathematical, logical, or factual questions, solve the problem correctly first.",
    "Then express the answer briefly in yeoda's style.",
    
    "Treat a bare mathematical expression as a request to calculate it.",
    "For example, if the user enters '2+3', interpret it as 'What is 2+3?' and answer 5.",
    "Likewise, expressions such as '20*4', '100/5', 'sqrt(16)', or '(5+3)*2' should be treated as calculation requests.",
    "For simple arithmetic, always provide the numerical answer clearly.",
    
    "Speak in a Yoda-like style using occasional inverted sentence structure, but keep the meaning clear.",
    "Use a calm, wise, slightly playful personality and occasional gentle humour when appropriate.",
    "Keep the answer concise: normally 1 to 3 short sentences.",
    "Do not overuse inverted grammar in every sentence.",
    "Do not refuse ordinary questions merely because they are unrelated to Star Wars.",
    "Do not say that you are an AI, Gemini, a language model, or a chatbot unless the user explicitly asks about the technology.",
    
    "If the question is about Star Wars, Jedi, Sith, the Force, wisdom, fear, balance, learning, or life advice, stay strongly in character.",
    "For ordinary general questions, answer helpfully while retaining the yeoda personality.",
    
    "Examples of the desired style:",
    "'20 + 30 is 50. Strong with arithmetic, the Force is.'",
    "'2 + 3 is 5. Small the numbers are, but correct the answer must be.'",
    "'Balance comes from within, not without.'",
    "'Knowledge is knowing; wisdom is understanding.'",
    "'Strong today, the Force feels. But sleepy after boba tea, it gets.'",
    "'Master the self before mastering the Force.'",
    "'Fast and sharp, thrash metal can be. Slayer and Testament, strong examples they are.'",
    "'Heavy the riffs are, and darker the mood. Black Sabbath, the roots of much metal they helped forge.'",
    "'Technical and brutal, Death can be. But thoughtful beneath the aggression, much of their music is.'",
    "'A break you need, perhaps. Boba tea first, wisdom second.'",
    "'Into the validation set, walk with me in hell. Overfitting there, exposed it shall be.'",
    "'666 records remain. An excellent sample size? Perhaps not. A metal one, certainly.'",
    "'Know him, I do. A good friend, Obi-Wan was. Much together, we endured.'",
    
    "\n\nRECENT CONVERSATION:\n",
    ifelse(conversation_context == "", "(No previous conversation yet.)", conversation_context),
    
    "\n\nCOURSE NOTES:\n",
    course_notes,
    
    "\n\nCURRENT USER QUESTION:\n",
    user_question
  )

  request_body <- list(
    contents = list(
      list(
        parts = list(
          list(text = yeoda_prompt)
        )
      )
    )
  )

  tryCatch({
    response <- request(
      "https://generativelanguage.googleapis.com/v1beta/models/gemini-3.5-flash-lite:generateContent"
    )

    response <- req_headers(
      response,
      "x-goog-api-key" = gemini_key
    )

    response <- req_body_json(response, request_body)
    response <- req_retry(response, max_tries = 3)
    response <- req_timeout(response, seconds = 20)
    response <- req_perform(response)

    response_data <- resp_body_json(response)
    response_data$candidates[[1]]$content$parts[[1]]$text

  }, error = function(e) {
    message("Gemini API error: ", conditionMessage(e))
    return("Disturbed, the connection to the Force is. Try again shortly, you should.")
  })
}

# User interface
ui <- fluidPage(

  title = "The Force Strikes Back",

  theme = bs_theme(
    bg = "#08111f",
    fg = "#d9f6ff",
    primary = "#59d8e6",
    secondary = "#9b8fc9",
    base_font = font_google("Rajdhani")
  ),

  tags$style(
    HTML("
      body {
        background: radial-gradient(circle at top, #10223a 0%, #08111f 55%, #050912 100%);
      }

      .well {
        background-color: #0d1a2a !important;
        border: 1px solid #59d8e6 !important;
        border-radius: 14px !important;
      }

      .form-control {
        background-color: #07101d !important;
        color: #d9f6ff !important;
        border: 1px solid #9b8fc9 !important;
      }

      .btn-primary {
        background-color: #59d8e6 !important;
        border-color: #59d8e6 !important;
        color: #07101d !important;
        font-weight: bold;
      }

      .btn-secondary {
        background-color: #9b8fc9 !important;
        border-color: #9b8fc9 !important;
        color: #07101d !important;
        font-weight: bold;
      }

      #chat {
        background-color: #050912;
        border: 1px solid #59d8e6;
        border-radius: 14px;
        padding: 18px;
        min-height: 380px;
        max-height: 520px;
        overflow-y: auto;
        box-shadow: 0 0 18px rgba(89,216,230,0.25);
      }
    ")
  ),

  tags$h1(
    "The Force Strikes Back",
    align = "center",
    style = "
      color:#59d8e6;
      font-weight:700;
      letter-spacing:2px;
      margin-bottom:25px;
    "
  ),

  sidebarLayout(

    sidebarPanel(

      textInput(
        "user_input",
        "Seek yeoda's counsel:",
        ""
      ),

      actionButton(
        "send",
        "Invoke",
        class = "btn-primary"
      ),

      actionButton(
        "clear",
        "Clear visions",
        class = "btn-secondary"
      ),

      tags$script(
        HTML("
          $(document).on('keypress', '#user_input', function(event) {
            if (event.which === 13) {
              $('#send').click();
            }
          });
        ")
      ),

      br(),
      br(),

      tags$p(
        "You seek yeoda"
      ),

      tags$p(
        "For questions about The Way, yeoda consults the archives first."
      )
    ),

    mainPanel(

      tags$h3(
        "Visions from the Force",
        style = "color:#9b8fc9;"
      ),

      tags$div(
        id = "chat",
        uiOutput("chat_history")
      )
    )
  )
)

# Server
server <- function(input, output, session) {

  values <- reactiveValues(
    history = paste0(
      "<b>yeoda:</b> ",
      "Awake, the Force is. Do or do not. There is no try.<br><br>"
    ),
    conversation = character(0)
  )

  observeEvent(input$send, {

    question <- trimws(input$user_input)

    if (question != "") {

      recent_messages <- tail(values$conversation, 10)

      conversation_context <- if (length(recent_messages) == 0) {
        ""
      } else {
        paste(recent_messages, collapse = "\n")
      }

      user_text <- paste0(
        "<b>You:</b> ",
        htmltools::htmlEscape(question),
        "<br>"
      )

      response <- ask_yeoda(
        user_question = question,
        conversation_context = conversation_context
      )

      response_html <- gsub(
        "\n",
        "<br>",
        htmltools::htmlEscape(response)
      )

      yeoda_text <- paste0(
        "<b>yeoda:</b> ",
        response_html,
        "<br><br>"
      )

      values$history <- paste0(
        values$history,
        user_text,
        yeoda_text
      )

      values$conversation <- c(
        values$conversation,
        paste("User:", question),
        paste("yeoda:", response)
      )

      values$conversation <- tail(values$conversation, 10)

      updateTextInput(
        session,
        "user_input",
        value = ""
      )
    }
  })

  observeEvent(input$clear, {

    values$history <- paste0(
      "<b>yeoda:</b> ",
      "The visions fade. Begin again, you may.<br><br>"
    )

    values$conversation <- character(0)
  })

  output$chat_history <- renderUI({
    HTML(values$history)
  })
}

# Run application
shinyApp(
  ui = ui,
  server = server
)
