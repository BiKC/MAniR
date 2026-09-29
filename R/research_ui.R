# Task-oriented start screen. The analysis remains in the original Shiny tabs,
# so researchers can switch between the guided route and all available tools.

manir_question_card <- function(title, purpose, action_id, action_label,
                                requirement, next_step) {
  shiny::div(class = "question-card",
    shiny::div(class = "question-top",
      shiny::h4(title),
      shiny::span(class = "question-requirement", requirement)),
    shiny::p(purpose),
    shiny::p(class = "question-next", next_step),
    shiny::actionButton(action_id, action_label, class = "btn-primary")
  )
}

manir_research_start <- function() {
  shiny::tagList(
    shiny::div(class = "start-hero",
      shiny::div(class = "start-kicker", "YOUR ANALYSIS"),
      shiny::h3("What do you want to find out?"),
      shiny::p("Choose a research question. MAniR opens the relevant analysis with a short explanation of what to look for.")
    ),
    shiny::uiOutput("research_status"),
    shiny::div(class = "question-grid",
      manir_question_card(
        "Do two methods give similar results?",
        "Examine whether pairs with high similarity in one matrix also have high similarity in another.",
        "start_agreement", "Compare the methods",
        "2 matrices",
        "See the association statistics, then inspect the pairwise scatterplot."
      ),
      manir_question_card(
        "Which isolate pairs deserve a closer look?",
        "Find pairs that occupy very different positions in two methods and inspect their exact values.",
        "start_pairs", "Find unusual pairs",
        "2 matrices",
        "Select an isolate pair, check both results and review its metadata."
      ),
      manir_question_card(
        "Do both methods make the same groups?",
        "Compare independently calculated clusters and identify isolates whose assignments differ.",
        "start_cluster", "Compare clusters",
        "2 matrices",
        "Choose the number of clusters, then read the overlap table."
      ),
      manir_question_card(
        "Are known groups or batches reflected in the data?",
        "Compare within-group and between-group pairwise measurements using sample metadata.",
        "start_groups", "Explore my groups",
        "1 matrix + metadata",
        "Select a group or batch field and examine both matrices separately."
      ),
      manir_question_card(
        "I want to inspect my matrix.",
        "Open the heatmaps, explore a region and check the exact value for a pair of isolates.",
        "start_heatmap", "Explore heatmaps",
        "1 matrix",
        "Start with the first matrix; switch to the combined view if both are loaded."
      )
    ),
    shiny::div(class = "start-footer",
      shiny::strong("Keep a record of your analysis"),
      shiny::p("When you have explored the results, use Export to save an editable research summary, the original data and your figures."),
      shiny::actionButton("start_export", "Open exports", class = "btn-default")
    )
  )
}
