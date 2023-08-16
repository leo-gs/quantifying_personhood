library(shiny)
library(shinydashboard)
library(shinyBS)
library(leaflet)
library(shinyWidgets)
library(stringr)
library(plotly)
library(DT)

options(shiny.sanitize.errors = FALSE)

convert_n_to_star_tag <- function(i) {
    star_color <- get_map_palette(5)[i]
    star_text <- str_dup("&#9733;", times = i)
    str_c("<span ",
          "id='slider_",
          i,
          "' class='slider_tooltip;'; style='color: ",
          star_color,
          "; ",
          "'>", 
          star_text, 
          "</span>")
}

get_map_palette <- function(n) {
    colorRampPalette(
        c("darkorange3", "darkorange1", "gold", "chartreuse2", "green3")
    )(n)
}

ui <- dashboardPage(
    title = "Thematic Coding Workshop",
    dashboardHeader(disable = TRUE),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
        fluidRow(
            box(
                title = "Instructions",
                width = 3,
                p(
                    "In this workshop you will learn about developing qualitative research questions using the ",
                    a("Yelp Academic Dataset.", href = "https://www.yelp.com/dataset")
                ),
                p("The clickable panels have prompts that will guide you through the workshop.")
            ),
            column(width = 9,
                   bsCollapse(
                       multiple = TRUE,
                       open = "part1",
                       bsCollapsePanel(
                           style = "primary",
                           title = "Part 1. Quantitative Research Questions",
                           value = "part1",
                           p("We're going to explore what kinds of questions can be explored qualitatively and ",
                             "how these questions are different from quantitative research questions."),
                           p("Suppose we had the following hypothesis which we'll call ", em("Hypothesis A:")),
                           tags$blockquote(strong("Hypothesis A.")," Yelp users are more likely to visit a bar with a low ",
                                           "rating if there are no other bars nearby."),
                           h4("Questions"),
                           tags$ol(
                               tags$li("Does Hypothesis A seem reasonable to you? Why or why not? You may use the map and plot ",
                                       "below to gather more information."),
                               tags$li("Use the plots in the Global View panel to explore some of the data. Then, write your ",
                                       "own hypothesis, Hypothesis B, about the data."),
                               tags$li("What kind of analysis could you perform to test Hypothesis ", strong("B"), 
                                       "? Describe the independent and dependent variables you would use and why ",
                                       "they are important."),
                               tags$li("If you found a large and statistically significant relationship supporting ",
                                       "Hypothesis ", strong("A"), ", how confident would you be in saying that Yelp users ",
                                       "chose a given bar ", em("because"), " there were no other bars nearby? Explain.")
                           )
                       ),
                       bsCollapsePanel(
                           title = "Part 2. Qualitative Research Questions",
                           value = "part2",
                           p(
                               "In Part 1, you (briefly) walked through the process of designing a quantitative study. ",
                               "In doing so, you developed a procedure for testing a hypothesis. This is where quantitative ",
                               "tools can come in particularly handy--for testing assumptions or illustrating patterns. ",
                               "However sometimes defining a hypothesis can be limiting, such as in situations where we don't ",
                               "have enough information to define a hypothesis, when we are studying something that is ",
                               "difficult to represent in numbers, or when we are interested in understanding ",
                               em("why"), " a certain pattern is occuring."
                           ),
                           h4("Questions"),
                           style = "primary",
                           tags$ol(
                               tags$li("You can view detailed information for individual bars in the Close-up View panel ",
                                       "by clicking on them in the map. Choose several bars and read through some of their ",
                                       "reviews. Try to read at least 20 reviews for several different businesses. ",
                                       "Write a brief description of any common themes (or lack thereof)."),
                               tags$li("Recall the hypothesis you wrote in Part 1. Did reading through reviews change ",
                                       "your perspective at all? Why or why not?")
                           )
                           
                       ),
                       bsCollapsePanel(
                           title = "Part 3. Reflection",
                           value = "part3",
                           h4("Questions"),
                           style = "primary",
                           tags$ol(
                               tags$li("Compare perspectives between the Global View panel and Close-up View ",
                                       "panel. How does each perspective differ? What are the benefits and ",
                                       "drawbacks of each one?")
                           )
                           
                       )
                   )
            )
        ),
        fluidRow(
            box(
                width = 12,
                title = "Global View",
                fluidRow(
                    column(
                        style='padding:20px;',
                        width = 3,
                        chooseSliderSkin(
                            skin = "Flat",
                            color = "#000000"
                        ),
                        sliderTextInput(
                            inputId = "stars_slider",
                            label = "Star Rating",
                            choices = sapply(1:5, convert_n_to_star_tag),
                            selected = c(
                                convert_n_to_star_tag(1),
                                convert_n_to_star_tag(5)
                            ),
                            grid = FALSE,
                            animate = FALSE,
                            hide_min_max = TRUE,
                            dragRange = FALSE,
                            force_edges = FALSE,
                            width = "100%"
                        ),
                        sliderTextInput(
                            inputId = "reviews_slider",
                            label = "Review Count",
                            choices = c(seq(5, 3264, by = 100), 3264),
                            selected = c(5, 3264),
                            grid = FALSE,
                            animate = FALSE,
                            hide_min_max = TRUE,
                            dragRange = FALSE,
                            force_edges = FALSE,
                            width = "100%"
                        )
                    ),
                    column(
                        width = 5,
                        leafletOutput(outputId = "barmap", width = "100%", height = 400),
                    ),
                    column(
                        width = 4,
                        plotlyOutput(
                            outputId = "stars_x_nreviews", 
                            height = 400
                        )
                    )
                ),
                collapsible = TRUE
            )
        ),
        fluidRow(
            box(
                width = 12,
                title = "Close-up View",
                fluidRow(
                    column(
                        width = 4,
                        uiOutput(outputId = "selected_bar_details"),
                        plotOutput("selected_plot_freq", height = "350px"),
                        plotOutput("selected_plot_bydate", height = "350px")
                    ),
                    column(
                        width = 8,
                        DTOutput(
                            outputId = "selected_bar_reviews_table",
                            height = 1000
                        )
                    )
                )
            )
        )
    )
)
