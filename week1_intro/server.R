library(shiny)
library(readr)
library(dplyr)
library(tibble)
library(leaflet)
library(scales)
library(ggplot2)
library(ggtext)
library(colorspace)
library(DT)

options(shiny.sanitize.errors = FALSE)

function(input, output, session) {
    
    ## CONVENIENCE FUNCTIONS
    
    ## A convenience function for formatting numbers in text
    big_num <- function(n) format(x = n, big.mark = ",")
    
    get_map_palette <- function(n) {
        colorRampPalette(
            c("darkorange3", "darkorange1", "gold", "chartreuse2", "green3")
        )(n)
    }
    
    rating_to_stars_fa <- function(i, half_stars = FALSE) {
        if (round(i) != i && !half_stars) {
            return ("")
        }
        
        star_strs <- ""
        j <- i
        
        for (k in 1:5) {
            if (j == 0) {
                star_strs <- c(star_strs, "'fa fa-star-o fa-fw'")
            } else if (j >= 1) {
                star_strs <- c(star_strs, "'fa fa-star fa-fw'")
                j <- j - 1
            } else {
                star_strs <- c(star_strs, "'fa fa-star-half-full fa-fw'")
                j <- 0
            }
        }
        
        str_c(
            "<span class=", 
            star_strs, 
            "style='display: inline-block; position: relative; width: 1.1em;'></span>", 
            sep = "", 
            collapse = ""
        )
    }
    
    rating_to_stars <- function(v) {
        sapply(v, function(i) str_dup("&#9733;", times = i))
    }
    
    ## DATA FUNCTIONS
    philly_bars <- reactive({
        map_pal <- get_map_palette(n = 9)
        
        philly_bars <- read_csv("philly_bars.csv", show_col_types = FALSE) %>%
            mutate(pcolor = map_pal[stars*2 - 1]) %>%
            mutate(
                map_label = str_c(
                    "<b>", name, "</b>", "<br>",
                    "<i>", address, "</i><br>",
                    format(stars, nsmall = 1), " stars<br>",
                    format(N_reviews, big.mark = ","), " reviews",
                    sep = ""
                )
            )
    })
    
    philly_bar_reviews <- reactive({
        read_csv("philly_bar_reviews_1.csv", show_col_types = FALSE) %>%
            bind_rows(
                read_csv("philly_bar_reviews_2.csv", show_col_types = FALSE)
            )
    })
    
    ## REACTING TO USER
    slider_updated <- reactive({
        list(
            input$stars_slider,
            input$reviews_slider
        )
    })
    
    selected_bar <- reactive({
        input$barmap_marker_click
        
    })
    
    
    ## DEFINING UI OUTPUTS
    
    ## Map
    output$barmap <- renderLeaflet({
        philly_map_data <- philly_bars()
        
        philly_map_data %>%
            leaflet(data = .) %>%
            addProviderTiles(providers$CartoDB.DarkMatter) %>%
            addCircleMarkers(
                layerId = ~business_id,
                lat = ~latitude,
                lng = ~longitude,
                color = ~pcolor,
                fillColor = ~pcolor,
                radius = 3,
                weight = 0.25,
                stroke = TRUE,
                label = ~lapply(map_label, HTML),
                opacity = 0.8,
                fillOpacity = ~rescale(N_reviews, to = c(0.1, 0.8))
            ) %>%
            fitBounds(
                lng1 = ~min(longitude),
                lng2 = ~max(longitude),
                lat1 = ~min(latitude),
                lat2 = ~max(latitude)
            )
    })
    
    
    output$stars_x_nreviews <- renderPlotly({
        
        stars_range <- input$stars_slider %>%
            str_extract(pattern = "slider_.") %>%
            str_remove(pattern = "slider_") %>%
            as.numeric()
        
        nReviews_range <- input$reviews_slider %>% 
            as.numeric()
        
        plot_ly(
            data = philly_bars() %>%
                mutate(
                    in_star_range = between(stars, stars_range[1], stars_range[2]),
                    in_review_range = between(N_reviews, nReviews_range[1], nReviews_range[2])
                ) %>%
                mutate(is_selected = in_star_range & in_review_range) %>%
                filter(is_selected == TRUE),
            hoverinfo = "text",
            x = ~log10(N_reviews), 
            y = ~jitter(stars),
            color = ~factor(stars),
            colors = get_map_palette(9),
            type = "scatter",
            mode = "markers",
            showlegend = FALSE,
            text = ~map_label,
            # text = ~is_selected,
            size = 10
            # split = ~is_selected,
            # opacity = ~is_selected
        ) %>%
            layout(
                yaxis = list(
                    title = "Yelp average star rating",
                    tickmode = "array",
                    nticks = 5,
                    tickvals = 1:5, 
                    ticktext = rating_to_stars(1:5),
                    range = c(0.5, 5.5)
                ),
                xaxis = list(
                    title = "Number of reviews (log<sub>10</sub> scale)",
                    tickmode = "array",
                    nticks = 4,
                    tickvals = 0:4,
                    ticktext = 10^(0:4),
                    range = c(0.5, 3.6)
                )
            ) %>%
            config(displayModeBar = FALSE)
    })
    
    
    observeEvent(slider_updated(), {
        
        stars_range <- input$stars_slider %>%
            str_extract(pattern = "slider_.") %>%
            str_remove(pattern = "slider_") %>%
            as.numeric()
        
        nReviews_range <- input$reviews_slider %>% 
            as.numeric()
        
        proxy_map <- leafletProxy(
            mapId = "barmap", 
            session = session
        ) %>%
            clearMarkers()
        
        filtered_bars <- philly_bars() %>%
            filter(between(stars, stars_range[1], stars_range[2])) %>%
            filter(between(N_reviews, nReviews_range[1], nReviews_range[2]))
        
        
        
        if (nrow(filtered_bars) > 0) {
            proxy_map <- proxy_map %>%
                addCircleMarkers(
                    data = filtered_bars,
                    layerId = ~business_id,
                    lat = ~latitude,
                    lng = ~longitude,
                    color = ~pcolor,
                    fillColor = ~pcolor,
                    radius = 3,
                    weight = 0.25,
                    stroke = TRUE,
                    label = ~lapply(map_label, HTML),
                    opacity = 0.8,
                    fillOpacity = ~rescale(N_reviews, to = c(0.1, 0.8))
                )
        }
        
        proxy_map
    })
    
    # Selected Bar Details
    output$selected_bar_details <- renderUI({
        selected_bar_id <- selected_bar()
        philly_bar_rvs <- philly_bar_reviews()
        
        if (is.null(selected_bar_id)) {
            tagList(p("Select a marker on the map to see details.", style = "padding:10px"))
            
        } else {
            bar_info <- philly_bars() %>%
                filter(business_id == selected_bar_id$id)
            tagList(
                p(strong(bar_info$name), style = "font-size: 18px; width: 100%;"),
                # p(bar_info$address, br(), em(bar_info$city, ", ", bar_info$state)),
                p(bar_info$address, br(), "Philadelphia, PA"),
                # hr(style = "border-color: white;"),
                div(em(bar_info$categories), style = "width: 100%;"),
                br(),
                span(HTML(rating_to_stars_fa(bar_info$stars, half_stars = TRUE))),
                p(format(bar_info$stars, nsmall = 1), " stars"),
                p(format(bar_info$N_reviews, big.mark = ","), " reviews")
            )
        }
    })
    
    output$selected_plot_freq <- renderPlot({
        selected_bar_id <- selected_bar()
        
        if (!is.null(selected_bar_id)) {
            selected_bar_reviews <- philly_bar_reviews() %>%
                filter(business_id == selected_bar_id$id) %>%
                select(business_id, stars)
            
            ggplot(
                data = selected_bar_reviews
            ) +
                geom_bar(
                    aes(
                        y = factor(stars, levels = 1:5),
                        fill = factor(stars, levels = 1:5),
                        color = factor(stars, levels = 1:5)
                    )
                ) +
                scale_color_manual(
                    palette = get_map_palette
                ) +
                scale_fill_manual(
                    palette = get_map_palette
                ) +
                scale_y_discrete(
                    name = "Stars",
                    drop = FALSE,
                    breaks = 1:5,
                    labels = rating_to_stars(1:5)
                ) +
                ggtitle(label = "Histogram of Star Ratings Received") +
                theme_classic() +
                theme(
                    text = element_text(family = "Lato"),
                    plot.title = element_text(size = 14, margin = margin(8, 0, 8, 0, unit = "pt")),
                    # plot.background = element_rect(color = "white", fill = "white"),
                    # panel.background = element_rect(fill = "#141414"),
                    axis.text.y = element_markdown(),
                    panel.grid.major.y = element_blank()
                ) +
                guides(color = "none", fill = "none")
        }
    })
    
    output$selected_plot_bydate <- renderPlot({
        selected_bar_id <- selected_bar()

        if (!is.null(selected_bar_id)) {
            selected_bar_reviews <- philly_bar_reviews() %>%
                filter(business_id == selected_bar_id$id) %>%
                arrange(date) %>%
                mutate(
                    reviews_by_date = row_number(),
                    cumulative_stars = cumsum(stars),
                ) %>%
                mutate(
                    average_stars_by_date = round(
                        cumulative_stars / reviews_by_date,
                        digits = 2
                    )
                ) %>%
                select(business_id, date, stars, average_stars_by_date, reviews_by_date)

            jitter <- 0.1

            ggplot(
                data = selected_bar_reviews
            ) +
                geom_jitter(
                    aes(
                        x = date,
                        y = stars,
                        color = factor(stars),
                    ),
                    width = 0,
                    height = jitter,
                    size = 3
                    # alpha = 0.8,
                    # shape = 1
                ) +
                geom_line(
                    aes(
                        x = date,
                        y = average_stars_by_date,
                        alpha = reviews_by_date
                    ),
                    linewidth = 1
                ) +
                scale_x_datetime(name = "Date of Review", date_labels = "%b %Y") +
                scale_y_continuous(
                    name = "Star Ratings (Line denotes mean value over time)",
                    breaks = 1:5,
                    limits = c(0.8, 5.2),
                    labels = rating_to_stars(1:5)
                ) +
                scale_color_manual(
                    values = get_map_palette(5)
                ) +
                guides(
                    color = "none",
                    alpha = "none"
                ) +
                ggtitle(label = "(Mean) Star Rating Over Time") +
                theme_minimal() +
                theme(
                    text = element_text(family = "Lato"),
                    plot.title = element_text(size = 14, margin = margin(8, 0, 8, 0, unit = "pt")),
                    panel.grid.minor.y = element_blank(),
                    axis.text.y = element_markdown()
                )
        }
    })
    
    output$selected_bar_reviews_table <- renderDT({
        selected_bar_id <- selected_bar()
        
        if (!is.null(selected_bar_id)) {
            selected_bar_reviews <- philly_bar_reviews() %>%
                filter(business_id == selected_bar_id) %>%
                mutate(Stars = rating_to_stars(stars)) %>%
                select(Date = date, Stars, Text = text)
            
            datatable(
                data = selected_bar_reviews,
                fillContainer = TRUE,
                escape = FALSE
            )
        }
    })
}
