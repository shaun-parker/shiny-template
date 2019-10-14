library(ggplot2)
library(tidyverse)
library(extrafont)

function(input, output) {
    df_original <- readRDS("./data/data.rds")
    starting_money = 1000

    number_games <- reactive({
        input$n_games
    })

    df <- reactive({
        df_original %>%
            filter(current_game <= number_games()) %>%
            group_by(simulation) %>%
            mutate("to_highlight" = ifelse(current_winner == 33, "yes", "no"), #flag winning number in df
            "ending_money" = last(current_money), # calculate money at the end of simulation 
            "winner" = ifelse(ending_money >= starting_money, 1, 0)) %>% #calculate if simulation is a winner or loser 
            ungroup()
            })

    summary_df <- reactive({
        df_original %>%
            filter(current_game <= number_games()) %>%
            group_by(current_game) %>%
            summarise("current_money" = mean(current_money)) #avg money by n_game in simulation
    })

    winrate <- reactive({
           mean(df()$winner)
    })

    winamt <- reactive({
        round(
            mean(df()$ending_money),
            2)
    })

    loadfonts(device="win")
    fontfam = "Century Gothic"
    colours <- c("#c48faf", "#afc48f")

    library(ggplot2)
    output$plot1 <- renderPlot({
        ggplot(data=df(), aes(x=current_game, y=current_money, group=simulation, color = as.factor(winner))) + 
        geom_line(alpha=0.7, size=2) + 
        geom_hline(yintercept = 1000) + 
        scale_color_manual(values=colours) +
        theme(
            legend.position = "none"
        ) +
        labs(title = paste(
            "Average ending win rate (left with more cash than started): ",
            winrate(),
            "% \nAverage ending cash: £",
            winamt(),
            sep=""
        ),
        caption = paste("Starting cash: £",
                        1000,
                        "\nStake per game: £10",
                        "\nn games: ",
                        number_games(),
                        "\nn_simulations: 100",
                        sep=""),
        x = "Game", 
        y = "Current money (£)"
        ) + 
        theme_minimal() + 
        theme(
            text=element_text(family=fontfam),
            legend.position = "none",
            panel.grid = element_blank()
        )
    }, bg="transparent")

    output$plot2 <- renderPlot({
        ggplot(data=df(), aes(x=as.factor(current_winner - 1), fill = to_highlight)) + 
        geom_histogram(stat="count") +
        geom_text(aes(y=0, label=current_winner - 1), check_overlap=TRUE, 
                  family=fontfam, nudge_y=number_games() * 0.05, vjust=0.3) +
        coord_flip() + 
        scale_fill_manual(values=colours) +
        scale_y_continuous(expand=c(0,0)) +
        labs(x = "Winning numbers") +
        theme_minimal() + 
        theme(
            text=element_text(family=fontfam),
            legend.position = "none",
            panel.grid = element_blank(), 
            axis.title.x = element_blank(),
            axis.text = element_blank()
        )
        #scale_fill_manual(values = c("yes" = "tomato", "no" = "gray"), guide=FALSE)
    }, bg="transparent") 

    output$plot3 <- renderPlot({
        ggplot(data=summary_df(), aes(x=current_game, y=current_money)) +
        geom_line(stat="identity", color = colours[2]) + 
        scale_color_manual(values=colours) +
        labs(x="Number of games",
             y="Average money (£)") +
        theme_minimal() +
        theme(
            text=element_text(family=fontfam),
            panel.grid=element_blank()
        )
    }, bg="transparent")
}

