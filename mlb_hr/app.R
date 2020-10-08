# library ####
library(shiny)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(DBI)
library(RSQLite)
library(DT)
library(tm)
library(wordcloud)
library(wordcloud2)
library(hexbin)
library(skimr)
library(shinyjs)


# https://paulsprouse.shinyapps.io/shiny_project/ ####
# http://128.199.0.115:3838/mlb_hr/ ####
# data connection to db and Load steroid list####
connector <- function(con, db) {
  con <- dbConnect(SQLite(), db)
  return (con)
}
dbcon <- connector(con, "baseball_stats.db")
as.data.frame(dbListTables(dbcon))
stats <- dbReadTable(dbcon, 'bat_p')
teams <- dbReadTable(dbcon, 'Teams')
# The steroid list from - https://bleacherreport.com/articles/232808-steroidology-l-hoops-projects-all-104-players-on-the-2003-steroid-list
the_list <- read_csv('steroid_list.csv')
#quotes <- read_csv('quotes.csv')
dbDisconnect(dbcon) # disconnect
# ** setting up data ** ####
# HR breaks and yrs breaks
breaks_HR = c(0, 30, 40, 50, 80)
breaks_yrs = c(1914, 1945, 1994, 2006, 2020)
# two data frames stats and the list - need to merge into one!
the_list <- the_list %>% rename(nameLast = last, nameFirst = first)
stats <- merge(x = stats,
               y = the_list,
               c("nameLast", "nameFirst"),
               all.x = TRUE)
# Just selecting the columns that I need
stats <-
  stats %>% select(
    playerID,
    yearID,
    nameFirst,
    nameLast,
    age,
    theList,
    weight,
    birthYear,
    teamID,
    num_years,
    G,
    AB,
    H,
    HR
  )

stats <-
  stats %>% filter(AB > 0) %>% mutate(per_at_bat = HR / AB, b_avg = (H / AB)) %>%
  mutate(theList = ifelse(is.na(theList), FALSE, TRUE)) %>%
  mutate(HR_per_500 = per_at_bat * 500) %>%
  mutate(HR_after_31 = ifelse(age > 31, HR, 0)) %>%
  mutate(HR_bin = cut(
    HR,
    breaks = breaks_HR,
    include.lowest = TRUE,
    right = FALSE,
    labels = c("under 30", "30 to 40", "40 to 50", "over 50")
  )) %>%
  mutate(y_bin = cut(
    yearID,
    breaks = breaks_yrs,
    include.lowest = TRUE,
    right = FALSE,
    labels = c(
      "1914_1945",
      "The_50yrs_before_roids",
      "during_steroids",
      "after_steroids"
    )
  ))
# sum_top is the top 500 HR hitting seasons --> all have over 35 HR
sum_top <- stats %>% filter(HR > 35, AB > 200) %>%
  arrange(desc(HR_per_500)) %>%
  top_n(500, HR_per_500) %>%
  group_by(., playerID, theList) %>%
  summarise(
    n = n(),
    max(HR),
    max(HR_per_500),
    min(HR_per_500),
    mean(HR_per_500),
    mean_age_per_top = mean(age),
    max_age = max(age)
  )
stats_grouped <-
  stats %>% group_by(playerID, theList) %>% summarise(
    n = n(),
    car_mean_HR = mean(HR),
    car_mean_AB = mean(AB),
    car_mean_HRp500 = mean(HR_per_500),
    tot_HR = sum(HR),
    car_avg = mean(b_avg) * 100,
    max_age = max(age),
    HR_after_31 = sum(HR_after_31),
    percent_after_31 = sum(HR_after_31 / sum(HR) * 100),
    last_year = max(yearID)
  ) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  mutate(percent_after_31 = percent_after_31 / 100, car_avg = car_avg / 100)
sum_top_c <-
  merge(x = sum_top,
        y = stats_grouped,
        "playerID",
        all.x = TRUE)
names(sum_top_c)[3] <- "n_yrs_top"
names(sum_top_c)[4] <- "Max_HR_T"
names(sum_top_c)[5] <- "Max_HR(500)-T"
names(sum_top_c)[6] <- "Min_HR(500)-T"
names(sum_top_c)[7] <- "Mean_HR(500)-T"
names(sum_top_c)[8] <- "Mean_Age-T"
names(sum_top_c)[9] <- "Max_age_in_top"
names(sum_top_c)[11] <- "n_yrs"
names(sum_top_c)[12] <- "Mean_HR"
names(sum_top_c)[13] <- "Mean_AB"
names(sum_top_c)[14] <- "Mean_HR(500)"
sum_top_c <-
  sum_top_c %>% mutate("Mean_Age-T" = round(`Mean_Age-T`, digits = 1))

max_hr_year_stat <-
  (stats %>% filter(HR > 20) %>% group_by(playerID) %>% top_n(1, HR) %>% arrange(desc(HR)))
#max_hr_year_stat %>% group_by(y_bin) %>% summarize(mean(age), mean(HR), n =n())
by_bins <-
  stats %>% group_by(y_bin, HR_bin) %>%
  summarize(mean(age), mean(HR), n = n()) %>% arrange(desc(HR_bin))
# team stats ####
teams_adj <-
  teams %>% group_by(yearID) %>% summarise(
    avg_G = mean(G),
    n = n(),
    tot_HR = sum(HR),
    avg_HR = mean(HR),
    avg_R = mean(R),
    b_avg = sum(H) / sum(AB)
  ) %>%
  mutate(adj_avg_HR = (avg_HR / avg_G * 162),
         adj_avg_R = (avg_R / avg_G * 162))
col_offset <- 'column(9, offset = 1'
sty = "font-size: 200%;"
sty_1 ="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"
sty_2 ="text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"
sty_3 ="text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px;
font-size:15px"
# end of data setup
# ** UI ** ####
ui <- navbarPage(
  id = "tabs",
  fluid = T,
  collapsible = T,
  
  title = "MLB - Hitting With Juice",
  
  # Word Cloud ####
  tabPanel(icon("home"), 
           fluidPage(fluidRow(
             column(1),
             fluidRow(
               column(
                 10,
                 offset = 1,
                 titlePanel(h1("Everyone Loves The Long Ball",
                               align = "center")),
                 br(),
                 fluidRow(
                   column(
                     10,
                     offset = 1,
                     h4(
                       'In the last 25 years, Homeruns are up for MLB, but individual Homerun records
           are not falling like the were the first 10 years of this trend! Why is this?
           Why were they up in the first place?', style= sty_1),
                     br(),
                     h4(
                       'What is different now? What is different the last 25 years from the previous 75 years?',
                       'So many questions, so few answers! - until now that is...', style=sty_2),
                     br(),
                     h4(
                       "First, for those that don't follow baseball, a little refresher on what I'm 
                       talking about From 1927 until 1961, the Homerun single season record was 
                       held by Babe Ruth - 60 in a season! the unbeatable record. Prior to that, 
                       it was Babe Ruth with multiple seasons of 40 - 59 homeruns.", style = sty_1),
                     br(),
                     h4(
                       "In 1961, Roger Maris and Mickey Mantle battled for the homerun title, 
                       they were trying to become the first to top 60 homeruns in a season! 
                       Roger Maris pulled through and hit 61 homeruns in 1961. That record stood 
                       until 1998 - both Mark McGwire and Sammy Sosa hit over 61 homers - 70 McGwire 
                       and 66 Sosa.", style = sty_2),
                     br(),
                     h4(
                       "But the story starts before that, starting in 1995 players started pushing the 50 Homerun mark.
            In the history of baseball until 1995 only 13 times had 50 HR's been matched or beat! 100+ years, 13 times...
           What follows in the last 25 years, is that number being beaten 28 times! ", style = sty_1),
                     br(),
                     h4("The start of the steroid era!", align = 'center'),
                     br()
                   )
                 ),
                 fluidRow(column(
                   9,
                   offset = 2,
                   h5('- Stats are from Baseball Reference'),
                   h5('- Steroid references are from bleacher report')
                 )),
                 
                 
                 fluidRow(column(
                   7,
                   offset = 2,
                   h5(
                     '- Any perceived accusation of steroid use or basic cheating, is purely coincidence! No such accusations are made by anyone but the DATA!',
                     align = 'left'
                   )
                 )),
                 fluidRow(column(
                   7,
                   offset = 2,
                   h5(
                     '- The aurthor of this app has no knowledge of anyone using steroids,
                        nor does he accuse anyone of using steroids!',
                     align = 'left'
                   )
                 ), br()),
                 fluidRow(column(
                   7,
                   offset = 2,
                   h4("- Players that have hit more than 40 Homeruns in a Season! -",
                      align = "left")
                 )),
                 fluidRow(
                   wordcloud2Output("word_cloud", height = "600px", width = "95%"),
                   align = "center"
                 ),
                 br(),
                 p()
               )
             )
           ))),
  # Tab Long Ball ####
  tabPanel(
    "The Long Ball",
    tags$style(
      type = 'text/css',
      '.navbar { background-color: lightgrey;}',
      '.navbar-default .navbar-brand{color: darkorange;}',
      '.tab-panel{ background-color: white; color: black;}',
      '.navbar{ font-size: 14px;}'
    ),
    
    fluidPage(
      titlePanel("MLB Homerun Stats - Frequency of Batters Hitting Above a min # of HR's in a Time Peroid "),
      sidebarLayout(
        sidebarPanel(title= 'Chart Controls',fluidRow(), br(), fluidRow(br()),
          sliderInput(
            "bins",
            tags$p("Number of bins:", style = sty),
            min = 5,
            max = 40,
            value = 22
          ),
          sliderInput(
            "min_hr",
            tags$p("Min Number of Homeruns", style = sty),
            min = 10,
            max = 60,
            value = 45
          ),
          checkboxInput(
            inputId = "addmedian",
            label = tags$p("Add median line", style = sty),
            value = FALSE
          ),
          checkboxInput(
            inputId = "addsteroidyears",
            label = tags$p("Add Lines for Steroid Years", style = sty),
            value = FALSE
          ),
          checkboxInput(
            inputId = "per500ab",
            label = tags$p("Per 500 At Bats", style = sty),
            value = FALSE
          ),
          checkboxInput(
            inputId = "per_game_500ab",
            label = tags$p("Scales per 500 At Bats and if 30 Teams", style = sty),
            value = FALSE
          ),
          width = 3
        ),
        mainPanel(
          fluidRow(h2("Hitting The Long Balls",
                      align = "center", style = "color:red")),
          br(),
          fluidRow(h3(textOutput("notes"), style = "color:PowderBlue")),
          fluidRow(h3(textOutput("notes1"), style = "color:PowderBlue"),
                   br(2)),
          fluidRow(
            h4("The Steroid Era is said to be from mid 90's until mid 2000's estimates are said to be from 25% to over 60%+ 
               of all players using during that time period Does this explain the jump in HR's? Are players 
               still hitting HR's at a high Rate?", style = sty_1)
          ),
          width = 9,
          fluidRow(),
          fluidRow(plotOutput("distPlot"), width = 9)
        )
      )
    )
  ),
  # Homeruns Stacked ####
  tabPanel(
    "Homeruns Stacked",
    fluidPage(
      fluidRow(column(9, offset=2, h1(
        "Hitting the longball throughout their career"))),
      fluidRow(
        column(width = 1),
        column(
          width = 2,
          checkboxInput(
            inputId = "showHR",
            label = "Show HR's in Chart",
            value = F)
        ),
        column(
          width = 3,
          sliderInput(
            inputId = "totHR_sld",
            label = "Total HR Range",
            min = 200,
            max = 800,
            value = c(550, 800),
            step = 50,
            sep = '')
        ),
        column(
          5,
          h5(
            "One of the things that caught my attention,
                        is the number of HR's by players late in their careers
                        and played since 1995 - Another attention grabber,
                        the amount of over 60 HR years are all
                        bunched around the same years", style = sty_2)
        )
      ),
      fluidRow(column(11, offset = 1, plotOutput("plot_player"))),
    )
  ),
  # Team Trends ####
  tabPanel(
    "Team Trends",
    titlePanel(
      h1("Average HR and Runs per team adjusted for 162 games", align = 'center')
    ),
    fluidRow(
      column(
        4,
        sliderInput(
          "s_year",
          tags$p("Start / End Year", style = sty),
          min = 1920,
          max = 2020,
          value = c(1919, 2021),
          step = 10,
          sep = ""
        )
      ),
      column(
        4,
        checkboxInput(
          inputId = "addruns",
          label = tags$p("Average runs per team per season", style = sty),
          value = FALSE
        ),
        checkboxInput(
          inputId = "addavg",
          label = tags$p("Batting average times 1000", style = sty),
          value = FALSE
        )
      ),
      column(
        4,
        sliderInput(
          "HRn",
          tags$p("Number of HRs:", style = sty),
          min = 10,
          max = 50,
          value = 35
        ),
        sliderInput(
          "AGE_",
          tags$p("Age of Batter Hitting HR:", style = sty),
          min = 18,
          max = 50,
          value = 20
        )
      )
    ),
    fluidRow(column(7,
                    plotOutput('team_plots')),
             column(5, plotOutput("hex"))),
    br(),
    fluidRow(column(7, offset=2,
    h5(
      "-> over the 1960's the rules had been relaxed to help the pitchers by
          creating a bigger strike zone 1968 was called year of the pitcher because they were 
          so dominate after 68 they lowered the pitching mound from 15 to 10 inches and the 
          strike zone was lowered from top of the shoulders to armpits", style = sty_1))),
    fluidRow(column(5, offset=3, 
                    tags$ul(style=sty_3,
    tags$li("-> 1981 only 107 games strike year"),
    tags$li("-> 1994 only 114 games strike year"),
    tags$li("-> 1995 only 144 games strike year"),
    tags$li("-> 1994 and 2005 are the red dotted lines - this is era in question"),
    tags$li(
      " - - - All years were adjusted to represent the same number of games, 162 - - -"
    ))))
  ),
  # HR over age 31####
  tabPanel("HRs Over Age 31",
           titlePanel(
             h1("Career HR's Before 32 Compared to After 32", align = 'center')
           ),
           fluidPage(
             fluidRow(
               column(
                 3,
                 sliderInput(
                   "t_HR",
                   tags$p("Total Min HR - Career", style = sty),
                   0,
                   800,
                   300,
                   100,
                   ticks = TRUE,
                   dragRange = T
                 )
               ),
               column(
                 3,
                 checkboxInput(
                   "totHR_HRpre31",
                   tags$p("Toggle bewteem total HRs or HR's before 32 on Y axis", style = sty),
                   FALSE
                 )
               ),
               column(5,
                      verbatimTextOutput("counts"))
             ),
             fluidRow(column(12, plotOutput("over_31")),
                      column(12, div(DT::dataTableOutput("over31_tb"), style = "font-size:180%")))
           )),
  # Summary ####
  tabPanel(
    "Summary",
    fluidPage(
      fluidRow(column(3,
      sliderInput(
        "HR_Sum",
        tags$p("Min Number of HR:", style = sty),
        min = 5,
        max = 50,
        value = 30
      )),
      column(3,
      sliderInput(
        "years_",
        tags$p("Years To Seperate:", style = sty),
        min = 1920,
        max = 2020,
        value = c(1994, 2005),
        step = 5,
        sep = "")),
      column(3,
      checkboxInput(
        inputId = "use_list",
        label = tags$p("Based on the list", style = sty),
        value = FALSE))
      ),
      fluidRow(),
      div(DT::dataTableOutput("summary"), style = "font-size:180%")
    )
  ),
  # Tab Per 500 ####
  tabPanel(
    "Data Frame For More Info",
    fluidPage(
      theme = "bootstrap.css",
      titlePanel("Homeruns Per at Bat If They Had 500 at Bats"),
      fluidRow(
        column(10, "Player HR/AB*500 - makes all players on same playing field")
      ),
      fluidRow(
        column(
          10,
          "This table then has the top 500 seasons and number of times
                       the top players have had one of these seasons"
        )
      ),
      fluidRow(br()),
      fluidRow(column(12, DT::dataTableOutput("per500")))
    )
  ),
  # Age at best ####
  tabPanel(
    "Age At Best",
    fluidPage(
      titlePanel(
        "Before 1995, How Old Was a Player When They Hit Their Most Homeruns in a Season?"
      ),
      titlePanel(
        checkboxInput(
          inputId = "adddots",
          label = "Add age points",
          value = FALSE
        )
      ),
      fluidRow(plotOutput("plot_hrbin_hr")),
      titlePanel("1995 and on, Players Age of Most Homeruns?"),
      fluidRow(plotOutput("plot_hrbin_age"))
    )
  ),
  tabPanel(
    'Code',
    'just a friendly little old test',
    # q to show a to hide
    useShinyjs(),
    tags$script(
      HTML(
        "$(function(){
      $(document).keyup(function(e) {
      if (e.which == 81) {
        $('#showTab').click()
      }
      if (e.which == 65) {
        $('#hideTab').click()
      }
      });
      })"
      )
    ),
    
    actionButton('hideTab', "Hide 'code' tab"),
    actionButton("showTab", "Show 'code' tab")
  ),
  # OddsProb ####
  tabPanel('OddsProb',
           fluidPage(
             fluidRow(
               style = "border: 4px double red;",
               column(
                 4,
                 textInput(
                   inputId = 'odds',
                   label = 'The Vegas Odds',
                   value = 0
                 ),
                 textInput(
                   inputId = 'bet',
                   label = 'Bet amount',
                   value = 100
                 )
               ),
               column(
                 5,
                 br(),
                 textOutput('impliedOdds'),
                 textOutput('win'),
                 textOutput('payout')
               )
             ),
             fluidRow(actionButton('runIt', 'Run It'))
           ))
)

# Server ####
server <- function(input, output) {
  # Plot Data ####
  plot_data <- reactive({
    stats %>% filter(HR > input$min_hr)#
    #ifelse(input$per500ab==T,HR_per_500 > input$min_hr, HR>input$min_hr))
  })
  plot_data500 <- reactive({
    stats %>% filter(HR_per_500 > input$min_hr)#
    #ifelse(input$per500ab==T,HR_per_500 > input$min_hr, HR>input$min_hr))
  })
  plot_data500_per_team <- reactive({
    #print(stats)
    stats %>% group_by(yearID) %>%
      summarise(
        n_factor = 1 / n_distinct(teamID) * 30,
        n = n_distinct(teamID),
        n1 = (sum(HR_per_500 > input$min_hr) * n_factor)
      ) %>%
      ungroup() %>% select(yearID, n1)
  })
  # distPlot ####
  output$distPlot <- renderPlot({
    if (input$per500ab) {
      x <- plot_data500()[, 2]
    } else {
      x <- plot_data()[, 2]
    }
    if (input$per_game_500ab) {
      x <- plot_data500_per_team()
    }
    # generate bins based on input$bins from ui.R
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    if (input$per_game_500ab) {
      plot(x)
      
    } else{
      hist(
        x,
        breaks = bins,
        col = 'blue',
        border = 'white',
        xlab = "Years",
        main = "Number of players hitting at Least selected homeruns"
      )#, title = 'Number of HR seasons')
    }
    if (input$addsteroidyears) {
      abline(
        v = 1994,
        lwd = 5,
        lty = 1,
        col = "red"
      )
      abline(
        v = 2005,
        lwd = 5,
        lty = 1,
        col = "red"
      )
    }
    if (input$addmedian) {
      if (input$per_game_500ab == F) {
        abline(v = median(x),
               lwd = 2,
               lty = 2)
      }
    }
  })
  # output notes ####
  output$notes <-
    renderText({
      paste0("Number of batters with at least ->",
             input$min_hr,
             " homeruns!")
    })
  output$notes1 <-
    renderText({
      paste0("Each bin represents ",
             sprintf("%#0.1f", 100 / input$bins),
             " years")
    })
  output$notes2 <-
    renderText({
      paste0("Steroid Years are considered to be from early 90's to mid 2000's,")
    })
  output$notes3 <-
    renderText({
      paste0("and are said to be at the peak between 1994 to 2005 with up to 60% of players using")
    })
  # per500 ####
  output$per500 <-
    DT::renderDataTable({
      DT::datatable(
        sum_top_c  %>% filter(n_yrs_top > 3) %>% select(
          -"Max_HR_T",-"Max_HR(500)-T",-"Mean_HR(500)-T",-"Mean_AB",-"Min_HR(500)-T"
        ) %>% arrange(desc(percent_after_31)),
        rownames = FALSE,
        options = list(
          pageLength = 50,
          autoWidth = T,
          searching = F
        )
      ) %>% formatStyle('theList.x',
                        target = 'row',
                        backgroundColor = styleEqual(c(0, 1), c("#eb6e1f ", "#00AFBB")))
    })
  # plot_hrbin_hr ####
  output$plot_hrbin_hr <- renderPlot({
    p = ggplot(max_hr_year_stat %>% filter(yearID < 1995),
               aes(x = HR_bin,
                   y = age,
                   fill = HR_bin)) + geom_boxplot() + stat_summary(
                     fun = mean,
                     geom = "point",
                     shape = 23,
                     size = 4
                   ) +
      theme(legend.position = "top") + labs(x = "Homerun Bins", y =
                                              "Age of Top Year") +
      geom_hline(
        yintercept = 30,
        linetype = "dashed",
        color = "red",
        size = 2
      )
    if (input$adddots) {
      p + geom_dotplot(binaxis = 'y',
                       stackdir = 'center',
                       position = position_dodge(1))
    } else{
      p
    }
  })
  # plot HR Bin Age ####
  output$plot_hrbin_age <- renderPlot({
    p <-
      ggplot(max_hr_year_stat %>% filter(yearID > 1994),
             aes(x = HR_bin,
                 y = age,
                 fill = HR_bin)) + geom_boxplot() + stat_summary(
                   fun = mean,
                   geom = "point",
                   shape = 23,
                   size = 4
                 ) +
      theme(legend.position = "bottom") + labs(x = "Homerun Bins", y =
                                                 "Age of Top Year") +
      geom_hline(
        yintercept = 30,
        linetype = "dashed",
        color = "red",
        size = 2
      )
    if (input$adddots) {
      p + geom_dotplot(binaxis = 'y',
                       stackdir = 'center',
                       position = position_dodge(1))
    } else{
      p
    }
  })
  # plot_player ####
  output$plot_player <- renderPlot({
    p <-
      stats %>% group_by(playerID, nameLast, theList)  %>% mutate(tot_hr = sum(HR)) %>%
      filter(between(tot_hr, input$totHR_sld[1], input$totHR_sld[2])) %>%
      group_by(playerID) %>% arrange(age) %>%
      ggplot(aes(
        x = reorder(paste(playerID, nameLast, theList, sep = ' - '),-tot_hr),
        y = HR,
        fill = HR,
        height = "1500px"
      )) + geom_bar(stat = "identity") +
      scale_color_gradient(low = 'lightblue', high = 'blue') +
      labs(
        title = 'Total Homeruns Stacked by Season',
        fill = 'HR - Season',
        x = 'Players - True-False from bleacher report Steroid List',
        y = "HR's, stacked first season on bottom",
        hjust = 0.5
      ) +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        size = 15,
        colour = "#00afbb"
      )) +
      theme(text = element_text(size = 25, colour = "#eb6e1f"))#ifelse(theList == T, "#00AFBB", "#eb6e1f")))
    if (input$showHR) {
      p + geom_text(
        aes(label = HR),
        position = position_stack(vjust = 0.5),
        colour = "white",
        size = 4
      )
    } else {
      p
    }
  }, height = 700)
  # wrapped_plot ####
  output$wrapped_plot <- renderPlot({
    by_bins %>% filter(HR_bin != "under 30") %>%
      ggplot(aes(
        fill = HR_bin,
        y = n,
        x = HR_bin,
        label = n
      )) +
      geom_bar(position = "dodge", stat = "identity") +
      #            #scale_fill_viridis(discrete = T, option = "E") +
      facet_wrap(~ y_bin) +
      ggtitle("MLB Long Balls!") +
      ylab("Number of palyers that hit at least 30 homeruns") +
      xlab("Four different periods of baseball (20 years, 50 years, 11 years, and 14 years") +
      geom_text(size = 4, position = position_stack(vjust = 0.5))
  })
  # word_cloud ####
  output$word_cloud <- renderWordcloud2({
    wordcloud2(
      stats %>% filter(HR > 40) %>% mutate(words_ = paste(nameFirst, nameLast, " ")) %>%
        select(words_, HR) %>% group_by(words_) %>% summarise(HR =
                                                                sum(HR)),
      size = .8,
      color = "random-light",
      backgroundColor = "grey"
    )
  })
  # team_plots ####
  output$team_plots <- renderPlot({
    p <-
      ggplot(data = teams_adj %>% filter(between(yearID, input$s_year[1], input$s_year[2])), aes(x = yearID)) +
      geom_line(aes(y = adj_avg_HR, color = "red")) +
      labs(title = "This is my chart", y = "Average HR per Team") +
      scale_x_continuous("Years", breaks = seq(
        input$s_year[1],
        input$s_year[2],
        (input$s_year[2] - input$s_year[1]) / 10
      ))
    if (input$addavg) {
      p = p +
        geom_line(aes(y = b_avg * 1000, color = "green"), linetype = "dashed")
    }
    if (input$addruns) {
      p = p +
        geom_line(aes(y = adj_avg_R - 500, color = "blue"), linetype = "twodash") +
        scale_y_continuous(
          "Average HR per Team",
          breaks = seq(25, 275, 50),
          sec.axis = sec_axis( ~ . + 500,
                               name = "Average Runs Scored per Team")
        )
    }
    p <- p + geom_vline(
      xintercept = c(1994, 2005),
      linetype = "dashed",
      color = "red",
      size = 1
    ) +
      scale_color_discrete(name = "Legend",
                           labels = c("Dingers", "AVG", "Runs"))
    p <- p + theme(
      axis.text = element_text(
        angle = 45,
        color = "red",
        size = 15,
        face = 3
      ),
      axis.title.x = element_text(
        color = "blue",
        size = 17,
        face = "bold"
      ),
      axis.title.y = element_text(
        color = "blue",
        size = 14,
        face = "bold"
      ),
      axis.ticks = element_line(size = 1, color = "red") ,
      axis.ticks.length = unit(.25, "cm")
    )
    p
  })
  # stacked_roids ####
  output$stacked_roids <-
    DT::renderDataTable({
      DT::datatable(
        stats %>% group_by(y_bin, HR_bin) %>% summarise(mean(HR), mean(HR_per_500), mean(AB), n =
                                                          n(), mean(age)),
        rownames = FALSE,
        options = list(
          pageLength = 50,
          autoWidth = T,
          searching = F
        )
      ) %>%
        formatStyle((last_year > 1994),
                    target = 'row',
                    backgroundColor = styleEqual(c(0, 1), c('lightgrey', 'lightblue')))
    })
  # over_31 ####
  output$over_31 <- renderPlot({
    gg <-
      stats_grouped %>% filter(tot_HR > input$t_HR) %>% arrange(desc(HR_after_31)) %>%
      ggplot(aes(x =
                   HR_after_31, y = if (input$totHR_HRpre31) {
                     (tot_HR - HR_after_31)
                   } else{
                     tot_HR
                   })) + geom_point(aes(col = (last_year > 1994), size = tot_HR)) + scale_fill_manual(values = c("#00AFBB", "#eb6e1f"))
    gg
  })
  output$over31_tb <-
    DT::renderDataTable({
      DT::datatable(
        stats_grouped %>% filter(tot_HR > input$t_HR) %>% arrange(desc(HR_after_31)),
        rownames = FALSE,
        options = list(
          pageLength = 20,
          autoWidth = T,
          searching = F
        )
      ) %>% formatStyle('theList',
                        target = 'row',
                        backgroundColor = styleEqual(c(0, 1), c("#eb6e1f", "#00AFBB")))
    })
  # counts ####
  output$counts <-
    renderPrint({
      (
        stats_grouped %>% filter(tot_HR > input$t_HR) %>% group_by(theList) %>%
          summarise(mean(HR_after_31), n = n())
      )
    })
  # Summary ####
  output$summary <- DT::renderDataTable({
    DT::datatable(
      stats %>% filter(HR > input$HR_Sum) %>% 
        mutate(in_range=ifelse(between(yearID, input$years_[1], input$years_[2]),T, F)) 
      %>% select(in_range, age, weight, num_years, HR, HR_per_500, HR_after_31) %>%
        group_by(in_range) %>% skim() %>% select(-n_missing, -complete_rate), options = list(dom = 't')) %>% formatStyle('in_range',
                      target = 'row',
                      backgroundColor = styleEqual(c(0, 1), c("#eb6e1f ", "#00AFBB"))) %>% 
      formatRound(c('numeric.mean', 'numeric.sd', 'numeric.p0', 'numeric.p25', 'numeric.p50', 
                    'numeric.p75', 'numeric.p100'),1) 
    
  # ifelse((input$use_list), ifelse(theList='TRUE',T, F)), (  
  })
  
  # Hex ####
  output$hex <- renderPlot({
    xB <- 25
    statsx <- stats %>% filter(HR > input$HRn, age > input$AGE_)
    Year <- statsx$yearID
    HRs <- statsx$HR
    Age <- statsx$age
    bin <- hexbin(Year, HRs, xbins = xB)
    colors <- as.numeric(statsx$y_bin)
    cols <-
      colorRampPalette(
        c(
          "lightblue",
          "deepskyblue1",
          "yellow",
          "darkorchid4",
          "darkblue",
          "green",
          "orchid",
          "hotpink1",
          "red",
          "tomato",
          "dimgrey",
          'darkslategrey',
          'midnightblue',
          'black',
          'black',
          'black'
        )
      )
    plot(
      bin,
      main = "Hexagonal Binning of Homeruns Over Years",
      legend = 1,
      colramp = function(n)
        cols(input$HRn)
    )
  })
  
  # output stats - finish and add tab ####
  output$tstats <- renderText({
    stats_wo <- stats %>% filter(theList == F, HR > 30)
    stats_ws <- stats %>% filter(theList == T, HR > 30)
  })
  # Starts with these Tabs hidden ####
  observe(hideTab(inputId = "tabs", target = 'Code'))
  observe(hideTab(inputId = "tabs", target = 'Age At Best'))
  observe(hideTab(inputId = "tabs", target = 'Data Frame For More Info'))
  observe(hideTab(inputId = "tabs", target = 'OddsProb'))
  
  # Hide Tabs - a key ####
  observeEvent(input$hideTab, {
    hideTab(inputId = "tabs", target = 'Code')
    hideTab(inputId = "tabs", target = 'Age At Best')
    hideTab(inputId = "tabs", target = 'Data Frame For More Info')
    hideTab(inputId = "tabs", target = 'OddsProb')
  })
  # Show Tabs = q key ####
  observeEvent(input$showTab, {
    showTab(inputId = "tabs", target = 'Code')
    showTab(inputId = "tabs", target = 'Age At Best')
    showTab(inputId = "tabs", target = 'Data Frame For More Info')
    showTab(inputId = "tabs", target = 'OddsProb')
  })
  # calculating Odds - Not Used in this project - Just for Fun ####
  observeEvent(input$runIt, {
    bet <- as.numeric(input$bet)
    odds <- as.numeric(input$odds)
    win <- if (odds < 0) {
      bet * (-100 / odds)
    } else {
      bet * (odds / 100)
    }
    impliedOdds <- if (odds < 0) {
      odds / (100 - odds) * -1
    } else{
      1 - (odds / (100 + odds))
    }
    impliedOdds <- scales::percent(impliedOdds, accuracy = 0.01)
    
    payOut <- win + bet
    output$win <-
      renderText({
        paste0('to Win! : $', sprintf(win, fmt = '%#.2f'))
      })
    output$impliedOdds <-
      renderText({
        paste0('Implied Odds! : ', impliedOdds)
      })
    output$payout <-
      renderText({
        paste0('Payout! : $', sprintf(payOut, fmt = '%#.2f'))
      })
  })
}
# Run the application
shinyApp(ui = ui, server = server)
