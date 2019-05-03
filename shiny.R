library(shiny)
library(shinydashboard)

# Create user interface ----
ui <- dashboardPage(
  dashboardHeader(title = "NOAA Chehalis Model"),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Spawner abundance'),
      menuSubItem('Basinwide', 'spawner_basin', newtab = TRUE),
      menuSubItem('By EDR', 'spawner_edr', newtab = TRUE),
      menuSubItem('By Subbasin', 'spawner_sub', newtab = TRUE),
      menuItem('Capacity'),
      menuSubItem('Rearing', 'cap_rear', newtab = TRUE),
      menuSubItem('Spawning', 'cap_spawn', newtab = TRUE),
      menuItem('Productivity'),
      menuSubItem('Egg to fry', 'prod_egg', newtab = TRUE),
      menuSubItem('Rearing', 'prod_rear', newtab = TRUE),
      menuSubItem('Prespawn', 'prod_spawn', newtab = TRUE),
      menuItem('Area'),
      menuItem('Comparison plots'),
      menuSubItem('Spawners: feature to dev', 'spawner_compare', newtab = TRUE),
      menuSubItem('Capacity: feature to dev', 'cap_compare', newtab = TRUE),
      menuSubItem('Productivity: feature to dev', 'prod_compare', newtab = TRUE)
    )
  ),
  dashboardBody(
    tabItems(
      # basinwide spawner plots
      tabItem(tabName = 'spawner_basin',
              fluidRow(
                box(
                  title = 'Basinwide spawners:  Diagnostic scenarios',
                  plotOutput('spawners_basinwide_diag', height = 800)),
                box(
                  title = 'Basinwide spawners: ASRP scenarios',
                  plotOutput('spawners_basinwide_asrp', height = 800))
              )
      ),
      tabItem(tabName = 'spawner_edr',
              fluidRow(
                column(width = 2,
                  box(
                    width = NULL,
                    selectInput('edr_scenario', 'Scenario:', choices = unique(spawners_edr$scenario))
                  )),
                  column(width = 10,
                    box(
                    width = NULL,
                    title = 'Total change in spawners per EDR',
                    plotOutput('spawners_tot_edr', height = 400)
                    ),
                    box(
                      width = NULL,
                      title = 'Percent change in spawners per EDR',
                      plotOutput('spawners_perc_edr', height = 400)
                  ))
                )
              ),
      tabItem(tabName = 'spawner_sub',
              fluidRow(
                column(width = 2,
                       box(
                         width = NULL,
                         selectInput('sub_scenario', 'Scenario:', choices = unique(spawners_sub$scenario))
                         )),
                column(width = 10,
                       box(width = NULL,
                           title = 'Total change in spawners per subbasin',
                           plotOutput('spawners_tot_sub', height = 400)
                           ),
                       box(
                         width = NULL,
                         title = 'Percent change in spawners per subbasin',
                         plotOutput('spawners_perc_sub', height = 400)
                       )
                       ))
              ),
      # spawner comparison plots
      tabItem(tabName = 'spawner_compare',
              fluidRow(
                box(
                  title = 'Basinwide spawner comparison: feature to dev',
                  plotOutput('spawner_compare', height = 800, width = 1200)
                )
              )
      ),
      # Productivity by reach map
      tabItem(tabname = 'prod_rear',
              fluidRow(
                box(
                  title = 'Summer productivity by reach',
                  plotOutput('summer_prod_map', height = 800)
                  )
                )
              )
    )
  )
)

# Create server/plotting ----

server <- function(input, output) {
  output$spawners_basinwide_diag <- renderPlot({
    diag_plot
  })
  output$spawners_basinwide_asrp <- renderPlot({
    asrp_plot
  })
  
  tot_prcnt_chng <- reactive({
    spawners_edr %>%
    filter(scenario == 'Current' | scenario == input$edr_scenario) %>%
    group_by(scenario) %>%
    summarize(spawners = sum(spawners)) %>%
    ungroup %>%
    summarize(prcnt.change = (spawners[scenario == input$edr_scenario] - spawners[scenario == "Current"])/spawners[scenario == "Current"]*100) %>%
    as.numeric()
  })
  
  dat <- reactive({
    dat <- spawners_edr %>%
      filter(scenario == input$edr_scenario) 
  })
  
  output$spawners_perc_edr <- renderPlot({
    dat() %>%
      ggplot() +
        theme_bw() +
        geom_bar(aes(EcoRegion, prcnt.change),
                 stat = 'identity',fill = unique(dat()$color),color = 'black', position = 'dodge',na.rm = T) +
        theme(axis.text.x = element_blank(),
              axis.title.y = element_text(margin = margin(r = 10))) +
      scale_y_continuous(labels = percent) +
      labs(x = NULL,
           y = 'Percent Difference\nfrom Current Scenario',
           title = input$edr_scenario) +
      annotate("text", x = 0, y = Inf, vjust = 1.02, hjust = -0.05,
               label = paste0("Overall = +",format(round(tot_prcnt_chng(), 0), big.mark = ","), "%"))

    })
  
  output$spawners_tot_edr <- renderPlot({
    dat() %>%
      ggplot(aes(EcoRegion,spawners.change)) +
        theme_bw() +
        geom_bar(stat = 'identity',fill = unique(dat()$color),color = 'black',na.rm = T) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              axis.title.y = element_text(margin = margin(r = 10))) +
        scale_y_continuous(labels = comma) +
        labs(x = NULL,
             y = 'Change in Spawners\nfrom Current Scenario',
             caption = paste0('Model version = ',hab.ver)
        ) +
        annotate("text", x = 0, y = Inf, vjust = 1.02, hjust = -0.05,
                 label = paste0("Total = +", format(round(dat() %>%
                                                            summarize(n = sum(spawners.change)) %>%
                                                            as.numeric(),
                                                          0), big.mark = ",")))
  })
  
  dat_sub <- reactive({
    dat_sub <- spawners_sub %>%
      filter(scenario == input$sub_scenario) 
  })
  
  output$spawners_perc_sub <- renderPlot({
    dat_sub() %>%
      ggplot() +
      theme_bw() +
      geom_bar(aes(Subbasin, prcnt.change),
               stat = 'identity',fill = unique(dat_sub()$color),color = 'black', position = 'dodge',na.rm = T) +
      theme(axis.text.x = element_blank(),
            axis.title.y = element_text(margin = margin(r = 10))) +
      scale_y_continuous(labels = percent) +
      labs(x = NULL,
           y = 'Percent Difference\nfrom Current Scenario',
           title = input$sub_scenario) +
      annotate("text", x = 0, y = Inf, vjust = 1.02, hjust = -0.05,
               label = paste0("Overall = +",format(round(tot_prcnt_chng(), 0), big.mark = ","), "%"))
    
  })
  
  output$spawners_tot_sub <- renderPlot({
    dat_sub() %>%
      ggplot(aes(Subbasin,spawners.change)) +
      theme_bw() +
      geom_bar(stat = 'identity',fill = unique(dat_sub()$color),color = 'black',na.rm = T) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.title.y = element_text(margin = margin(r = 10))) +
      scale_y_continuous(labels = comma) +
      labs(x = NULL,
           y = 'Change in Spawners\nfrom Current Scenario',
           caption = paste0('Model version = ',hab.ver)
      ) +
      annotate("text", x = 0, y = Inf, vjust = 1.02, hjust = -0.05,
               label = paste0("Total = +", format(round(dat_sub() %>%
                                                          summarize(n = sum(spawners.change)) %>%
                                                          as.numeric(),
                                                        0), big.mark = ",")))
  })
  
  
  output$spawner_compare <- renderPlot({
    compare_plots
  })
  output$summer_prod_map <- renderPlot({})
}

shinyApp(ui, server)

