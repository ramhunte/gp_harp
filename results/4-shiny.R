library(shiny)
library(shinydashboard)

# Create user interface ----
ui <- dashboardPage(
  dashboardHeader(title = "NOAA Chehalis Model"),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Totals'),
      menuSubItem('Basinwide', 'total_basin', newtab = TRUE),
      menuSubItem('By Subbasin or EDR', 'total_sub', newtab = TRUE),
      menuItem('Comparisons'),
      menuSubItem('Scenarios to Current', 'compare_data', newtab = TRUE),
      menuSubItem('Compare to Previous Branch', 'compare_branch', newtab = TRUE),
      menuItem('Maps'),
      menuSubItem('Basin Map', 'map_data', newtab = TRUE)
    )
  ),
  dashboardBody(
    tabItems(
      # Totals at the basinwide level ----
      tabItem(tabName = 'total_basin',
              fluidRow(
                column(width = 2,
                       box(
                         width = NULL,
                         selectInput('species', 'Species:', choices = unique(shiny_lookup_tbl$species))
                       )
                ),
                column(width = 10,
                       box(
                         width = NULL,
                         title = 'Basinwide spawners:  Diagnostic scenarios',
                         plotOutput('total_basinwide_diag', height = 800)),
                       box(
                         width = NULL,
                         title = 'Basinwide spawners: ASRP scenarios',
                         plotOutput('total_basinwide_asrp', height = 800))
              )
              )
      ),
      # Totals at the subbasin or edr level ----
      tabItem(tabName = 'total_sub',
              fluidRow(
                column(width = 2,
                       box(
                         width = NULL,
                         selectInput('total_sub_species', 'Species:', choices = unique(shiny_lookup_tbl$species))
                       ),
                       box(
                         width = NULL,
                         selectInput('total_sub_param', 'Parameter:', choices = unique(shiny_lookup_tbl$param))
                       ),
                       box(
                         width = NULL,
                         selectInput('total_sub_scenario', 'Scenario:', choices = unique(shiny_lookup_tbl$scenario))
                       ),
                       box(
                         width = NULL,
                         selectInput('total_sub_scale', 'Scale:', choices = unique(shiny_lookup_tbl$basin_type))
                       )),
                column(width = 10,
                       box(
                         width = NULL,
                         title = 'Total change in spawners per EDR',
                         plotlyOutput('total_sub', height = 400)
                       ))
              )
      ),
      # Comparison with Current Scenario ----
      tabItem(tabName = 'compare_data',
              fluidRow(
                column(width = 2,
                       box(
                         width = NULL,
                         selectInput('compare_species', 'Species:', choices = unique(shiny_lookup_tbl$species))
                       ),
                       box(
                         width = NULL,
                         selectInput('compare_attribute', 'Choose attribute to compare', 
                                     choices = unique(shiny_lookup_tbl$param))
                       ),
                       box(
                         width = NULL,
                         selectInput('compare_scenario', 'Scenario:', 
                                     choices = unique(shiny_lookup_tbl$scenario))
                       ),
                       box(
                         width = NULL,
                         selectInput('compare_scale', 'Scale:',
                                     choices = c('EcoRegion', 'Subbasin'))
                       )),
                column(width = 10,
                       box(width = NULL,
                           title = 'Comparison Plot: Percent chg',
                           plotlyOutput('compare_data_prcnt', height = 400, width = 1200)
                       ),
                       box(width = NULL,
                         title = 'Comparison Plot: Total chg',
                         plotlyOutput('compare_data_tot', height = 400, width = 1200)
                       )
                       )
              )),
      # Compare feature branch to dev ----
      tabItem(tabName = 'compare_branch',
              fluidRow(
                column(width = 2,
                       box(
                         width = NULL,
                         selectInput('dev_comp_species', 'Species:', choices = unique(lookup_tbl_compare$species))
                       ),
                       box(
                         width = NULL,
                         selectInput('dev_comp_param', 'Parameter:', choices = unique(lookup_tbl_compare$param))
                       ),
                       box(
                         width = NULL,
                         selectInput('dev_comp_scenario', 'Scenario:', choices = unique(lookup_tbl_compare$scenario))
                       ),
                       box(
                         width = NULL,
                         selectInput('dev_comp_scale', 'Scale:', choices = unique(lookup_tbl_compare$basin_type))
                       )),
                column(width = 10,
                       box(
                         width = NULL,
                         title = 'Comparison Plot: Feature to dev',
                         plotlyOutput('compare_branch', height = 400, width = 1200)
                       ),
                       box(
                         width = NULL,
                         title = 'Comparison Plot: Total change from dev',
                         plotlyOutput('compare_branch_diff', height = 400, width = 1200)
                       ))
              )
      )#,
      # tabItem(tabName = 'map_data',
      #         fluidRow(
      #           column(width = 2,
      #                  # box(
      #                  #   wdith = NULL,
      #                  #   selectInput('map_species', 'Species:', choices = unique(fl$species))
      #                  # ),
      #                  box(
      #                    width = NULL,
      #                    selectInput('map_param', 'Parameter:', choices = unique(fl$param))
      #                  )),
      #           column(width = 10,
      #                  box(width = NULL,
      #                      title = 'Basin Map',
      #                      plotlyOutput('basin_map', height = 800, width = 1200)))
      #         ))
    )
  )
)

# Create server/plotting ----

server <- function(input, output) {
  output$total_basinwide_diag <- renderPlot({
    diag_plot
  })
  output$total_basinwide_asrp <- renderPlot({
    asrp_plot
  })
  
 tot_data <- reactive({
   tot_data <- shiny_lookup_tbl %>%
     filter(species == input$total_sub_species,
            basin_type == input$total_sub_scale,
            scenario == input$total_sub_scenario,
            param == input$total_sub_param) %>%
     group_by(basin, scenario, color, param) %>%
     summarize(data = sum(data, na.rm = T))
 })
 
 output$total_sub <- renderPlotly({
   ggplotly(
     tot_data() %>%
     ggplot() + 
     theme_bw() + 
     geom_bar(aes(basin, data),
              stat = 'identity', fill = unique(tot_data()$color), color = 'black', position = 'dodge', na.rm = T) +
     theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.y = element_text(margin = margin(r = 10))) +
     scale_y_continuous() +
     labs(x = NULL,
          y = 'Total',
          title = 'Total')) 
 })
  
  tot_prcnt_chng_compare <- reactive({
    shiny_lookup_tbl %>%
      filter(scenario == 'Current' | scenario == input$compare_scenario,
             param == input$compare_attribute) %>%
      group_by(scenario) %>%
      summarize(data = ifelse(input$compare_attribute %in% c('egg.to.fry.survival', 'prespawn.survival', 'summer.survival', 'winter.survival'),
                              mean(data, na.rm = T),
                              sum(data, na.rm = T))) %>%
      ungroup %>%
      summarize(prcnt.change = (data[scenario == input$compare_scenario] - data[scenario == "Current"])/data[scenario == "Current"]*100) %>%
      as.numeric()
  })
  
  compare_data <- reactive({
   compare_data <- shiny_lookup_tbl %>%
     filter(species == input$compare_species,
            basin_type == input$compare_scale,
            scenario == input$compare_scenario, 
            param == input$compare_attribute) %>%
     group_by(basin, scenario, color, param) %>%
     summarize(chg = sum(dat.chg, na.rm = T),
               prcnt = sum(prcnt.chg, na.rm = T))
    })
  
  output$compare_data_prcnt <- renderPlotly({
    ggplotly(
      compare_data() %>% 
      ggplot() +
      theme_bw() +
      geom_bar(aes(basin, prcnt),
               stat = 'identity',fill = unique(compare_data()$color),color = 'black', position = 'dodge',na.rm = T) +
      theme(axis.text.x = element_blank(),
            axis.title.y = element_text(margin = margin(r = 10))) +
      scale_y_continuous(labels = percent) +
      labs(x = NULL,
           y = 'Percent Difference\nfrom Current Scenario',
           title = paste0(input$compare_scenario, input$compare_attribute, input$compare_scale)) +
      annotate("text", x = 0, y = Inf, vjust = 1.02, hjust = -0.05,
               label = paste0("Overall = +",format(round(tot_prcnt_chng_compare(), 0), big.mark = ","), "%")))
    
  })
  
  output$compare_data_tot <- renderPlotly({
    ggplotly(
      compare_data() %>%
      ggplot(aes(basin, chg)) +
      theme_bw() +
      geom_bar(stat = 'identity',fill = unique(compare_data()$color),color = 'black',na.rm = T) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.title.y = element_text(margin = margin(r = 10))) +
      scale_y_continuous(labels = comma) +
      labs(x = NULL,
           y = 'Change in Spawners\nfrom Current Scenario',
           caption = paste0('Model version = ',hab.ver)
      )) 
  })
  
  compare_to_dev <- reactive({
    lookup_tbl_compare %>%
      filter(species == input$dev_comp_species,
             scenario == input$dev_comp_scenario,
             param == input$dev_comp_param,
             basin_type == input$dev_comp_scale)
  })
  
  output$compare_branch <- renderPlotly({
    ggplotly(
      compare_to_dev() %>%
      ggplot() + 
      theme_bw() +
      geom_bar(aes(basin, value, fill = version),
               stat = 'identity', position = 'dodge') +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0)))
  })
  
  output$compare_branch_diff <- renderPlotly({
    ggplotly(
      compare_to_dev() %>%
      spread(version, value) %>%
      mutate(tot_diff = dev - feature) %>%
      ggplot() + 
      theme_bw() + 
      geom_bar(aes(basin, tot_diff),
               fill = unique(compare_to_dev()$color), stat = 'identity') +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0)))
  })
  
  # fl_map <- reactive({
  #   fl %>%
  #     filter(param == input$map_param)
  # 
  # })
  # 
  # fl_color <- reactive({fl_map() %>% pull(map_color) %>% droplevels() %>% levels})
  
  # fl_color <- reactive({
  #   switch(input$map_param,
  #          'curr_temp' = c('navy', 'yellow', 'red3'),
  #          'hist_temp' = c('navy', 'yellow', 'red3'),
  #          'can_ang'  =  c('navy', 'dodgerblue', 'yellow', 'darkorange', 'red3'),
  #          'hist_ang' =  c('navy', 'dodgerblue', 'yellow', 'darkorange', 'red3'),
  #          'pass_tot' = c('navy', 'dodgerblue', 'yellow', 'darkorange', 'red3'),
  #          'temp_diff_2040'  = c('navy', 'dodgerblue', 'lightskyblue', 'grey', 'yellow', 'darkorange', 'red3'),
  #          'temp_diff_2080' = c('navy', 'dodgerblue', 'lightskyblue', 'grey', 'yellow', 'darkorange', 'red3'))
  # 
  # })
  # 
  # output$basin_map <- renderPlotly({
  #   plot_ly(fl_map(), split = ~level, text = ~value, hoverinfo = 'text', color = fl_color())#, trace = ~value)
  # })

  
}

shinyApp(ui, server)

