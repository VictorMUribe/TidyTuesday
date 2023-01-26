#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggfortify)

members <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/members.csv')


everest <- members %>% # should have an age
  filter(age != 'NA' & peak_name == "Everest" & season != "Unknown") %>% 
  select(-c(peak_id,peak_name,expedition_id,member_id, death_height_metres,
            injury_height_metres, highpoint_metres, death_cause,
            citizenship, expedition_role, injury_type)) %>%
  mutate(
    #death_height_metres = ifelse(death_height_metres == NA, 0, death_height_metres), # issue wiith levels for some reason
    #injury_height_metres = ifelse(injury_height_metres == NA, 0, injury_height_metres), # gives leveling issue 
    #highpoint_metres = ifelse(highpoint_metres == 'NA', 0, highpoint_metres), # thows off the p values
    season = factor(season),
    sex = factor(sex),
    #citizenship = factor(citizenship), # not significant after testing
    #expedition_role = factor(expedition_role), # not significant after testing
    hired = factor(hired, levels = c(TRUE, FALSE)), 
    success = factor(success), # value being predicted
    solo = factor(solo, levels = c(TRUE, FALSE)),
    oxygen_used = factor(oxygen_used, levels = c(TRUE, FALSE)),
    died = factor(died, levels = c(TRUE, FALSE)),
    #death_cause = factor(death_cause), # issue
    injured = factor(injured, levels = c(TRUE, FALSE))#,
    #injury_type = factor(injury_type) # issue with levels
  )







ui <- fluidPage(
  
  theme = bslib::bs_theme(bootswatch = "darkly"),
  
  navlistPanel(
    id = "tabset",
    "Everest",
    tabPanel("Success of injured over time", "Panel one contents",
             sliderInput("x", step = 5,  label = "Year", min = min(everest$year),
                         max = max(everest$year), value = min(everest$year),
                         animate = animationOptions(interval = 1000, loop = TRUE)),
             plotOutput("plot")
    ),    
    "Age Attempt",
    tabPanel("Attempted age throught the years", "Panel two contents",
             sliderInput("y", step = 5,  label = "Year", min = min(everest$year),
                         max = max(everest$year), value = min(everest$year), 
                         animate = animationOptions(interval = 1000, loop = TRUE)),
             plotOutput("plot_2")
    ),
    "Heading 3",
    tabPanel("panel 3", "Panel three contents")
  )
)  



server = function(input, output, seasion){
  thematic::thematic_shiny()
  
  output$plot <- renderPlot({
    dat <- everest %>% 
      filter(year <= input$x)
    
    ggplot(dat) + geom_boxplot(aes(x = age, y = injured, fill = injured),
                               alpha = .6) + facet_wrap(~success)
  })
  output$plot_2 <- renderPlot({
    dat2 <- everest %>% 
      filter(year <= input$y)
    
    autoplot(ts(dat2$age, start = min(dat2$year),
                end = max(dat2$year), frequency = 1)) + 
      geom_hline(yintercept = mean(dat2$age), color = "gold")
  })
}  

# 1953 they started keeping track if climbers reached the peak???
# 1962 they started keeping track if climbers got injured or not???

shinyApp(ui, server)
