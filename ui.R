
#--------
# renames numerical month variables as strings while retaining numeric class
#---------

JScode <-
    "$(function() {
setTimeout(function(){
var vals = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];
for (i = 1; i >= vals.length; i++) {
var val = (1,12);
vals.push(val);
}
$('#month').data('ionRangeSlider').update({'values':vals})
}, 12)})"


navbarPage("NYC311 Noise Complaints", id = "map", theme = shinytheme("flatly"),
          
           #========================
           # city map tab
           #========================
           
           tabPanel("City Map",
                    div(class="outer",
                        tags$head(tags$script(HTML(JScode)), # using JScode from above
                                  includeCSS("styles.css") # custom styles
                                  ),
                        leafletOutput("mymap", width = "100%", height = "100%")
                        ),
                    absolutePanel(id = "controls", class = "panel panel-default", fixed = F,
                                  draggable = TRUE, top = "23%", left = 20, right = "auto", bottom = "auto",
                                  width = 300, height = "auto",
                                  
                                  br(),
                                  
                                  selectInput(inputId = "year1",
                                              label = "Year",
                                              choices = c("All", "2010", "2011", "2012", "2013", "2014", "2015", "2016"),
                                              selected = "All"),
                                  sliderInput(inputId = "month",
                                              label = "Month",
                                              min = 0,
                                              max = 11,
                                              value = 0,
                                              step = 1,
                                              animate = animationOptions(interval = 2000, loop = F)),
                                  selectInput(inputId = "complaint1",
                                              label = "Complaint Type",
                                              choices = c("All",
                                                          "Alarm",
                                                          "Animal",
                                                          "Banging/Pounding",
                                                          "Construction",
                                                          "Helicopter",
                                                          "HVAC Equipment",
                                                          "Loud Music/Party",
                                                          "Loud Talking",
                                                          "Loud TV",
                                                          "Vehicle",
                                                          "Other"),
                                              selected = "All"),
                                  sliderInput(inputId = "slider",
                                              label = "Sensitivity",
                                              min = 0, max = 2000,
                                              value = 250)
                                ),
                    absolutePanel(id = "controls2", class = "panel panel-default", fixed = T,
                                  draggable = T, top = "10%", left = "auto", right = 20, bottom = "auto",
                                  width = 210, height = 210,
                                  plotOutput(outputId = "bar2")
                                ),
                    absolutePanel(id = "controls2", class = "panel panel-default", fixed = T,
                                  draggable = TRUE, top = "40%", left = "auto", right = 20, bottom = "auto",
                                  width = 210, height = 210,
                                  plotOutput(outputId = "bar")
                                ),
                    absolutePanel(id = "controls2", class = "panel panel-default", fixed = T,
                                  draggable = TRUE, top = "70%", left = "auto", right = 20, bottom = "auto",
                                  width = 210, height = 210,
                                  plotOutput(outputId = "bar3")
                                )
                    
                    ),
           
           #========================
           # complaint trends tab
           #========================
           
           tabPanel("Complaint Trends",
                    wellPanel(
                        fluidRow(
                            column(3,
                                   selectInput(inputId = "year2",
                                               label = "Year",
                                               choices = c("All", "2010", "2011", "2012", "2013", "2014", "2015", "2016"),
                                               selected = "All")
                                               ),
                            column(3,
                                   selectInput(inputId = "boro",
                                               label = "Borough",
                                               choices = c("All", Bronx = "BRONX", 
                                                           Brooklyn = "BROOKLYN", 
                                                           Manhattan = "MANHATTAN", 
                                                           Queens = "QUEENS", 
                                                           `Staten Island` = "STATEN ISLAND"),
                                               selected = "All")
                                               ),
                            column(3,
                                   selectInput(inputId = "complaint2",
                                               label = "Complaint Type",
                                               choices = c("All",
                                                           "Alarm",
                                                           "Animal",
                                                           "Banging/Pounding",
                                                           "Construction",
                                                           "Helicopter",
                                                           "HVAC Equipment",
                                                           "Loud Music/Party",
                                                           "Loud Talking",
                                                           "Loud TV",
                                                           "Vehicle",
                                                           "Other"),
                                               selected = "All")
                                               ),
                        column(3,
                               selectInput(inputId = "grouped",
                                           label = "Time Dimension",
                                           choices = c(Month = "calendar_", Hour = "start_hour"),
                                           selected = "calendar_")
                                           )
                               )
                        ),
                wellPanel(
                    fluidRow(class = "barplots",
                        splitLayout(cellWidths = c("50%", "50%"),
                                    plotOutput(outputId = "barPlot"),
                                    plotOutput(outputId = "barPlot2")
                                    )
                            )
                        ),
                wellPanel(
                    fluidRow(class = "horizontal bar",
                             plotOutput(outputId = "horizontal_bar")
                                        )
                        )
                ),
           
           #========================
           # resolution times tab
           #========================
           
           tabPanel("Resolution Times",
                    wellPanel(
                        fluidRow(
                            column(4,
                                   selectInput(inputId = "year3",
                                               label = "Year",
                                               choices = c("All", "2010", "2011", "2012", "2013", "2014", "2015", "2016"),
                                               selected = "All")
                                               ),
                            column(4,
                                   selectInput(inputId = "boro2",
                                               label = "Borough",
                                               choices = c("All", 
                                                           Bronx = "BRONX", 
                                                           Brooklyn = "BROOKLYN", 
                                                           Manhattan = "MANHATTAN", 
                                                           Queens = "QUEENS", 
                                                           `Staten Island` = "STATEN ISLAND"),
                                               selected = "All")
                                               ),
                            column(4,
                                   selectInput(inputId = "complaint3",
                                               label = "Complaint Type",
                                               choices = c("All",
                                                           "Alarm",
                                                           "Animal",
                                                           "Banging/Pounding",
                                                           "Construction",
                                                           "Helicopter",
                                                           "HVAC Equipment",
                                                           "Loud Music/Party",
                                                           "Loud Talking",
                                                           "Loud TV",
                                                           "Vehicle",
                                                           "Other"),
                                               selected = "All")
                                                )
                                )
                        ),
                    wellPanel(
                        fluidRow(class = "boxplot_boro",
                                plotOutput(outputId = "boxPlot_boro"), tags$head(tags$style(".boxplot_boro{height:275px}"))
                                )
                            ),
                    wellPanel(
                        fluidRow(class = "boxplot_complaint",
                                 plotOutput(outputId = "boxPlot_complaint"), tags$head(tags$style(".boxplot_complaint{height:350px}"))
                                )
                            )
                
                    )
)