library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(stringr)
library(here)
library(tidyverse)
library(reactable)
library(ggrepel)
library(readr)
library(ggthemes)
library(ggpubr)


# Read the file using read_delim with correct locale
comp_data <- read_delim(here("compensation_data.csv"),
                        delim = ";",
                        locale = locale(decimal_mark = ","),
                        show_col_types = FALSE)

# Convert all numeric-looking columns to numeric (safely)
comp_data <- comp_data %>%
  mutate(across(where(~ all(grepl("^[-+]?\\d*[\\,\\.]?\\d*(E[+-]?\\d+)?$", .x[.x != ""]))),
                ~ as.numeric(gsub(",", ".", .x))))

# convert scoping variabels to millions and rename them accordingly
vars_to_recalc <- c("free_cash_flow", "total_revenue", "market_cap", "operating_cash_flow", "ebitda", "gross_profits")
comp_data[vars_to_recalc] <- comp_data[vars_to_recalc] / 1000
names(comp_data)[names(comp_data) %in% vars_to_recalc] <- paste0(vars_to_recalc, "_k")


#turn off scientific notations
options(scipen = 100)


# filter countries (remove Denmark and Sweden)
comp_data <- comp_data %>% arrange(country_code, company_name)
comp_data$target_company = FALSE
comp_data$peer_group = FALSE
grouping_variables <- c("company_name", "survey_year", "job_function", "country_code", "sector", "market_cap_k", "operating_cash_flow_k", "gross_profits_k", "total_revenue_k", "ebitda_k", "free_cash_flow_k", "target_company", "peer_group")
scoping_variables <- c("free_cash_flow_k", "total_revenue_k", "market_cap_k", "operating_cash_flow_k", "ebitda_k", "gross_profits_k")
comp_variables <- c("annual_base_salary", "target_total_cash", "target_total_direct_compensation")
paymix_components <- factor(c("Base", "STI", "LTI"), levels <- c("Base", "STI", "LTI"))



# Combine variables which needs to be converted to numeric
numeric_vars <- c(scoping_variables, comp_variables)

# Convert selected variables to numeric
comp_data[numeric_vars] <- lapply(comp_data[numeric_vars], function(x) as.numeric(as.character(x)))

# change sector to "Other" for sectors with low sample sizes
comp_data <- comp_data %>% mutate(sector = replace(sector, sector %in% c("Basic Materials","Energy","Real Estate", "Utilities"), "Other"))


#############################################################################################################################################################
                                                                            ##UI
#############################################################################################################################################################

#specifying sidebar (left part of shiny app)
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Benchmarking Tool", tabName = "benchmarking_tool", icon = icon("angle-double-right")),
        menuItem("Regression", tabName = "regression", icon = icon("angle-double-right")),
        menuItem("Compensation Structure", tabName = "compensation_structure", icon = icon("angle-double-right"))
    ),
    br(),
    br(),
    uiOutput("company_selection"),
    selectInput("position", "Position", selected = "CEO", choices = sort(unique(comp_data$job_function))),
    selectInput("salary_type", "Salary Type", selected ="Annual Base Salary", choices = c("Annual Base Salary", "Target Total Cash", "Target Total Direct Compensation", "Target Total Remuneration")),
    selectInput("peer_group_type", "Peer Group Type", selected = "Sector", choices = c("All available companies", "Sector", "Country", "Custom")),
    conditionalPanel(condition = "input.peer_group_type == 'Country'",
                     uiOutput("country_peer_group")),
    conditionalPanel(condition = "input.peer_group_type == 'Sector'",
                     uiOutput("sector_peer_group")),
    conditionalPanel(condition = "input.peer_group_type == 'Custom'",
                     uiOutput("peer_group_selection"))
)

body <- dashboardBody(
    #possible to color every element of app with code below. More possibilites: https://stackoverflow.com/questions/52198452/how-to-change-the-background-color-of-the-shiny-dashboard-body
    tags$head(tags$style(HTML('
                                /* body */
                                .content-wrapper, .right-side {
                                background-color: white;
                                }
                                
                                '))),
    chooseSliderSkin("Flat"),
    tabItems(
        tabItem(tabName = "benchmarking_tool",
                fluidRow(
                    column(width = 12, 
                           box(
                               solidHeader = TRUE, #removes small grey line above box,
                               title = HTML('<span class="fa-stack fa-lg" style="color:#308695">
                                <i class="fa fa-square fa-stack-2x"></i>
                                <i class="fa fa-mouse-pointer fa-inverse fa-stack-1x"></i>
                                </span> <span style="font-weight:bold;font-size:20px">
                                Please make your Selections</span>'),
                               width = 4,
                               "Please select a target company and a peer group on the left tab. The graph on the right covers more Information about your selected company and the peer group, which is marked in turkish. You can also mark an area in the plot on the right to change your peer group. On the bottom, you will find a boxplot and table from your peer group with further company and remuneration information. Please select at least 5 companies in your peer group for the boxplot to show up!" 
                           ),
                           box(
                               solidHeader = TRUE, #removes small grey line above box,
                               title = HTML('<span class="fa-stack fa-lg" style="color:#308695">
                                        <i class="fa fa-square fa-stack-2x"></i>
                                        <i class="fa fa-chart-bar fa-inverse fa-stack-1x"></i>
                                        </span> <span style="font-weight:bold;font-size:20px">
                                          Salary overview by country</span>'),
                               selectInput("selection_variable", "Selection Variable", selected ="Annual Base Salary", choices = gsub("_", " ", str_to_title(c(comp_variables, scoping_variables))),width = "300px"),
                               plotOutput("plotted_compensation_overview", brush = "plot_brush"),
                               width = 8
                           )
                    )
                ),
                fluidRow(
                    br(),
                    column(width = 12, offset = 0,
                        box(
                               solidHeader = TRUE, #removes small grey line above box,
                               title = HTML('<span class="fa-stack fa-lg" style="color:#308695">
                                        <i class="fa fa-square fa-stack-2x"></i>
                                        <i class="fa fa-chart-bar fa-inverse fa-stack-1x"></i>
                                        </span> <span style="font-weight:bold;font-size:20px">
                                          Peer Group Boxplot</span>'),
                               plotOutput("peer_group_boxplot"),
                               width = 3
                           ),
                        box(
                            solidHeader = TRUE, #removes small grey line above box,
                            title = HTML('<span class="fa-stack fa-lg" style="color:#308695">
                                        <i class="fa fa-square fa-stack-2x"></i>
                                        <i class="fa fa-info fa-inverse fa-stack-1x"></i>
                                        </span> <span style="font-weight:bold;font-size:20px">
                                          Peer Group Information</span>'),
                            width = 9,
                            reactableOutput("company_level_table"),
                            tags$style(type="text/css",
                                   ".shiny-output-error { visibility: hidden; }",
                                   ".shiny-output-error:before { visibility: hidden; }")
                        )
                    ),
                    
                )
        ),tabItem(tabName = "regression",
                tags$style(".small-box.bg-yellow { background-color: #308695 !important; color: #FFFFFF !important; }"),     #copied this code to use other colours than default for value boxes on regression tab
                fluidRow(
                               valueBoxOutput("value1"),
                               valueBoxOutput("value2"),
                               valueBoxOutput("value3"),
                ),
                fluidRow(
                    column(width = 12,
                           box(
                                solidHeader = TRUE, #removes small grey line above box,
                                title = HTML('<span class="fa-stack fa-lg" style="color:#308695">
                                        <i class="fa fa-square fa-stack-2x"></i>
                                        <i class="fa fa-line-chart fa-inverse fa-stack-1x"></i>
                                        </span> <span style="font-weight:bold;font-size:20px">
                                          Relationship of Compensation with Company Informations </span>'),
                                uiOutput("independent_variables"),
                                plotOutput("relationship_scatterplots"),
                                 width = 7
                            ),
                           box(
                               solidHeader = TRUE, #removes small grey line above box,
                               title = HTML('<span class="fa-stack fa-lg" style="color:#308695">
                                        <i class="fa fa-square fa-stack-2x"></i>
                                        <i class="fa fa-info fa-inverse fa-stack-1x"></i>
                                        </span> <span style="font-weight:bold;font-size:20px">
                                          Model Information </span>'),
                               verbatimTextOutput("RegOut"),
                               width = 5
                           ),
                           
                    )
                )
        ),tabItem("compensation_structure",
                  fluidRow(column(width = 12,
                                  box(
                                      solidHeader = TRUE, #removes small grey line above box,
                                      title = HTML('<span class="fa-stack fa-lg" style="color:#308695">
                                        <i class="fa fa-square fa-stack-2x"></i>
                                        <i class="fa fa-cogs fa-inverse fa-stack-1x"></i>
                                        </span> <span style="font-weight:bold;font-size:20px">
                                          Remuneration Components </span>'),
                                      plotOutput("comp_structure"),
                                      width = 6
                                  ), box(
                                         solidHeader = TRUE, #removes small grey line above box,
                                         HTML('<span class="fa-stack fa-lg" style="color:#308695">
                                            <i class="fa fa-square fa-stack-2x"></i>
                                            <i class="fa fa-pie-chart fa-inverse fa-stack-1x"></i>
                                            </span> <span style="font-weight:bold;font-size:20px">
                                            Pay Mix - Selected Company </span>'),
                                         br(),
                                         plotOutput("donut_input_company"),
                                         width = 3
                                  ), box(
                                      solidHeader = TRUE, #removes small grey line above box,
                                      HTML('<span class="fa-stack fa-lg" style="color:#308695">
                                            <i class="fa fa-square fa-stack-2x"></i>
                                            <i class="fa fa-pie-chart fa-inverse fa-stack-1x"></i>
                                            </span> <span style="font-weight:bold;font-size:20px">
                                            Pay Mix - Peer Group </span>'),
                                      br(),
                                      plotOutput("donut_peer_group"),
                                      width = 3
                                  )
                                  
                                  
                  )),
                  fluidRow(
                      column(width = 3,
                             offset = 7,
                             box(
                                 solidHeader = TRUE, #removes small grey line above box,
                                 HTML('<span class="fa-stack fa-lg" style="color:orange">
                                            <i class="fa fa-square fa-stack-2x"></i>
                                            <i class="fa fa-pie-chart fa-inverse fa-stack-1x"></i>
                                            </span> <span style="font-weight:bold;font-size:20px">
                                            Pay Mix - Predictions </span>'),
                                 br(),
                                 plotOutput("donut_prep_predicted"),
                                 width = 12
                             )
                             
                          
                      )
                  )
        )
        
    )
)

ui <- dashboardPage(
    skin = "black",
    dashboardHeader(title = "Benchmarking Tool"),
    sidebar,
    body
)
    
#############################################################################################################################################################
                                                                            ##Server
#############################################################################################################################################################


plot_salary <- function(dataset, peer_group_input, company_input, position_input, salary_input){
    dataset <- dataset %>% mutate(sector = factor(sector)) # first to lines are there to insert line break in sector
    levels(dataset$sector) <- gsub(" ", "\n", levels(dataset$sector)) #first to lines are there to insert line break in sector
    if(peer_group_input == "Sector"){p <- ggplot(dataset, aes(x=sector,y=selection_variable))}
    else {p <- ggplot(dataset, aes(x = country_code,y=selection_variable))}
    if(peer_group_input == "Custom"){
        p = p +
            geom_boxplot(data = . %>% filter(peer_group != TRUE), outlier.shape = NA) + 
            geom_jitter(data = . %>% filter(peer_group  != TRUE), width=0.05, alpha=0.2) +
            geom_jitter(data = . %>% filter(peer_group == TRUE), width=0.05, alpha=1, shape = 4,  size = 4, stroke = 2, color = "#308695") 
        }
    else{
        p = p +
        geom_boxplot(data = . %>% filter(peer_group != TRUE), outlier.shape = NA) + 
        geom_jitter(data = . %>% filter(peer_group  != TRUE), width=0.05, alpha=0.85) +
        geom_boxplot(data = . %>% filter(peer_group == TRUE), fill='#308695', alpha = 0.3, outlier.shape = NA) +
        geom_jitter(data = . %>% filter(peer_group == TRUE), width=0.05,alpha=0.85, color = "#308695") 
    }
    p + geom_label_repel(data = . %>% filter(company_name == company_input), aes(label = company_name), arrow = arrow(length = unit(0.02, "npc")), box.padding = 1) +
    xlab("") +
    ylab("") +
    theme_classic() +
    scale_y_continuous(labels = scales::label_dollar()) +
    theme(axis.text=element_text(size=10, face = "bold")) 
}

plot_relationships <- function(dataset, salary_input, regression_input){
    alpha_all = 0.7
    
    background_color_g1 <- ifelse("total_revenue_k" %in% regression_input, "white", "grey")
    g1 <- dataset %>%
            ggplot(aes(x=total_revenue_k, y=!!sym(salary_input))) +
            geom_point(col = "grey", alpha = alpha_all) +
            geom_smooth(method='lm', se = FALSE, col = ifelse("total_revenue_k" %in% regression_input,"#308695","grey")) +
            xlab("Revenues k") + 
            ylab("") + 
            ggtitle("") +
            theme_classic() +
            scale_x_continuous(labels = scales::label_dollar()) +
            theme(axis.text.y = element_blank(),
                  panel.background = element_rect(fill = background_color_g1))
    
    background_color_g2 <- ifelse("market_cap_k" %in% regression_input, "white", "grey")
    g2 <- dataset %>% 
            ggplot(aes(market_cap_k, !!sym(salary_input))) +
            geom_point(col = "grey", alpha = alpha_all) +
            geom_smooth(method='lm', se = FALSE, col = ifelse("gross_profits_k" %in% regression_input,"#308695","grey")) +
            xlab("Market cap k") + 
            ylab("") + 
            ggtitle("") +
            theme_classic() +
            scale_x_continuous(labels = scales::label_dollar()) +
            theme(axis.text.y = element_blank(),
                  panel.background = element_rect(fill = background_color_g2))
    
    background_color_g3 <- ifelse("free_cash_flow_k" %in% regression_input, "white", "grey")
    g3 <- dataset %>%
            ggplot(aes(free_cash_flow_k, !!sym(salary_input))) +
            geom_point(col = "grey", alpha = alpha_all) +
            geom_smooth(method='lm', se = FALSE, col = ifelse("free_cash_flow_k" %in% regression_input,"#308695","grey")) +
            xlab("Free cash flow k") + 
            ylab("") + 
            ggtitle("") +
            theme_classic() +
            scale_x_continuous(labels = scales::label_dollar()) +
            theme(axis.text.y = element_blank(),
                  panel.background = element_rect(fill = background_color_g3))
    
    background_color_g4 <- ifelse("ebitda_k" %in% regression_input, "white", "grey")
    g4 <- dataset %>%
            ggplot(aes(ebitda_k, !!sym(salary_input))) +
            geom_point(col = "grey", alpha = alpha_all) +
            geom_smooth(method='lm', se = FALSE, col = ifelse("ebitda_k" %in% regression_input,"#308695","grey")) +
            xlab("Ebitda k") + 
            ylab("") + 
            ggtitle("") +
            theme_classic() +
        scale_x_continuous(labels = scales::label_dollar()) +
            theme(axis.text.y = element_blank(),
                  panel.background = element_rect(fill = background_color_g4))
    
    background_color_g5 <- ifelse("gross_profits_k" %in% regression_input, "white", "grey")
    g5 <- dataset %>%
        ggplot(aes(gross_profits_k, !!sym(salary_input))) +
        geom_point(col = "grey", alpha = alpha_all) +
        geom_smooth(method='lm', se = FALSE, col = ifelse("market_cap_k" %in% regression_input,"#308695","grey")) +
        xlab("Gross profits k") + 
        ylab("") + 
        ggtitle("") +
        theme_classic() +
        scale_x_continuous(labels = scales::label_dollar()) +
        theme(axis.text.y = element_blank(),
              panel.background = element_rect(fill = background_color_g5))
    
    background_color_g6 <- ifelse("operating_cash_flow_k" %in% regression_input, "white", "grey")
    g6 <- dataset %>%
        ggplot(aes(operating_cash_flow_k, !!sym(salary_input))) +
        geom_point(col = "grey", alpha = alpha_all) +
        geom_smooth(method='lm', se = FALSE, col = ifelse("operating_cash_flow_k" %in% regression_input,"#308695","grey")) +
        xlab("Operating cash flow k") + 
        ylab("") + 
        ggtitle("") +
        theme_classic() +
        scale_x_continuous(labels = scales::label_dollar()) +
        theme(axis.text.y = element_blank(),
              panel.background = element_rect(fill = background_color_g6))
    
    ggarrange(g1, g2, g3, g4, g5, g6,
              ncol = 3, nrow = 2)
}


donut_preperations <- function(dataset){
    dataset <- dataset %>% gather(key = "salary_type", value = "salary_amount") %>% mutate(salary_percentage = salary_amount/sum(salary_amount), salary_type = paymix_components) #make salary_type a factor so that the order is right (STI before LTI in plot)
    # Compute the cumulative percentages (top of each rectangle)
    dataset$ymax = cumsum(dataset$salary_percentage)
    # Compute the bottom of each rectangle
    dataset$ymin = c(0, head(dataset$ymax, n=-1))
    # Compute label position
    dataset$labelPosition <- (dataset$ymax + dataset$ymin) / 2
    # Compute a good label
    dataset$label <- paste0(format(round(dataset$salary_percentage*100,1), nsmall = 1), " %")
    dataset
}


donut_plot <- function(dataset){
    ggplot(dataset, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill= salary_type, alpha = salary_type)) +
        geom_rect() +
        coord_polar(theta="y") + 
        xlim(c(2, 4)) + 
        theme_void() +
        theme(legend.position="bottom",
              legend.justification='left',
              legend.text=element_text(size=13),
              legend.direction='vertical',
              legend.title = element_blank()) +
        scale_fill_manual(values = c("#308695","#308695","#308695")) +
        scale_alpha_manual(values = c(0.4,0.75,1))+
        geom_label( x=3.5, aes(y=labelPosition, label=label), size=6, color = "white", inherit.aes = TRUE, show.legend = FALSE) 
}

server <- shinyServer(
    function(input, output) {
        
        #convert the name of the selected salary so it fits the variable name (eg. turn "Annual Base Salary" into "annual_base_salary)
        salary_input <- reactive({
            gsub(" ", "_", tolower(input$salary_type))
        })
        
        overview_plot_input <- reactive({
            gsub(" ", "_", tolower(input$selection_variable))
        })
        
        #return character value which indicates the country of the selected targetcompany
        target_country <- reactive({
            comp_data %>% filter(company_name == input$company) %>% select(country_code) %>% unique() %>% pull
        })
        
        #peprocess data so that only companies where compensation data is available can be selected as target company
        populate_companies <- reactive({
            comp_data %>% filter(job_function == input$position, !is.na(target_total_direct_compensation)) %>% count(company_name, country_code)
        })
        
        # filter dataset on input from user regarding position (CEO, CFO, Other) and salary (Annual Base vs Target total direct etc.)
        data_filtered <- reactive({
            result <- comp_data %>% 
                filter(job_function == input$position, !is.na(target_total_direct_compensation))
            if(input$peer_group_type == "All available companies"){result$peer_group <- TRUE}
            else if(input$peer_group_type == "Country"){result <- result %>% mutate(peer_group = replace(peer_group, country_code == input$peerGroupC, TRUE))}
            else if(input$peer_group_type == "Sector"){result <- result %>% mutate(peer_group = replace(peer_group, sector == input$peerGroupS, TRUE))}
            else if(input$peer_group_type == "Custom"){result <- result %>% mutate(peer_group = replace(peer_group, company_name %in% input$peerGroup, TRUE))}
            result %>%
                group_by_at(vars(one_of(grouping_variables))) %>%
                summarise(annual_base_salary = mean(annual_base_salary, na.rm = TRUE), 
                          target_total_cash = mean(target_total_cash, na.rm = TRUE),
                          target_total_direct_compensation = mean(target_total_direct_compensation, na.rm = TRUE),
                          selection_variable = mean(!!sym(overview_plot_input()), na.rm = TRUE), .groups = "drop")
        })
        
        #Group data and calculate average salary on company level
        data_comp_level <- reactive({
            output <- data_filtered() %>% 
                mutate(target_company = replace(target_company, company_name == input$company, TRUE)) 
            if(!is.null(input$plot_brush)){
                output$peer_group <- brushedPoints(data_filtered(), input$plot_brush, allRows = TRUE)$selected_
            }
            output %>% arrange(desc(target_company), desc(!!sym(salary_input())))
        })
        
        
        #to-do update only when selection from plot is finished (not during blushing)
        data_inc_level <- reactive({
            comp_data %>% 
                filter(job_function == input$position, !is.na(target_total_direct_compensation)) %>%
                left_join(data_comp_level() %>% select(company_name, peer_group), by = "company_name") %>%
                    mutate(peer_group.x = peer_group.y) %>% 
                    rename(peer_group = peer_group.x) %>%
                    select(-peer_group.y)
        })
            
        
        # show companies grouped by country in ascending order on ui input
        output$company_selection <- renderUI({
            selectInput(inputId = "company", "Company", choice = split(populate_companies()$company_name, populate_companies()$country_code), selected = "adidas AG", multiple = F)
        })
        
        populate_Country <- reactive({
             unique(comp_data$country_code)
        })
        
        populate_Sector <- reactive({
            unique(comp_data$sector)
        })
        
        selection_Sector <- reactive({
            comp_data %>% filter(company_name == input$company) %>% select(sector) %>% unique() %>% pull
        })
        
        output$country_peer_group <- renderUI({
            selectInput(inputId = "peerGroupC", NULL, choice = populate_Country(), selected = target_country())
        })
        
        output$sector_peer_group <- renderUI({
            selectInput(inputId = "peerGroupS", NULL, choice = populate_Sector(), selected = selection_Sector())
        })
        
        output$peer_group_selection <- renderUI({
            selectInput(inputId = "peerGroup", NULL, choice = split(populate_companies()$company_name, populate_companies()$country_code), multiple = T)
        })
        
        output$plotted_compensation_overview <- renderPlot({
            shiny::validate(
                need(input$company, "")
            )
            
            plot_salary(data_filtered(), input$peer_group_type, input$company, input$position, overview_plot_input())
            
        })
        
        output$peer_group_boxplot <- renderPlot({
            if(sum(data_comp_level()$peer_group) < 4){
                textplot("None", halig = "center", cex = 2, col = "red")
            } else {
            data_comp_level() %>%
                filter(target_company == TRUE | peer_group == TRUE) %>%
                ggplot(aes(x="", y=!!sym(salary_input()))) +
                geom_boxplot(fill='#308695', alpha = 0.3, outlier.shape = NA) +
                geom_jitter(width=0.02,alpha=0.85, color = "#308695") +
                geom_label_repel(data = . %>% filter(company_name == input$company), aes(label = input$company), arrow = arrow(length = unit(0.02, "npc")), box.padding = 1) +
                xlab("") +
                ylab("") +
                theme_classic() +
                theme(axis.text=element_text(size=13, face = "bold")) +
                scale_y_continuous(labels = scales::label_dollar()) +
                ggtitle(paste(input$salary_type, "-", input$position))
            }
            
        })
        
        output$company_level_table <- renderReactable({
            reactable(
                compact = TRUE,
                borderless = FALSE,
                striped = FALSE,
                defaultColDef = colDef(
                    align = "center",
                    format = colFormat(currency = "USD")
                ),
                # Add theme for the top border
                theme = reactableTheme(
                    headerStyle = list(
                        borderColor = "#555"
                    )
                ),
                columns = list("Company Name" = colDef(width = 160, align = "left"),
                "Country Code" = colDef(width = 65),
                "Sector" = colDef(width = 107),
                "Number Of Employees" = colDef(format = colFormat(currency = NULL))),
                data_comp_level() %>% filter(target_company == TRUE | peer_group == TRUE) %>% select(c(company_name, country_code, sector, gross_profits_k, total_revenue_k, ebitda_k, free_cash_flow_k, !!sym(salary_input())))  %>% 
                    rename_with(~ str_to_title(gsub("_", " ", .x, fixed = TRUE))),
                rowStyle = function(index) {
                    if (data_comp_level()[index, "company_name"] == input$company) list(fontWeight = "bold", color = "#308695", background = adjustcolor( "light grey", alpha.f = 0.25))
                }
            )
        })
        
##################################################### Regression Tab ###########################################################################
        
        actual_compensation <- reactive({
            act_comp <- data_comp_level() %>% filter(company_name == input$company) %>% select(!!sym(salary_input())) %>% pull
            paste("USD", format(act_comp, 0, big.mark=","))
        })
        
        regression_model <- reactive({
            lm(reformulate(input$regression_variables, salary_input()), data = data_comp_level())
        })
        predicted_compensation <- reactive({
            data_for_prediction <- data_comp_level() %>% filter(company_name == input$company)
            paste("USD", format(predict(regression_model(), data_for_prediction),0, big.mark=","))
        })
        
        number_companies_regression <- reactive({
            nobs(regression_model())
        })
        
        output$relationship_scatterplots <- renderPlot({
            plot_relationships(data_comp_level(), salary_input(), input$regression_variables)
        })
        
        output$value1 <- renderValueBox({
            valueBox(
                predicted_compensation(),
                paste(input$salary_type, " - Predicted (linear regression)"),
                color = "yellow" #colour is changed to turkish on UI part through CSS
            )
        })
        
        output$value2 <- renderValueBox({
            valueBox(
                number_companies_regression(),
                paste('Companies used for Regression'),
                color = "yellow" #colour is changed to turkish on UI part through CSS
            )
        })
        
        output$value3 <- renderValueBox({
            valueBox(
                actual_compensation(),
                paste(input$salary_type, " - Implemented"),
                color = "yellow" #colour is changed to turkish on UI part through CSS
            )
        })
        
        populate_independent_variables <- reactive({
            scoping_variables[sapply(scoping_variables, function(x){!is.na(data_comp_level() %>% filter(company_name == input$company) %>% select(!!sym(x)))})]
        })
        
        output$independent_variables <- renderUI({
            selectInput(inputId = "regression_variables", "Independent Variables", choices = populate_independent_variables(), selected = populate_independent_variables(), multiple = T)
        })
        
        #calculate output independ_Variables even if the tab is not opened (necessary to be load third tab before second, because third tab uses regression results)
        outputOptions(output, "independent_variables", suspendWhenHidden = FALSE)
        
        output$RegOut <- renderPrint({summary(regression_model())})
        
##################################################### Compensation Structure Tab ###########################################################################
        
        predictions <- reactive({
            data_for_prediction <- data_comp_level() %>% filter(company_name == input$company)
            #iterate through all 4 variables to define regression model and make prediction for input$company
            results <- sapply(comp_variables, function(x){predict(lm(reformulate(input$regression_variables, x), data = data_comp_level()), data_for_prediction)})
            results <- data.frame(salary_type = comp_variables, salary_amount = results)
            results
        })
        
        output$comp_structure <- renderPlot({
            data_comp_level() %>%
                filter(target_company == TRUE | peer_group == TRUE) %>% 
                select(company_name,annual_base_salary, target_total_cash, target_total_direct_compensation) %>%
                gather(salary_type, salary_amount, -company_name) %>%
                ggplot(aes(x=salary_type,y=salary_amount)) +
                geom_boxplot(outlier.shape = NA) + 
                geom_jitter(data = . %>% filter(company_name != input$company), width=0.05, alpha=0.1) +
                geom_point(data = . %>% filter(company_name == input$company), shape = 24, aes(fill="#308695"), size = 5) +
                geom_point(data = predictions(), shape = 23, color="orange", aes(fill="orange"), size = 4) +
                xlab("") +
                xlab("") +
                ylab("") +
                theme_classic() +
                theme(axis.text=element_text(size=13, face = "bold")) +
                scale_fill_identity(name = "",
                                    labels = c("Implemented Salary", "Predicted Salary from Regression"),
                                    guide = guide_legend(override.aes = list(shape = c(24, 23),
                                                                             color = c("#308695", "orange")))) +
                theme(legend.position="top",
                      legend.justification='left',
                      legend.text=element_text(size=13),
                      legend.direction='vertical') +
                scale_x_discrete(labels= c("Annual Base\nSalary", "Target Total\nCash", "Target Total\nDirect Compensation", "Target Total\nRemuneration")) +
                ggtitle(paste(input$company, "-", input$position))
            
        })

        donut_prep_input_company <- reactive({
            percentages_data <- data_comp_level() %>% filter(company_name == input$company) %>% mutate(STI = target_total_cash - annual_base_salary, LTI = target_total_direct_compensation - target_total_cash) %>% select(Base = annual_base_salary, STI,LTI)
            percentages_data <- donut_preperations(percentages_data)
        })
        
        donut_prep_peer_group <- reactive({
            percentages_data <- data_comp_level() %>% filter(peer_group, company_name != input$company) %>% mutate(STI = target_total_cash - annual_base_salary, LTI = target_total_direct_compensation - target_total_cash) %>% select(Base = annual_base_salary, STI,LTI)
            percentages_data <- sapply(percentages_data, median, na.rm = TRUE)
            percentages_data <- data.frame(salary_type = names(percentages_data), salary_amount = percentages_data)
            percentages_data <- donut_preperations(percentages_data)
        })
        
        donut_prep_predicted <- reactive({
            percentages_data <- predictions() %>% spread(key = salary_type, value = salary_amount) %>% mutate(STI = target_total_cash - annual_base_salary, LTI = target_total_direct_compensation - target_total_cash) %>% select(Base = annual_base_salary, STI,LTI)
            percentages_data <- donut_preperations(percentages_data)
        })
        
        output$donut_input_company <- renderPlot({
            donut_plot(donut_prep_input_company())
        })
        
        output$donut_peer_group <- renderPlot({
            donut_plot(donut_prep_peer_group()) 
        })
        
        output$donut_prep_predicted <- renderPlot({
            donut_plot(donut_prep_predicted()) +
            scale_fill_manual(values = c("orange","orange","orange")) +
            scale_alpha_manual(values = c(0.3,0.6,0.9))
        })
    })

shinyApp(ui = ui, server = server)


