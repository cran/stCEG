#' Launch the stCEG Shiny Application
#'
#' Launches the interactive stCEG Shiny application for constructing,
#' visualising and analysing Event Trees, Staged Trees and Chain Event Graphs
#' (CEGs), including spatial visualisation through interactive maps.
#'
#' The application provides a graphical workflow for:
#' \itemize{
#'   \item Uploading and filtering datasets.
#'   \item Constructing event trees.
#'   \item Manual and automatic stage colouring.
#'   \item Prior specification and editing.
#'   \item Generating staged trees and Chain Event Graphs.
#'   \item Interactive spatial analysis using shapefiles.
#'   \item Computing conditional probability maps from CEGs.
#'   \item Exploring reduced CEGs and florets.
#' }
#'
#' @details
#' The application supports both manual stage specification and automated
#' stage discovery using Agglomerative Hierarchical Clustering (AHC).
#'
#' Users can:
#' \enumerate{
#'   \item Upload tabular datasets and shapefiles.
#'   \item Select variables and spatial regions for analysis.
#'   \item Create and modify event trees interactively.
#'   \item Specify prior distributions for stages.
#'   \item Generate Chain Event Graphs and posterior summaries.
#'   \item Visualise probabilities on interactive leaflet maps.
#' }
#'
#' The application is intended as a graphical interface to the functionality
#' provided throughout the package.
#'
#' @return
#' Launches a Shiny application and returns a
#' \code{\link[shiny]{shinyApp}} object.
#'
#' @examples
#' \dontrun{
#'   run_stceg()
#' }
#'
#' @seealso
#' \code{\link{create_event_tree}},
#' \code{\link{ahc_colouring}},
#' \code{\link{compute_ceg}},
#' \code{\link{compute_reduced_ceg}},
#' \code{\link{generate_CEG_map}}
#'
#' @export
run_stceg <- function(){

  if (!requireNamespace("randomcoloR", quietly = TRUE)) {
    stop("Package 'randomcoloR' needed for this function to work. Please install it.", call. = FALSE)
  }
  if (!requireNamespace("colourpicker", quietly = TRUE)) {
    stop("Package 'colourpicker' needed for this function to work. Please install it.", call. = FALSE)
  }

    ui <- shiny::fluidPage(
      shiny::titlePanel("stCEG - Modelling Over Spatial Areas Using Chain Event Graphs"),
      shiny::tags$head(
        shiny::tags$style(shiny::HTML("
    .leaflet-left .leaflet-control {
      visibility: hidden;
    }
  "))
      )
      ,
      shiny::tabsetPanel(
        shiny::tabPanel("Upload Data",
                 shiny::sidebarLayout(
                   shiny::sidebarPanel(shiny::fileInput(
                     "file1",
                     "Choose CSV File",
                     multiple = TRUE,
                     accept = c(
                       "text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv"
                     )
                   ),

                   shiny::uiOutput("area_division_checkboxes"),
                   shiny::uiOutput("time_division_checkboxes"),


                   # Horizontal line ----
                   tags$hr(),

                   #textInput("na_values", "Specify NA values", value = ""),
                   checkboxInput("exclude_row_numbers", "Exclude First Column as Row Numbers", FALSE),

                   # Input: Checkbox if file has header ----
                   checkboxInput("header", "Header", TRUE),

                   # Input: Select separator ----
                   radioButtons(
                     "sep",
                     "Separator",
                     choices = c(
                       Comma = ",",
                       Semicolon = ";",
                       Tab = "\t"
                     ),
                     selected = ","
                   ),

                   # Input: Select quotes ----
                   radioButtons(
                     "quote",
                     "Quote",
                     choices = c(
                       None = "",
                       "Double Quote" = '"',
                       "Single Quote" = "'"
                     ),
                     selected = '"'
                   ),

                   # Horizontal line ----
                   tags$hr(),

                   # Input: Select number of rows to display ----
                   radioButtons(
                     "disp",
                     "Display",
                     choices = c(
                       Head = "head",
                       All = "all"
                     ),
                     selected = "all"
                   ),
                   uiOutput("prediction_var"),
                   #actionButton("finish", "Finished"),
                   ,width = 3),
                   mainPanel(DTOutput("rawdata")))),



        tabPanel("Select Data", fluid = TRUE,
                 sidebarLayout(

                   sidebarPanel(

                     uiOutput("area_dropdown"),
                     uiOutput("time_type_input"),
                     uiOutput("date_format_input"),
                     uiOutput("time_dropdown"),


                     uiOutput("num_vars_ui"),
                     # Dynamically generate pickers based on the number of variables
                     uiOutput("pickers_ui"),
                     uiOutput("timeframe_slider"),
                     actionButton(inputId = "defaultButton", label = "Set Default Selections"),
                     actionButton("view", "View Selection"),
                     width = 3),

                   mainPanel(

                     h2('Data Frame'),
                     DTOutput("table")
                   ))),
        tabPanel("Plots", fluid = TRUE,
                 tags$head(
                   tags$style(HTML("
      /* Fix sidebar panel width and alignment */
      #sidebar {
        width: 23%;
        position: fixed;
        top: 104;
        left: 1%;
        z-index: 1000;
      }
      /* Adjust main panel to have a margin-left equal to the sidebar width */
      #main {
        margin-left: 25%; /* Adjust this value to match sidebar's width plus spacing */

      }
    "))
                 ),

                 sidebarLayout(
                   # Sidebar panel
                   sidebarPanel(
                     shinyjs::useShinyjs(),
                     id = "sidebar",  # Assign ID for custom styling
                     style = "padding-top: 20px; padding-bottom: 20px;",
                     actionButton("vieweventtree", "View Event Tree"),
                     tags$hr(),
                     conditionalPanel(
                       condition = "input.viewOption == 'Map'",
                       style = "margin-top: 20px; margin-bottom: 20px;",
                       fileInput(
                         "shapefile",
                         "Upload Shapefile (ZIP)",
                         accept = ".zip"
                       ),
                       textInput("crs", "Specify CRS (if missing):", value = NA),
                       sliderInput("mapOpacity", "Layer Opacity", min = 0, max = 1, value = 0.7, sep = ""),
                       actionButton("process_shapefile", "Process Shapefile")
                     ),
                     colourpicker::colourInput("nodeColor", "Choose colour", value = "#FFFFFF"),
                     actionButton("updateColor", "Update Colour"),
                     actionButton("AHCColoring", "Colour using AHC"),
                     actionButton("viewstagedtree", "View Staged Tree"),
                     actionButton("viewceg", "View Chain Event Graph"),
                     width = 3
                   ),

                   # Main panel
                   mainPanel(
                     id = "main",  # Assign ID for custom styling
                     selectInput(
                       "viewOption",
                       "Choose Colouring Method:",
                       choices = c("Event Tree", "Map"),
                       selected = "Map"
                     ),

                     h2(textOutput("mainPanelTitle")),
                     checkboxInput("toggleLabels", "Hide data values", value = TRUE),
                     fluidRow(
                       column(6, leafletOutput("map", height = "600px")),
                       column(6, visNetworkOutput("eventtree_network", height = "600px"))
                     ),
                     fluidRow(column(6, uiOutput("ExchangeabilityHideView")),
                              column(2, actionButton("deleteNode", "Delete Selected Node")),
                              column(2, actionButton("finishedColoring", "Finished Colouring"))
                     ),

                     h2('Staged Tree'),
                     selectInput(
                       inputId = "priorChoice",
                       label = "Choose Prior Type:",
                       choices = c("Phantom Individuals Prior", "Specify Prior", "Uniform 1,1 Prior"),
                     ),
                     DTOutput("colorLevelTable", width = "95%"),
                     actionButton("finishedPrior", "Finished Prior Specification"),
                     checkboxInput("usePriorLabels", "Show Prior Mean", value = FALSE),
                     visNetworkOutput("stagedtree", height = "1000px"),

                     h2('Chain Event Graph'),

                     fluidRow(
                       column(3, # adjust the column width for the selectInput
                              selectInput(
                                "viewcegmap",
                                "Choose View:",
                                choices = c("Chain Event Graph", "Chain Event Graph and Map"),
                                selected = "Chain Event Graph and Map"
                              ),
                              selectInput("color_palette", "Choose Colour Palette:",
                                          choices = c("viridis", "magma", "plasma", "inferno", "cividis", "mako", "rocket", "turbo"), selected = "viridis")),
                       column(3,uiOutput("last_group_ui"),
                              uiOutput("unique_values_ui"),
                       ),
                       column(6, # adjust the column width for the other inputs
                              sliderInput("levelSeparation", "Level Separation", min = 500, max = 10000, value = 1000, sep = ""),
                              checkboxInput("viewUpdateTable", "View Prior-Posterior Update Table", value = TRUE),
                              checkboxInput("showposteriormean", "Show Posterior Mean", value = TRUE),
                       )
                     ),
                     #visNetworkOutput("ceg_network", height = "1000px"),
                     fluidRow(
                       column(
                         6,
                         leafletOutput("ceg_map", height = "600px"),
                         # Hidden initially
                       ),
                       column(
                         6,
                         visNetworkOutput("ceg_network", height = "600px")
                       )
                     ),

                     #actionButton("ViewSelected", "View Selected Areas"),
                     conditionalPanel(
                       condition = "input.viewUpdateTable == true",
                       DTOutput("UpdateTable", width = "95%")
                     )
                   )
                 )
        ),
      ))


    server <- function(input, output, session) {
      options(shiny.maxRequestSize=30*1024^2)
      original_data <- reactiveVal()  # To store the original, unfiltered dataframe
      homicides <- reactiveVal()      # To store the filtered dataframe

      output$rawdata <- renderDT({
        req(input$file1)

        df <- read.csv(
          input$file1$datapath,
          header = input$header,
          sep = input$sep,
          quote = input$quote
        )

        # Ensure the Date columns are correctly formatted
        if ("DateColumn" %in% names(df)) {
          df$DateColumn <- as.Date(df$DateColumn, format = "%Y-%m-%d")  # Adjust format as needed
        }

        if (input$exclude_row_numbers) {
          df <- df[, -1]
        }

        original_data(df)  # Save the original data
        homicides(df)      # Set initial filtered data to the original data

        datatable(df) %>%
          formatStyle(
            columns = input$predict_var,
            backgroundColor = "rgba(0, 255, 0, 0.1)"
          )
      })

      output$prediction_var <- renderUI({
        req(homicides())
        df <- homicides()
        selectInput(
          "predict_var",
          "Select Prediction Variable",
          choices = names(df),
          selected = NULL
        )
      })

      output$area_division_checkboxes <- renderUI({
        req(homicides())
        df <- homicides()
        area_columns <- colnames(df)
        selectInput(
          "selected_area_columns",
          "Select Area Division",
          choices = c("",area_columns),
          selected = NULL,
        )
      })

      output$time_division_checkboxes <- renderUI({
        req(homicides())
        df <- homicides()
        time_columns <- colnames(df)
        selectInput(
          "selected_time_columns",
          "Select Time Division",
          choices = c("",time_columns),
          selected = NULL,
        )
      })

      output$area_dropdown <- renderUI({
        df_homicides <- homicides()

        # Check if selected_area_columns is not NULL and has at least one column selected
        if (input$selected_area_columns != "" && length(input$selected_area_columns) > 0) {

          # Dynamically select the columns from the dataframe (df_homicides assumed here)
          selected_columns2 <- df_homicides[, input$selected_area_columns, drop = FALSE]

          # Get the unique values from the selected columns
          unique_values_area <- unique(do.call(c, selected_columns2))

          # Create the selectInput UI with the unique values from the selected columns
          selectInput(
            "Area",
            "Choose Area",
            choices = unique_values_area,
            multiple = TRUE  # Enable multiple selections
          )
        } else {
          NULL
        }
      })


      observe({
        req(original_data())
        # Reset homicides to the original data
        homicides(original_data())
      })

      output$time_type_input <- renderUI({
        req(homicides(), input$selected_time_columns != "")
        selectInput(
          "time_type",
          "Select Time Type",
          choices = c("Date", "Month-Year", "Year", ""),
          selected = ""
        )
      })

      output$date_format_input <- renderUI({
        req(input$selected_time_columns != "", input$time_type)  # Ensure both are available

        if (input$time_type == "Date") {
          textInput("date_format", "Specify Date Format", value = "%Y-%m-%d")
        }
        else if (input$time_type == "Month-Year") {
          textInput("month_format", "Specify Date Format", value = "%Y-%m")
        } else {
          NULL  # You can return NULL or handle other cases as needed
        }
      })


      # Rendering the time dropdown slider based on the selected time division
      # Check the selected time column and convert appropriately
      output$time_dropdown <- renderUI({
        req(input$selected_time_columns != "", input$time_type)

        df <- homicides()
        time_col <- input$selected_time_columns

       if (!time_col %in% colnames(df)) {
          stop("Error: Selected column does not exist in the data frame.")
      }


        if (input$time_type == "Month-Year") {
          req(input$month_format)

          tryCatch({
            df[[time_col]] <- zoo::as.yearmon(df[[time_col]], format = input$month_format)
            #print("Converted MonthYear to yearmon format successfully.")

            start_date <- min(df[[time_col]], na.rm = TRUE)
            end_date <- max(df[[time_col]], na.rm = TRUE)

            if (is.na(start_date) || is.na(end_date)) {
              stop("Invalid start or end date for month-year slider.")
            }

            # Create sequence of months between start_date and end_date
            month_year_seq <- seq(start_date, end_date, by = 1/12)  # Monthly increments

            # Convert yearmon to Date (first day of the month) and format it properly
            month_year_labels <- format(as.Date(month_year_seq, frac = 0), "%b %Y")

            #print("Rendering Month-Year slider")

            sliderTextInput(
              inputId = "timeframe_slider",
              label = "Select Month-Year Range",
              choices = month_year_labels,
              selected = c(month_year_labels[1], month_year_labels[length(month_year_labels)])
            )

          }, error = function(e) {
            stop("Error generating Month-Year slider:")
            stop(e)
          })
        }
        else if (input$time_type == "Date") {
          req(input$date_format)
          df[[time_col]] <- as.Date(df[[time_col]], format = input$date_format)

          #print("Rendering Date slider")

          sliderInput(
            "timeframe_slider",
            "Select Date Range",
            min = min(df[[time_col]], na.rm = TRUE),
            max = max(df[[time_col]], na.rm = TRUE),
            value = c(min(df[[time_col]], na.rm = TRUE), max(df[[time_col]], na.rm = TRUE)),
            timeFormat = "%Y-%m-%d"
          )
        }
        else if (input$time_type == "Year") {
          df[[time_col]] <- as.numeric(df[[time_col]])

          #print("Rendering Year slider")

          sliderInput(
            "timeframe_slider",
            "Select Year Range",
            min = min(df[[time_col]], na.rm = TRUE),
            max = max(df[[time_col]], na.rm = TRUE),
            value = c(min(df[[time_col]], na.rm = TRUE), max(df[[time_col]], na.rm = TRUE)),
            step = 1,
            sep = "",
            ticks = TRUE
          )
        }
      })


      # Update the picker inputs based on available choices
      # Reactive function for initial column choices
      initial_choices <- reactive({
        df_homicides <- homicides()
        if (is.null(df_homicides)) return(NULL)

        choices <- colnames(df_homicides)[!colnames(df_homicides) %in% c(input$predict_var)]
        setNames(choices, choices)
      })

      # Render the numericInput for number of variables
      output$num_vars_ui <- renderUI({
        req(homicides())  # Ensure the data is uploaded

        df <- homicides()

        numericInput(
          inputId = "num_vars",
          label = "Choose number of variables:",
          value = 2,  # Default starting value
          min = 1,
          max = length(initial_choices())  # Set the max value dynamically
        )
      })

      # Render the dynamic pickers based on the value of num_vars
      output$pickers_ui <- renderUI({
        req(homicides(), input$num_vars)  # Ensure dataset and num_vars are available

        # Number of variables selected by the user
        num_vars <- input$num_vars

        # Create a list of picker inputs dynamically
        picker_inputs <- lapply(1:num_vars, function(i) {
          pickerInput(
            inputId = paste0('pick', i),
            label = paste0('Choose variable ', i, ':'),
            choices = initial_choices(),
            options = list(`actions-box` = TRUE),
            multiple = FALSE
          )
        })

        # Return the picker inputs as a tagList to render them in the UI
        do.call(tagList, picker_inputs)
      })


      observeEvent(input$Division, {
        req(homicides())
        df <- homicides()
        updateSelectInput(session, "Area", choices = unique(df[[input$Division]]))
      })

      observeEvent(input$defaultButton, {
        req(homicides())
        df <- homicides()

        selected_area_columns <- input$selected_area_columns
        if (length(selected_area_columns) > 0) {
          selected_division <- selected_area_columns[1]
          available_areas <- unique(df[[selected_division]])
          random_areas <- if (length(available_areas) >= 2) sample(available_areas, 2) else available_areas

          updateSelectInput(session, "Area", choices = available_areas, selected = random_areas)

          # Update the pickers dynamically based on the number of variables chosen
          num_vars <- input$num_vars
          choices <- initial_choices()

          # Loop through and update each picker
          lapply(1:num_vars, function(i) {
            updatePickerInput(session, paste0("pick", i), choices = choices, selected = names(choices)[i])
          })
        }
      })


      homicide_data <- eventReactive(input$view, {
        req(homicides())
        df_homicides <- homicides()

        # Time Filtering
        if (input$selected_time_columns != "" && input$time_type != "") {
          time_col <- input$selected_time_columns
          req(time_col)

          #print(paste("Time column selected:", time_col))
          #print(paste("Time column type:", class(df_homicides[[time_col]])))
          #print(paste("Timeframe input values:", input$timeframe_slider))

          if (input$time_type == "Date") {
            df_homicides[[time_col]] <- as.Date(df_homicides[[time_col]], format = input$date_format)
            start_time <- as.Date(input$timeframe_slider[1])
            end_time <- as.Date(input$timeframe_slider[2])
            df_homicides <- df_homicides %>%
              filter(df_homicides[[time_col]] >= start_time & df_homicides[[time_col]] <= end_time)

          } else if (input$time_type == "Month-Year") {
            # Convert the column to yearmon properly
            df_homicides[[time_col]] <- zoo::as.yearmon(df_homicides[[time_col]], "%Y-%m")

            # Convert input slider values to yearmon
            start_time <- zoo::as.yearmon(input$timeframe_slider[1], "%b %Y")
            end_time <- zoo::as.yearmon(input$timeframe_slider[2], "%b %Y")

            # Debugging prints to check values
            #print(paste("Converted time column class:", class(df_homicides[[time_col]])))
            #print(paste("Start time (yearmon):", start_time))
            #print(paste("End time (yearmon):", end_time))

            # Filter using yearmon values directly
            df_homicides <- df_homicides %>%
              filter(df_homicides[[time_col]] >= start_time & df_homicides[[time_col]] <= end_time)
          }

            else if (input$time_type == "Year") {
            req(input$timeframe_slider)

            if (inherits(df_homicides[[time_col]], "character")) {
              df_homicides[[time_col]] <- as.numeric(df_homicides[[time_col]])
            }

            start_year <- input$timeframe_slider[1]
            end_year <- input$timeframe_slider[2]

            if (!is.null(start_year) && !is.null(end_year)) {
              df_homicides <- df_homicides %>%
                filter(df_homicides[[time_col]] >= start_year & df_homicides[[time_col]] <= end_year)
            } else {
              stop("Invalid year range in the slider.")
            }
          }
        }

        # Area Filtering
        selected_area_columns <- input$selected_area_columns
        if (length(selected_area_columns) > 0 && !is.null(input$Area) && length(input$Area) > 0) {
          df_homicides <- df_homicides %>%
            filter(df_homicides[[selected_area_columns[1]]] %in% input$Area)
        }

        # Dynamically gather the selected columns from the pickers
        num_vars <- input$num_vars
        selected_columns <- lapply(1:num_vars, function(i) input[[paste0("pick", i)]])

        # Include the prediction variable if it's selected
        if (!is.null(input$predict_var) && input$predict_var != "") {
          selected_columns <- c(selected_columns, input$predict_var)
        }

        df_homicides <- df_homicides %>%
          select(dplyr::all_of(unlist(selected_columns)))

        return(df_homicides)
      })





      output$table <- renderDT({
        req(homicide_data())
        datatable(homicide_data())
      })

      #--------------------------------------------------------------------------------------------------
      # ============================================================
      # EVENT TREE STATE
      # ============================================================

      eventtree_pressed <- reactiveVal(FALSE)
      conditional_values <- reactiveVal(NULL)
      selected_nodes <- reactiveVal(character(0))
      current_tree <- reactiveVal(NULL)

      observeEvent(homicide_data(), {
        current_tree(NULL)
      })

      # ============================================================
      # CREATE / LOAD EVENT TREE
      # ============================================================

      homicide_set <- eventReactive(input$vieweventtree, {

        eventtree_pressed(TRUE)

        create_event_tree(homicide_data())

      })

      observeEvent(input$vieweventtree, {
        current_tree(homicide_set())
      })

      # ============================================================
      # EVENT TREE NETWORK
      # ============================================================

      output$eventtree_network <- renderVisNetwork({

        et <- current_tree()

        # If there is no modified tree yet, use the original tree
        if (is.null(et)) {
          et <- homicide_set()
        }

        if (is.null(et)) {
          return(NULL)
        }

        lt <- if (input$toggleLabels) "names" else "both"

        plot(et, label_type = lt) %>%

          visEvents(

            # --------------------------------------------------------
            # NODE SELECTED
            # --------------------------------------------------------
            selectNode = "function(params) {

        Shiny.setInputValue(
          'eventtree_network_selectedNodes',
          this.getSelectedNodes(),
          {priority: 'event'}
        );

      }",

            # --------------------------------------------------------
            # NODE DESELECTED
            # --------------------------------------------------------
            deselectNode = "function(params) {

        Shiny.setInputValue(
          'eventtree_network_selectedNodes',
          this.getSelectedNodes(),
          {priority: 'event'}
        );

      }"
          )
      })


      # ============================================================
      # EXCHANGEABILITY / FLORET BUTTON
      # ============================================================

      output$ExchangeabilityHideView <- renderUI({

        if (input$viewOption == "Map") {

          actionButton(
            "showFloretModal",
            "Show Floret"
          )

        } else {

          NULL

        }

      })


      # ============================================================
      # MAIN PANEL TITLE
      # ============================================================

      output$mainPanelTitle <- renderText({

        if (input$viewOption == "Map") {

          "Colouring on Map"

        } else {

          "Colouring on Event Tree"

        }

      })


      # ============================================================
      # STORE CURRENT EVENT-TREE SELECTION
      # ============================================================

      observeEvent(input$eventtree_network_selectedNodes, {

        selected <- input$eventtree_network_selectedNodes

        if (is.null(selected)) {
          selected <- character(0)
        }

        selected_nodes(selected)

        print("Current event tree selection:")
        print(selected_nodes())

      })


      # ============================================================
      # COLOUR SELECTED EVENT-TREE NODES
      # ============================================================

      observeEvent(input$updateColor, {

        selected_nodes_list <- selected_nodes()

        # ----------------------------------------------------------
        # Nothing selected
        # ----------------------------------------------------------

        if (is.null(selected_nodes_list) ||
            length(selected_nodes_list) == 0) {

          return()

        }


        # ----------------------------------------------------------
        # Get current tree
        # ----------------------------------------------------------

        et <- current_tree()

        if (is.null(et)) {
          et <- homicide_set()
        }

        if (is.null(et)) {
          return()
        }


        # ----------------------------------------------------------
        # Colour
        # ----------------------------------------------------------

        colours <- input$nodeColor


        # 5. Update colour dropdown
        stored_colors$all_colors <- unique(
          c(stored_colors$all_colors, colours)
        )
        updateSelectInput(
          session,
          "existing_colors",
          choices = c("", stored_colors$all_colors),
          selected = ""
        )

        # ----------------------------------------------------------
        # Safety wrapper
        # ----------------------------------------------------------

        safe_update <- function(tree, nodes, col) {

          tryCatch(

            {

              update_node_colours(
                event_tree_obj = tree,
                node_groups    = list(nodes),
                colours        = col
              )

            },

            error = function(e) {

              showModal(
                modalDialog(
                  title = "Colouring Error",

                  paste(
                    "This colouring cannot be applied because selected nodes don't have the same outgoing edge labels"
                  ),

                  easyClose = TRUE,

                  footer = modalButton("Dismiss")
                )
              )

              return(NULL)

            }
          )
        }


        # ----------------------------------------------------------
        # Apply colouring
        # ----------------------------------------------------------

        updated_tree <- safe_update(
          tree  = et,
          nodes = selected_nodes_list,
          col   = colours
        )


        # ----------------------------------------------------------
        # Stop if colouring failed
        # ----------------------------------------------------------

        if (is.null(updated_tree)) {
          return()
        }


        # ----------------------------------------------------------
        # Store modified tree
        # ----------------------------------------------------------

        current_tree(updated_tree)


        # ----------------------------------------------------------
        # Update the displayed network
        # ----------------------------------------------------------

        visNetworkProxy("eventtree_network") %>%
          visUpdateNodes(
            nodes = updated_tree$nodes
          )


        # ----------------------------------------------------------
        # Clear selection in the network
        # ----------------------------------------------------------

        visNetworkProxy("eventtree_network") %>%
          visUnselectAll()


        # ----------------------------------------------------------
        # Clear reactive selection
        # ----------------------------------------------------------

        selected_nodes(character(0))

      })


      # ============================================================
      # DELETE SELECTED EVENT-TREE NODES
      # ============================================================

      observeEvent(input$deleteNode, {

        selected_nodes_list <- selected_nodes()


        # ----------------------------------------------------------
        # Nothing selected
        # ----------------------------------------------------------

        if (is.null(selected_nodes_list) ||
            length(selected_nodes_list) == 0) {

          showModal(
            modalDialog(
              title = "No Nodes Selected",

              "Please select at least one node to delete.",

              easyClose = TRUE,

              footer = modalButton("Dismiss")
            )
          )

          return()

        }


        # ----------------------------------------------------------
        # Get current tree
        # ----------------------------------------------------------

        et <- current_tree()

        if (is.null(et)) {
          et <- homicide_set()
        }

        if (is.null(et)) {
          return()
        }


        # ----------------------------------------------------------
        # Delete nodes
        # ----------------------------------------------------------

        updated_tree <- compute_deleted_nodes(
          tree_obj        = et,
          nodes_to_delete = selected_nodes_list
        )


        # ----------------------------------------------------------
        # Store modified tree
        # ----------------------------------------------------------

        current_tree(updated_tree)


        # ----------------------------------------------------------
        # Re-render network
        #
        # IMPORTANT:
        # We use the SAME selection system as above.
        # ----------------------------------------------------------

        output$eventtree_network <- renderVisNetwork({

          lt <- if (input$toggleLabels) "names" else "both"

          plot(
            updated_tree,
            label_type = lt
          ) %>%

            visEvents(

              selectNode = "function(params) {

          Shiny.setInputValue(
            'eventtree_network_selectedNodes',
            this.getSelectedNodes(),
            {priority: 'event'}
          );

        }",

              deselectNode = "function(params) {

          Shiny.setInputValue(
            'eventtree_network_selectedNodes',
            this.getSelectedNodes(),
            {priority: 'event'}
          );

        }"
            )
        })


        # ----------------------------------------------------------
        # Clear selection
        # ----------------------------------------------------------

        selected_nodes(character(0))

      })

      observeEvent(input$AHCColoring, {

        et <- current_tree()

        if (is.null(et)) {
          et <- homicide_set()
        }

        if (is.null(et)) {
          return()
        }
        # Run AHC colouring
        ahc_tree <- tryCatch({

          ahc_colouring(
            event_tree_obj = et,
            level_separation = 1000,
            node_distance = 300
          )

        }, error = function(e) {

          showModal(modalDialog(
            title = "AHC Colouring Error",
            paste(
              "AHC colouring could not be completed:",
              e$message
            ),
            easyClose = TRUE,
            footer = modalButton("Dismiss")
          ))

          NULL
        })

        # Stop if AHC failed
        if (is.null(ahc_tree)) {
          return()
        }

        # Store the AHC-coloured tree
        current_tree(ahc_tree)

        # Update the displayed event tree
        visNetworkProxy("eventtree_network") %>%
          visUpdateNodes(nodes = ahc_tree$nodes)

      })

      node_colors_levels <- reactiveVal(NULL)
      finished_coloring <- reactiveVal(NULL)
      #hi <- reactiveVal(NULL)

      # Observe the finishedColoring button click
      observeEvent(input$finishedColoring, {
        finished_coloring(TRUE)
        data <- current_tree()
        print(data$nodes)
        print(data$edges)
        print(data)

        # Get unique levels from the nodes
        levels <- unique(data$nodes$level)
        min_level <- min(levels)
        max_level <- max(levels)

        # Initialize vectors to store incorrect node IDs
        incorrect_color_nodes <- c()
        duplicate_color_nodes <- c()


        # Iterate over nodes to check the color and level conditions
        for (i in 1:nrow(data$nodes)) {
          node <- data$nodes[i, ]

          # Check if the node has color #ffffff and its level isn't min or max
          if (node$color == "#FFFFFF" && !(node$level2 %in% c(min_level, max_level))) {
            incorrect_color_nodes <- c(incorrect_color_nodes, node$id)
          }
        }

        # Check for duplicate colors in different levels (ignoring min and max levels)
        non_min_max_nodes <- data$nodes %>% filter(!(level2 %in% c(min_level, max_level)))
        duplicate_colors <- non_min_max_nodes %>%
          group_by(color) %>%
          filter(n_distinct(level2) > 1) %>%
          pull(id)

        if (length(duplicate_colors) > 0) {
          duplicate_color_nodes <- unique(duplicate_colors)
        }

        # If there are any incorrect color nodes, display a modal with all node IDs
        if (length(incorrect_color_nodes) > 0) {
          showModal(modalDialog(
            title = "Incorrect Node Colours",
            paste("The following nodes have not been coloured correctly:", paste(incorrect_color_nodes, collapse = ", ")),
            easyClose = TRUE
          ))
          return() # Exit the observer to stop further processing
        }

        # If there are duplicate color nodes in different levels, show an error
        if (length(duplicate_color_nodes) > 0) {
          showModal(modalDialog(
            title = "Duplicate Node Colours",
            paste("The following nodes have the same colour but different levels:",
                  paste(duplicate_color_nodes, collapse = ", ")),
            easyClose = TRUE
          ))
          return() # Exit the observer to stop further processing
        }

        # If no issues, proceed with processing the data
        node_colors_levels_data <- data$nodes


        #############!!!!!!!!!!!!!!!!!!!!!!!!!!
        if (!"outgoing_edges2" %in% names(node_colors_levels_data)) {
          node_colors_levels_data$outgoing_edges2 <- node_colors_levels_data$outgoing_edges
        }

        # Group by color, level, and outgoing edges, and calculate the number of nodes
        number_edges <- node_colors_levels_data %>%
          group_by(color, level2, outgoing_edges2) %>%
          summarize(number_nodes = sum(number))

        # Join the node colors and levels data with the number of nodes per color/level/outgoing_edges
        if (!is.null(node_colors_levels_data) && !is.null(number_edges)) {
          node_colors_levels_data <- full_join(
            node_colors_levels_data,
            number_edges,
            by = c("color", "level2", "outgoing_edges2")
          )
        }

        # Store the node colors and levels in the reactive value
        node_colors_levels(node_colors_levels_data)
      })






      # Render the color and level table
      node_colors_levels2 <- reactiveVal(NULL)  # Keep track of edits
      asymmetric_tree_prior <- reactiveVal(NULL)

      observe({
        req(current_tree())
        req(finished_coloring())
        # Convert event_tree → staged_tree if needed

        if (isTRUE(priors_locked())) return()

        staged <- current_tree()

        map_prior_type <- function(x) {
          switch(
            x,
            "Uniform 1,1 Prior"        = "Uniform",
            "Phantom Individuals Prior" = "Phantom",
            "Specify Prior"             = "Custom",
            "Uniform"                   = "Uniform"  # fallback
          )
        }

        custom_priors_list <- list()
        # Apply priors using your function
        prior_tbl <- specify_priors(
          staged_tree_obj = staged,
          prior_type      = map_prior_type(input$priorChoice),
          custom_priors   = if (input$priorChoice == "Specify Prior") custom_priors_list else NULL
        )

        # Store for editing
        asymmetric_tree_prior(prior_tbl)

        # Render table
        output$colorLevelTable <- renderDT({
          datatable(
            prior_tbl$table,
            escape = FALSE,
            editable = TRUE,
            options = list(dom = 't', pageLength = 50),
            rownames = FALSE
          ) %>%
            formatStyle(
              columns = "Colour",
              valueColumns = "Colour",
              backgroundColor = styleEqual(prior_tbl$table$Colour, prior_tbl$table$Colour),
              color = styleEqual(prior_tbl$table$Colour, prior_tbl$table$Colour)
            )
        })
      })

      proxy_color_table <- dataTableProxy("colorLevelTable")

      priors_locked <- reactiveVal(FALSE)
      observeEvent(input$colorLevelTable_cell_edit, {
        priors_locked(TRUE)
        info <- input$colorLevelTable_cell_edit
        tbl  <- asymmetric_tree_prior()

        row_index <- info$row
        new_prior <- info$value

        # --- SAFETY WRAPPER ---
        updated <- tryCatch(
          {
            edit_priors(
              prior_table = tbl,
              rows        = row_index,
              new_priors  = list(new_prior)
            )
          },
          error = function(e) {
            showModal(modalDialog(
              title = "Invalid Prior",
              paste("Error:", e$message),
              easyClose = TRUE,
              footer = modalButton("Dismiss")
            ))
            return(NULL)
          }
        )

        # Stop if invalid
        if (is.null(updated)) return()

        # Store updated prior table
        asymmetric_tree_prior(updated)

        # --- PARTIAL UPDATE (no full redraw) ---
        DT::replaceData(
          proxy_color_table,
          updated$table,
          resetPaging = FALSE,
          rownames = FALSE
        )
      })



      staged_tree_data <- reactiveVal(NULL)

      observeEvent(input$finishedPrior, {

        edited <- asymmetric_tree_prior()$table
        data   <- current_tree()

        # Clear priors ONCE
        data$nodes$prior <- ""

        for (i in seq_len(nrow(edited))) {

          color  <- edited$Colour[i]
          level2 <- edited$Level[i]
          prior  <- edited$Prior[i]

          data$nodes$prior[
            data$nodes$color == color &
              data$nodes$level2 == level2
          ] <- prior
        }

        print("data$nodes fin prior")
        print(data$nodes)

        current_tree(data)
      })


      observeEvent(input$viewstagedtree, {

        data <- current_tree()
        prior_type <- input$priorChoice

        # -------------------------------
        # SAFE PRIOR PARSER
        # -------------------------------
        convertPrior <- function(prior) {
          if (is.null(prior) || prior == "" || all(is.na(prior))) return(NA_real_)
          prior <- gsub(" ", "", prior)
          prior <- gsub(",$", "", prior)
          parts <- unlist(strsplit(prior, ","))
          parts <- parts[parts != ""]
          nums <- suppressWarnings(as.numeric(parts))
          if (any(is.na(nums))) return(NA_real_)
          nums
        }

        splitPriors <- convertPrior

        # -------------------------------
        # ENSURE REQUIRED NODE COLUMNS
        # -------------------------------
        if (!"prior" %in% colnames(data$nodes)) data$nodes$prior <- ""
        if (!"adjusted_prior" %in% colnames(data$nodes)) data$nodes$adjusted_prior <- ""
        if (!"ratio" %in% colnames(data$nodes)) data$nodes$ratio <- ""
        if (!"priorvariance" %in% colnames(data$nodes)) data$nodes$priorvariance <- ""

        # -------------------------------
        # ENSURE REQUIRED EDGE COLUMNS
        # -------------------------------
        if (!"label_prior_frac" %in% colnames(data$edges)) data$edges$label_prior_frac <- rep("", nrow(data$edges))
        if (!"label_prior_mean" %in% colnames(data$edges)) data$edges$label_prior_mean <- rep("", nrow(data$edges))
        if (!"label3" %in% colnames(data$edges)) data$edges$label3 <- rep(NA_real_, nrow(data$edges))
        if (!"ratio" %in% colnames(data$edges)) data$edges$ratio <- rep("", nrow(data$edges))

        # -------------------------------
        # NON-PHANTOM PRIOR ADJUSTMENT
        # -------------------------------
        if (prior_type != "Phantom Individuals Prior") {

          groups <- unique(data$nodes[, c("color", "level2")])

          for (g in seq_len(nrow(groups))) {
            color  <- groups$color[g]
            level2 <- groups$level2[g]

            same_group <- data$nodes[data$nodes$color == color &
                                       data$nodes$level2 == level2, ]

            if (nrow(same_group) > 0) {
              count <- nrow(same_group)

              for (i in seq_len(nrow(same_group))) {
                idx <- which(data$nodes$id == same_group$id[i])
                prior <- data$nodes$prior[idx]

                vals <- convertPrior(prior)
                if (!all(is.na(vals))) {
                  adj <- vals / count
                  data$nodes$adjusted_prior[idx] <- paste(round(adj, 3), collapse = ",")
                }
              }
            }
          }

        } else {

          # -------------------------------
          # PHANTOM INDIVIDUALS PRIOR
          # -------------------------------
          asym <- asymmetric_tree_prior()$table

          for (i in seq_len(nrow(asym))) {

            color  <- asym$Colour[i]
            level2 <- asym$Level[i]
            prior  <- asym$Prior[i]

            vals <- convertPrior(prior)
            if (all(is.na(vals))) next

            data$nodes$adjusted_prior[
              data$nodes$color == color &
                data$nodes$level2 == level2
            ] <- paste(round(vals, 3), collapse = ", ")
          }
        }

        # -------------------------------
        # ASSIGN ADJUSTED PRIORS TO EDGES
        # -------------------------------
        for (i in seq_len(nrow(data$nodes))) {

          from_node <- data$nodes$id[i]
          adj_prior <- data$nodes$adjusted_prior[i]

          vals <- convertPrior(adj_prior)
          if (all(is.na(vals))) next

          edges_idx <- which(data$edges$from == from_node)

          for (j in seq_along(edges_idx)) {
            data$edges$label_prior_frac[edges_idx[j]] <-
              paste(data$edges$label1[edges_idx[j]], "\n", vals[j])
            data$edges$label3[edges_idx[j]] <- vals[j]
          }
        }

        # ============================================================
        # POPULATE label_prior_frac BY SPLITTING STAGE PRIOR ACROSS NODES
        # ============================================================

        prior_tbl <- asymmetric_tree_prior()$table

        for (i in seq_len(nrow(prior_tbl))) {

          color  <- prior_tbl$Colour[i]
          level2 <- prior_tbl$Level[i]
          prior  <- prior_tbl$Prior[i]

          # Convert stage prior to numeric vector
          stage_vals <- convertPrior(prior)
          if (all(is.na(stage_vals))) next

          # Find all nodes in this stage
          stage_nodes <- data$nodes$id[
            data$nodes$color == color &
              data$nodes$level2 == level2
          ]

          n_nodes <- length(stage_nodes)
          if (n_nodes == 0) next

          # Divide stage prior equally across nodes
          node_share <- stage_vals / n_nodes   # <-- THIS IS THE KEY RULE

          # Assign node-share to each node's outgoing edges
          for (node_id in stage_nodes) {

            edges_idx <- which(data$edges$from == node_id)

            for (j in seq_along(edges_idx)) {
              data$edges$label_prior_frac[edges_idx[j]] <-
                paste(data$edges$label1[edges_idx[j]], "\n", round(node_share[j], 3))
            }
          }
        }


        # -------------------------------
        # PRIOR MEAN (RATIOS)
        # -------------------------------
        calculateRatios <- function(priors) {
          total <- sum(priors)
          if (total == 0) return(rep(0, length(priors)))
          priors / total
        }

        for (i in seq_len(nrow(data$nodes))) {

          prior <- data$nodes$prior[i]
          vals <- convertPrior(prior)
          if (all(is.na(vals))) next

          ratios <- round(calculateRatios(vals), 3)
          data$nodes$ratio[i] <- paste(ratios, collapse = ", ")

          edges_idx <- which(data$edges$from == data$nodes$id[i])

          for (j in seq_along(edges_idx)) {
            data$edges$ratio[edges_idx[j]] <- ratios[j]
            data$edges$label_prior_mean[edges_idx[j]] <-
              paste(data$edges$label1[edges_idx[j]], "\n", ratios[j])
          }
        }

        # -------------------------------
        # PRIOR VARIANCE
        # -------------------------------
        calculateVariance <- function(priors) {
          total <- sum(priors)
          if (total == 0) return(rep(0, length(priors)))
          (priors * (total - priors)) / (total^2 * (total + 1))
        }

        for (i in seq_len(nrow(data$nodes))) {

          prior <- data$nodes$prior[i]
          vals <- convertPrior(prior)
          if (all(is.na(vals))) next

          vars <- round(calculateVariance(vals), 3)
          data$nodes$priorvariance[i] <- paste(vars, collapse = ", ")
        }

        # -------------------------------
        # TOOLTIP ASSIGNMENT
        # -------------------------------
        createTooltipWithPrior <- function(prior, ratio, priorvariance) {
          if (!is.na(prior) && prior != "") {
            paste(
              "Prior Distribution: Dirichlet(", prior, ")<br>",
              "Prior Mean: (", ratio, ")<br>",
              "Prior Variance: (", priorvariance, ")"
            )
          } else {
            "Leaf nodes have no prior"
          }
        }

        data$nodes$title <- apply(
          data$nodes, 1,
          function(row) createTooltipWithPrior(row["prior"], row["ratio"], row["priorvariance"])
        )

        staged_tree_data(data)
      })


      output$stagedtree <- renderVisNetwork({

        st <- staged_tree_data()

        if (is.null(st)) return(NULL)

        lt <- if (input$usePriorLabels) "priormeans" else "priorfrac"

        plot(st, label_type = lt) %>%
          visEvents(
            selectNode = "function(params) {
        Shiny.setInputValue(
          'stagedtree_selectedNodes',
          this.getSelectedNodes(),
          {priority: 'event'}
        );
      }",
            deselectNode = "function(params) {
        Shiny.setInputValue(
          'stagedtree_selectedNodes',
          this.getSelectedNodes(),
          {priority: 'event'}
        );
      }"
          )
      })


      contracted_data <- reactiveVal(NULL)
      grouped_df2 <- reactiveVal(NULL)
      observeEvent(input$viewceg, {

        # 1. Get staged tree with priors already computed
        data <- staged_tree_data()

        # 2. Build staged_tree_priors object
        staged_tree_priors <- list(
          nodes       = data$nodes,
          edges       = data$edges,
          prior_table = asymmetric_tree_prior()
        )
        class(staged_tree_priors) <- "staged_tree_priors"

        # 3. Compute CEG using your package function
        ceg_obj <- compute_ceg(staged_tree_priors)

        # 4. Store contracted nodes + edges for map rendering
        contracted_data(list(
          nodes = ceg_obj$nodes,
          edges = ceg_obj$edges,
          table = ceg_obj$table
        ))

        # 5. Store aggregated stage-level table
        grouped_df2(ceg_obj$table)
      })


      observe({
        data <- contracted_data()
        req(data)
        print("data$nodes")
        print(data$nodes)
        print(data$edges)
        print(data$table)
        print("------------")

        if (input$showposteriormean) {
          data$edges$label <- paste(data$edges$label1, "\n", data$edges$posterior_mean)
        } else {
          data$edges$label <- paste(data$edges$label1)
        }

        contracted_data(data)

      })

      grouped_df3 <- reactive({
        req(contracted_data())
        df <- contracted_data()$table
        print(df)

        # Convert "a,b,c" → numeric vector
        extract_alpha <- function(s) {
          as.numeric(unlist(strsplit(s, ",")))
        }

        # Dirichlet variance formula
        calculate_variance <- function(alpha) {
          total <- sum(alpha)
          (alpha * (total - alpha)) / (total^2 * (total + 1))
        }

        df %>%
          rowwise() %>%
          mutate(
            alpha_params = list(extract_alpha(Posterior)),
            variances    = list(round(calculate_variance(alpha_params), 3)),
            title = paste0(
              "Posterior Distribution: Dirichlet(", Posterior, ")<br>",
              "Posterior Mean: (", Posterior_Mean, ")<br>",
              "Posterior Variance: (", paste(variances, collapse = ", "), ")"
            )
          ) %>%
          ungroup()
      })


      # Function to calculate posterior mean products for all paths
      calculate_path_products <- function(nodes_df, edges_df, root_node = "w0") {

        # Initialize a list to store paths and products
        paths_list <- list()

        # Find all paths from root node (could use graph traversal here)
        traverse_paths <- function(node, path, product) {
          # Find outgoing edges from current node
          next_edges <- edges_df[edges_df$from == node, ]

          if (nrow(next_edges) == 0) {
            # If no outgoing edges, it's a terminal node, save the path and product
            paths_list <<- append(paths_list, list(list(path = path, product = product)))
          } else {
            # Otherwise, traverse the next nodes
            for (i in 1:nrow(next_edges)) {
              # Access the posteriormean from the edge
              posteriormean <- as.numeric(next_edges$posteriormean[i])

              # Append the label1 (from edges_df) to the path
              new_path <- c(path, next_edges$label1[i])

              # Continue traversal with the new node
              traverse_paths(next_edges$to[i], new_path, product * posteriormean)
            }
          }
        }

        # Start traversal from the root node
        traverse_paths(root_node, path = character(0), product = 1)

        # Convert list of paths and products into a data frame
        path_df <- do.call(rbind, lapply(paths_list, function(x) data.frame(path = paste(x$path, collapse = " -> "), product = x$product)))

        return(path_df)
      }


      conditional_df <- reactiveVal(NULL)

      observe({
        data <- contracted_data()
        req(data)

        edges <- data$edges
        req(edges)

        # Group labels by level
        grouped <- split(edges$label1, edges$level)

        conditional_values(grouped)
      })

      output$unique_values_ui <- renderUI({
        unique_values <- conditional_values()
        req(unique_values)

        selectInput(
          inputId = "unique_values",
          label = "Choose Conditionals:",
          choices = c("None" = "NONE"),   # <-- add this
          selectize = TRUE,
          multiple = TRUE
        )
      })


      observe({
        # Get the unique values
        unique_values <- conditional_values()

        # Get shapefile data
        shapefile_vals <- shapefileData()
        shapefile_vals <- shapefile_vals[[1]]

        # Prepare the choices with optgroups and excluding values in shapefileData
        choices <- list()
        unique_value_index <- c()  # To store the index for each value

        for (i in 1:(length(unique_values) - 1)) {
          # Exclude values found in shapefileData
          filtered_values <- setdiff(unique_values[[i]], shapefile_vals)

          # Only add filtered values if they aren't empty
          if (length(filtered_values) > 0) {
            # Store the filtered values in choices
            choices[[paste("Variable", i)]] <- filtered_values

            # Store the corresponding index for each value
            unique_value_index <- c(unique_value_index, rep(i, length(filtered_values)))
          }
        }

        # Store the index mapping in session for use later
        session$userData$unique_value_index <- setNames(unique_value_index, unlist(choices))

        # Update the selectInput choices dynamically, with groups and labels
        all_choices <- list()
        for (i in 1:(length(unique_values) - 1)) {
          # Exclude values found in shapefileData
          filtered_values <- setdiff(unique_values[[i]], shapefile_vals)

          # Only add filtered values if they aren't empty
          if (length(filtered_values) > 0) {
            # Grouped by the category (e.g., "Variable 1", "Variable 2")
            all_choices[[paste("Variable", i)]] <- filtered_values
          }
        }

        # Update the selectInput choices dynamically with proper categories
        updateSelectInput(session, "unique_values",
                          choices = all_choices,
                          selected = NULL)  # Default selection
      })

      # Observe selected values and output the selected values with their indices
      observe({
        # Get the selected values from the UI
        selected_values <- input$unique_values

        if (length(selected_values) > 0) {
          # Get the corresponding indices from the stored mapping
          selected_indices <- session$userData$unique_value_index[selected_values]

        }
      })



      # Create the dropdown for only the last group in the list (single-select)
      output$last_group_ui <- renderUI({
        # Get the unique values list
        unique_values <- conditional_values()

        # Get the last group (i.e., the final [[]])
        last_group <- unique_values[[length(unique_values)]]

        selectInput(
          inputId = "last_group",
          label = "Colour by:",
          choices = last_group # Enable selectize (optional, makes it searchable)
        )
      })

      observe({
        unique_values <- conditional_values()
        req(unique_values)

        last_group <- unique_values[[length(unique_values)]]

        default_last <- if (!is.null(last_group) && length(last_group) > 0) {
          last_group[[1]]
        } else {
          NULL
        }

        updateSelectInput(
          session,
          "last_group",
          choices  = last_group,
          selected = default_last
        )
      })


      calculate_conditional_prob <- function(path_df, unique_values, selected_indices, last_group) {
        # Ensure unique_values is a character vector
        unique_values <- as.character(unique_values)

        # Group values by their assigned index
        grouped_conditions <- split(unique_values, selected_indices)

        # Step 1: Filter paths directly based on AND/OR logic
        condition_paths <- path_df[sapply(path_df$path, function(path) {
          path_components <- unlist(strsplit(path, " -> "))  # Convert path to vector

          # Apply AND/OR logic for each group
          all(sapply(grouped_conditions, function(group) {
            if (length(group) == 1) {
              group %in% path_components  # AND condition: must be present
            } else {
              any(group %in% path_components)  # OR condition: at least one must be present
            }
          }))
        }), , drop = FALSE]  # Prevent accidental list conversion

        # If no matching paths exist, return 0 probability
        if (nrow(condition_paths) == 0) {
          #print(paste("P(", last_group, "|", paste(unique_values, collapse = ", "), ") = 0"))
          return(0)
        }

        # Step 2: Compute joint probability P(last_group and unique_values)
        joint_prob <- sum(condition_paths$product[sapply(condition_paths$path, function(path) {
          last_group %in% unlist(strsplit(path, " -> "))
        })])

        # Step 3: Compute marginal probability P(unique_values)
        marginal_prob <- sum(condition_paths$product)

        # Step 4: Compute conditional probability P(last_group | unique_values)
        conditional_prob <- ifelse(marginal_prob > 0, joint_prob / marginal_prob, 0)

        #print(paste("P(", last_group, "|", paste(unique_values, collapse = ", "), ") = ", conditional_prob, sep = ""))
        return(conditional_prob)
      }

      calculate_area_probabilities <- function(path_df, unique_values, selected_indices, last_group, shapefile_vals) {
        area_probs <- list()  # Store probabilities for each area

        if (is.null(unique_values)) {
          # Marginal probability: sum of path products containing the area
          area_probs <- lapply(shapefile_vals, function(area) {
            area_paths <- path_df[sapply(path_df$path, function(p) {
              area %in% unlist(strsplit(p, " -> "))
            }), , drop = FALSE]

            if (nrow(area_paths) == 0) return(NA_real_)
            sum(area_paths$product)
          })

          names(area_probs) <- shapefile_vals
          return(area_probs)
        }

        # Loop over each area in shapefile_vals
        for (area in shapefile_vals) {
          # Filter paths for the current area
          area_paths <- path_df[sapply(path_df$path, function(path) {
            area %in% unlist(strsplit(path, " -> "))  # Ensure area is in path
          }), , drop = FALSE]

          # If no paths exist for this area, assign probability 0
          if (nrow(area_paths) == 0) {
            area_probs[[area]] <- NA
          } else {
            # Compute probability for the given area
            area_probs[[area]] <- calculate_conditional_prob(area_paths, unique_values, selected_indices, last_group)
          }
        }

        return(area_probs)  # Return a named list of probabilities
      }


      output$ceg_network <- renderVisNetwork({

        ceg <- contracted_data()
        if (is.null(ceg)) return(NULL)

        ceg_obj <- structure(
          list(
            nodes = ceg$nodes,
            edges = ceg$edges,
            table = grouped_df3()
          ),
          class = "ceg"
        )

        node_tooltips <- grouped_df3() %>%
          select(Colour, Level, title)

        ceg_obj$nodes <- ceg_obj$nodes %>%
          left_join(node_tooltips,
                    by = c("color" = "Colour", "level" = "Level")) %>%
          mutate(
            title = ifelse(
              is.na(title),
              "Leaf nodes have no posterior",
              title
            )
          )

        lt <- if (input$showposteriormean) "posterior_mean" else "posterior"

        # IMPORTANT: plot.ceg() already contains visEvents
        plot(ceg_obj, label = lt, level_separation = input$levelSeparation)
      })

      observe({

        req(shapefileData())
        req(contracted_data())

        ceg_obj <- structure(
          list(
            nodes = contracted_data()$nodes,
            edges = contracted_data()$edges,
            table = grouped_df3()
          ),
          class = "ceg"
        )


        # --- NEW: detect no conditionals ---
        if (is.null(input$unique_values) || "NONE" %in% input$unique_values) {

          ceg_map_obj <- generate_CEG_map(
            shapefile   = shapefileData(),
            ceg_object  = ceg_obj,
            conditionals = NULL,        # <-- no conditioning
            colour_by    = input$last_group,        # <-- colour by max-level label
            color_palette = input$color_palette
          )

        } else {

          ceg_map_obj <- generate_CEG_map(
            shapefile   = shapefileData(),
            ceg_object  = ceg_obj,
            conditionals = input$unique_values,
            colour_by    = input$last_group,
            color_palette = input$color_palette
          )
        }

        output$ceg_map <- renderLeaflet({
          plot(ceg_map_obj)
        })
      })



      extract_floret <- function(tree_obj, start_label1) {

        # Accept both event_tree and staged_tree
        if (!inherits(tree_obj, "event_tree") &&
            !inherits(tree_obj, "staged_tree")) {
          stop("Input must be an event_tree or staged_tree object.")
        }

        nodes <- tree_obj$nodes
        edges <- tree_obj$edges

        # Find all edges whose label1 matches the starting label
        start_edges <- edges[edges$label1 == start_label1, ]

        if (nrow(start_edges) == 0) {
          return(structure(
            list(nodes = nodes[0, ], edges = edges[0, ]),
            class = class(tree_obj)
          ))
        }

        floret_edges <- data.frame()
        visited_nodes <- character()

        collect_floret <- function(current_node) {

          outgoing_edges <- edges[edges$from == current_node, ]

          # Avoid self-loops
          outgoing_edges <- outgoing_edges[outgoing_edges$to != current_node, ]

          if (nrow(outgoing_edges) > 0) {
            floret_edges <<- rbind(floret_edges, outgoing_edges)
          }

          to_nodes <- outgoing_edges$to
          new_nodes <- to_nodes[!to_nodes %in% visited_nodes]

          visited_nodes <<- c(visited_nodes, new_nodes)

          for (node in new_nodes) {
            collect_floret(node)
          }
        }

        # Start traversal from each edge whose label1 matches
        for (i in seq_len(nrow(start_edges))) {
          start_node <- start_edges$to[i]

          if (!start_node %in% visited_nodes) {
            visited_nodes <- c(visited_nodes, start_node)
            collect_floret(start_node)
          }
        }

        # Filter nodes to include only those in the floret
        floret_nodes_df <- nodes[nodes$id %in% visited_nodes, ]

        # Return same class as input
        structure(
          list(
            nodes = floret_nodes_df,
            edges = floret_edges,
            data = tree_obj$filtereddf
          ),
          class = class(tree_obj)
        )
      }



      output$UpdateTable <- renderDT({
        req(grouped_df3())  # Ensure data is available

        # Reorder the columns in the dataframe and sort by `Stage`
        reordered_df <- grouped_df3() %>%
          mutate(stage_num = as.numeric(gsub("\\D", "", Stage))) %>%  # Extract the numeric part
          arrange(stage_num)

        display_df <- reordered_df %>%
          select(Stage, Colour, Level, Data, Prior, Prior_Mean, Posterior, Posterior_Mean, Prior_Type)  # Select only the columns to display

        # Render the DataTable with equal column widths
        datatable(
          display_df,
          escape = FALSE,  # Ensure we don't escape HTML if not necessary
          editable = FALSE,
          options = list(
            dom = 't', pageLength = 50,# Enable automatic column width calculation
            columnDefs = list(
              list(width = '14%', targets = '_all')  # Set width for all columns
            )
          ),
          rownames = FALSE
        ) %>%
          # Format the Stage Colour column for background color
          formatStyle(
            'Stage',
            backgroundColor = styleEqual(display_df$Stage, display_df$Colour),
            color = "#000000"
          )
      })


      process_shapefile <- function(shape_data, input_crs) {
        if (is.na(st_crs(shape_data))) {
          # If the shapefile has no CRS, check if the user has provided one
          if (is.na(input_crs) || input_crs == "") {
            showModal(modalDialog(
              title = "Shapefile has no CRS",
              "Please specify a CRS to proceed.",
              easyClose = TRUE,
              footer = modalButton("Close")
            ))

            showNotification("Shapefile has no CRS. Please assign a CRS to continue.", type = "warning")
            return(NULL)  # Exit the function if no CRS is provided
          } else {
            # Manually assign the CRS (assuming the user provides the correct CRS code)
            # Example for UK Ordnance Survey National Grid, EPSG:27700
            st_crs(shape_data) <- as.numeric(input_crs)

          }

          # Transform the shapefile to WGS84 (EPSG:4326) which is standard for Leaflet maps
          shape_data <- st_transform(shape_data, crs = 4326)
        } else {
          # If the shapefile already has a CRS, transform it to WGS84 (EPSG:4326)
          shape_data <- st_transform(shape_data, crs = 4326)
        }

        return(shape_data)
      }

      shapefileData <- reactive({
        req(input$shapefile)

        # Create a temporary directory to extract the ZIP
        temp_dir <- file.path(tempdir(), as.character(Sys.time()))
        dir.create(temp_dir)
        zip_path <- input$shapefile$datapath

        # Unzip the uploaded file
        unzip(zip_path, exdir = temp_dir)

        # Search for .shp files recursively in the temporary directory
        shp_files <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)

        if (length(shp_files) == 0) {
          showNotification("No .shp file found in ZIP archive.", type = "error")
          return(NULL)
        }

        # Use the first shapefile found
        shape_data <- st_read(shp_files[1])

        # Example: Assign CRS and transform to WGS84 if needed
        shape_data <- process_shapefile(shape_data, input$crs)

        return(shape_data)
      })

      dynamic_fill <- reactiveVal(NULL)
      # Reactive value to store the currently selected polygon ID
      selected_polygon <- reactiveVal(NULL)

      observeEvent(input$process_shapefile, {

        # ------------------------------------------------------------
        # Check that the Event Tree has been created
        # ------------------------------------------------------------

        if (!eventtree_pressed()) {

          showModal(
            modalDialog(
              title = "Action Required",
              "Please create Event Tree first.",
              easyClose = TRUE,
              footer = NULL
            )
          )

          return()
        }


        # ------------------------------------------------------------
        # Get shapefile data
        # ------------------------------------------------------------

        shape_data <- shapefileData()

        req(shape_data)


        # ------------------------------------------------------------
        # Render map
        # ------------------------------------------------------------

        output$map <- renderLeaflet({

          req(shape_data)

          # Always use the CURRENT event tree
          visoutputdata <- current_tree()

          if (is.null(visoutputdata)) {
            visoutputdata <- homicide_set()
          }

          req(visoutputdata)


          # ----------------------------------------------------------
          # Currently selected polygons
          # ----------------------------------------------------------

          selected_ids <- selected_polygon()

          if (is.null(selected_ids)) {
            selected_ids <- character(0)
          }


          # ----------------------------------------------------------
          # Start with white/default fill colours
          # ----------------------------------------------------------

          shape_data$fillColor <- "white"


          # ----------------------------------------------------------
          # Calculate colour for each polygon
          # ----------------------------------------------------------

          polygon_ids <- shape_data[[1]]

          for (i in seq_along(polygon_ids)) {

            polygon_id <- polygon_ids[i]


            # Skip NA polygon IDs
            if (is.na(polygon_id)) {
              next
            }


            # --------------------------------------------------------
            # Extract floret for this polygon
            # --------------------------------------------------------

            floret3 <- tryCatch(

              {
                extract_floret(
                  visoutputdata,
                  polygon_id
                )
              },

              error = function(e) {
                NULL
              }
            )


            # --------------------------------------------------------
            # Default colour
            # --------------------------------------------------------

            fillColor <- "white"


            # --------------------------------------------------------
            # Determine floret colour
            # --------------------------------------------------------

            if (!is.null(floret3)) {

              nodes <- floret3$nodes

              if (
                nrow(nodes) > 0 &&
                "level" %in% names(nodes) &&
                "color" %in% names(nodes) &&
                !all(is.na(nodes$level))
              ) {

                max_level <- max(
                  nodes$level,
                  na.rm = TRUE
                )

                non_max_level_nodes <- nodes[
                  nodes$level != max_level,
                  ,
                  drop = FALSE
                ]


                if (nrow(non_max_level_nodes) > 0) {

                  node_colours <- non_max_level_nodes$color


                  if (all(node_colours == "#FFFFFF")) {

                    fillColor <- "orangered"

                  } else if (all(node_colours != "#FFFFFF")) {

                    fillColor <- "darkgreen"

                  } else {

                    fillColor <- "orange"

                  }
                }
              }
            }


            # --------------------------------------------------------
            # Assign calculated colour to this polygon
            # --------------------------------------------------------

            shape_data$fillColor[
              polygon_ids == polygon_id
            ] <- fillColor
          }


          # ----------------------------------------------------------
          # Save colours BEFORE applying selection colour
          # ----------------------------------------------------------

          shape_data$previous_fillColor <- shape_data$fillColor


          # ----------------------------------------------------------
          # Selected polygons are blue
          # ----------------------------------------------------------

          shape_data$fillColor <- ifelse(

            polygon_ids %in% selected_ids,

            "blue",

            shape_data$previous_fillColor
          )


          # ----------------------------------------------------------
          # Render Leaflet
          # ----------------------------------------------------------

          leaflet(data = shape_data) %>%

            addTiles() %>%

            onRender(
              "function(el, x) {
          L.control.zoom({
            position: 'bottomright'
          }).addTo(this);
        }"
            ) %>%

            addPolygons(

              layerId = ~as.character(polygon_ids),

              fillColor = ~fillColor,

              color = "black",

              weight = 1,

              highlightOptions = highlightOptions(
                weight = 1,
                color = "black",
                fillOpacity = 0.7,
                bringToFront = TRUE
              ),

              opacity = 1,

              fillOpacity = input$mapOpacity,

              popup = NULL,

              label = ~as.character(polygon_ids)
            )
        })

      })





      floret2 <- reactiveVal(NULL)

      #selected_polygons <- reactiveVal(character())


      # Observe click event on the map
      observeEvent(input$map_shape_click, {
        clicked_id <- input$map_shape_click$id

        if (!is.null(clicked_id)) {
          # Get the current selection state (list of selected polygons)
          current_selection <- selected_polygon()

          # Print the current selection to debug
          #
          print(paste("Current selected polygon(s):", toString(current_selection)))

          # Check if the clicked polygon is already selected
          if (clicked_id %in% current_selection) {
            # Deselect the polygon if it is already selected (remove from selection)
            updated_selection <- setdiff(current_selection, clicked_id)
            selected_polygon(updated_selection)

            # Remove the highlight by clearing the "highlighted" group
            leafletProxy("map") %>%
              clearGroup("highlighted")

            #print(paste("Deselected polygon:", clicked_id))  # Debugging print
          } else {
            # Select the polygon if it is not already selected (add to selection)
            updated_selection <- c(current_selection, clicked_id)
            selected_polygon(updated_selection)



            #print(paste("Selected polygon:", clicked_id))  # Debugging print
          }

          # Print the updated list of selected polygons
          #print(paste("Updated selected polygon(s):", toString(selected_polygon())))
        }
      })








      first_floret <- reactiveVal(NULL)
      all_florets <- reactiveVal(NULL)




      # Show the modal when the button is clicked
      observeEvent(input$showFloretModal, {

        clicked_ids <- selected_polygon()
        print(paste("Clicked IDs:", toString(clicked_ids)))
        if (is.null(clicked_ids)) return()

        shape_data     <- shapefileData()
        print(shape_data)
        visoutputdata  <- current_tree()
        #print(visoutputdata$nodes)

        leafletProxy("map") %>% clearGroup("highlighted")

        florets_list        <- list()
        floret_colors_list  <- list()

        # Extract florets for each selected polygon
        for (area_id in clicked_ids) {

          clicked_data <- shape_data[shape_data[[1]] == area_id, ]

          # If the polygon has no matching data row → popup + skip
          if (nrow(clicked_data) == 0) {
            showModal(modalDialog(
              title = "No Data for Selected Area",
              paste(
                "The selected area:", area_id,
                "does not exist in the event tree data."
              ),
              easyClose = TRUE,
              footer = modalButton("Dismiss")
            ))
            next
          }

          start_label1 <- clicked_data[[1, 1]]

          floret <- tryCatch({
            extract_floret(visoutputdata, start_label1)
          }, error = function(e) NULL)

          # If extract_floret() fails or returns empty → popup + skip
          if (is.null(floret) || nrow(floret$nodes) == 0 || nrow(floret$edges) == 0) {
            showModal(modalDialog(
              title = "Floret Not Found",
              paste(
                "No floret exists for the selected area:", area_id,
                "\nThis area may not be present in the event tree."
              ),
              easyClose = TRUE,
              footer = modalButton("Dismiss")
            ))
            next
          }

          # Continue as normal...
          floret$nodes$nodeid <- floret$nodes$id
          floret$nodes$area   <- area_id
          floret$edges$area   <- area_id
          floret$nodes$id     <- floret$nodes$nodeid

          if (is.null(first_floret())) {
            first_floret(floret)
          }

          florets_list[[area_id]]       <- floret
          floret_colors_list[[area_id]] <- floret$nodes$color
        }


        if (length(florets_list) == 0) {
          showModal(modalDialog(
            title = "No Valid Florets",
            "None of the selected areas exist in the event tree.",
            easyClose = TRUE,
            footer = modalButton("Dismiss")
          ))
          first_floret(NULL)
          all_florets(NULL)
          return()
        }

        # Check colour consistency across areas
        if (length(florets_list) > 1) {
          all_colors_match <- all(
            sapply(floret_colors_list, function(x)
              identical(x, floret_colors_list[[1]]))
          )

          if (!all_colors_match) {
            showModal(modalDialog(
              title = "Inconsistent Floret Colouring",
              "The selected polygons have inconsistent floret colouring.",
              easyClose = TRUE,
              footer = modalButton("Close")
            ))
            first_floret(NULL)
            all_florets(NULL)
            selected_polygon(NULL)
            return()
          }
        }

        # --- COMBINE FLORETS (correct version) ---
        if (length(florets_list) > 0) {

          combined_nodes <- do.call(rbind, lapply(florets_list, function(f) f$nodes))
          combined_edges <- do.call(rbind, lapply(florets_list, function(f) f$edges))

          # DO NOT remove duplicates — different areas share node IDs

          # Wrap as proper event_tree object
          all_florets(structure(
            list(
              nodes = combined_nodes,
              edges = combined_edges
            ),
            class = class(visoutputdata)
          ))
        } else {
          all_florets(NULL)
        }

        # Show modal with first floret
        floret <- first_floret()
        if (is.null(floret)) {
          showModal(modalDialog(
            title = "Error",
            "No floret exists for the selected node(s).",
            easyClose = TRUE,
            footer = modalButton("Close")
          ))
          return()
        }

        output$dynamic_vis <- renderVisNetwork({
          lt <- if (input$toggleLabels) "names" else "both"

          plot(floret, label_type = lt) %>%
            visEvents(
              selectNode = "function(params) {
        Shiny.setInputValue(
          'dynamic_vis_selectedNodes',
          this.getSelectedNodes(),
          {priority: 'event'}
        );
      }",

              deselectNode = "function(params) {
        Shiny.setInputValue(
          'dynamic_vis_selectedNodes',
          this.getSelectedNodes(),
          {priority: 'event'}
        );
      }"
            )
        })

        showModal(modalDialog(
          title = paste("Floret(s) starting from:", toString(clicked_ids)),
          pickerInput("existing_colors", "Choose Existing Color:",
                      choices = c("", stored_colors$all_colors),
                      choicesOpt = list(
                        style = paste0("background:", c("#FFFFFF", stored_colors$all_colors), ";")
                      )
          ),
          colourpicker::colourInput("modal_nodeColor", "Choose Node Color", value = "#FFFFFF"),
          actionButton("colorSelectedModalNodes", "Colour Selected Nodes"),
          visNetworkOutput("dynamic_vis"),
          easyClose = FALSE,
          footer = tagList(
            actionButton("close_floret_modal", "Close")
          )
        ))
      })


      observeEvent(input$close_floret_modal, {


        # Reset variables and close the modal
        first_floret(NULL)
        all_florets(NULL)
        selected_polygon(NULL)
        removeModal()
      })


      stored_colors <- reactiveValues(all_colors = character(0))

      observe({
        # Ensure graph_data is available before accessing its color
        graph_data <- homicide_data()

        # Check if graph_data$nodes$color exists and has data
        if (!is.null(graph_data$nodes) && "color" %in% colnames(graph_data$nodes)) {
          # Extract unique colors from graph_data$nodes$color and update stored_colors
          stored_colors$all_colors <- unique(graph_data$nodes$color)
        }
      })

      get_matching_floret_nodes <- function(selected_nodes, combined_nodes, combined_edges) {

        # 1. Find the weapon-type labels for the selected nodes
        selected_labels <- unique(
          combined_edges$label1[combined_edges$to %in% selected_nodes]
        )

        if (length(selected_labels) == 0) return(selected_nodes)

        # 2. Find all nodes (across all areas) whose incoming edge
        #    has one of those weapon-type labels
        matching_nodes <- unique(
          combined_edges$to[combined_edges$label1 %in% selected_labels]
        )

        # 3. Return union of selected + matched
        unique(c(selected_nodes, matching_nodes))
      }


      observeEvent(input$colorSelectedModalNodes, {

        selected_floral_nodes <- input$dynamic_vis_selectedNodes
        if (is.null(selected_floral_nodes)) return()

        floret_obj <- first_floret()
        all_obj    <- all_florets()
        main_tree  <- current_tree()

        if (is.null(floret_obj) || is.null(all_obj)) return()

        # 1. Compute matching nodes across all areas
        all_selected_nodes <- get_matching_floret_nodes(
          selected_floral_nodes,
          all_obj$nodes,
          all_obj$edges
        )

        # 2. Determine colour
        selected_color <- if (input$existing_colors != "") {
          input$existing_colors
        } else {
          input$modal_nodeColor
        }

        # --- ⭐ SAFETY WRAPPER FOR update_node_colours() ---
        safe_update <- function(tree, nodes, col) {
          tryCatch(
            {
              update_node_colours(
                event_tree_obj = tree,
                node_groups    = list(nodes),
                colours        = col
              )
            },
            error = function(e) {
              showModal(modalDialog(
                title = "Colouring Error",
                paste(
                  "This colouring cannot be applied because:",
                  e$message
                ),
                easyClose = TRUE,
                footer = modalButton("Dismiss")
              ))
              return(NULL)
            }
          )
        }

        # 3. Colour floret
        floret_coloured <- safe_update(floret_obj, all_selected_nodes, selected_color)
        if (is.null(floret_coloured)) return()   # stop gracefully

        first_floret(floret_coloured)

        visNetworkProxy("dynamic_vis") %>%
          visUpdateNodes(nodes = floret_coloured$nodes)

        # 4. Colour main event tree
        updated_main <- safe_update(main_tree, all_selected_nodes, selected_color)
        if (is.null(updated_main)) return()

        current_tree(updated_main)

        visNetworkProxy("eventtree_network") %>%
          visUpdateNodes(nodes = updated_main$nodes)

        # 5. Update colour dropdown
        stored_colors$all_colors <- unique(
          c(stored_colors$all_colors, selected_color)
        )
        updateSelectInput(
          session,
          "existing_colors",
          choices = c("", stored_colors$all_colors),
          selected = ""
        )

        # 6. Clear modal selection
        visNetworkProxy("dynamic_vis") %>% visUnselectAll()
      })





      observe({
        if (input$viewcegmap == "Chain Event Graph and Map") {

          shinyjs::show("ceg_map")

          shinyjs::runjs('
      $("#ceg_col").removeClass("col-sm-12 col-md-12 col-lg-12");
      $("#ceg_col").addClass("col-sm-6 col-md-6 col-lg-6");
              $("#ceg_network").css({
    "width": "100%"
  });
    ')

        } else {

          shinyjs::hide("ceg_map")

          shinyjs::runjs('
      $("#ceg_col").removeClass("col-sm-6 col-md-6 col-lg-6");
      $("#ceg_col").addClass("col-sm-12 col-md-12 col-lg-12");
        $("#ceg_network").css({
    "width": "150%"
  });
    ')
        }
      })

      observeEvent(input$ceg_map_shape_click, {

        cat("\n================ CLICK EVENT ================\n")

        clicked_id <- input$ceg_map_shape_click$id
        cat("Clicked polygon ID:", clicked_id, "\n")
        req(clicked_id)

        visoutputdata <- structure(
          list(
            nodes = contracted_data()$nodes,
            edges = contracted_data()$edges,
            table = grouped_df3()
          ),
          class = "ceg"
        )

        cat("\n--- NODES ---\n")
        print(head(visoutputdata$nodes, 20))
        cat("Total nodes:", nrow(visoutputdata$nodes), "\n")

        cat("\n--- EDGES ---\n")
        print(head(visoutputdata$edges, 20))
        cat("Total edges:", nrow(visoutputdata$edges), "\n")

        # Check if clicked_id exists in edges$label1
        cat("\n--- CHECK: Does clicked_id appear in edges$label1? ---\n")
        match_edge <- visoutputdata$edges$label1 == clicked_id
        cat("Matches found:", sum(match_edge), "\n")

        if (sum(match_edge) > 0) {
          cat("Matching edges:\n")
          print(visoutputdata$edges[match_edge, ])
        } else {
          cat("NO edges match clicked_id. This is the root cause.\n")
        }

        # Use the edge label directly
        start_label <- clicked_id
        cat("\nStart label passed to compute_reduced_ceg():", start_label, "\n")

        # Try running your package function
        cat("\n--- Running compute_reduced_ceg() ---\n")
        reduced_ceg <- tryCatch({
          compute_reduced_ceg(visoutputdata, start_labels = start_label)
        }, error = function(e) {
          cat("compute_reduced_ceg ERROR:\n")
          print(e)
          return(NULL)
        })

        cat("\n--- reduced_ceg RESULT ---\n")
        if (is.null(reduced_ceg)) {
          cat("reduced_ceg is NULL\n")
        } else {
          cat("reduced_ceg nodes:", nrow(reduced_ceg$nodes), "\n")
          cat("reduced_ceg edges:", nrow(reduced_ceg$edges), "\n")

          cat("\nreduced_ceg nodes preview:\n")
          print(head(reduced_ceg$nodes, 20))

          cat("\nreduced_ceg edges preview:\n")
          print(head(reduced_ceg$edges, 20))
        }

        # If empty → modal
        if (is.null(reduced_ceg) || nrow(reduced_ceg$nodes) == 0 || nrow(reduced_ceg$edges) == 0) {
          showModal(modalDialog(
            title = "Error",
            paste("No reduced CEG exists for:", clicked_id),
            easyClose = TRUE,
            footer = modalButton("Close")
          ))
          cat("Modal shown: No reduced CEG exists.\n")
          return()
        }

        output$dynamic_vis2 <- renderVisNetwork({
          lt <- if (input$showposteriormean) "posterior_mean" else "posterior"


          plot(reduced_ceg, label_type = lt, level_separation = input$levelSeparation) %>%
            visEvents(
              selectNode = "function(params) {
        Shiny.setInputValue(
          'dynamic_vis_selectedNodes',
          this.getSelectedNodes(),
          {priority: 'event'}
        );
      }",

              deselectNode = "function(params) {
        Shiny.setInputValue(
          'dynamic_vis_selectedNodes',
          this.getSelectedNodes(),
          {priority: 'event'}
        );
      }"
            )
        })


        showModal(modalDialog(
          title = paste("Reduced CEG starting from:", clicked_id),
          visNetworkOutput("dynamic_vis2"),
          easyClose = TRUE,
          footer = modalButton("Close")
        ))

        cat("Modal shown: Reduced CEG displayed.\n")
        cat("=============== END CLICK EVENT ================\n\n")

      })







    }


    shinyApp(ui, server)
}
