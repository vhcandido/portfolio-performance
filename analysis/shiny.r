library(d3heatmap)
library(shiny)
library(plot3D)

load('../results/RSI/EURUSD_days_.rda')
z.range <- range(bt$strat.balance, na.rm = TRUE)

ui <- shinyUI(fluidPage(
	h1("Strategy return heatmap"),
	hr(),
	sidebarPanel(width = 12,
		checkboxInput("cluster", "Apply clustering", value = FALSE),
		checkboxInput("d3heatmap", "D3 Heatmap", value = FALSE),
		checkboxInput("image2D", "2D Image", value = FALSE),
		
		sliderInput("x", "Param1",
			min = metadata$params$values[[1]][1],
			max = tail(metadata$params$values[[1]],1),
			value = range(metadata$params$values[[1]])
		),
		sliderInput("y", "Param2",
			min = metadata$params$values[[2]][1],
			max = tail(metadata$params$values[[2]],1),
			value = range(metadata$params$values[[2]])
		),
		
		h2(textOutput('chosen.date')),
		sliderInput("date", "Date/Time:",
			min = bt$dates[1],
			max = tail(bt$dates, 1),
			value = tail(bt$dates, 1),
			timeFormat="%Y-%m-%d"
		)
	),
	hr(),
	fluidRow(
		column(5, offset = 1,
			d3heatmapOutput("heatmap", width = '40vw', height = '40vw')
		),
		column(6,
			plotOutput('image', width = '40vw', height = '40vw')
		)
	)
	
))

server <- shinyServer(function(input, output, session) {
	
	heatmap <- reactive({
		if(!input$d3heatmap) return(NULL)
		d3heatmap(
			bt$strat.balance[
				which(bt$dates==input$date),
				input$x[1]:input$x[2],
				input$y[1]:input$y[2]],
			show_grid = F,
			labRow = input$x[1]:input$x[2],
			labCol = input$y[1]:input$y[2],
			colors = colorRamp(c('blue', 'cyan', 'green', 'yellow', 'red')),
			dendrogram = if (input$cluster) "both" else "none"
		)
	})
	
	image <- reactive({
		if(!input$image2D) return(NULL)
		image2D(
			x = input$x[1]:input$x[2],
			y = input$y[1]:input$y[2],
			z = bt$strat.balance[
				which(bt$dates==input$date),
				input$x[1]:input$x[2],
				input$y[1]:input$y[2]],
			zlim = z.range
		)
	})
	
	output$chosen.date <- renderText(paste0(
		format(as.Date(input$date), '%d %b %Y'),
		' - ', weekdays(as.Date(input$date))
	))
	output$heatmap <- renderD3heatmap({ heatmap() })
	output$image <- renderPlot({ image() })
})

shinyApp(ui, server)
