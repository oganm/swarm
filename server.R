


shinyServer(function(input, output, session) {
    swarmCount = reactiveVal(0)
    
    swarmOuts = reactiveValues()
    
    observeEvent(input$addSwarm,{

        swarmCount(swarmCount()+1)
        insertUI(
            selector = "#addSwarm",
            where = 'beforeBegin',
            ui = swarmUI(paste0('swarm',swarmCount()))
        )
    }, priority = 0)
    
    observeEvent(input$addSwarm,{
        swarmOuts[[as.character(swarmCount())]] = callModule(swarm,id = paste0('swarm',swarmCount()))
    },priority = -1)
    
    
    observeEvent(input$removeSwarm,{
        removeUI(
            selector = paste0("#swarm",swarmCount())
        )
        swarmCount(swarmCount()-1)
    })
    
    observe({
        print(swarmOuts[['1']])
    })
    
})
