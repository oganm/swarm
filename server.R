


shinyServer(function(input, output, session) {
    swarmCount = reactiveVal(0)
    
    swarmOuts = reactiveValues()
    previousButtonCounts = reactiveVal(list())
    
    observeEvent(input$addSwarm,{
        swarmCount(swarmCount()+1)
        insertUI(
            selector = "#addSwarm",
            where = 'beforeBegin',
            ui = swarmUI(paste0('swarm',swarmCount()))
        )
        
        swarmOuts[[as.character(swarmCount())]] = callModule(swarm,id = paste0('swarm',swarmCount()))
    })
    
    
    observeEvent(input$removeSwarm,{
        removeUI(
            selector = paste0("#swarm",swarmCount())
        )
        swarmCount(swarmCount()-1)
    })
    
    # get all swarm outputs
    swarmOut = reactive({
        out = seq_len(swarmCount()) %>% lapply(function(i){
            swarmOuts[[as.character(i)]]()
        })    
    })
    
    rolledSwarm = reactive({
        swarms = swarmOut()
        isolate({
            browser()
            buttonCounts = swarms %>% purrr::map('buttonCount')
            names(buttonCounts) = seq_along(buttonCounts)
            previousButtons = previousButtonCounts()
            whichButton = seq_along(buttonCounts) %>% sapply(function(i){
                i = as.character(i)
                (!is.null(buttonCounts[[i]]) && !is.null(previousButtons[[i]]) && buttonCounts[[i]]>previousButtons[[i]]) || (!is.null(buttonCounts[[i]]) && is.null(previousButtons[[i]]) && buttonCounts[[i]]>0)
            }) %>% as.logical()
            
            previousButtonCounts(buttonCounts)
            print(which(whichButton))
            # print(swarms)
            if(sum(whichButton) == 1){
                return(swarms[[which(whichButton)]])
            } else{
                return(NULL)
            }
        })

    })
    
    # observe({
    #     swarmData = rolledSwarm()
    #     browser()
    #     
    #     
    #     
    # })
    
    
    finalOutput = reactive({
        swarmData = rolledSwarm()
        if(!is.null(swarmData) && length(swarmData$hits)>= 1){
            text = glue::glue('{swarmData$swarmName} attacks:\n',
                       length(swarmData$hits),' members hit for\n',
                       paste(swarmData$hits,collapse= ', '),' damage\n',
                       'A total of {sum(swarmData$hits)}')
        } else if(!is.null(swarmData) && length(swarmData$hits == 0)){
            text = glue::glue("{swarmData$swarmName} attacks:\nEveryone missed")
        } else{
            text = ''
        }
        
        return(text)
    })

    
    callModule(console,'console',consoleLength = 60,finalOutput)
    
})
