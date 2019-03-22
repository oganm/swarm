


shinyServer(function(input, output, session) {
    swarmCount = reactiveVal(0)
    
    swarmOuts = reactiveValues()
    previousButtonCounts = reactiveVal(list())
    initialText = reactiveVal('Written by Ogan Mancarci\nSource code at github.com/oganm/swarm\nMonster data is released by Wizards of the Coast under open game license\nData is translated into JSON format by Walter Kammerer\n ')
    # observe instead of observeEvent because I want this to run
    # once when the app initalizes

    observe({
        input$addSwarm
        isolate({
            swarmCount(swarmCount()+1)
            insertUI(
                selector = "#addSwarm",
                where = 'beforeBegin',
                ui = swarmUI(paste0('swarm',swarmCount()))
            )
            
            swarmOuts[[as.character(swarmCount())]] = callModule(swarm,id = paste0('swarm',swarmCount()),swarmLimit = swarmLimit)
        })
        shinyjs::show('main_content')
        
        
    })
    
    
    observeEvent(input$removeSwarm,{
        if(swarmCount() > 1){
            removeUI(
                selector = paste0("#swarm",swarmCount())
            )
            swarmCount(swarmCount()-1)
        }
    })
    
    # get all swarm outputs
    swarmOut = reactive({
        out = seq_len(swarmCount()) %>% lapply(function(i){
            swarmOuts[[as.character(i)]]()
        })    
    })
    
    # this is there to prevent returning anything if 
    # a new swarm is added instead of an attack button press
    swarmButtonCount = reactiveVal(0)
    
    # decide which attack button is pressed and send the correct output 
    # for processing
    rolledSwarm = reactive({
        swarms = swarmOut()
        isolate({
            # if this is triggered because add swarm button is pressed, ignore
            if(input$addSwarm == swarmButtonCount()){
                buttonCounts = swarms %>% purrr::map('buttonCount')
                names(buttonCounts) = seq_along(buttonCounts)
                previousButtons = previousButtonCounts()
                whichButton = seq_along(buttonCounts) %>% sapply(function(i){
                    i = as.character(i)
                    (!is.null(buttonCounts[[i]]) && !is.null(previousButtons[[i]]) && buttonCounts[[i]]>previousButtons[[i]]) || (!is.null(buttonCounts[[i]]) && is.null(previousButtons[[i]]) && buttonCounts[[i]]>0)
                }) %>% as.logical()
                
                previousButtonCounts(buttonCounts)
                # print(which(whichButton))
                # print(swarms)
                if(sum(whichButton) == 1){
                    return(swarms[[which(whichButton)]])
                } else{
                    return(NULL)
                }
            } else{
                swarmButtonCount(input$addSwarm)
                return(NULL)
            }
        })

    })
    
    # process the output to human readable format
    finalOutput = reactive({
        # this assignment must be above here. otherwise since initial return
        # is the initial text the rest is ignored. It seems like dependency
        # tree is created after a run. premature endings prevent some dependencies from
        # getting registered
        swarmData = rolledSwarm()
        isolate({
            if(initialText()!=''){
                text = initialText()
                initialText('')
                return(text)
            }
        })
        
        if(!is.null(swarmData) && length(swarmData$hits) == 1 && swarmData$hits == -1){
            text = paste0("Maximum allowed swarm size is ",swarmLimit,"\n ")
        } else if(!is.null(swarmData) && length(swarmData$hits)>= 1){
            text = glue::glue('{swarmData$swarmName} attacks:\n',
                       length(swarmData$hits),' members hit for\n',
                       paste(swarmData$hits,collapse= ', '),' damage\n',
                       'A total of {sum(swarmData$hits)}\n \n ')
        } else if(!is.null(swarmData) && length(swarmData$hits) == 0){
            text = glue::glue("{swarmData$swarmName} attacks:\nEveryone missed\n \n ")
        } else{
            text = ''
        }
        
        return(text)
    })

    
    callModule(console,'console',consoleLength = 100,finalOutput)
    
})
