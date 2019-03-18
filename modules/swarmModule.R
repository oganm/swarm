swarmUI = function(id){
    ns = NS(id)
    tagList(
        div(id = id,
            wellPanel(
                numericInput(ns('AC'), 'Target AC',value = 10,min = 0)
            )
        )
    )
}

swarm = function(input,output,session){
    observe({
        input$AC
        print('inmodule change')
    })
    
    output = reactive({
        input$AC
    })
    
    return(output)
}