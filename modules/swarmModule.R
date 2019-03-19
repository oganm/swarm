swarmUI = function(id){
    ns = NS(id)
    tagList(
        div(id = id,
            wellPanel(
                textInput(ns('name'),'Swarm name',value = paste(id)),
                shiny::selectInput(ns('monster'),label = 'Load SRD monster',choices = c('',names(monsteR::monsters))),
                fluidRow(
                    column(6,
                           numericInput(ns('count'), 'Swarm size',min = 1,value = 1),
                           textInput(ns('damageDice'),'Damage dice',placeholder = 'eg. 1d6+3')
                    ),
                    column(6,
                           numericInput(ns('attackBonus'),'Attack bonus', min = 0, value = 0),
                           numericInput(ns('damageBonus'),'Damage bonus',min = 0, value = 0))
                ),
                shinyWidgets::radioGroupButtons(inputId = ns('advantage'),
                                                choices = c('DisAdv','Norm','Adv'),
                                                selected = 'Norm',
                                                status = "primary"),
                fluidRow(
                    column(6,
                           numericInput(ns('AC'), 'Target AC',value = 10,min = 0)),
                    column(6,
                           actionButton(ns('attack'),'Attack!'))
                    )
            )
        )
    )
}

swarm = function(input,output,session){
    out = reactive({
        input$attack
        isolate({
            if(!is.null(input$attack) && input$attack>0){
                validate(
                    need (input$damageDice != "","Please provide a damage dice")
                )
                hits = diceSyntax::swarm(
                    AC = input$AC,
                    count = input$count,
                    damageDice = input$damageDice,
                    attackBonus = input$attackBonus,
                    damageBonus = input$damageBonus,
                    advantage = "N")
                
                list(hits = hits,
                     swarmName = input$name,
                     buttonCount = as.integer(input$attack))
            }
 
        })
    })
    
    return(out)
}