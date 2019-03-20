monsterAttacks = monsters %>% purrr::map('actions') %>% purrr::map(monsteR:::attackable) %>% unlist %>% unname

seq_along(monsters) %>% lapply(function(i){
    validAttacks = monsteR:::attackable(monsters[[i]]$actions)
    validAttacks %<>% gsub('/','-',.)
    if(length(validAttacks)>0){
        return(paste(names(monsters)[i],validAttacks,sep = '/'))
    } else{
        return(NULL)
    }
}) %>% unlist -> monsterAttacks

swarmUI = function(id){
    ns = NS(id)
    tagList(
        div(id = id,
            wellPanel(
                fluidRow(
                    column(6,
                           textInput(ns('name'),'Swarm name',value = paste(id)),
                           numericInput(ns('count'), 'Swarm size',min = 1,value = 1),
                           textInput(ns('damageDice'),'Damage dice',value = '1d6',placeholder = 'eg. 1d6+3')
                    ),
                    column(6,
                           shiny::selectInput(ns('monster'),label = 'Load SRD monster',choices = c('',monsterAttacks)),
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
                           br(),
                           actionButton(ns('attack'),'Attack!'))
                    )
            )
        )
    )
}

swarm = function(input,output,session){
    
    observeEvent(input$monster,{
        if(input$monster !=''){
            
            monster_attack = input$monster %>% strsplit('/') %>% {.[[1]]}
            
            attack = monsters[[monster_attack[1]]]$actions[[monster_attack[2]]]

            updateTextInput(session,'name',value= monster_attack[1])
            updateNumericInput(session,'damageDice',value =attack$damage_dice)
            updateNumericInput(session,'attackBonus',value=attack$attack_bonus)
            updateNumericInput(session,'damageBonus',value= attack$damage_bonus)
        }
    })
    
    out = reactive({
        input$attack
        isolate({
            if(!is.null(input$attack) && input$attack>0){
                validate(
                    need (input$damageDice != "","Please provide a damage dice")
                )
                
                adv = switch(input$advantage,
                             "DisAdv" = "D",
                             "Norm" = "N",
                             "Adv"= "A")
                
                if(input$count > swarmLimit){
                    return(list(hits = -1,
                         swarmName = input$name,
                         buttonCount = as.integer(input$attack)))
                }
                
                hits = diceSyntax::swarm(
                    AC = input$AC,
                    count = input$count,
                    damageDice = input$damageDice,
                    attackBonus = input$attackBonus,
                    damageBonus = input$damageBonus,
                    advantage = adv)
                
                list(hits = hits,
                     swarmName = input$name,
                     buttonCount = as.integer(input$attack))
            }
 
        })
    })
    
    return(out)
}