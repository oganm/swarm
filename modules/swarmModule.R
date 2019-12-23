monsters$`Animate Objects` =
    list(actions = 
             list(
                 'Tiny' = list(
                     attack_bonus = 8,
                     damage_dice = '1d4',
                     damage_bonus = 5,
                     default_count = 10,
                     saves = c(Str = -3, Dex = 4, Con = 0, Int = -4, Wis = -4, Cha = -5)),
                 'Small' = list(
                     attack_bonus = 6,
                     damage_dice = '1d8',
                     damage_bonus = 2,
                     default_count = 10,
                     saves = c(Str = -2, Dex = 3, Con = 0, Int = -4, Wis = -4, Cha = -5)),
                 "Medium" = list(
                     attack_bonus = 5,
                     damage_dice = '2d6',
                     damage_bonus = 1,
                     default_count = 5,
                     saves = c(Str = 0, Dex = 2, Con = 0, Int = -4, Wis = -4, Cha = -5)),
                 "Large" = list(
                     attack_bonus = 6,
                     damage_dice = '2d10',
                     damage_bonus = 2,
                     default_count = 2,
                     saves = c(Str = 2, Dex = 0, Con = 0, Int = -4, Wis = -4, Cha = -5)),
                 "Huge" = list(
                     damage_bonus = 8,
                     damage_dice = '2d12',
                     damage_bonus = 4,
                     saves = c(Str = 4, Dex = -2, Con = 0, Int = -4, Wis = -4, Cha = -5))
                 )
             )


seq_along(monsters) %>% lapply(function(i){
    validAttacks = monsteR:::attackable(monsters[[i]]$actions)
    validAttacks %<>% gsub('/','-',.)
    if(length(validAttacks)>0){
        return(paste(names(monsters)[i],validAttacks,sep = '/'))
    } else{
        return(NULL)
    }
}) %>% unlist %>% sort -> monsterAttacks

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
                           numericInput(ns('attackBonus'),'Attack bonus', value = 0),
                           numericInput(ns('damageBonus'),'Damage bonus', value = 0))
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
                    ),
                fluidRow(
                    column(6,
                           numericInput(ns('DC'), 'Save DC',value = 10,min = 0)),
                    column(6,
                           br(),
                           actionButton(ns('save'),'Save'),
                           dropdownButton(strong('Saves'),
                                          checkboxInput(ns('detailed'),label = 'Return details?',value = TRUE),
                                          numericInput(ns('str-save'), 'Str',value = 0,min = 0),
                                          numericInput(ns('dex-save'), 'Dex',value = 0,min = 0),
                                          numericInput(ns('con-save'), 'Con',value = 0,min = 0),
                                          numericInput(ns('int-save'), 'Int',value = 0,min = 0),
                                          numericInput(ns('wis-save'), 'Wis',value = 0,min = 0),
                                          numericInput(ns('cha-save'), 'Cha',value = 0,min = 0),
                                          size = 'xs', icon = icon('ellipsis-h'),
                                          width = '30px',
                                          right = FALSE,
                                          inline = TRUE))
                )
            )
        )
    )
}

swarm = function(input,output,session,swarmLimit = 1000){
    
    finalOut = reactiveVal()
    
    observeEvent(input$monster,{
        if(input$monster !=''){
            
            monster_attack = input$monster %>% strsplit('/') %>% {.[[1]]}
            
            attack = monsters[[monster_attack[1]]]$actions[[monster_attack[2]]]
            if(is.null(attack$damage_bonus)){
                attack$damage_bonus = 0
            }
            if(is.null(attack$attack_bonus)){
                attack$attack_bonus = 0
            }

            updateTextInput(session,'name',value= glue::glue('{monster_attack[1]} ({monster_attack[2]})'))
            updateTextInput(session,'damageDice',value =attack$damage_dice)
            updateNumericInput(session,'attackBonus',value=attack$attack_bonus)
            updateNumericInput(session,'damageBonus',value= attack$damage_bonus)
            

            updateNumericInput(session,'str-save',value= unname(monsters[[monster_attack[1]]]$saves['Str']))
            updateNumericInput(session,'dex-save',value= unname(monsters[[monster_attack[1]]]$saves['Dex']))
            updateNumericInput(session,'con-save',value= unname(monsters[[monster_attack[1]]]$saves['Con']))
            updateNumericInput(session,'int-save',value= unname(monsters[[monster_attack[1]]]$saves['Int']))
            updateNumericInput(session,'wis-save',value= unname(monsters[[monster_attack[1]]]$saves['Wis']))
            updateNumericInput(session,'cha-save',value= unname(monsters[[monster_attack[1]]]$saves['Cha']))
            
            
            if(!is.null(attack$default_count)){
                updateNumericInput(session,'count',value=attack$default_count)
            }
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
                     buttonCount = as.integer(input$attack) + as.integer(input$save))
            }
 
        })
    })
    
    observeEvent(out(),{
        finalOut(out())
    })
    
    out2 = reactive({
        input$save
        isolate({
            if(!is.null(input$save) && input$save>0){
                

                rolls = roll(glue::glue('{input$count}d20'),returnRolls = TRUE)[[1]]$dice
                
                
                return(list(saves = rolls,
                            swarmName = input$name,
                            passes = list(
                                Str = which((rolls + input$`str-save`) >= input$DC),
                                Dex = which((rolls + input$`dex-save`) >= input$DC),
                                Con = which((rolls + input$`con-save`) >= input$DC),
                                Int = which((rolls + input$`int-save`) >= input$DC),
                                Wis = which((rolls + input$`wis-save`) >= input$DC),
                                Cha = which((rolls + input$`cha-save`) >= input$DC)
                            ),
                            detailed = input$detailed,
                            buttonCount = as.integer(input$attack) + as.integer(input$save)))

            }
           
        })
    })
    
    observeEvent(out2(),{

        finalOut(out2())
    })
    
    return(finalOut)
}