#SVScarpino

shinyServer(function(input, output) {
  #generate a threat via openAI by comvining user inputs
  gen_threat_response <- eventReactive(input$Submit_threat, {
    ai <- chat_openai(
      system_prompt = input$text_system_prompt,
      turns = NULL,
      base_url = "https://api.openai.com/v1",
      model = input$ai_model,
      seed = NULL,
      api_args = list(),
      echo = c("none", "text", "all")
    )
    
    health_prompt <- make_health_prompt(country = input$text_location) #gets health indicators
    dev_prompt <- make_dev_prompt(country = input$text_location) #gets development indicators
    
    prompt_raw <- make_prompt(input$text_prompt) #splits input prompt
    
    prompt <- paste0(prompt_raw[["start_pathogen"]], input$text_pathogen, prompt_raw[["severity"]], input$text_severity, prompt_raw[["geopolitical"]], input$text_location, prompt_raw[["location"]], health_prompt, prompt_raw[["health"]], dev_prompt, prompt_raw[["dev_ind_end"]]) #combines input prompt with input params and indicators
    
    if(input$ai_rag == "No"){
      response <- ai$chat(prompt)
    }
    if(input$ai_rag == "Yes"){
      thread = client$beta$threads$create()
      message = client$beta$threads$messages$create(
        thread_id= thread$id, role="user", content=prompt
      )
      run = client$beta$threads$runs$create_and_poll(
        thread_id=thread$id,
        assistant_id=assistant$id,
        model = input$ai_model
      )
      if(run$status == "completed"){
        messages_raw = client$beta$threads$messages$list(
          thread_id=thread$id
        )
        messages <- as.character(messages_raw)
        response <- format_rag_response(messages)
      }else{
        stop("RAG process failed. Check logs")
      }
    }
    
    ontology <- get_ontology(response)
    list("response" = response, "ontology" = ontology)
  })
  
  #promed lookup
  scen_gen_promed <- eventReactive(input$Submit_promed, { 
    text_prompt <- URLencode(input$text_promed)
    url_use <- paste0(url_kg, "alerts/", text_prompt)
    tmp <- tempfile()
    download.file(url_use, tmp)
    readLines(tmp)
    
  })
  
  #gets ontology data
  scen_gen_ontology <- eventReactive(input$Submit_ontology, { 
    get_ontology(input$text_ontology)
  })
  
  #grabs a similar promed alert, currently not in use
  scen_gen_ontology_promed <- eventReactive(input$Submit_ontology, { 
    results <- scen_gen_ontology()
    text_prompt <- URLencode(results$promed)
    url_use <- paste0(url_kg, "alerts/", text_prompt)
    tmp <- tempfile()
    download.file(url_use, tmp)
    readLines(tmp)
  })
  
  #does a pupmed search
  scen_gen_pubmed <- eventReactive(input$Submit_pubmed, { 
    text_prompt <- gsub(pattern = "[,]", replacement = " +", input$text_pubmed)
    url_raw <- URLencode(text_prompt)
    url_use <- paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=", url_raw)
    tmp <- tempfile()
    download.file(url_use, tmp)
    
    hits <- readLines(tmp)
    tophits <- unlist(lapply(X = hits[c(4,6,8,10,20)], FUN = function(x) substr(x = x, start = 5, 12)))
    
    tophits <- na.omit(tophits)
    
    if(length(tophits) == 0){
      out <- data.frame(NA, NA, NA, NA, NA ,NA)
      colnames(out) <- c("PMID","title", "year",  "author", "journal", "doi")
      return(out)
    }
    
    if(length(tophits) > 10){
      tophits <- tophits[1:10]
    }
  
    counter <- 0
    for(i in 1:length(tophits)){
      pmid <- tophits[i]
      cit.i <- ReadPubMed(query = pmid, database = "PubMed")
      cit.df.raw.i <- as.data.frame(cit.i)
      cit.df.i <- data.frame(pmid, cit.df.raw.i[,c("title", "year",  "author", "journal", "doi")])
      if(counter > 0){
        citations <- rbind(citations, cit.df.i)
      }else{
        citations <- cit.df.i
      }
      counter <- counter + 1
      Sys.sleep(time = 3) #have to pause here because there's a rate limit on the API
    }
    citations
  })
  
  #does a literature via knowledge graph, currently not in use
  lit_search <- eventReactive(input$Submit_pubmed, { 
    results <- get_ontology(input$text_pubmed)
    MESHS <- paste(results$annotations$`CURIE ID`, collapse = ",")
    url_raw <- URLencode(MESHS)
    url_use <- paste0("https://outbreak-kg.indra.bio/v1/find_literature?mesh_ids=", url_raw)
    tmp <- tempfile()
    options(timeout=120)
    output <- download.file(url_use, tmp)
  })
  
  #loads excel file for bulk assessment
  bulk_file_data <- reactive({
    req(input$bulk_data_excel_file)
    data <- read_excel(input$bulk_data_excel_file$datapath)
    data
  })
  
  bulk_data <- eventReactive(input$bulk_action, {
    data <- bulk_file_data()
    withProgress(message = 'Processing', value = 0, {
      loop_var <- input$num_rows_bulk[1]:input$num_rows_bulk[2]
      n <- length(loop_var)
      counter <- 0
      for(i in loop_var){
        results.i <- get_ontology(data[i,input$use_cols_bulk])
        
        max_i <- max(unlist(lapply(list(results.i$realism_cat, results.i$promed, results.i$annotations, results.i$realism), nrow)), na.rm = TRUE)
        
        ID.i <- rep(as.character(data[i,input$use_id_bulk]), max_i)
        realism_cat.i <- rep(results.i$realism_cat, max_i)
        promed.i <- rep(results.i$promed, max_i)
        annots.i <- results.i$annotations
        realism_mat.i <- results.i$realism
        if(nrow(annots.i) < max_i){
          while(nrow(annots.i) < max_i){
            row.while <- matrix(data = rep(NA, ncol(annots.i)), ncol =  ncol(annots.i))
            colnames(row.while) <- colnames(annots.i)
            row.while <- as.data.frame(row.while)
            annots.i <- rbind(annots.i, row.while)
          }
        }
        
        if(nrow(realism_mat.i) < max_i){
          while(nrow(realism_mat.i) < max_i){
            row.while <- matrix(data = rep(NA, ncol(realism_mat.i)), ncol =  ncol(realism_mat.i))
            colnames(row.while) <- colnames(realism_mat.i)
            row.while <- as.data.frame(row.while)
            realism_mat.i <- rbind(realism_mat.i, row.while)
          }
        }
 
        results_out.i <- data.frame(ID.i, realism_cat.i, promed.i, annots.i, realism_mat.i)
        colnames(results_out.i) <- c("Application ID", "Realism Category", "Similar Promed", "Named entity", "Entity type", "CURIE.ID", "Realism entity 1", "Realism entity 2", "Realism pair score")
        if(counter == 0){
          results <- results_out.i
        }else{
          results <- rbind(results, results_out.i)
        }
        counter <- counter + 1
        incProgress(1/n, detail = paste("row", i))
      }
    })
    results
  })
  
  #results for threat gen
  output$results_text_threat <- renderText({
    results_raw <- gen_threat_response()
    response <- results_raw$response
    realism <- results_raw$ontology$realism_cat
    if(input$ai_rag == "No"){
      response_html <- HTML(mark_html(response, options = "-standalone"))
    }
    if(input$ai_rag == "Yes"){
      response_pretty <- gsub(pattern = "\\n", replacement = "\n", x = response, fixed = TRUE)
      response_html <- HTML(mark_html(response_pretty, options = "-standalone"))
    }
    results <- paste0(h3(paste0("Overall realism is ",realism)), response_html)
    results
  })
  
  #realism results for threat gen
  output$results_text_threat_realism <- renderTable({
    realism_raw <- gen_threat_response()$ontology
    realism_raw$realism
  })
  
  #ontology results
  output$results_text_ontology_realism_assess<- renderText({
    results <- scen_gen_ontology()
    realism <- results$realism_cat
    out <- paste0("Overall realism is ",realism)
    paste0(h3(out))
  })
  
  #ontology results named entities
  output$results_text_ontology <- renderTable({
    results <- scen_gen_ontology()
    results$annotations
  })
  
  #ontology results realism table
  output$results_text_ontology_realism <- renderTable({
    results <- scen_gen_ontology()
    results$realism
  })
  
  #geo indicators from ontology
  output$results_geo_ontology <- renderTable({
    results <- scen_gen_ontology()
    results$geo_info$geo
  })
  
  #outputs promed text
  output$results_text_promed <- renderText({
    results_raw <- scen_gen_promed()
    results <- HTML(mark_html(results_raw, options = "-standalone"))
    results
  })
  
  #outputs pubmed search
  output$results_text_pubmed <- renderTable({
    results <- scen_gen_pubmed()
    results
  })
  
  #bulk data output
  output$bulk_output <- renderDT({
    bulk_data()
  })

  output$dl_bulk <- downloadHandler(
    filename = function() { paste0(as.numeric(Sys.time()), "_bulk_data.xlsx")},
    content = function(file) {write_xlsx(bulk_data(), path = file)}
  )
  
  output$ui.action.rows <- renderUI({
    if (is.null(bulk_file_data())) return()
    sliderInput(inputId = "num_rows_bulk", label = "Rows to process", min = 1, max = nrow(bulk_file_data()), value = c(1,10), step = 1)
  })
  
  output$ui.action.cols <- renderUI({
    if (is.null(bulk_file_data())) return()
    selectInput(inputId = "use_cols_bulk", label = "Column to analyze", choices = colnames(bulk_file_data()), selected = colnames(bulk_file_data())[2])
  })
  
  output$ui.action.id <- renderUI({
    if (is.null(bulk_file_data())) return()
    selectInput(inputId = "use_id_bulk", label = "Column with ID", choices = colnames(bulk_file_data()), selected = colnames(bulk_file_data())[1])
  })
  
  output$ui.action.run <- renderUI({
    if (is.null(bulk_file_data())) return()
    actionButton("bulk_action", "Run analysis")
  })
})
