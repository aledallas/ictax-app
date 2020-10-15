library(shiny)
library(DT)
library(tidyverse)
library(lubridate)

fund <- readRDS("data/fund.rds")
yearend <- readRDS("data/yearend.rds")
div <- readRDS("data/dividends.rds")


# Define server logic
function(input, output, session) {
    
    
    # Import and creation of detailed df ------------------------------------------
    df_detail <- reactive({
        
        # Import Degiro transactions data -----------------------------------------
        req(input$file1)
        degiro <- read_csv(input$file1$datapath) %>% 
            rename(cur_price = "X7",
                   cur_local_value = "X9",
                   cur_value = "X11",
                   cur_fee = "X14",
                   cur_total = "X16")
        
        # positive feedback after upload
        # feedbackSuccess(
        #     "file1",
        #     show,
        #     text = NULL,
        #     color = "#5cb85c",
        #     icon = shiny::icon("ok", lib = "glyphicon"),
        #     session = shiny::getDefaultReactiveDomain()
        # )
        
        # Cumulated quantity per instruments as for degiro transactions -----------
        
        # clean and prepare
        trans <- degiro %>% 
            rename_with(tolower) %>% 
            mutate(trans_date = dmy(date)) %>% 
            select(trans_date, time, product, isin, quantity)
        
        # cumulated quantity
        trans <- trans %>% 
            arrange(isin, trans_date) %>% 
            group_by(isin) %>% 
            mutate(cum_quantity = cumsum(quantity)) 
        
        
        # Combine with dividends data ---------------------------------------------
        
        # only used instruments
        isin_list <- trans %>% 
            filter(year(trans_date) == "2019") %>% 
            select(isin) %>% 
            unique()
        
        # filter dividends
        applied_div <- inner_join(div, isin_list, by = "isin")
        
        # merge transactions and dividends
        trans_per_date <- full_join(applied_div, trans, by = "isin") %>% 
            mutate(diff = div_date - trans_date) %>% 
            filter(diff > 0)    # exclude transactions happening on the same day of the dividends
            # filter(year(trans_date) == "2019")
        
        div_trans <- trans_per_date %>% 
            group_by(isin, div_date) %>% 
            mutate(min_diff = min(diff)) %>% 
            filter(diff == min_diff) %>% 
            ungroup()
        
        # TODO for a next release, keep dividends with q = 0 to show them in a detail page
        
        
        # Prepare data for the report ---------------------------------------------
        
        # fund info
        rep_fund <- inner_join(fund, isin_list, by = "isin") %>% 
            mutate(fund_name = if_else(is.na(institutionappendix), institutionname, institutionappendix),
                   fund_type = if_else(securitytype == "FUND.ACCUMULATION", "ACC",
                                       if_else(securitytype == "FUND.DISTRIBUTION", "DIS",
                                               if_else(securitytype == "FUND.REALESTATE", "RE", NA_character_)))) %>% 
            select(valornumber, isin, fund_name, 
                   # country, 
                   fund_currency, fund_type)
        
        # eoy quantity
        rep_trans_q <- trans %>% 
            filter(year(trans_date) == "2019") %>% 
            group_by(isin) %>%
            summarise(eoy_q = max(cum_quantity))
        
        # tax value
        rep_taxvalue <- inner_join(yearend, isin_list, by = "isin") %>% 
            # rename(valornumber = "valorNumber") %>% 
            select(isin, taxvaluechf) %>% 
            mutate(taxvaluechf = as.numeric(taxvaluechf))
        
        # dividends
        div_trans <- div_trans %>%
            mutate(tot_div_q = div_valuechf * cum_quantity,
                   dividends_a = if_else(withholdingtax == 1, div_valuechf * cum_quantity, NA_real_),
                   dividends_b = if_else((is.na(withholdingtax) | withholdingtax != 1), div_valuechf * cum_quantity, NA_real_))
        
        rep_div <- div_trans %>% 
            group_by(isin) %>% 
            summarise(dividends_a = round(sum(dividends_a), digits = 3),
                      dividends_b = round(sum(dividends_b), digits = 3))
        
        
        
        # Create report -----------------------------------------------------------
        
        # join infos
        rep_data <- rep_fund %>% 
            left_join(rep_trans_q, by = "isin") %>% 
            left_join(rep_taxvalue, by = "isin") %>% 
            left_join(rep_div, by = "isin") %>% 
            mutate(taxvaluechf_q = eoy_q * taxvaluechf) %>% 
            select("valornumber", "isin", "fund_name", "fund_currency", "fund_type", "eoy_q", "taxvaluechf", 
                   "taxvaluechf_q", "dividends_a", "dividends_b")
        
        return(rep_data)
        
    })
    
    
    # Aggregation of data ---------------------------------------------------------
    df_aggr <- reactive({
        
        df_detail() %>% 
            summarise("title" = "Degiro account",
                      "tot_taxvaluechf" = sum(taxvaluechf_q, na.rm = TRUE),
                      "tot_dividends_a" = sum(dividends_a, na.rm = TRUE),
                      "tot_dividends_b" = sum(dividends_b, na.rm = TRUE))
    })
    
    
    # Output Table -- Aggregated View
    output$tbl_agg <- renderDT({
        datatable(df_aggr(),
                  selection = list(mode = 'none'),   
                  rownames = FALSE,
                  colnames = c("Account" = 1,
                               "Total Tax Value" = 2,
                               "Dividends A" = 3,
                               "Dividends B" = 4),
                  options = list(dom = 't'),
                  caption = 'Values in CHF',
                  class = 'nowrap display'
        )
    })
    
    
    # Output table -- Detailed View
    output$tbl_det <- renderDT({
        datatable(df_detail(), 
                  selection = list(mode = 'single', target = 'cell'),
                  rownames = FALSE,
                  colnames = c("Valor" = 1,
                               "ISIN" = 2,
                               "Name" = 3, 
                               # "Domicile" = 4,
                               "Currency" = 4,
                               "Type" = 5,
                               "Number of Shares" = 6,
                               "Tax Value Per Share" = 7,
                               "Tax Value" = 8,
                               "Dividends A" = 9,
                               "Dividends B" = 10
                  ),
                  caption = 'Values in CHF'
                  # style = 'bootstrap'
                  # class = 'nowrap display'
        )
        # formatStyle(columns = c(1:10), fontSize = '90%')
        # server = FALSE
    })
    # output$y31 = renderPrint(input$x31_cells_selected)
    
    output$tbl_agg_header <- renderText({
        req(input$file1)
        "Consolidated View"})

    output$tbl_det_header <- renderText({
        req(input$file1)
        "View by Financial Instrument"})

    output$info <- renderText({
        req(input$file1)
        "If you don't see your fund in the table above, it might not be listed on ICTax.
        In this case, you may want to request its entry to the Federal Tax Administration."})

    # output$disclaimer <- renderText({
    #     req(input$file1)
    #     "Disclaimer: the tax calculator is a tool to support you in your tax declaration.
    #     No responsibility is taken on the accuracy of the information provided."})
    
}