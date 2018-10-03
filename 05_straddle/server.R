source("helpers.R")

shinyServer(function(input, output, session) {
    
    pfo_components <- eventReactive(input$backtest, {
        yaml.load(input$portfolio)
    })
    
    hedge_components <- eventReactive(input$backtest, {
        yaml.load(input$hedge)
    })
    
    pfo <- eventReactive(input$backtest, {
        symbols <- names(pfo_components())
        weights <- unlist(pfo_components()[symbols]) %>>% (./sum(.))
        get_returns(symbols, weights, discount_rate=input$discount_rate)
    })
    
    hedge <- eventReactive(input$backtest, {
        symbols <- names(hedge_components())
        weights <- unlist(hedge_components()[symbols]) %>>% (./sum(.))
        get_returns(symbols, weights, discount_rate=input$discount_rate)
    })
    
    backtest <- reactive({
        marginal_rate <- input$marginal_rate
        capital_gains_rate <- input$capital_gains_rate
        
        merge(pfo(), hedge()) %>>%
            na.trim("left", "any") %>>%
            apply(MARGIN=2, FUN=function(x) x - first(x)) %>>%
            reclass(pfo()) %>>%
            (~ xts.pfo <- .[,1]) %>>%
            (~ xts.hedge <- .[,2])
            
        xts.hedge_ret_y <- apply.yearly(xts.hedge, sum)
        
        xts.hedge_ret <- Reduce(recognize, as.vector(xts.hedge_ret_y), init=Inf, accumulate=TRUE) %>>%
          tail(-1) %>>%
          exp %>>%
          reclass(xts.hedge_ret_y)

        # each time you sell, you re-hedge using approximately the size of the long position in that year
        xts.hedge_size <- Reduce(recognize, as.vector(xts.hedge_ret_y), init=Inf, accumulate=TRUE) %>>%
          (ifelse(. > 0, 1, NA)) %>>%
          head(-1) %>>%
          `*`(apply.yearly(cumsum(xts.pfo), first)) %>>%
          exp %>>%
          na.locf
        
        xts.losses <- merge(xts.hedge_size, xts.hedge_ret) %>>% 
            apply(MARGIN=1, FUN=function(x) prod(x) - x[1]) %>>% 
            reclass(xts.hedge_ret)
        xts.losses <- c(pmax(0, head(xts.losses, -1)), last(xts.losses)) %>>%
            (. * input$hedge_size) %>>%
            reclass(xts.losses)
        colnames(xts.losses) <- 'loss'
        
        xts.gains <- xts(exp(sum(xts.pfo))-1, order.by=last(index(xts.pfo))) * input$portfolio_size
        colnames(xts.gains) <- 'gain'
        
        merge(xts.gains, -xts.losses) %>>%
            as.data.table %>>%
            (.[, tax_hedge := -(`loss` * marginal_rate)]) %>>%
            (.[, tax_portfolio := -(`gain` * capital_gains_rate)]) %>>%
            melt.data.table(id.vars='index') %>>%
            (.[, type := ifelse(variable %in% c("tax_hedge", "tax_portfolio"), "Tax", "Realized Gains")])
    })
    
    output$returns <- renderPlot({

        combined <- merge(portfolio=pfo(), hedge=hedge()) %>>%
            na.trim("left", "any") %>>%
            apply(MARGIN=2, FUN=function(x) x - x[1]) %>>%
            apply(MARGIN=2, FUN=cumsum) %>>%
            apply(MARGIN=2, FUN=exp) %>>%
            (. %*% diag(c(input$portfolio_size, -input$hedge_size))) %>>%
            apply(MARGIN=2, FUN=function(x) x - x[1]) %>>%
            (merge.xts(., combined=as.xts(rowSums(., na.rm=TRUE))))

        plotReturn(combined, .combine='identity', scales=list(cex=1.5)
            , col = c("firebrick3", "darkgray", "steelblue")
            , auto.key = list(columns = 3, text=c("Portfolio", "Hedge", "Combined"))
            , ylab = list(label = "Returns", cex = 1.5))
    })
    
    output$straddle <- renderPlot({
        barchart(backtest(), x = value ~ index | type
            , group=variable, horizontal=FALSE
            , origin=0
            , xscale.components = function(...) {
              ans <- xscale.components.default(...)
              ans$bottom$labels$labels <- year(as.Date(ans$bottom$labels$labels))
              ans }
            , yscale.components = function(...) {
              ans <- yscale.components.default(...)
              ans$left$labels$labels <- formatSi('$', 3, bln.parens=TRUE)(ans$left$labels$at)
              ans }
            , auto.key = list(columns=4, text=c("Portfolio", "Hedge", "Hedge (Tax)", "Portfolio (Tax)"))
            , scales = list(cex=1.5, x = list(rot=45), y = list(rot=0, tck = FALSE, relation='free'))
            , layout = c(1,2)
            , as.table=TRUE
            , ylab = NULL
        )
    })
    
    output$tax_savings <- renderPlot({
        dt <- backtest()
        dt[type == "Tax", list(tax_savings = sum(value, na.rm=TRUE)), by='index'] %>>%
            plotReturn(.combine='sum'
                , scales = list(cex = 1.5)
                , main = "Tax Savings"
                , ylab = list(label = "Returns", cex = 1.5))
    })
    
    output$realized_gains <- renderPlot({
        dt <- backtest()
        dt[, list(gains = sum(value, na.rm=TRUE)), by=index] %>>%
            plotReturn(.combine='sum'
                , scales = list(cex = 1.5)
                , main = "Realized Gains + Tax Savings"
                , ylab = list(label = "Returns", cex = 1.5))
    })
    
    output$correlations <- renderPlot({
        xts.pfo <- pfo()
        xts.hedge <- hedge()
        xlims <- coalesce(xts.pfo, 0) %>>% (c(min(.) * .9, max(.) * 1.1))
        ylims <- coalesce(xts.hedge, 0) %>>% (c(min(.) * .9, max(.) * 1.1))
        
        xyplot(xts.hedge ~ xts.pfo
            , col = 'steelblue'
            , type = c('p', 'r'), col.line = 'firebrick3'
            , aspect = 1
            , scales = list(cex = 1.5)
            , ylab = list(label = 'log-returns (hedge)', cex = 1.5)
            , xlab = list(label = 'log-returns (portfolio)', cex = 1.5)
            , panel = function(...) {
                panel.xyplot(...)
                panel.abline(a=0, b=1, col='black', lty='dotted')
            }
            , xlim = xlims
            , ylim = ylims)
    })
    
    output$substantial_overlap_text <- renderText({
        sprintf("1.246-5(c) overlap: %f", max(
            substantial_overlap(pfo_components(), input$portfolio_size
                , hedge_components(), input$hedge_size)))
    })
    
    output$substantial_overlap <- renderDataTable({
        
        dt.pfo <- data.table(ticker=names(pfo_components()), wt = unlist(pfo_components())[names(pfo_components())] %>>% (. / sum(.) * input$portfolio_size), key='ticker')
        dt.hedge <- data.table(ticker=names(hedge_components()), wt = unlist(hedge_components())[names(hedge_components())] %>>% (. / sum(.) * input$hedge_size), key='ticker')
        
        dt <- merge(dt.pfo, dt.hedge, all=TRUE, suffixes = c("_pfo", "_hedge"))
        dt[, sub := pmin(wt_pfo, wt_hedge) %>>% coalesce(0)]
        dt        
    })

})
