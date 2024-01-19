

library(shiny)

# Define server logic required to draw a Normal Curve
shinyServer(function(input, output) {

    output$powPlot <- renderPlot({
      
### General User Input Parameters
# Parameters for the Null Curve
      alpha = input$Alpha
      mu = c(0)
      sd = input$se
      
      l_end <- mu-6.0*sd
      r_end <- mu+6.0*sd
# Parameters for the Alternative Curve     
      X_bar <- input$x_bar
      se <- input$se
      
      alt_l_end <- X_bar - 4.5*se
      alt_r_end <- X_bar + 4.5*se
  

### CREATE NULL CURVE & ALT CURVE PARAMETERS/ARGUMENTS FOR LATER PLOTTING      

## NULL CURVE PARAMETERS     
      # Condition of "tailtype" for grid & shading parameters
#      
      if (input$tailtype == 0) { # 2-Tailed
        #alpha cutoffs
        l_ctf <- qnorm(alpha/2,mu,sd)
        r_ctf <- qnorm(1-alpha/2,mu,sd)
        #X values for Left & Right Tails
        l_shade <- seq(l_end,l_ctf,.01)
        r_shade <- seq(r_ctf, r_end, .01)
        # X values (null_btwn) and Y values (null_btw_shd) for between space
        null_btwn <- seq(l_ctf, r_ctf, .01)
        null_btwn_shd <- rep(0, length(null_btwn))
        myLine <- TRUE
        # X-values 'X_shade' & Y-values 'Y_shade' for polygon()
        X_shade <- c(l_shade,null_btwn, r_shade)
        Y_shade <- c(dnorm(l_shade,mu,sd), null_btwn_shd, 
                     dnorm(r_shade,mu,sd))

        
        ### ENTER 6 CONDITIONALS FOR ALTERNATIVE CURVE SHADING PARAMETERS
        # TWO TAILED
        if (alt_l_end >= r_ctf) 
          {# Alt curve is to the Right of Cutoff
          # All Power
          
          # 2 variables below define X & Y coordinates for Power Shading
          x_pow_grid <- seq(alt_l_end,alt_r_end,.01)
          y_pow_grid <- dnorm(x_pow_grid, X_bar, se)
          
          # 2 variables below define X & Y coordinates for Power Shading
          # as arguments for "polygon()"
          X_pow_par <- c(x_pow_grid, rev(x_pow_grid))
          Y_pow_par <- c(y_pow_grid, rep(0,length(x_pow_grid)))
          
          # 2 variables below define X & Y coordinates for Error Shading
          # as arguments for "polygon()"; special case because no error
          X_err_par <- c(x_pow_grid, rev(x_pow_grid))
          Y_err_par <- c(rep(0, length(x_pow_grid)),rep(0, length(x_pow_grid)) )
          
          #for Power GUI display
          pow_calc <- c(1)
        } 
        else if (alt_r_end <= l_ctf) 
          {# Alt is to the Left of Cutoff 
          # Alt curve is left of cutoff, all power
          x_pow_grid <- seq(alt_l_end,alt_r_end,.01)
          y_pow_grid <- dnorm(x_pow_grid, X_bar, se)
          
          X_pow_par <- c(x_pow_grid, rev(x_pow_grid))
          Y_pow_par <- c(y_pow_grid, rep(0,length(x_pow_grid)))
          
          X_err_par <- c(x_pow_grid,rev(x_pow_grid))
          Y_err_par <- c(rep(0, length(x_pow_grid)),rep(0, length(x_pow_grid)) )
          
          #for Power GUI display
          pow_calc <- c(1)
        } 
        else if (alt_l_end < l_ctf && alt_r_end <= r_ctf )
        {# Alt is around Left Tail
          
          # 2 variables below define X & Y coordinates for Power Shading
          x_pow_grid <- seq(alt_l_end, l_ctf,.01)
          y_pow_grid <- dnorm(x_pow_grid, X_bar,se)
          
          # 2 variables below define X & Y coordinates for Error Shading
          x_err_grid <- seq(l_ctf,r_ctf,.01)
          y_err_grid <- dnorm(x_err_grid, X_bar, se )
          
          # 2 variables below define X & Y coordinates for Power Shading
          # as arguments for "polygon()"
          X_pow_par <- c(x_pow_grid, rev(x_pow_grid))
          Y_pow_par <- c(y_pow_grid, rep(0,length(x_pow_grid)))
          
          # 2 variables below define X & Y coordinates for Error Shading
          # as arguments for "polygon()"
          X_err_par <- c(x_err_grid, rev(x_err_grid))
          Y_err_par <- c(y_err_grid, rep(0,length(y_err_grid))) 
          
          #for Power GUI display
          pow_calc <- round(pnorm(l_ctf,X_bar,se),3)
        }
        else if (alt_l_end < l_ctf && alt_r_end > r_ctf )
        {# Alt is around Left Tail, but moving towards the right reaching
          # and passing the r_ctf, so Alt is around cutoffs
          l_pow <- seq(alt_l_end, l_ctf,.01)
          btwn <- seq(l_ctf,r_ctf,.01)
          r_pow <- seq(r_ctf, alt_r_end,.01)
          x_pow_grid <- c(l_pow,btwn,r_pow )
          y_pow_grid <- dnorm(x_pow_grid, X_bar,se)
          
          x_err_grid <- btwn
          y_err_grid <- dnorm(x_err_grid, X_bar, se )
          
          X_pow_par <- c(l_pow, btwn, r_pow, rev(x_pow_grid))
          Y_pow_par <- c(dnorm(l_pow,X_bar,se),rep(0,length(btwn)),
                         dnorm(r_pow,X_bar, se), rep(0,length(x_pow_grid)))
          
          X_err_par <- c(x_err_grid, rev(x_err_grid))
          Y_err_par <- c(y_err_grid, rep(0,length(x_err_grid))) 
          
          #for Power GUI display
          added <- pnorm(l_ctf,X_bar,se) + pnorm(r_ctf,X_bar,se,lower.tail = FALSE)
          pow_calc <- round(added,3)
        }
        else if (alt_l_end >= l_ctf && alt_r_end <= r_ctf) 
        {# Alt is inside of cutoffs; all Error
          x_pow_grid <- seq(alt_l_end,alt_r_end,.01)
          y_pow_grid <- dnorm(x_pow_grid, X_bar, se)
          
          X_pow_par <- c(x_pow_grid, rev(x_pow_grid))
          Y_pow_par <- c(rep(0, length(x_pow_grid)),rep(0, length(x_pow_grid)) )
          
          X_err_par <- c(x_pow_grid, rev(x_pow_grid))
          Y_err_par <- c(y_pow_grid, rep(0,length(x_pow_grid)))
          
          #for Power GUI display
          pow_calc <- c(0)
        } 
        else if (alt_l_end <= r_ctf && alt_r_end > r_ctf)
        { # Alt is around Right Tail
          x_pow_grid <- seq(r_ctf,alt_r_end,.01)
          y_pow_grid <- dnorm(x_pow_grid, X_bar,se)
          
          x_err_grid <- seq(alt_l_end, r_ctf,.01)
          y_err_grid <- dnorm(x_err_grid, X_bar, se )
          
          X_pow_par <- c(x_pow_grid, rev(x_pow_grid))
          Y_pow_par <- c(y_pow_grid, rep(0,length(x_pow_grid)))
          X_err_par <- c(x_err_grid, rev(x_err_grid))
          Y_err_par <- c(y_err_grid, rep(0,length(y_err_grid)))
          
          #for Power GUI display
          pow_calc <- round(pnorm(r_ctf,X_bar,se,lower.tail = FALSE),3)
        } 
        ### EXIT CONDITIONALS FOR ALTERNATIVE CURVE SHADING PARAMETERS
        
        
      } else if (input$tailtype == 1) { # Left-Tailed
        #alpha cutoffs
        l_ctf <- qnorm(alpha,mu,sd)
        #X values for Left tail
        l_shade <- seq(l_end,l_ctf,.01)

        # X-values 'X_shade' & Y-values 'Y_shade' for polygon()
        X_shade <- c(l_shade, rev(l_shade))
        Y_shade <- c(dnorm(l_shade,mu,sd), rep(0,length(l_shade)))
        
        myLine<- FALSE
        
        ### ENTER 3 CONDITIONALS FOR ALTERNATIVE CURVE SHADING PARAMETERS
        #   LEFT TAIL
        # Alpha cutoff is between Alt Curve tails
        if (alt_l_end <= l_ctf && alt_r_end >= l_ctf){
          x_pow_grid <- seq(alt_l_end, l_ctf,.01)
          y_pow_grid <- dnorm(x_pow_grid, X_bar,se)
          
          x_err_grid <- seq(l_ctf, alt_r_end,.01)
          y_err_grid <- dnorm(x_err_grid, X_bar, se )
          
          X_pow_par <- c(x_pow_grid, rev(x_pow_grid))
          Y_pow_par <- c(y_pow_grid, rep(0,length(x_pow_grid)))
          X_err_par <- c(x_err_grid, rev(x_err_grid))
          Y_err_par <- c(y_err_grid, rep(0,length(y_err_grid)))
          
          #for Power GUI display
          pow_calc <- round(pnorm(l_ctf,X_bar,se),3)
          
        } else if ( alt_r_end <= l_ctf) { 
          # Alt curve is left of cutoff, all power
          x_pow_grid <- seq(alt_l_end,alt_r_end,.01)
          y_pow_grid <- dnorm(x_pow_grid, X_bar, se)
          
          X_pow_par <- c(x_pow_grid, rev(x_pow_grid))
          Y_pow_par <- c(y_pow_grid, rep(0,length(x_pow_grid)))
          
          X_err_par <- c(x_pow_grid,rev(x_pow_grid))
          Y_err_par <- c(rep(0, length(x_pow_grid)),rep(0, length(x_pow_grid)) )
          
          #for Power GUI display
          pow_calc <- c(1)
          
        } else if ( alt_l_end >= l_ctf) {
          # Alt is to right, All Error
          x_pow_grid <- seq(alt_l_end,alt_r_end,.01)
          y_pow_grid <- dnorm(x_pow_grid, X_bar, se)
          
          X_pow_par <- c(x_pow_grid, rev(x_pow_grid))
          Y_pow_par <- c(rep(0, length(x_pow_grid)),rep(0, length(x_pow_grid)) )
            
          X_err_par <- c(x_pow_grid, rev(x_pow_grid))
          Y_err_par <- c(y_pow_grid, rep(0,length(x_pow_grid)))
          
          #for Power GUI display
          pow_calc <- c(0)
        }
        ### EXIT CONDITIONALS FOR ALTERNATIVE CURVE SHADING PARAMETERS
        
      } else { # Right-Tailed
        #alpha cutoff
        r_ctf <- qnorm(1-alpha,mu,sd)
        #X values for Right tail
        r_shade <- seq(r_ctf,r_end,.01)
        
        # X-values 'X_shade' & Y-values 'Y_shade' for polygon()
        X_shade <- c(r_shade, rev(r_shade))
        Y_shade <- c(dnorm(r_shade,mu,sd), rep(0,length(r_shade)))
        
        myLine <- FALSE
        
        ### ENTER 3 CONDITIONALS FOR ALTERNATIVE CURVE SHADING PARAMETERS
        #   RIGHT TAIL
        # Alpha cutoff is between Alt Curve tails
        if (alt_l_end <= r_ctf && alt_r_end >= r_ctf){
          x_pow_grid <- seq(r_ctf,alt_r_end,.01)
          y_pow_grid <- dnorm(x_pow_grid, X_bar,se)
          
          x_err_grid <- seq(alt_l_end, r_ctf,.01)
          y_err_grid <- dnorm(x_err_grid, X_bar, se )
          
          X_pow_par <- c(x_pow_grid, rev(x_pow_grid))
          Y_pow_par <- c(y_pow_grid, rep(0,length(x_pow_grid)))
          X_err_par <- c(x_err_grid, rev(x_err_grid))
          Y_err_par <- c(y_err_grid, rep(0,length(y_err_grid)))
          
          #for Power GUI display
          pow_calc <- round(pnorm(r_ctf,X_bar,se,lower.tail = FALSE),3)
          
        } else if ( alt_l_end >= r_ctf) { 
          # Alt curve is right of cutoff, all power
          x_pow_grid <- seq(alt_l_end,alt_r_end,.01)
          y_pow_grid <- dnorm(x_pow_grid, X_bar, se)
          
          X_pow_par <- c(x_pow_grid, rev(x_pow_grid))
          Y_pow_par <- c(y_pow_grid, rep(0,length(x_pow_grid)))
          
          X_err_par <- c(x_pow_grid, rev(x_pow_grid))
          Y_err_par <- c(rep(0, length(x_pow_grid)),rep(0, length(x_pow_grid)) )
          
          #for Power GUI display
          pow_calc <- c(1)
          
        } else if ( alt_r_end <= r_ctf) {
          # Alt is left of cutoff All Error
          x_pow_grid <- seq(alt_l_end,alt_r_end,.01)
          y_pow_grid <- dnorm(x_pow_grid, X_bar,se)
          
          X_pow_par <- c(x_pow_grid, rev(x_pow_grid))
          Y_pow_par <- c(rep(0,length(x_pow_grid)), rep(0,length(x_pow_grid)))
          
          X_err_par <- c(x_pow_grid, rev(x_pow_grid))
          Y_err_par <- c(y_pow_grid, rep(0,length(x_pow_grid)))
          
          #for Power GUI display
          pow_calc <- c(0)
        }
        ### EXIT CONDITIONALS FOR ALTERNATIVE CURVE SHADING PARAMETERS
        
      }
      

      
####  PLOT  & SHADE BOTH CURVES
      
      ## NULL CURVE X & Y Values
      
      # X values
      
      # to adjust plot for SE in Alternative curve; (so plot doesn't get
      # cutoff by window size)
      if (se < 1 & se > .4) {
        X_null<- seq(min(l_end, X_bar -3.5), max(r_end, X_bar+3.5), .01)
        incrs <- 1 - se
        mylim <- c(0.0, .35 + incrs)
        
      } else if (se == .4) {
        X_null<- seq(min(l_end, X_bar -3.5), max(r_end, X_bar+3.5), .01)
        mylim <- c(0.0, 1.0)
      } else if (se == .3) {
        X_null<- seq(min(l_end, X_bar -3.5), max(r_end, X_bar+3.5), .01)
        mylim <- c(0.0, 1.3)
      } else if (se == .2) {
        X_null<- seq(min(l_end, X_bar -3.5), max(r_end, X_bar+3.5), .01)
        mylim <- c(0.0, 2.0)
      } else if (se == .1) {
        X_null<- seq(min(l_end, X_bar -3.5), max(r_end, X_bar+3.5), .01)
        mylim <- c(0.0, 4.0)
      } else {
        X_null <- seq(min(l_end, alt_l_end), max(r_end, alt_r_end), .01)
        mylim <- c(0.0, 0.4)
      } 
      
      # Y values
      Y_null <- dnorm(X_null,mu,sd) 
      
      
      ## WHAT'S THE TRUTH PARAMETERS (TOGGLES CURVE COLORS & SHADING)
      # (need before call to 'plot')
      troof <- input$truth
      if(troof == 0){
        # Null Curve
        null_l_col <- "black"
        ltp <- 1 # 1 for unbroken; 2 for dashed
        
        r_col <- "red" # reject area 
        r_ln <- 1 # unbroken line 1 ; 2 for dashed
        n_lwd <- 3
        # Alt Curve
        alt_l_col <- "grey"
        altp <- 2
        
        pw_col <-"#1E90FF42" #"dodgerblue" less opaque
        err_col<- "#FFD70042" # "gold" less opaque
        a_lwd <- 2
        
        # Alt curve Density & Opacity
        
        # GUI Display
        powcol1 <- paste("<span><h1 style=\"color:dimgrey;font-size:25px;margin-top:0\">Power</h1></span>")
        pwnumcol1 <-paste("<span style=\"color:dimgrey;font-size:25px;margin-top:0\">",textOutput("power"),"</span>")
        
        bet1 <- paste("<span><h1 style=\"color:dimgrey;font-size:25px;margin-top:0\">&beta;</h1></span>")
        btnumcol1 <- paste("<span style=\"color:dimgrey;font-size:25px;margin-top:0\">",textOutput("beta"),"</span>")
        
        theAlt1 <-paste("<span><h4 style=\"line-height:70px;font-family:Lucida Handwriting;color:dimgrey\">The Alt. Hyp.</h4></span>")
        theNul1 <- paste("<span><h4 style=\"line-height:70px;font-family:Lucida Handwriting;color:orange\">The Null Hyp.</h4></span>")
        
        nid1 <-  paste("<span><h1 style=\"color:orange;font-size:25px;margin-top:0\">&alpha;</h1></span>")
        nidnum1 <-paste("<span style=\"color:orange;font-size:25px;margin-top:0\">",textOutput("alpha1"),"</span>")
        
        ncd1 <-paste("<span><h1 style=\"color:orange;font-size:25px;margin-top:0\">1 - &alpha;</h1></span>")
        ncdnum1 <- paste("<span style=\"color:orange;font-size:25px;margin-top:0\">",textOutput("null_cd"),"</span>")
        
      } else {
        # Null Curve
        # Null Curve
        null_l_col <- "grey"
        ltp <- 2 # 1 for unbroken; 2 for dashed
        
        r_col <- "red" # reject area 
        r_ln <- 2 # unbroken line 1 ; 2 for dashed
        n_lwd <- 3
        # Alt Curve
        alt_l_col <- "black"
        altp <- 1
        
        pw_col <-"#1E90FFFF" #"dodgerblue" fully opaque
        err_col<- "#FFD700FF" # "gold" fully opaque
        a_lwd <- 2
        
        # GUI Display
        powcol1 <- paste("<span><h1 style=\"color:orange;font-size:25px;margin-top:0\">Power</h1></span>")
        pwnumcol1 <-paste("<span style=\"color:orange;font-size:25px;margin-top:0\">",textOutput("power"),"</span>")
         
        bet1 <- paste("<span><h1 style=\"color:orange;font-size:25px;margin-top:0\">&beta;</h1></span>")
        btnumcol1 <-paste("<span style=\"color:orange;font-size:25px;margin-top:0\">",textOutput("beta"),"</span>")
        
        theAlt1 <-paste("<span><h4 style=\"line-height:70px;font-family:Lucida Handwriting;color:orange\">The Alt. Hyp.</h4></span>")
        theNul1 <- paste("<span><h4 style=\"line-height:70px;font-family:Lucida Handwriting;color:dimgrey\">The Null Hyp.</h4></span>")
        
        nid1 <-  paste("<span><h1 style=\"color:dimgrey;font-size:25px;margin-top:0\">&alpha;</h1></span>")
        nidnum1 <-paste("<span style=\"color:dimgrey;font-size:25px;margin-top:0\">",textOutput("alpha1"),"</span>")
        
        ncd1 <-paste("<span><h1 style=\"color:dimgrey;font-size:25px;margin-top:0\">1 - &alpha;</h1></span>")
        ncdnum1 <- paste("<span style=\"color:dimgrey;font-size:25px;margin-top:0\">",textOutput("null_cd"),"</span>")
        
      }
      
      # Plot Null Curve
      plot(X_null, Y_null, type = "l", lty = ltp , col= null_l_col
           , lwd = n_lwd, asp = 10, ylim = mylim, 
           xlab ="SE's", ylab = "P(X)")
      # Shade Null Tail(s)
      polygon(X_shade,Y_shade, col= r_col, density = 15, angle = 45 
              , lwd = n_lwd, lty = r_ln)
      # Get rid of red line inbetween 2-tailed reject regions
      if (myLine == TRUE){segments(l_ctf+.05,0,r_ctf-.05,0, col="white",lwd=4)}
      
      
      ## ALTERNATIVE CURVE
      # X & Y values
      X_alt <- seq(alt_l_end, alt_r_end, .1)
      Y_alt <- dnorm(X_alt, X_bar, se) 
      #Plot Alternative Curve
      lines(X_alt, Y_alt, type = "l", lty = altp, col = alt_l_col
            , lwd = a_lwd)
      # SHADE ALTERNATIVE CURVE
      # Shade Power
      polygon(X_pow_par, Y_pow_par, col= pw_col, density = 15, 
              angle = 135, lwd = a_lwd, lty = altp)
      # Shade Error
      polygon(X_err_par, Y_err_par, col= err_col, density = 15, 
              angle = 135, lwd = a_lwd, lty = altp)
      
      
      #### POWER & TYPE I & II ERRORS DISPLAY FOR GUI (ui.R)
       output$power <- renderText({pow_calc})
       output$beta <- renderText({1 - pow_calc})
          
       output$alpha1 <- renderText({alpha})
       output$null_cd <- renderText({1 - alpha})
       
       output$powcol <- renderText({powcol1})
       output$pwnumcol <- renderText({pwnumcol1})
       
       output$bet <- renderText({bet1})
       output$btnumcol <- renderText({btnumcol1})
       
       output$theAlt <- renderText({theAlt1})
       output$theNul <- renderText({theNul1})
       
       output$nid <- renderText({nid1})
       output$nidnum <- renderText({nidnum1})
       
       output$ncd <- renderText({ncd1})
       output$ncdnum <- renderText({ncdnum1})
       
    }) # renderplot

}) # shiny server
