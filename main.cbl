       >>SOURCE FORMAT FREE
identification division.
program-id. uncontrolledsafedistance.

environment division.

data division.
working-storage section.
01 counter         pic 9         value 1.

01 xmtr-power      pic 9(4)      value 1000.
01 feedline-length pic 9(2)      value 73.
01 duty-cycle      pic 9(1)V9(2) value .5.
01 per-30          pic 9(1)V9(2) value .5.

01 k1 pic 9(1)V9(6) value .122290.
01 k2 pic 9(1)V9(6) value .000260.

01 freq1    pic 9(2)V9(2) value 7.3.
01 swr1     pic 9(1)V9(2) value 2.25.
01 gaindbi1 pic 9(1)V9(1) value 1.5.

01 freq2    pic 9(2)V9(2) value 14.35.
01 swr2     pic 9(1)V9(2) value 1.35.
01 gaindbi2 pic 9(1)V9(1) value 1.5.

01 freq3    pic 9(2)V9(2) value 18.1.
01 swr3     pic 9(1)V9(2) value 3.7.
01 gaindbi3 pic 9(1)V9(1) value 1.5.

01 freq4    pic 9(2)V9(2) value 21.45.
01 swr4     pic 9(1)V9(2) value 4.45.
01 gaindbi4 pic 9(1)V9(1) value 1.5.

01 freq5    pic 9(2)V9(2) value 24.99.
01 swr5     pic 9(1)V9(2) value 4.1.
01 gaindbi5 pic 9(1)V9(1) value 1.5.

01 freq6    pic 9(2)V9(2) value 29.7.
01 swr6     pic 9(1)V9(2) value 2.18.
01 gaindbi6 pic 9(1)V9(1) value 4.5.

01 gamma                         pic 9(4)V9(4).
01 feedlinelossper100ft          pic 9(4)V9(4).
01 feedlinelossformatchedload    pic 9(4)V9(4).
01 feedlinelossformatchedloadpct pic 9(4)V9(4).
01 gamma-squared                 pic 9(4)V9(4).
01 feedlinelossforswr            pic 9(4)V9(4).
01 feedlinelossforswrpct         pic 9(4)V9(4).
01 uncontrolledsafedistance      pic 9(4)V9(2).

local-storage section.
01 l_swr                pic 9(2)V9(2).
01 l_freq               pic 9(2)V9(2).
01 l_gaindbi            pic 9(2)V9(2).
01 l_powerlossatswr     pic 9(4)V9(4).
01 l_pepatantenna       pic 9(4)V9(4).
01 l_uncontrolledavgpep pic 9(4)V9(4).
01 l_mpe_s              pic 9(4)V9(4).
01 l_gaindecimal        pic 9(4)V9(4).

procedure division.
main-procedure.
       perform do-a-thing until counter > 6
       stop run.

do-a-thing.
       if counter equal to 1 then
           move freq1 to l_freq
           move gaindbi1 to l_gaindbi
           move swr1 to l_swr
       else if counter equal to 2 then
           move freq2 to l_freq
           move gaindbi2 to l_gaindbi
           move swr2 to l_swr
       else if counter equal to 3 then
           move freq3 to l_freq
           move gaindbi3 to l_gaindbi
           move swr3 to l_swr
       else if counter equal to 4 then
           move freq4 to l_freq
           move gaindbi4 to l_gaindbi
           move swr4 to l_swr
       else if counter equal to 5 then
           move freq5 to l_freq
           move gaindbi5 to l_gaindbi
           move swr5 to l_swr
       else if counter equal to 6 then
           move freq6 to l_freq
           move gaindbi6 to l_gaindbi
           move swr6 to l_swr           
       end-if.
       
       perform calculate-reflection-coefficient.
       perform calculate-feedline-loss-per-100ft-at-frequency.    
       perform calculate-feedline-loss-for-matched-load-at-frequency.
       perform calculate-feedline-loss-for-matched-load-at-frequency-pct.
       perform calculate-gamma-squared.  
       perform calculate-feedline-loss-for-swr.
       perform calculate-feedline-loss-for-swr-pct.
       compute l_powerlossatswr = feedlinelossforswrpct * xmtr-power.
       compute l_pepatantenna = xmtr-power - l_powerlossatswr.
       compute l_uncontrolledavgpep = l_pepatantenna * duty-cycle * per-30.
       compute l_mpe_s = 180/(l_freq**2).
       compute l_gaindecimal = 10**(l_gaindbi/10).
       compute uncontrolledsafedistance = function sqrt((0.219 * l_uncontrolledavgpep * l_gaindecimal)/l_mpe_s).    
       display uncontrolledsafedistance.
       compute counter = counter + 1.

calculate-reflection-coefficient.
       compute gamma = function abs((l_swr - 1)/(l_swr + 1)).     

calculate-feedline-loss-per-100ft-at-frequency.
       compute feedlinelossper100ft = k1 * function sqrt(l_freq + k2 * l_freq).

calculate-feedline-loss-for-matched-load-at-frequency.
       compute feedlinelossformatchedload = (feedline-length/100) * feedlinelossper100ft.

calculate-feedline-loss-for-matched-load-at-frequency-pct.
       compute feedlinelossformatchedloadpct = 10**( -feedlinelossformatchedload/10).

calculate-gamma-squared.
       compute gamma-squared = function abs(gamma)**2.

calculate-feedline-loss-for-swr.
       compute feedlinelossforswr = -10 * function log10(feedlinelossformatchedloadpct * ((1 - gamma-squared)/(1 - feedlinelossformatchedloadpct**2 * gamma-squared))).

calculate-feedline-loss-for-swr-pct.
       compute feedlinelossforswrpct = (100 - 100/( 10**(feedlinelossforswr/10)))/100.
