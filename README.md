# ge_2024_predictor_uns_ps

This folder contains all the R code and data for the Shiny app General Election 2024 Predictor that I have uploaded at https://livedataoxford.shinyapps.io/GE2024_Predictor_UNS_and_PS/

The main Shiny app is contained in app.R data which can be run in RStudio in a folder with all the other files (some might be superfluous I suppose but I'll let you figure that out).

Please feel free to contact me at ben.ansell@politics.ox.ac.uk or @benwansell if you have questions about the code or suggestions for improvement.

More information below:

Information:

Predictions all use Uniform National Swing (UNS) or Proportionate Swing (PS) from GE 2019 on new constituency boundaries. Proportionate Swing only used for Conservatives and Labour in PS model. Small parties always use UNS to prevent very large shifts and this means numbers won't always add to one hundred for PS.

Dotted line is 325 seats (pure majority). Working majority closer to 320 because of Speakers and Sinn Fein. Tactical voting shifts a proportion of Labour, Liberal, and Green voters to the party among these who did best in GE 19. Shifting Reform voters to Conservative only moves Reform voters in that direction. Constituencies are weighted by 2019 absolute turnout. Northern Ireland and Chorley (Speaker) excluded.

2019 vote share for new boundaries taken from estimates compiled by Professors Colin Rallings and Michael Thrasher on behalf of BBC News, ITV News, Sky News and the Press Association. Calculations for Scotland done by Professor David Denver, those for Northern Ireland by Nicholas Whyte. Brexit vote estimates for new boundaries from Chris Hanretty. New boundaries hex map from Philip Brown and Alasdair Rae, Automatic Knowledge Ltd. Demographics are from House of Commons library and ONS - NB Scotland only has age profile.

All code produced by Ben Ansell. Note: this is an updated version of my previous GE24 calculator which used the old constituency boundaries. It can be found here: https://livedataoxford.shinyapps.io/GE24Simulator/. Articles written about it can be found at https://benansell.substack.com/p/tactical-coping and https://benansell.substack.com/p/tactical-coping-a-postscript
