Author: Zachary Kramer
Email: kramer.zachary.nau@gmail.com
Date: 11/10/17

Each column represents a scenario of ppm values for atmospheric carbon dioxide. The row number represents the year, whereas the value in the row/column combo is the ppm value.

In the experimental/treatment designs, a user can enable LookupCarbonScenarios and provide either "FILL" or a scenario name, both of which are case-insensitive.

If "FILL" is provided, the scenario name will be automatically extracted from what is being simulated and used in the lookup. The default scenario uses the default values of 360 ppm for every year.

If a value other than "FILL" is provided, that value will be used in the lookup.

rSFSW2 will search this CSV for the scenario name, ensure the ppm data is valid, and pass the ppm data to SOILWAT2 via the swCarbon class. This ppm data will be used in the CO2 power equations defined in SOILWAT2/SW_Carbon.c to calculate multipliers for biomass and water-use efficiency.

You can see the ppm values that were used in sw_input.RData under swRunScenariosData[[1 or scenario index]]@carbon@CO2ppm

You can see the resulting multipliers in sw_output.RData under runDataSC@CO2effects


Data for scenarios RCP85
have been downloaded on Aug 22, 2016 from the website "RCP Concentration Calculations and Data: Final Version, background data, acknowledgements and further info" (http://www.pik-potsdam.de/~mmalte/rcps/). We extracted the columns "CO2" (3rd column) from the files 'scenario_MIDYEAR_CONCENTRATIONS.DAT'.

Description of the data sets:
Meinshausen, M., S. J. Smith, K. V. Calvin, J. S. Daniel, M. L. T. Kainuma, J.-F. Lamarque, K. Matsumoto, S. A. Montzka, S. C. B. Raper, K. Riahi, A. M. Thomson, G. J. M. Velders and D. van Vuuren (2011). "The RCP Greenhouse Gas Concentrations and their Extension from 1765 to 2300." Climatic Change (Special Issue), DOI: 10.1007/s10584-011-0156-z
