# Future battery recycling in North America: Capacity expansion under shifting demand and circularity strategies

Scripts usually load raw data to stored it in a processed format in the folder 'Inputs'. The processed data inputs are latter used for the Material Flow Analysis Model.

All scripts are numbered in order suggested to run to replicate full results.

* **01a-Sales_Vehicle.R**: Load ICCT Vehicle Sales data - Filter to North America.
* **01b-Sales_Stationary.R**: Stationary storage based on forecast of solar and wind generation capacity.
* **01c-SalesConsumerElectronics.R**: Load Consumer Electronics input data from RMI.
* **02a-LDV_Battery_Capacity.R**: Battery Capacity for light duty vehicles - Historical Sales and Average battery capacity from EV Volumes.
* **02b-MDV_HDV_Battery_Capacity.R**: Same battery capacity for medium and heavy duty vehicles.
* **02c-Battery_Forecast.R**: Battery capacity forecast scenarios.
* **02d-BatQuantileFigure.R**: Figure for appendix of Quantiles of battery size, by country.
* **03a-EV-StocksFlows.R**: Estimate EV stock, additional LIB requirements, LIBs for recycling and LIBs for availability using the survival curves and the product-component framework. **Key script to run the main inputs for the material flow analysis**.
* **4-CathodeWeight.R**: Cathode weight and energy density of batteries based on 2024 chemistry and BatPac battery parameters.
