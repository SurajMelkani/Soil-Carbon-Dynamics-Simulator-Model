# Soil Carbon Dynamics Simulator Model

An interactive platform to compare multiple soil organic carbon model structures. This platform can simulate soil organic carbon trajectories and final pool composition across **9 models** under a consistent starting soil organic carbon stock and carbon input scenario.

## Launch the App
**Live app:** (add your link here once deployed)  
`[https://YOUR-APP-LINK](https://ljdrky-suraj-melkani.shinyapps.io/Soil_C_Simulation/)`

---

## Models Included

### Conceptual compartment architectures
**Two-pool models**
- Series
- Parallel
- Feedback

**Three-pool models**
- Series
- Parallel
- Feedback

### Process-based models
- **Rothamsted Carbon Model (RothC)**
- **Century Model **
- **Introductory Carbon Balance Model (ICBM)**

---

## What the latform Simulates

### 1) Total soil organic carbon projections (time series)
A line plot comparing simulated total soil organic carbon (tonnes of carbon per hectare) through time.

### 2) Final pool composition
A separate stacked bar chart for each model showing the **final pool sizes** at the end of the simulation.  
Each panel includes its own legend because pool names differ by model.

---

## Inputs Required

### Monthly climatology (averages)
- Monthly temperature (degrees Celsius)
- Monthly precipitation (millimeters)
- Monthly potential evapotranspiration (millimeters)
- Monthly soil cover (covered or bare)

### Soil and carbon scenario inputs
- Clay content (percent)
- Annual carbon input (tonnes of carbon per hectare per year)
- Initial total soil organic carbon (tonnes of carbon per hectare)
- Simulation duration (years)

> Note: In this implementation, the simulation time step is annual for all models.  
> Monthly climate is summarized into a scalar climate modifier for the RothC Carbon Model.


### 1) Install R packages
```r
install.packages(c("shiny","ggplot2","plotly","dplyr","tidyr","SoilR"))
