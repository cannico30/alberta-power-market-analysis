# Alberta Power Market Analysis — 2024

## Project Overview

This project analyzes hourly Alberta electricity market data from 2024 to explore the question:

**What market conditions were associated with Alberta's highest electricity prices?**

The analysis combines AESO pool prices, Alberta Internal Load (AIL), wind and solar generation, intertie flows, asset-level generation, and generation capacity/outage data.

The goal was to understand how demand, renewable generation, dispatchable generation, imports/exports, and generator availability differed during extreme-price conditions.

## Data

Data were obtained from the Alberta Electric System Operator (AESO).

The analysis includes:

- Hourly pool price
- Alberta Internal Load (AIL)
- Wind generation
- Solar generation
- Intertie imports and exports
- Asset-level generation by fuel type
- Generation maximum capability and outage data

The analysis covers all 8,784 hours of 2024.

## Methods

Net load was calculated as:

Net Load = AIL - Wind Generation - Solar Generation

High-price hours were defined as hours in the top 5% of 2024 pool prices.

Hourly conditions during these high-price periods were compared with the remaining 95% of hours.

The analysis also examined hourly net-load ramps and selected January and July high-price events to compare winter and summer market conditions.

## Key Findings

High-price hours were characterized by substantially tighter market conditions.

Compared with normal-price hours:

| Metric | Normal-price hours | Top-5% price hours |
|---|---:|---:|
| Pool price | $38.5/MWh | $523/MWh |
| Demand | 10,072 MW | 10,872 MW |
| Net load | 8,234 MW | 10,230 MW |
| Wind generation | 1,507 MW | 384 MW |
| Net imports | -237 MW | +273 MW |
| Gas generation | 4,109 MW | 5,413 MW |
| Hydro generation | 176 MW | 325 MW |
| Gas outage share | 25.7% | 26.8% |
| Available gas capacity | 9,590 MW | 9,236 MW |

The strongest pattern was the increase in **net load** during high-price hours.

Wind generation fell substantially, while gas and hydro generation increased. Alberta also shifted from being a net exporter on average during normal-price hours to a net importer during high-price hours.

Generator outages were somewhat higher and available gas capacity somewhat lower during high-price hours, but these differences were modest relative to changes in net load and generation.

Large hourly ramps were not required for extreme prices, suggesting that the **level of system tightness was more important than the size of the immediate hourly ramp**.

## Figures

### Pool Price vs Net Load
Higher net load was strongly associated with higher Alberta pool prices.

![Pool Price vs Net Load](figures/01_pool_price_vs_net_load.png)

### High-Price Hours by Time of Day
High-price events were concentrated in the late afternoon and evening.

![High Price Frequency](figures/02_high_price_frequency_by_hour.png)

### Market Conditions During High-Price Hours
High-price periods were associated with higher net load, lower wind generation, and greater gas and hydro generation.

![Normal vs High Price Conditions](figures/03_normal_vs_high_price_conditions.png)

### January vs July
Winter and summer extreme-price periods showed different seasonal conditions but a common pattern of high net load and greater reliance on dispatchable generation.

![January vs July](figures/04_january_vs_july_comparison.png)

## Interpretation

The results suggest that Alberta's highest electricity prices in 2024 were associated with periods of high residual demand and system tightness.

Extreme prices tended to occur when demand was high, wind generation was low, dispatchable generation was heavily utilized, and Alberta relied more strongly on imports.

The analysis is observational and does not establish that any individual variable caused high prices. Electricity prices also depend on generator offers, transmission constraints, market rules, and other system conditions that were not modeled here.

## Tools

- R
- tidyverse
- ggplot2
- AESO market data
- AESO Generation Capacity API

## Project Structure

Alberta_Power_Analysis/
