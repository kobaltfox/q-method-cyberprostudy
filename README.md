# Q-Method Cybersecurity Study Analysis Application
# Author: Todd M. Fletcher (PhD Candidate)
# Affilication: Birmingham City University

This R Shiny application is designed to analyze my Q-methodology data from the 2024-2025 cybersecurity personalty and perspectives study, with a focus on cross-correlating findings with demographic information and personality trait inventories from participants. Learn more at https://kobaltfox.com/defcon32

## Overview

Q-methodology is a research method used to study people's subjective viewpoints on a topic. In a Q-methodology study, participants sort a set of statements (the Q-set) according to their level of agreement or disagreement. The resulting Q-sorts are then analyzed to identify shared viewpoints or perspectives among participants.

This application provides tools for:

1. **Q-Method Analysis**: Analyze Q-sort data to identify factors (shared viewpoints), factor loadings (how strongly each participant aligns with each factor), factor scores (how each statement ranks within each factor), and consensus/distinguishing statements.

2. **Demographic Analysis**: Analyze demographic data to understand the characteristics of the study participants and how these characteristics relate to the identified factors.

3. **Personality Analysis**: Analyze personality trait data to understand the psychological profiles of the study participants and how these traits relate to the identified factors.

4. **Correlation Analysis**: Analyze correlations between Q-method factors, demographic variables, and personality traits to identify relationships and patterns.

## Getting Started

### Data Requirements

The application requires the following data files:

1. **Q Statements CSV File**: Contains the statements used in the Q-sort. Required columns:
   - `id`: Unique identifier for each statement
   - `statement`: Text of the statement

2. **Q-Sort Data CSV File**: Contains the Q-sort data from participants. Required columns:
   - `participant_id`: Unique identifier for each participant
   - `statement_id`: Identifier of the statement being sorted
   - `score`: Score assigned to the statement by the participant

3. **Demographics CSV File** (optional): Contains demographic information about participants. Required columns:
   - `participant_id`: Unique identifier for each participant
   - Additional columns for demographic variables (e.g., age, gender, education)

4. **Personality Traits CSV File** (optional): Contains personality trait data for participants. Required columns:
   - `participant_id`: Unique identifier for each participant
   - Additional columns for personality traits (e.g., openness, conscientiousness)

### Using the Application

1. **Data Import**: Import your data files or use the sample data provided.

2. **Q-Method Analysis**: Run the Q-method analysis to identify factors, factor loadings, and factor scores.

3. **Demographic Analysis**: Analyze demographic variables and their relationship with Q-method factors.

4. **Personality Analysis**: Analyze personality traits and their relationship with Q-method factors.

5. **Correlation Analysis**: Analyze correlations between Q-method factors, demographic variables, and personality traits.

6. **Reports**: Generate reports of your analyses that can be downloaded as HTML files.

## Sample Data

The application includes sample data files for a cybersecurity study focusing on attitudes and beliefs about cybersecurity practices and policies. The sample data includes:

- 36 statements about cybersecurity
- Q-sort data from 20 participants
- Demographic data for the participants
- Personality trait data for the participants

## Technical Details

The application is built using the following R packages:

- `shiny` and `shinydashboard` for the web application framework
- `dplyr`, `tidyr`, and other tidyverse packages for data manipulation
- `psych` and `qmethod` for Q-methodology analysis
- `ggplot2` and `corrplot` for data visualization
- `DT` for interactive tables
- `markdown` and `rmarkdown` for report generation

## References

For more information about Q-methodology, see:

- Brown, S. R. (1980). Political subjectivity: Applications of Q methodology in political science. Yale University Press.
- Watts, S., & Stenner, P. (2012). Doing Q methodological research: Theory, method and interpretation. Sage.
