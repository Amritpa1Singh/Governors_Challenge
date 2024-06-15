# Governor's Challenge Forecasts

This repository contains R scripts used for generating monthly and rotating quarterly forecasts for the Bank of Canada's Governor's Challenge.

## Files

### [Governor's Challenge Monthly Forecasts Version 2.R](https://github.com/Amritpa1Singh/Governors_Challenge/blob/main/Governor's%20Challenge%20Monthly%20Forecasts%20Version%202.R)

This script is designed to produce monthly forecasts. It includes data preprocessing, model training, and prediction generation. The key steps in the script are as follows:

- **Data Loading and Preprocessing**: Load the dataset and perform necessary preprocessing steps, such as handling missing values, feature engineering, and scaling.
- **Model Selection**: Choose appropriate forecasting models based on the data characteristics.
- **Training and Validation**: Train the models on the training data and validate their performance using cross-validation or a holdout validation set.
- **Prediction**: Generate monthly forecasts and evaluate their accuracy.

### [Rotating Quarterly Forecasts.R](https://github.com/Amritpa1Singh/Governors_Challenge/blob/main/Rotating%20Quarterly%20Forecasts.R)

This script focuses on generating rotating quarterly forecasts. The main processes in this script include:

- **Data Preparation**: Prepare the dataset for quarterly forecasting, including handling any seasonality and trend components.
- **Model Implementation**: Implement models that are suitable for quarterly data, ensuring they can capture any cyclic patterns present in the data.
- **Forecast Generation**: Produce forecasts for each quarter, rotating through the data to maintain up-to-date predictions.
- **Model Evaluation**: Assess the performance of the forecasts using appropriate evaluation metrics.

### [2024 Governor's Challenge Final Round.pdf](https://github.com/Amritpa1Singh/Governors_Challenge/blob/main/2024%Governor's%Challenge%Final%Round.pdf)

This PowerPoint presentation provides an overview of the final round of the 2024 Governor's Challenge. It includes key insights, methodologies, and results of our forecasts.

## Usage

1. **Clone the Repository**:
    ```
    git clone https://github.com/Amritpa1Singh/Governors_Challenge.git
    ```

2. **Navigate to the Repository**:
    ```
    cd Governors_Challenge
    ```

3. **Install Required Packages**: Ensure you have the necessary R packages installed. You can install them using the following command in R:
    ```
    install.packages(c("package1", "package2", ...))
    ```

4. **Run the Scripts**: Open the R scripts in your R environment (such as RStudio) and execute them to generate the forecasts.

## Contributing

Feel free to contribute to this project by submitting pull requests or opening issues for any bugs or feature requests.
