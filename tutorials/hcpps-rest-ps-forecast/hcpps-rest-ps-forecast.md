---
title: Test the Forecast service
description: Using a REST client, you will test the Forecast SAP Predictive service
auto_validation: true
primary_tag: products>sap-predictive-service
tags: [ tutorial>beginner, topic>machine-learning, products>sap-predictive-service, products>sap-cloud-platform ]
---

## Prerequisites
  - **Proficiency:** Beginner
  - **Tutorials:** [Configure the SAP Predictive services](https://www.sap.com/developer/groups/ps-configure.html)

## Next Steps
- [Test the SAP Predictive services using a REST client](https://www.sap.com/developer/groups/ps-test-rest.html)

## Details
### You will learn
 - How to use the **Forecast** SAP Predictive services from a REST Client in both synchronous and asynchronous mode.
 The asynchronous mode will create a job with a status that you can check the status for completion.

> ### **Note**: if you are running into some issue, you can check the [SAP Predictive services Troubleshooting guide](https://www.sap.com/developer/tutorials/hcpps-troubleshoot.html) to diagnose the most common ones.

### Time to Complete
  **10 minutes**

[ACCORDION-BEGIN [Info: ](Application URL)]

In order to ease the readability of this tutorial, the **C4PAURL** token was used
 to replace the predictive services **Application URL** displayed on the overview page.

Therefore you can replace any occurrence of the token by your value listed.

The **Application URL** should look like this (where ***XYZ*** is your SAP Cloud Platform account name):

```url
https://aac4paservicesXYZ.hanatrial.ondemand.com/com.sap.aa.c4pa.services
```

If you are unclear with what is your SAP Cloud Platform account name, you can refer to the following blog entry: [SAP Cloud Platform login, user name, account id, name or display name: you are lost? Not anymore!](https://blogs.sap.com/2017/01/31/sap-hana-cloud-platform-trial-login-name-user-name-account-name-account-identifier-you-are-lost-not-anymore/)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Info:](A short description of the Forecast service)]
The Forecasts service analyzes a dataset containing the successive values of a target indicator over time to predict the next values.

This service:

 - Analyzes a time series and generates forecasts based on identified patterns
 - Returns forecast and real values for past data of the time series
 - Provides confidence intervals computed for each forecast
 - Provides the trend, cycles, and fluctuations of the time series
 - Provides indicators on the reliability of the results

The predictive model combines the trend, cycles, and fluctuations found in the time series to generate forecasts. The prediction also depends on information provided through extra-predictive variables if any.

The granularity of the prediction is the same as the granularity used in the dataset. For example, if the dataset contains daily observations of a time series, the service computes the values of the series in the next days. See the Time Series Scenarios on the SAP Help Portal for a description of the time series components.

> **Note:**
> If you use extra-predictive variables, which are variables other than date and target indicator, their values must be known for each date of the forecasts.
> The service may return forecasts without error bars beyond the maximum confident horizon.

To summarize, in order to execute the forecast service, you will need a dataset with:

 - a date variable
 - a variable to predict (usually a continuous number variable), the target variable
 - optionally a set of ***extra*** predictors which can be used to better predict the forecast variable

The parameters required to run the service are:

 - a dataset identifier (registered with the Dataset service)
 - the name of the date and the target variable
 - a number of forecast to be computed

Optionally, you can define the following parameters to enhance your analysis:

 - **the reference date** (date after which the entries are call predictions), if none is specified then the last date will be used
 - **forecast method**: the method to use to generate the forecasts (default, smoothing or linear regression)
 - **max lag**: the maximum lag to consider to compute forecasts
 - **number of past values in the output**: the number of past data to return with the forecasts (default value is 0)
 - **skipped variables**: a list of variables to skip from the analysis
 - **smoothing cycle length**: the length to consider for a cycle. This parameter is enforced only when using smoothing techniques
 - **variable description**: a more details description of the dataset

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Info:](A short description of the Cash Flow dataset)]

The dataset will be using during this tutorial is extracted from the sample dataset available with SAP Predictive Analytics.

The file presents daily measures of cash flows from January 2, 1998 to September, 30 1998. Each observation is characterized by 25 data items. The data or variables are described in the following table.

Variable | Description | Example of values
:--------------|--------------|--------------
Date | Day, month and year of the readings | A date in the format `yyyy-mm-dd` such as 1998-01-02
<nobr>`Cash`</nobr> | Cash flow | A numerical value with n decimals
<nobr>`BeforeLastMonday`</nobr><br><nobr>`LastMonday`</nobr><br><nobr>`BeforeLastTuesday`</nobr><br><nobr>`LastTuesday`</nobr><br><nobr>`BeforeLastWednesday`</nobr><br><nobr>`LastWednesday`</nobr><br><nobr>`BeforeLastThursday`</nobr><br><nobr>`LastThursday`</nobr><br><nobr>`BeforeLastFriday`</nobr><br><nobr>`LastFriday`</nobr> | Boolean variables that indicate if the information is true or false | 1 if the information is true.
`Last5WDays`</nobr><br><nobr>`Last4WDays`</nobr> | Boolean variables that indicate if the date is in the 5 or 4 last working days of the month | 1 if the information is true.
<nobr>`LastWMonth`</nobr><br><nobr>`BeforeLastWMonth`</nobr> | Boolean variables that indicate if the information is true or false | 1 if the information is true.
<nobr>`WorkingDaysIndices`</nobr><br><nobr>`ReverseWorkingDaysIndices`</nobr> | Indices or reverse indices of the working days | An integer value
<nobr>`MondayMonthInd`</nobr><br><nobr>`TuesdayMonthInd`</nobr><br><nobr>`WednesdayMonthInd`</nobr><br><nobr>`ThursdayMonthInd`</nobr><br><nobr>`FridayMonthInd`</nobr> | Indices of the week days in the month | An integer value
<nobr>`Last5WDaysInd`</nobr><br><nobr>`Last4WDaysInd`</nobr> | Indices of the 5 or 4 last working days of the month | An integer value

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Register the Cash Flow dataset)]

First you need to register the dataset you will be using during this tutorial.

Additionally, you will adjust the variables description which will help improve the quality of our model.

As described in [**Step 1** of **Test the Dataset services** tutorial](https://www.sap.com/developer/tutorials/hcpps-rest-ps-dataset.html), register the Cash Flow dataset using the following elements:

Open a new tab in ***Postman***.

> If you don't have ***Postman*** installed yet, you can refer to the following how-to guide: [Install Postman extension for Google Chrome as a REST client](https://www.sap.com/developer/tutorials/api-tools-postman-install.html)

Fill in the following information:

Field Name     | Value
:------------- | :--------------
Request Type   | <code><b>POST</b></code>
URL            | <code><b>C4PAURL</b></code>`/api/analytics/dataset/sync`

Select the **Body** tab, enable the **raw** mode and select `JSON (application/json)` in the drop down, then add the following content:

```json
{
  "location": {
    "schema" : "PSDEMO",
    "table" : "CashFlow"
  }
}
```

**Take note of the returned dataset identifier.**

Now as described in the **Step 4: Modify registered variable details**,

Field Name     | Value
:------------- | :--------------
Request Type   | <code><b>POST</b></code>
URL            | <code><b>C4PAURL</b></code>`/api/analytics/dataset/`<code><b>ID</b></code>`/variables/update`

> Make sure you replace the <code><b>ID</b></code> token in the URL with the one returned by the previous service call.

Select the **Body** tab, enable the **raw** mode and select `JSON (application/json)` in the drop down, then add the following content:

```json
[
  {"name":"Date", "value":"continuous"},
  {"name":"WorkingDaysIndices", "value":"continuous"},
  {"name":"ReverseWorkingDaysIndices", "value":"continuous"},
  {"name":"MondayMonthInd", "value":"ordinal"},
  {"name":"TuesdayMonthInd", "value":"ordinal"},
  {"name":"WednesdayMonthInd", "value":"ordinal"},
  {"name":"ThursdayMonthInd", "value":"ordinal"},
  {"name":"FridayMonthInd", "value":"ordinal"},
  {"name":"BeforeLastMonday", "value":"nominal"},
  {"name":"LastMonday", "value":"nominal"},
  {"name":"BeforeLastTuesday", "value":"nominal"},
  {"name":"LastTuesday", "value":"nominal"},
  {"name":"BeforeLastWednesday", "value":"nominal"},
  {"name":"LastWednesday", "value":"nominal"},
  {"name":"BeforeLastThursday", "value":"nominal"},
  {"name":"LastThursday", "value":"nominal"},
  {"name":"BeforeLastFriday", "value":"nominal"},
  {"name":"LastFriday", "value":"nominal"},
  {"name":"Last5WDaysInd", "value":"ordinal"},
  {"name":"Last5WDays", "value":"nominal"},
  {"name":"Last4WDaysInd", "value":"ordinal"},
  {"name":"Last4WDays", "value":"nominal"},
  {"name":"LastWMonth", "value":"nominal"},
  {"name":"BeforeLastWMonth", "value":"nominal"},
  {"name":"Cash", "value":"continuous"}
]
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Run the Forecast service in synchronous mode)]

With the synchronous mode, the SAP Predictive service will be waiting until the operation completes to return the response.

Open a new tab in ***Postman***.

Fill in the following information:

Field Name     | Value
:------------- | :--------------
Request Type   | <code><b>POST</b></code>
URL            | <code><b>C4PAURL</b></code>`/api/analytics/forecast/sync`
![Postman URL](01.png)

Select the **Authorization** tab and fill in the following information:

Field Name     | Value
:------------- | :-------------
Type           | **`Basic Auth`**
Username       | your ***SAP Cloud Platform Account*** login*
Password*      | your ***SAP Cloud Platform Account*** password

>**Note:**
Your SAP Cloud Platform Account login is usually the email address used to register your ***SAP Cloud Platform*** account.

![Postman URL](02.png)

Select the **Body** tab, enable the **raw** mode and select `JSON (application/json)` in the drop down, then add the following content:

```json
{
  "datasetID": 1,
  "targetColumn": "Cash",
  "dateColumn": "Date",
  "numberOfForecasts": 5,
  "referenceDate" : "2001-12-01"
}
```

> Make sure the `datasetID` (here the value 9999999) is correct. To get the list of valid identifier, you can run ***Step 6: List all registered datasets*** from the [Test the Data Set SAP Predictive services using a REST client
](https://www.sap.com/developer/tutorials/hcpps-rest-ps-dataset.html) tutorial

With these settings, you will forecast the next 5 values of the Cash variable after the 1st of December 2001.

Click on **Send**

Congratulations! You have just run the forecast service on the `Cash` variable and requested the next 5 values after the reference date (2001-12-01).

In the output you will get the following information:

  - **`forecastValue`**: the forecast values
  - **`realValue`**: the current values in case you have provided a reference date where you already have the target value, and your goal is more to confirm that the real value follows a trend or is between boundaries
  - **`errorBarHigherBound`** & **`errorBarLowerBound`**: the upper and lower limit of the confidence interval for the forecast value (+/-5%)
  - **model information**: the structure of the forecast model
  - **model performance**: the accuracy indicators

Provide an answer to the question below then click on **Validate**.

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Run the Forecast service in asynchronous mode)]

With the asynchronous mode, the process is split across 3 service calls, where the first will post the request, the second will be used to check the status and the third will get the response.

Open a new tab in ***Postman***.

Fill in the following information

Field Name     | Value
:------------- | :--------------
Request Type   | <code><b>POST</b></code>
URL            | <code><b>C4PAURL</b></code>`/api/analytics/forecast`

As you can notice the only difference here, is that you don't use the ***sync*** keyword in the URL.

Select the **Authorization** tab and fill in the same details as in the previous call.

Select the **Body** tab, enable the **raw** mode and select `JSON (application/json)` in the drop down, then add the following content:

```json
{
  "datasetID": 1,
  "targetColumn": "Cash",
  "dateColumn": "Date",
  "numberOfForecasts": 5,
  "referenceDate" : "2001-12-01"
}
```
> Make sure the `datasetID` (here the value 9999999) is correct. To get the list of valid identifier, you can run ***Step 6: List all registered datasets*** from the [Test the Data Set SAP Predictive services using a REST client
](https://www.sap.com/developer/tutorials/hcpps-rest-ps-dataset.html) tutorial

Click on **Send**

Here is the result:

```
{
    "ID": 3,
    "status": "NEW",
    "type": "forecasts",
    "input": {
        "dateColumn": "Date",
        "numberOfForecasts": 5,
        "referenceDate": "2001-12-01",
        "targetColumn": "Cash",
        "datasetID": 1
    }
}
```

Now, you can use the <code><b>ID</b></code> value representing the forecast job identifier to get its status.

Open a new tab in ***Postman***.

Fill in the following information

Field Name     | Value
:------------- | :--------------
Request Type   | <code><b>GET</b></code>
URL            | <code><b>C4PAURL</b></code>`/api/analytics/forecast/1/status`

Select the **Authorization** tab and fill in the same details as in the previous call.

Click on **Send**

Here is the result:

```
{
  "ID": 1,
  "status": "SUCCESSFUL",
  "type": "forecasts"
}
```

The forecasting call was successful, now let's get the result:

Open a new tab in ***Postman***.

Fill in the following information:

Field Name     | Value
:------------- | :--------------
Request Type   | <code><b>GET</b></code>
URL            | <code><b>C4PAURL</b></code>`/api/analytics/forecast/`<code><b>ID</b></code>

> Make sure to replace the `ID` by your Job ID as returned in the previous step

Select the **Authorization** tab and fill in the same details as in the previous call.

Click on **Send**

You should obtain the same output as with the Synchronous mode.

Ultimately, you can delete the job and its content using a the following details:

Field Name     | Value
:------------- | :--------------
Request Type   | <code><b>DELETE</b></code>
URL            | <code><b>C4PAURL</b></code>`/api/analytics/forecast/`<code><b>ID</b></code>

> Make sure to replace the `ID` by your Job ID as returned in the previous step

[DONE]
[ACCORDION-END]

### Optional

For more details on the SAP Predictive services, you can check the following the [`Forecasts APIs`](https://help.sap.com/viewer/20cd1b0396db4826a9b76b4ce869f00a/Cloud/en-US/8c4150c7711a4d1b865ea9628597d3e0.html) documentation.

## Next Steps
- [Test the SAP Predictive services using a REST client](https://www.sap.com/developer/groups/ps-test-rest.html)
