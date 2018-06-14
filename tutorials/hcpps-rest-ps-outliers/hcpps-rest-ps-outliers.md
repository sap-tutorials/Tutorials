---
title: Test the Outliers service
description: Using a REST client, you will test the Outliers SAP Predictive service
auto_validation: true
primary_tag: products>sap-predictive-service
tags: [ tutorial>beginner, topic>machine-learning, products>sap-predictive-service, products>sap-cloud-platform ]
---

## Prerequisites
  - **Proficiency:** Beginner
  - **Tutorials:** [Configure the SAP Predictive services](https://www.sap.com/developer/groups/ps-configure.html)

## Next Steps
  - Continue with: [Test the SAP Predictive services using a REST client](https://www.sap.com/developer/groups/ps-test-rest.html)
  - Or [Build an SAPUI5 application to interact with the SAP Predictive services](https://www.sap.com/developer/groups/ps-sapui5.html)

## Details

### You will learn
 - How to use the **Outliers** SAP Predictive services from a REST Client.

Only the synchronous mode will be tested here but you can mimic what was done in the [Test the Forecast SAP Predictive services using a REST client](https://www.sap.com/developer/tutorials/hcpps-rest-ps-forecast.html) tutorial for the asynchronous mode.

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

[ACCORDION-BEGIN [Info:](A short description of the Outliers service)]
The Outliers service identifies the odd profiles of a dataset whose target indicator is significantly different from what is expected.

This service:

  - Identifies outliers contained in a dataset with regard to a target indicator
  - Ranks the outliers to get the oddest on top
  - Provides the reasons why an identified outlier is odd

In general, an outlier can either result from a data quality issue to correct or represent a suspicious case to investigate.

An observation is considered an outlier if the difference between its ***predicted value*** and its ***real value*** exceeds the value of the error bar where the error bar is a deviation measure of the values around the predicted score.

Reasons will list the variables whose values have the most influence in the score. For each variables, the contribution corresponding to the score is compared to its contribution for the whole population. The variables for which the contribution is the most differential are selected as the most important reason.

**Note:** The target of the dataset must be either binary or continuous. Multinomial targets are not supported.

To summarize, in order to execute the outliers service, you need a dataset with:

  - a target variable
  - a set of variables that will be analyzed

Optionally, you can define the following parameters to enhance your analysis:

  - **number of outliers** : number of outliers to return
  - **number of reasons** : number of reasons to return for each outlier
  - **weight variable**: column to be used to increase the importance of a row
  - **skipped variables**: a list of variables to skip from the analysis
  - **variable description**: a more details description of the dataset
  - **weight variable**: a column to be used to increase the importance of a row

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Info:](A short description of the Census dataset)]

The dataset will be using during this tutorial is extracted from the sample dataset available with SAP Predictive Analytics.

The Census sample data file that you will use to follow the scenarios for Regression/Classification and Segmentation/Clustering is an excerpt from the American Census Bureau database, completed in 1994 by Barry Becker.

> **Note:** For more information about the American Census Bureau, see <http://www.census.gov> published on non-SAP site.

This file presents the data on 48,842 individual Americans, of at least 17 years of age. Each individual is characterized by 15 data items. These data, or variables, are described in the following table.

Variable | Description | Example of Values
:------------- | :-------------- | :--------------
<nobr>`age`</nobr> | Age of individuals | Any numerical value greater than 17
<nobr>`workclass`</nobr> | Employer category of individuals | Private, Self-employed-not-inc, ...
<nobr>`fnlwgt`</nobr> | Weight variable, allowing each individual to represent a certain percentage of the population | Any numerical value, such as 0, 2341 or 205019
<nobr>`education`</nobr> | Level of study, represented by a schooling level, or by the title of the degree earned | 11th, Bachelors
<nobr>`education_num`</nobr> | Number of years of study, represented by a numerical value | A numerical value between 1 and 16
<nobr>`marital_status`</nobr> | Marital status | Divorced, Never-married, ...
<nobr>`occupation`</nobr> | Job classification | Sales, Handlers-cleaners, ...
<nobr>`relationship`</nobr> | Position in family | Husband, Wife, ...
<nobr>`race`</nobr> | Ethnicity |
<nobr>`sex`</nobr> | Gender | Male, Female, ...
<nobr>`capital_gain`</nobr> | Annual capital gains | Any numerical value
<nobr>`capital_loss`</nobr> | Annual capital losses | Any numerical value
<nobr>`native country`</nobr> | Country of origin| United States, France, ...
<nobr>`class`</nobr> | Variable indicating whether or not the salary of the individual is greater or less than $50,000| ***1*** if the individual has a salary of greater than $50,000 & ***0*** if the individual has a salary of less than $50,000

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Register the Census dataset)]

As described in [**Step 1** of **Test the Dataset service** tutorial](https://www.sap.com/developer/tutorials/hcpps-rest-ps-dataset.html), register the Census dataset using the following elements:

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
    "table" : "Census"
  }
}
```

**Take note of the returned dataset identifier.**

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Run the Outliers service)]

Open a new tab in ***Postman***.

Fill in the following information:

Field Name     | Value
:------------- | :--------------
Request Type   | <code><b>POST</b></code>
URL            | <code><b>C4PAURL</b></code>`/api/analytics/outliers/sync`

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
  "datasetID": 9999999,
  "targetColumn": "class",
  "skippedVariables" : ["id", "sex", "race"],
  "variableDescription" : [
  	{"position" : "1", "variable" : "id", "storage" : "number" , "value" : "nominal" ,  "key" : "1"},
  	{"position" : "2", "variable" : "age", "storage" : "number" , "value" : "continuous"},
  	{"position" : "3", "variable" : "workclass", "storage" : "string" , "value" : "nominal" ,  "missing" : "?"},
  	{"position" : "4", "variable" : "fnlwgt", "storage" : "number" , "value" : "continuous"},
  	{"position" : "5", "variable" : "education", "storage" : "string" , "value" : "nominal"},
  	{"position" : "6", "variable" : "education_num", "storage" : "number" , "value" : "ordinal"},
  	{"position" : "7", "variable" : "marital_status", "storage" : "string" , "value" : "nominal"},
  	{"position" : "8", "variable" : "occupation", "storage" : "string" , "value" : "nominal" ,  "missing" : "?"},
  	{"position" : "9", "variable" : "relationship", "storage" : "string" , "value" : "nominal"},
  	{"position" : "10", "variable" : "race", "storage" : "string" , "value" : "nominal"},
  	{"position" : "11", "variable" : "sex", "storage" : "string" , "value" : "nominal"},
  	{"position" : "12", "variable" : "capital_gain", "storage" : "number" , "value" : "continuous" ,  "missing" : "99999"},
  	{"position" : "13", "variable" : "capital_loss", "storage" : "number" , "value" : "continuous"},
  	{"position" : "14", "variable" : "hours_per_week", "storage" : "number" , "value" : "continuous"},
  	{"position" : "15", "variable" : "native_country", "storage" : "string" , "value" : "nominal" ,  "missing" : "?"},
  	{"position" : "16", "variable" : "class", "storage" : "number" , "value" : "nominal"}
  ]
}
```
> Make sure the `datasetID` (here the value 9999999) is correct. To get the list of valid identifier, you can run ***Step 6: List all registered datasets*** from the [Test the Data Set SAP Predictive services using a REST client](https://www.sap.com/developer/tutorials/hcpps-rest-ps-dataset.html) tutorial

With these settings, you will get the list of entries where the difference between the ***predicted value*** and the ***real value*** exceeds the value of the error bar, which make these entries as odd entries or outliers.

Click on **Send**

Congratulations! You have just run the outliers service on the Census dataset.

You can see that about 350 records out of the 48842 are marked as outliers.

The list is sorted by descending order to give first the records with the highest difference.

Provide an answer to the question below then click on **Validate**.

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Additional test)]

You can also play with the following parameters and check the differences:

- **number of outliers** : ask for 10, 50 and 100
- **number of reasons** : ask for 1, 5 and 10
- **skipped variables**: exclude `marital_status`
- **variable description**: for example `education_num` as an ordinal variable

[DONE]
[ACCORDION-END]

### Optional

For more details on the SAP Predictive services, you can check the following the [`Outliers APIs`](https://help.sap.com/viewer/20cd1b0396db4826a9b76b4ce869f00a/Cloud/en-US/0c5454d0e1c840588fc072de5cad2474.html) documentation.

## Next Steps
  - Continue with: [Test the SAP Predictive services using a REST client](https://www.sap.com/developer/groups/ps-test-rest.html)
  - Or [Build an SAPUI5 application to interact with the SAP Predictive services](https://www.sap.com/developer/groups/ps-sapui5.html)
