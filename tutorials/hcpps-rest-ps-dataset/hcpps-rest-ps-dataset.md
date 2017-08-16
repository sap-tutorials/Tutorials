---
title: Test the "Dataset" service
description: Using a REST client, you will test the "Dataset" SAP Cloud for predictive services
primary_tag: products>sap-cloud-platform-predictive-service
tags: [ tutorial>beginner, topic>machine-learning, products>sap-cloud-platform-predictive-service, products>sap-cloud-platform ]
---

## Prerequisites
  - **Proficiency:** Beginner
  - **Tutorials:** [Configure the SAP Cloud Platform predictive services](https://www.sap.com/developer/groups/ps-configure.html)

## Next Steps
  - [Test the "Forecast" service](http://www.sap.com/developer/tutorials/hcpps-rest-ps-forecast.html)

## Details
### You will learn
  - How to "Register" your data set with the SAP Cloud for predictive services using a REST Client.


### Time to Complete
  **10 minutes**

> In order to ease the readability of this tutorial, we have used tokens to replace long URLs.
Therefore you can replace any occurrence of the token by the value listed above.
>
> Token               | Value
------------------- | -------------
<code><b>&lt;Account name&gt;</b></code>  | your SAP Cloud Platform account name. On a developer trial account, it should end by `trial`
<code><b>&lt;C4PA URL&gt;</b></code> | `https://aac4paservices<`<code><b>Account name</b></code>`>.hanatrial.ondemand.com/com.sap.aa.c4pa.services`
>
> If you are unclear with what is your SAP Cloud Platform account name, you can refer to the following blog entry: [SAP Cloud Platform login, user name, account id, name or display name: you are lost? Not anymore!](https://blogs.sap.com/2017/01/31/sap-hana-cloud-platform-trial-login-name-user-name-account-name-account-identifier-you-are-lost-not-anymore/)

[ACCORDION-BEGIN [Info:](A short description of the Dataset service)]

In order to use any of the SAP Cloud for predictive services, you will need a registered dataset.

When registering a dataset, you will define and store the structure of the dataset which will be used by the other services.

We upload 3 datasets in one of the previous steps, but we will be using the `CashFlow` dataset as an example here. You can replicate the steps for the other 2 datasets.

This service:

 - Analyze the database object DDL to return you the name and the data type of each columns
 - Analyze a few hundreds rows to determine the value type of the column (continuous, nominal or ordinal)
 - Get the number of rows and columns

Once registered, you will be able to use the dataset "ID" to call the other services.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Register a dataset)]
Open a new tab in ***Postman***.

> If you don't have ***Postman*** installed yet, you can refer to the following how-to guide: [Install Postman extension for Google Chrome as a REST client](https://www.sap.com/developer/how-tos/2017/07/api-tools-postman-install.html)

Fill in the following information:

Field Name     | Value
-------------- | --------------
Request Type   | **`POST`**
URL            | **`<`<code><b>C4PA URL</b></code>`>/api/analytics/dataset/sync`**

![Postman URL](01.png)

Select the **Authorization** tab and fill in the following information:

Field Name     | Value
-------------- | -------------
Type           | **`Basic Auth`**
Username       | your ***SAP Cloud Platform Account*** login*
Password*      | your ***SAP Cloud Platform Account*** password

>**Note:**
Your SAP Cloud Platform Account login is usually the email address used to register your ***SAP Cloud Platform*** account.

-

![Postman URL](02.png)

Select the **Body** tab.

Enable the **raw** mode.

Pick **`JSON (application/json)`** in the drop down (instead of Text).

![Postman URL](03.png)

Add the following content in the text area as displayed above:

```json
{
  "location": {
    "schema" : "DEMO",
    "table" : "CashFlow"
  }
}
```

Click on **Send**

It will now display the data set registration identifier, the number of rows and the variable descriptions.

Now, we can use the `ID` value to reference the registered dataset with other SAP Cloud for predictive services calls.

The below extract was shortened to ease the reading.
```
{
  "ID": 1,
  "name": "CashFlow",
  "numberOfColumns": 25,
  "numberOfRows": 272,
  "variables": [
    {
      "name": "Date",
      "position": 0,
      "storage": "date",
      "value": "continuous"
    },
    {
      "name": "WorkingDaysIndices",
      "position": 1,
      "storage": "integer",
      "value": "continuous"
    },
    ....
  ]
}
```

Click on **Send** an additional 4 or 5 times, so you will have multiple dataset registered.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Retrieve the registration details for one dataset)]
Open a new tab in ***Postman***.

Fill in the following information:

Field Name     | Value
:------------- | :-------------
Request Type   | `GET`
URL            | `<`<code><b>C4PA URL</b></code>`>/api/analytics/dataset/1`

Select the **Authorization** tab and fill in the same details as in the previous call.

Click on **Send**

You should receive the same description as from the previous call.

If you try with an unknown `ID`, you should receive an error message.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Retrieve the registration details for one variable)]
Open a new tab in ***Postman***.

Fill in the following information

Field Name     | Value
:------------- | :-------------
Request Type   | `GET`
URL            | `<`<code><b>C4PA URL</b></code>`>/api/analytics/dataset/1/variable/1`

Select the **Authorization** tab and fill in the same details as in the previous call.

Click on **Send**

You should receive the description of the first variable from your data set.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Modify registered variable details)]

>**Note:**
>This service is available since version 1.7, please make sure you have upgraded the C4PA application, else you will receive a 404 error.
>
>To check which version you are currently using, please go to the following URL and check the `X-Maven-Project-Version` property:
>  - `<<code><b>C4PA URL</b></code>>/adminUI/index.html#/about`

-

By default, the variable storage and value type (nominal, continuous,  ordinal) properties returned by the registration service are "guessed" from the data, which may sometime be inaccurate.

For example, with variable 4 (`MondayMonthInd`), it is guessed as a continuous integer, but should rather be an ordinal integer.

Let's see how we can fix that.

Open a new tab in ***Postman***.

Fill in the following information:

Field Name     | Value
-------------- | --------------
Request Type   | `POST`
URL            | `<`<code><b>C4PA URL</b></code>`>/api/analytics/dataset/1/variables/update`

Select the **Authorization** tab and fill in the same details as in the previous call.

Select the **Body** tab, enable the **raw** mode and select `JSON (application/json)` in the drop down, then add the following entries

```json
[
    {
     "name": "MondayMonthInd",
     "value": "ordinal"
    }
]
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Delete a dataset registration details)]

Open a new tab in ***Postman***.

Fill in the following information

Field Name     | Value
:------------- | :-------------
Request Type   | `DELETE`
URL            | `<`<code><b>C4PA URL</b></code>`>/api/analytics/dataset/2`

Select the **Authorization** tab and fill in the same details as in the previous call.

Click on **Send**

You have now deleted the second data set we have registered.

You can try to retrieve the registration details as in step 2 for dataset 2, and you should receive an error message.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](List all registered datasets)]

Open a new tab in ***Postman***.

Fill in the following information

Field Name     | Value
:------------- | :-------------
Request Type   | `GET`
URL            | `<`<code><b>C4PA URL</b></code>`>/api/analytics/dataset/`

Select the **Authorization** tab and fill in the same details as in the previous call.

Click on **Send**

You have now listed all the data set registered in your environment.

[DONE]
[ACCORDION-END]

### Optional
For more details on the SAP Cloud for predictive services, you can check the following URL that can also allow you to run the service:
  - `<`<code><b>C4PA URL</b></code>`>/raml/console/index.html?raml=../api/aa-cloud-services.raml`
Or the public documentation
  - [`https://help.hana.ondemand.com/c4pa/api/aa-cloud-services.html#api_analytics_forecast_post`](https://help.hana.ondemand.com/c4pa/api/aa-cloud-services.html#api_analytics_forecast_post)

## Next Steps
  - [Test the "Forecast" service](http://www.sap.com/developer/tutorials/hcpps-rest-ps-forecast.html)
