---
title: SAP Cloud Platform predictive services, Test the "Dataset" SAP Cloud Platform predictive services using a REST client
description: Using a REST client, you will test the "Dataset" SAP Cloud Platform predictive services
tags: [ tutorial>beginner, products>sap-hana, products>sap-cloud-platform ]
---

## Prerequisites
  - **Proficiency:** Beginner
  - **Tutorials:** [Access your predictive demo data set using a HANA XS OData services from a REST client](http://www.sap.com/developer/tutorials/hcpps-rest-odata.html)

## Next Steps
  - [Test the "Forecast" SAP Cloud Platform predictive service from a REST client](http://www.sap.com/developer/tutorials/hcpps-rest-ps-forecast.html)

## Details
### You will learn
  - How to "Register" your data set with the SAP Cloud Platform predictive services using a REST Client.
  In order to use any of the SAP Cloud Platform predictive services, you will need a registered dataset. When registering a dataset, you will define and store the structure of the dataset which will be used by the other services.
  We upload 3 datasets in one of the previous steps, but we will be using the `CashFlow` dataset as an example here. You can replicate the steps for the other 2 datasets.

### Time to Complete
  **10 minutes**

> In order to ease the readability of this tutorial, we have used tokens to replace long URLs.
Therefore you can replace any occurrence of the token by the value listed above.
>
> Token               | Value
------------------- | -------------
<code><b>&lt;Account name&gt;</b></code>  | your SAP Cloud Platform account name. On a developer trial account, it should end by `trial`
<code><b>&lt;C4PA URL&gt;</b></code> | `http://aac4paservices<`<code><b>Account name</b></code>`>.hanatrial.ondemand.com/com.sap.aa.c4pa.services`
>
> If you are unclear with what is your SAP Cloud Platform account name, you can refer to the following blog entry: [SAP Cloud Platform login, user name, account id, name or display name: you are lost? Not anymore!](https://blogs.sap.com/2017/01/31/sap-hana-cloud-platform-trial-login-name-user-name-account-name-account-identifier-you-are-lost-not-anymore/)

[ACCORDION-BEGIN [Step 1: ](Register a dataset)]
Open a new tab in ***Postman***.

Fill in the following information

Field Name     | Value
-------------- | --------------
Request Type   | `POST`
URL            | `<`<code><b>C4PA URL</b></code>`>/api/analytics/dataset/sync`

![Postman URL](01.png)

Select the **Authorization** tab and fill in the following information:

Field Name     | Value
-------------- | -------------
Type           | `Basic Auth`
Username       | your ***SAP Cloud Platform Account*** login*
Password*      | your ***SAP Cloud Platform Account*** password

**Note:**
  Your SAP Cloud Platform Account login is usually the email address used to register your ***SAP Cloud Platform*** account.

![Postman URL](02.png)

Select the **Body** tab, enable the **raw** mode and select `JSON (application/json)` in the drop down, then add the following entries

```json
{
"hanaURL":"DEMO/CashFlow"
}
```

![Postman URL](03.png)

Click on **Send**

Click on **Send** an additional 4 or 5 times, so you will have multiple dataset registered.

1. It will now display the data set registration identifier, the number of rows and the variable descriptions.

Now, we can use the `ID` value to reference the registered dataset with other SAP Cloud Platform predictive services calls.

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
>  - <<code><b>C4PA URL</b></code>>/adminUI/index.html#/about

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
For more details on the SAP Cloud Platform predictive services, you can check the following URL:
  - `<`<code><b>C4PA URL</b></code>`>/raml/index.html?raml=../aa-cloud-services.raml`

## Next Steps
  - [Test the "Forecast" SAP Cloud Platform predictive service from a REST client](http://www.sap.com/developer/tutorials/hcpps-rest-ps-forecast.html)
