---
title: Test the Dataset service
description: Using a REST client, you will test the Dataset SAP Predictive service
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
  - How to **Register** and **Manage** your data set with the SAP Predictive services using a REST Client.

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

[ACCORDION-BEGIN [Info:](A short description of the Dataset service)]

In order to use any of the SAP Predictive services, you will need a registered dataset.

When registering a dataset, you will define and store the structure of the dataset which will be used by the other services.

You uploaded 3 datasets in one of the previous steps, but here you will be using the `CashFlow` dataset as an example.

You can replicate the steps for the other 2 datasets.

This service:

 - Analyze the database object DDL to return you the name and the data type of each columns
 - Analyze a few hundreds rows to determine the value type of the column (continuous, nominal or ordinal)
 - Get the number of rows and columns

Once registered, you will be able to use the dataset ***ID*** to call the other services.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Register a dataset)]
Open a new tab in ***Postman***.

> If you don't have ***Postman*** installed yet, you can refer to the following how-to guide: [Install Postman extension for Google Chrome as a REST client](https://www.sap.com/developer/tutorials/api-tools-postman-install.html)

Fill in the following information:

Field Name     | Value
:------------- | :--------------
Request Type   | <code><b>POST</b></code>
URL            | <code><b>C4PAURL</b></code>`/api/analytics/dataset/sync`

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

Select the **Body** tab.

Enable the **raw** mode.

Pick **`JSON (application/json)`** in the drop down (instead of Text).

![Postman URL](03.png)

Select the **Body** tab, enable the **raw** mode and select `JSON (application/json)` in the drop down, then add the following content:

```json
{
  "location": {
    "schema" : "PSDEMO",
    "table" : "CashFlow"
  }
}
```

Click on **Send**

It will now display the data set registration identifier, the number of rows and the variable descriptions.

Now, you can use the <code><b>ID</b></code> value to reference the registered dataset with other SAP Predictive services calls.

Click on **Send** an additional 2 or 3 times, so you will have multiple dataset registered to play with.

Provide an answer to the question below then click on **Validate**.

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Retrieve the registration details for one dataset)]
Open a new tab in ***Postman***.

Fill in the following information:

Field Name     | Value
:------------- | :-------------
Request Type   | <code><b>GET</b></code>
URL            | <code><b>C4PAURL</b></code>`/api/analytics/dataset/`<code><b>ID</b></code>

> Make sure you replace the <code><b>ID</b></code> token in the URL with the one returned by the dataset <code><b>ID</b></code> return in the previous service call.

Select the **Authorization** tab and fill in the same details as in the previous call.

Click on **Send**

You should receive the same description as from the previous call.

If you try with an unknown <code><b>ID</b></code>, you should receive a 404 response.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Retrieve the registration details for one variable)]
Open a new tab in ***Postman***.

Fill in the following information

Field Name     | Value
:------------- | :-------------
Request Type   | <code><b>GET</b></code>
URL            | <code><b>C4PAURL</b></code>`/api/analytics/dataset/`<code><b>ID</b></code>`/variable/1`

> Make sure you replace the <code><b>ID</b></code> token in the URL with the one returned by the dataset <code><b>ID</b></code> return in the first service call.

Select the **Authorization** tab and fill in the same details as in the previous call.

Click on **Send**.

You should receive the description of the first variable from your data set.

Provide an answer to the question below then click on **Validate**.

[VALIDATE_2]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Modify registered variable details)]

> **Note:**
>This service is available since version 1.7, please make sure you have upgraded the C4PA application, else you will receive a 404 error.
>
>To check which version you are currently using, please go to the following URL and check the `X-Maven-Project-Version` property:
>  - <code><b>C4PAURL</b></code>`/adminUI/index.html#/about`

By default, the variable storage and value type (nominal, continuous,  ordinal) properties returned by the registration service are ***guessed*** from the data, which may sometime be inaccurate.

For example, with variable 4 (`MondayMonthInd`), it is guessed as a continuous integer, but should rather be an ordinal integer.

Let's see how you can fix that.

Open a new tab in ***Postman***.

Fill in the following information:

Field Name     | Value
:------------- | :--------------
Request Type   | <code><b>POST</b></code>
URL            | <code><b>C4PAURL</b></code>`/api/analytics/dataset/`<code><b>ID</b></code>`/variables/update`

> Make sure you replace the <code><b>ID</b></code> token in the URL with the one returned by the dataset <code><b>ID</b></code> return in the first service call.

Select the **Authorization** tab and fill in the same details as in the previous call.

Select the **Body** tab, enable the **raw** mode and select `JSON (application/json)` in the drop down, then add the following entries:

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

Fill in the following information:

Field Name     | Value
:------------- | :-------------
Request Type   | <code><b>DELETE</b></code>
URL            | <code><b>C4PAURL</b></code>`/api/analytics/dataset/`<code><b>ID</b></code>

> Make sure you replace the <code><b>ID</b></code> token in the URL with the one returned by the dataset <code><b>ID</b></code> return in the first service call.

Select the **Authorization** tab and fill in the same details as in the previous call.

Click on **Send**.

You have now deleted the second data set you have registered.

You can try to retrieve the registration details as in step 2 for dataset 2, and you should receive an error message.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](List all registered datasets)]

Open a new tab in ***Postman***.

Fill in the following information:

Field Name     | Value
:------------- | :-------------
Request Type   | <code><b>GET</b></code>
URL            | <code><b>C4PAURL</b></code>`/api/analytics/dataset/`

Select the **Authorization** tab and fill in the same details as in the previous call.

Click on **Send**.

You have now listed all the data set registered in your environment.

[DONE]
[ACCORDION-END]

### Optional

For more details on the SAP Predictive services, you can check the following the [`Dataset APIs`](https://help.sap.com/viewer/20cd1b0396db4826a9b76b4ce869f00a/Cloud/en-US/ac9dc4ed145646a0b76b4729210fd067.html) documentation.

## Next Steps
  - Continue with: [Test the SAP Predictive services using a REST client](https://www.sap.com/developer/groups/ps-test-rest.html)
  - Or [Build an SAPUI5 application to interact with the SAP Predictive services](https://www.sap.com/developer/groups/ps-sapui5.html)
