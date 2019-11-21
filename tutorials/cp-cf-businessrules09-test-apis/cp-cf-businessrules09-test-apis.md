---
title: Test the Business Rules API from SAP API Business Hub
description: Use SAP API Business Hub to test and run the business rules API to see how business rules can be consumed from a custom application.
auto_validation: true
time: 10
tags: [ tutorial>beginner, topic>cloud, topic>cloud,products>sap-cloud-platform,products>sap-cloud-platform-for-the-cloud-foundry-environment, topic>sap-api-business-hub]
primary_tag: products>sap-cloud-platform-business-rules
---

## Prerequisites
 - You have assigned the roles **`RuleRuntimeSuperUser`** and **`RuleRepositorySuperUser`** to you in SAP Cloud Platform Cockpit. For more information, see [Assign Roles to Users for Managing Business Rules](cp-cf-businessrules02-assign-roles).

## Details
### You will learn
  - How to use Business Rules APIs
  - How to test Business Rules project

SAP Cloud Platform Business Rules REST APIs are available on SAP API Business Hub which lets you execute rules from custom applications and external REST Clients. Since these APIs are based on OAuth 2.0, you need the service key parameters to use the APIs.

Note the following parameters from the service keys of your business rules instance:

- `clientid`
- `clientsecret`
- `url`

![Configure environment](testing5.png)


[ACCORDION-BEGIN [Step 1: ](Log onto SAP API Business Hub)]

1. Log on to [SAP API Business Hub](https://api.sap.com/)

    ![API Hub Homepage](testing1.png)

2. In the search bar, search for **Business rule**. From the search results, select the API package for SAP Cloud Platform Business Rules.

    ![Search bar](testing2.png)

3. Choose the **Rule Execution API for Cloud Foundry** tile of **Version v2**.

    ![Rule Execution API tile](testing3.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Configure the environment)]

Here, you need to configure the environment to link it to the business rules project which you have created in Manage Rules Project application. Since Business Rules APIs are based on OAuth 2.0 authentication, you need to provide the authentication details too.

1. Choose **Configure Environments**.

    ![Configure environment](testing4.png)

2. In the **Configure Environments** window, provide the following details:

    |  Field Name     | Value
    |  :------------- | :-------------
    |  `Display Name for Environments`          | **`EU10`**
    |  `runtimeurl`        | **`bpmruleruntime.cfapps.eu10.hana.ondemand.com`**
    |  `Client Id`    | **`<clientid from service key>`**
    |  `Secret`      | **`<clientsecret from service key>`**
    |  `tokenurl`     | **`<url from service key>`**


    ![Configure environment](testing6.png)

    Then, choose **Save**.

    ![Configure environment](testing7.png)


[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Invoke a rule service)]

1. Select **Invoke a Rule Service** API and then choose **`/v2/workingset-rule-services`**. Then choose **Try out** to execute the API.

    ![Configure environment](testing8.png)

2. Fetch the rule service ID from Manage Rules Project application. This is required for the API JSON payload.

    In the **Rule services** tab, choose **Settings** icon.

    ![Configure environment](testing9.png)

    Select **ID** and choose **OK**.

    ![Configure environment](testing10.png)

    Copy the ID from the rule service.

    ![Configure environment](testing11.png)

3. Paste the rule service ID in place of **`<rule-service-ID>`** in the following JSON payload and copy it to the body of the API:

    ```JSON
        {
      "RuleServiceId": "<rule-service-ID>",
      "Vocabulary": [
        {
          "Employee": {
            "isFullTimeEmployee": true,
            "countryofCompany": "USA",
            "jobTitle": "Engineer II",
             "company": "2000"
           }
        }
      ]
    }
    ```

    Then choose **Execute**.

    ![Configure environment](testing12.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Observe the API response)]

You can view the equipment assigned to the employee in the response body of the API.

![Configure environment](testing13.png)

[VALIDATE_1]

[ACCORDION-END]
