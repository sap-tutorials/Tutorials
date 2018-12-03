---
title: Using ABAP with the SAP API Business Hub
description: Add a code snippet from the API Hub into your ABAP report.
primary_tag: topic>abap-development
auto_validation: false
author_name: Meredith Hassett
author_profile: https://github.com/mhassett92
tags: [  tutorial>beginner, topic>abap-development  ]
---

## Prerequisites  
 - ABAP system should be configured to call http APIs
 - [Testing API Business Hub APIs with Curl](https://developers.sap.com/tutorials/hcp-abh-test-locally.html)


## Details
### You will learn  
In this tutorial, you will learn how to use the pre-generated code from the SAP API Business Hub in an ABAP report. You will need to have configured your ABAP system to make an HTTP request.

Configuring the proxy settings is not a required step for all systems. Verify that your system needs to have the proxy configured before completing steps 1 through 3.

### Time to Complete
**15 Min**

---

[ACCORDION-BEGIN [Step 1: ](Configure your ABAP Proxy)]
From the SAP Logon pad, launch the SAP Logon screen.

![ABAP SAP logon screen](1-39.png)

Enter the login credentials in R/3 ABAP System.

![logon credentials screen](2-31.png)

Configure your proxy settings by going to `transaction SICF` and clicking on **Execute**.

![proxy settings](3-40.png)

Open Proxy settings page by pressing **Ctrl + F2** and set the proxy.

![proxy configuration screen](4-26.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Get your SSL Certificate)]
In Google Chrome, export SSL certification.

![Google chrome SSL certificate](5-18.png)

To get to the SSL certificate, enter **F12**, go to **Security** tab, click on **View Certificate**, click on **Details**, and select **Copy to File**. From here, you can export the certificate by path.

![SSL Certificate details](6-18.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Add your certificate to ABAP)]
Once you export the certificate by path, you will need to import it into your ABAP system. To import the certificate in your ABAP system, go to the `transaction STRUST`, open **SSL client SSL Client (Standard)**, and switch to **Edit mode**.

![ABAP trust manager](7-15.png)

Click on **Import Certificate**. Select the certificate you exported from Google Chrome in the previous step. Click **Add to Certificate List** and save.

![SSL client SSL client standard certificate](8-13.png)

In order to reflect your new SSL settings, you will need to **restart your ICM**. To restart the ICM, go to the `transaction SMICM`. Navigate to **Administration > ICM > Exit Hard > Global**.

![location of global restart option](9-14.png)

The ICM restart message will appear. Select **Yes**.

![ICM restart message](10-13.png)

Restart the ICM processes. Make sure no client is communicating during restart.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create the ABAP program report)]

In your ABAP system, go to the `transaction se38`.

![ABAP editor initial screen](11-10.png)

Create a new report. Enter the program name, such as `ZAPI_HUB_TEST`, select **Source Code**, and click **Create**.

![report creation screen](12-11.png)

Provide the following details on the **Program Attributes** screen.

| Field Name | Value |
|:--------|:--------|
| Title | testing API |
| Type | Executable program |
| OPTIONAL: Status | Test Program |

**Save**, **Activate**, and **Execute** the report.

![report program attributes](13-10.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 5: ](Get the ABAP code from API Business Hub)]

To get the ABAP code snippets that are available in the SAP API Business Hub, go to the [SAP API Business Hub](https://api.sap.com), which can be found at `api.sap.com`, from your browser. Navigate to the **API packages page** and find the **SAP Translation Hub API**.

![translation hub API](14-6.png)

Click the **Expand Operations** link on the **Domains** API. Click **Generate Code** on the `GET /domains` method.

![GET domains API method](15-6.png)

Click on the tab for **ABAP** from available languages and click **Copy to Clipboard**.

![ABAP code snippet](16-5.png)

[VALIDATE_5]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Add API call and run from ABAP)]
Back in your **ABAP system**, **paste the code snippet** into the report.

![ABAP report with added code snippet](17-5.png)

Replace the `<API_KEY>` with your API key value. This is also available in the SAP API Business Hub and is specific to your user and app. **Save**, **Activate**, and **Execute** the report.

![successful API call from ABAP](18-6.png)

If you see a message returned with data, and not an error, you have successfully tested a call to an API from the SAP API Business Hub.

[DONE]
[ACCORDION-END]


