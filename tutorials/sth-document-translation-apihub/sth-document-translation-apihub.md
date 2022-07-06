---
title: Translate Documents with SAP API Business Hub
description: Use your SAP BTP account to translate documents with SAP API Business Hub.
auto_validation: true
time: 10
tags: [ tutorial>beginner, topic>sap-api-business-hub, software-product>sap-business-technology-platform]
primary_tag: software-product>sap-translation-hub
---

## Prerequisites
- You have created a Free Tier account on SAP Business Technology Platform: [Get an Account on SAP BTP to Try Out Free Tier Service Plans](btp-free-tier-account) or you have your own SAP BTP Enterprise Account.
- You have created the credentials for Document Translation, as described in [Create Service Instance for Document Translation](sth-enable-document-translation).
- You want to translate a document that is not larger than 5 MB because this is the limit for the SAP API Business Hub and contains less than 40.000 characters (document translation API restriction for synchronous API).

## Details
### You will learn
 - How to get started with SAP API Business Hub
 - How to maintain the Document Translation credentials in SAP API Business Hub
 - How to translate a Document in SAP API Business Hub

[ACCORDION-BEGIN [Step 1: ](Navigate to the SAP API Business Hub)]
Follow the link to the [SAP API Business Hub](https://api.sap.com/).

!![API Hub](01_API_hub.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Log into SAP API Business Hub)]
The SAP API Business Hub offers a test facility for APIs. In order to take advantage of this, you need to log on.

Select **Login** at the top of the page.

!![Log On](04_API_hub_log_on.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Search for Document Translation API)]
Search for `document translation` to find the Document Translation API and select the Document Translation API.

!![Search in API Hub](02_API_hub_search.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 4: ](Explore the Document Translation API)]
The Document Translation API Reference page shows the two available APIs.

!![Show document translation](03_API_hub_document.png)

Take a minute to explore both APIs.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 5: ](Maintain the API environment)]
The service offers a sandbox system out of the box, but the idea of this tutorial is to show, how to connect your SAP BTP free tier or enterprise account to the SAP API Business Hub.

1. Select the **Try Out** button.

    !![Configure](05_API_hub_configure.png)

2. Click **Environment: Sandbox** to open the menu and select **+ Add New Environment**

    !![Configure](05a_API_hub_configure.png)

3. In the **Configure Environment** screen, enter a display name for the environment: `myaccount`and select the **Starting URL** corresponding to the region of your SAP BTP account.

    !![Configure](06_API_hub_configure.png)

4. For client ID and client secret, take the information from the Document Translation credentials as described in [Create Service Instance for Document Translation](sth-enable-document-translation)

    !![Configure](07_API_hub_configure.png)

5. For subaccount, take the subdomain information from your SAP Business Technology Platform cockpit. For example, on the screen-shot below, the sub-account is `80c353f5trial`.

    !![Configure](08a_API_hub_account.png)

    Enter your value in SAP Business API Hub.

    !![Configure](08_API_hub_configure.png)

6. Save your settings for future sessions.

    !![Configure](09_API_hub_configure.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Test with your own account)]
Now that your API environment is maintained, you will see the `myaccount` entry.

Select **Synchronous document translation** and **POST /translation**.

!![Configure](11_API_hub_try_out.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Test an API endpoint)]
You can maintain the languages in the **Parameters** section. Source and Target Language fields are already pre-filled with values for English and German. If you want to try out other language codes, please refer to the available language information [here](https://help.sap.com/viewer/9f73362817cd48339dd8a6acba160f7f/Cloud/en-US/6fc2e5ab04a94da4a0c3d0740a9bb2ff.html).

!![Configure](12_API_hub_try_out.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Upload a file)]
Go to the **Body** section and select **Form Data**.

**Choose File** to select a document from your file source system (Microsoft Word, Microsoft PowerPoint, Microsoft Excel or other file formats which are mentioned [here](https://help.sap.com/viewer/9f73362817cd48339dd8a6acba160f7f/Cloud/en-US/a2dedd7861624a1a82d7ec7ea431a8e4.html)) and click **Run**.

!![Configure](15_API_hub_try_out.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](View the translation result)]
After some seconds you will see in the **Response Body** section a **Download file** link. Click the link and the translated file will be downloaded.

!![Configure](13_API_hub_translation.png)

Open the file and verify the translation result.

!![Configure](14_API_hub_translation.png)

[VALIDATE_1]
[ACCORDION-END]

---
