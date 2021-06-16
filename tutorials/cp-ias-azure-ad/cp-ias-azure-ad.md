---
title: Connect Azure Active Directory to Identity Authentication Service
description: Configure the Identity Authentication service to leverage your Azure Active Directory setup.
auto_validation: true
time: 30
tags: [ tutorial>beginner, products>sap-business-technology-platform, tutorial>license, products>identity-authentication]
primary_tag: topic>security
author_name: Valentin Atanassov
author_profile: https://github.com/ValAta
---

## Prerequisites
 - [Identity Authentication service tenant](https://www.sapstore.com/solutions/40132/SAP-Cloud-Platform-Identity-Authentication)
 - [Sign Up for Microsoft Azure](https://azure.microsoft.com/en-us/resources/videos/sign-up-for-microsoft-azure/)

## Details
### You will learn
  - How to establish trust between your Identity Authentication service and Azure Active Directory

The Identity Authentication service offers end-to-end security including several authentication methods between your end users and applications. Leverage a variety of authentication methods including form-based/SAML, client certificate, username and password, and OAuth. In this tutorial we are initially setting up the connection between your Azure Active Directory and the Identity Authentication service in form of exchanging metadata.

Once the connection between your Azure Active Directory and the Identity Authentication service is done you can simply use it to connect it to several applications and environments.

To summarize the steps in this tutorial overall: You are going to exchange metadata (files) of Azure Active Directory and your Identity Authentication service to make them known to each other.

---

[ACCORDION-BEGIN [Step 1: ](Log into Azure Portal)]

Login to **Azure Portal** by going to [https://portal.azure.com](https://portal.azure.com) and provide your credentials.

Make sure you are using the right directory within the Azure subscription. The current directory is shown below your user name.

![current directory shown below the user name in the upper right corner of the page](current-directory.png)

If it's not the one you want to use, **click on your profile** and choose **Switch Directory**.

![current directory shown below the user name in the upper right corner of the page](switch-directory.png)

Either set your **Default directory** by selecting an item in the Dropdown or choose the directory for this particular session by selecting one of the listed options. (see both variants on the screenshot below)

![choose between changing the default directory or to a temporary directory for this session](choose-directory.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create enterprise application in Azure Active Directory)]

**Search** for **Azure Active Directory** in the search bar on the top of the page and select the according entry in the shown results below.

![search for Active directory](search-active-directory.png)

Click the menu item **Enterprise applications**.

![enterprise applications menu item](enterprise-application-entry.png)

Click **New application**.

![click new applicaction](create-new-application.png)

Azure Active Directory has templates for a variety of applications, one of them is the SAP Cloud Platform Identity Authentication Service. **Search** for this and select it.

![search for Identity Authentication service](scpias-template.png)

A new column on the right side will appear to give the application a name. Give the application a name and click **Add**.

![give the application and name and click Add](enterprise-application-name.png)

[VALIDATE_1]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Download single sign-on metadata from Azure Active Directory)]

Click the menu item **Single sign-on**.

![single sign on menu item](single-sign-on-entry.png)

Choose **SAML** as the Single-Sign On method.

![choose saml as single sign on method  ](saml-auth.png)

Download the federation metadata as shown below.

![download federation metadata](download-aad-metadata.png)

With this information we can setup the trust between Azure Active Directory and Identity Authentication service.


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Log onto Identity Authentication service)]

Login to the administration console of Identity Authentication service through your particular URL.

The URL therefore is: **`https://[TENANT_ID].accounts.ondemand.com/admin`**

>Tenant ID is an automatically generated ID by the system. The first administrator created for the tenant receives an activation e-mail with a URL in it. This URL contains the tenant ID. For more information have a look at [SAP Cloud Identity Services - Identity Authentication](https://discovery-center.cloud.sap/serviceCatalog/identity-authentication) in the SAP Discovery Center.


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Create corporate identity provider)]

Navigate to **Identity Providers** and click **Corporate Identity Providers**.

![corporate identity provider](corporate-idp.png)

Click **Add** at the bottom of the page and define a name for the Identity Provider. Click **Save** to finally create the Identity Provider.

![create a new IdP and give it a name](define-new-idp.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Upload Azure Active Directory federation metadata file)]

Click **SAML 2.0 Configuration** and to upload the recently downloaded federation metadata from Azure Active Directory.

![click SAML 2.0 Configuration](configure-saml-idp.png)

Choose the file from your local file system.

![browse and upload the Azure Active Directory metadata file](upload-aad-metadata.png)

All fields below are automatically going to be filled due to the information provided through the uploaded file.

![enter a name](idp-name-logon.png)

Click **Save** at the bottom of the page.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Change identity provider type)]

**Click** on **Identity Provider Type**.

![navigate to Identity Provider Type](choose-idp-type.png)

Change the selection to **Microsoft ADFS / Azure AD**. Save the the configuration through clicking the **Save** button at the bottom of the page.

![change selection of Identity Provider Type to Azure AD](select-idptype-aad.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Download Identity Authentication service tenant metadata)]

Go to the **Tenant Settings** in SAP Cloud Platform Identity Authentication Service and navigate to the **SAML 2.0 Configuration**.

![Navigate to Tenant Settings and SAML 2.0 Configuration](saml-config-ias.png)

**Scroll down** to the bottom of the page and **Download** the metadata file.

![Download the metadata file](download-ias-metadata.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Upload Identity Authentication service tenant metadata to Azure Active Directory)]

You have already uploaded the metadata file from Azure Active Directory to Identity Authentication service. It's time to do it the other way round now and upload the metadata of Identity Authentication service to Azure Active Directory.

Go back to the [https://portal.azure.com](https://portal.azure.com) and search for **Enterprise application** in the search bar on top of the page. Select the according result.

![search for enterprise applications](search-enterprise-applications.png)

Select the application you created previously in this tutorial.

![select previously created application](my-sap-tutorial.png)

Go to **Single sign-on** and select **SAML** as Single-Sign On method. Click  **Upload metadata** to upload the metadata file from Identity Authentication service.

![Upload the metadata file from Identity Authentication service](upload-ias-metadata.png)

All the details are now taken from the metadata file. There's nothing to do for you other than saving the details. Therefore, click **Save**.

![save metadata information](save-ias-metadata-details.png)

[DONE]
[ACCORDION-END]

---
