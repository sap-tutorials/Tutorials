---
title: Register a Neo Subaccount in SAP Cloud Platform Identity Authentication Service
description: Establish trust between a SAML 2.0 identity provider in SAP Cloud Platform Identity Authentication service and an SAP Cloud Platform Neo subaccount.
auto_validation: true
time: 25
tags: [ tutorial>beginner, products>sap-cloud-platform, products>sap-cloud-platform-identity-authentication]
primary_tag: topic>security
---

## Prerequisites
 - [Get a Free Trial Account on SAP Cloud Platform](hcp-create-trial-account)

## Details
### You will learn
  - How to establish trust between your SAP Cloud Platform Neo subaccount and the SAP Identity Authentication service
  - How to configure users for single sign-on usage

---

[ACCORDION-BEGIN [Step 1: ](Log onto SAP Cloud Platform)]

Go to [SAP Cloud Platform](https://cockpit.hanatrial.ondemand.com/cockpit/#/home/trial) and **log in** with your credentials.

>If you don't have an account yet, sign up for an [SAP Cloud Platform Trial account](hcp-create-trial-account).

![SAP Cloud Platform Trial Cockpit](trial-cockpit.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Navigate to SAP Cloud Platform Neo subaccount)]

**Scroll down** to the bottom of the page and **Click** on **Access Neo Trial**.

![Click Access Neo trial](neo-access.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Change local service provider to Custom)]

**Expand** the **Security** menu item and navigate to **Trust**.

![Navigate to the Trust menu](trust-menu.png)

Switch the **Local Service Provider** from Default to **Custom**. Some more input fields are appearing.

![Switch Local Service Provider to Custom](custom-local-sp.png)

Press the **Generate Key Pair** button to fill the `Signing Key` and `Signing Certificate` fields. Change the **Principal Propagation** to **Enabled** and **Save**.

![Generate new key pair and enable Principal Propagation](generate-key-pair.png)

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Add trusted identity provider)]

Click the **Application Identity Provider** tab. Add a new **Trusted Identity Provider** through the according link.

![Add a new Trusted Identity Provider](add-trusted-idp.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Upload metadata file from SAP Cloud Platform Identity Authentication service)]

Upload the SAML metadata file you have download in the previous Tutorial. If you don't have it anymore, you can download it in the **Tenant Settings > SAML 2.0** section in the SAP Cloud Platform Identity Authentication Service.

> Note: You can also download the SAML 2.0 metadata through accessing the following URL. https://[TENANT_ID].accounts.ondemand.com/saml2/metadata Tenant ID is an automatically generated ID by the system. The first administrator created for the tenant receives an activation e-mail with a URL in it. This URL contains the tenant ID. For more information have a look at the [Product Page](https://www.sap.com/products/cloud-platform/capabilities/foundation.identity-authentication.html#identity-authentication)

![Download Tenant SAML Configuration ](add-trusted-idp.png)

Click **Browse** and select the metadata file.

![Select metadata file from local file system](browse-file.png)

All other information from the file should be taken into the shown form. No further manual actions necessary.

Click **Save** to continue.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Download SAP Cloud Platform subaccount SAML metadata)]

Go back to the **Local Service Provider** tab and download the metadata of your SAP Cloud Platform subaccount. **Click** on **Get Metadata** to do so.

![Download SAP Cloud Platform subaccount metadata](get-subaccount-metadata.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Create application in SAP Cloud Platform Identity Authentication service)]

Log in to the administration console of SAP Cloud Platform Identity Authentication service through your particular URL.

The URL therefore is: **`https://[TENANT_ID].accounts.ondemand.com/admin`**

Navigate to the menu item **Applications**, **Click** on **Add** provide a name for the application.

![add a new application](add-application-ias.png)

**Save** to continue.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Upload SAP Cloud Platform subaccount SAML metadata)]

Navigate to **SAML 2.0 Configuration** and **Click** on the menu item.

![Navigate to SAML 2.0 Configuration](neo-saml-menu.png)

Choose the recently downloaded SAP Cloud Platform SAML metadata file from your local file system.

> You have downloaded the according file in Step 6, `Download SAP Cloud Platform subaccount SAML metadata`.

![Upload the metadata file and change the name](application-name.png)

All fields below are automatically going to be filled due to the information provided through the uploaded file.

**Save** to continue.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Change subject name identifier)]

Although the configuration is already done and would work, change the unique identifier from User ID to the Email-Address so that you can enable Single-Sign On from SAP Cloud Platform to Azure Active Directory later on more easily.

**Click** on the menu item **Subject Name Identifier**.

![Click the menu item Subject Name Identifier](subject-name-identifier.png)

Change the basic attribute to **E-Mail**. This attribute is used to match User IDs from Azure AD and SAP Cloud Platform.

![change basic attribute to E-Mail](basic-attribute.png)

**Save** the configuration.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 10: ](Create new test user)]

Now, users that have access to the Application in Azure Active Directory and to the application on the SAP Cloud Platform can seamlessly access the application with their Azure Active Directory credentials.

Therefore, we will create a new test user in Azure Active Directory and add it to the enterprise application in Azure Active Directory.

Go to the [Azure Portal](https://portal.azure.com) and search for **Azure Active Directory** and select the according result.

![search for azure active directory](azure-active-directory.png)

**Navigate** to the menu item **Users**.

![navigate to the menu item Users](user-menu-item.png)

Click **New guest user**.

![navigate to the menu item Users](user-menu-item.png)

Choose **Invite user** and provide name and a mail address of the invited user - in this case any of your mail addresses.

![invite guest user](invite-guest-user.png)

Press **Invite** at the bottom of the page to send out the invitation. The user will receive an invitation mail.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 11: ](Assign user to the enterprise application )]

The user generally exists now in the Azure Active Directory without having access to the enterprise application. Thus, add it to the enterprise application as follows:

Go to your **enterprise applications** (either through the search bar on top of the page or through the menu bar) and select your application you have created for SAP Cloud Platform Identity Authentication Service.

![select your enterprise application](enterprise-application.png)

Select the tile **Assign users and groups**.

![Select the tile Assign users and groups](assign-user-and-groups.png)

Click **Add user**.

![Click Add user](add-user-enterprise-application.png)

Click **Users - None Selected** to search for available users. Either search for a user or directly select one from the result list below.

Click **Select** and afterwards on **Save** at the bottom of the page to continue.

![add user assignment to the enterprise application](add-assignment.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 12: ](Access SAP Web IDE)]

To test the recent configurations, try to access the SAP Web IDE Full-Stack. Therefore, go to the [SAP Cloud Platform Trial cockpit](https://cockpit.hanatrial.ondemand.com/cockpit/#/home/trial) and **scroll down** to the bottom of the page and **click** on **Access Neo Trial**.

![Click Access Neo trial](neo-access.png)

**Navigate** to the **Services** menu item and search for **SAP Web IDE Full-Stack**. Click the tile to go to the service.

![Open SAP Web IDE Full-Stack](webide-service.png)

You are now on the overview page of the service. **Open** the service now in a new **Incognito Window**.

You'll be redirected to an Azure Active Directory authentication page, where you can **log in** with the recently created user in Azure Active Directory.

![Open service in Incognito Window](open-webide-incognito.png)

An error message will appear saying that you are not authorized to access SAP Web IDE Full-Stack.

![Open service in Incognito Window](errormessage-webide.png)

Why is that? The necessary role for SAP Web IDE Full-Stack is not yet assigned to this user.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 13: ](Assign roles to user)]

Go back to the **overview page** of **SAP Web IDE Full-Stack**. Click **Configure Service**.

![Click Configure Service](configure-service.png).

Select the **`DiDeveloper`** Role and **Click** on **Assign** to an individual user. Enter the mail address of the user you have defined in Azure Active Directory and **Assign**.

![Assign DiDeveloper to mail address](assign-role-to-mail.png).

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 13: ](Log into SAP Web IDE Full-Stack again)]

The assignment will only be taken into account in a new session. Therefore go back to the **overview page** of the service.

![Go back to the overview page](go-back-to-overview.png).

**Open** the service now in a new **Incognito Window** and log in again.

You should now be forwarded to **SAP Web IDE Full-Stack**.

[DONE]
[ACCORDION-END]

Congratulations!

You have successfully connected Azure Active Directory with your SAP Cloud Platform Identity Authentication Service tenant. Furthermore, the SAP Cloud Platform Neo subaccount can now leverage all the capabilities of SAP Cloud Platform Identity Authentication service.

Good Job!

---
