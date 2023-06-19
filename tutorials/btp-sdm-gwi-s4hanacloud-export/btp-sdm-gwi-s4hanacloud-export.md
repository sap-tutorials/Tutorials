---
title: Establish Trust Configuration between SAP S/4HANA Cloud, public edition and SAP BTP Subaccount
description: Configure trust between SAP S/4HANA Cloud, public edition and the BTP subaccount. During the configuration, you download the identity providers generated in SAP S/4HANA Cloud, public edition. You import SAML identity provider metadata into your SAP BTP Cloud Foundry account.
auto_validation: true
time: 15
tags: [ tutorial>beginner, software-product>sap-business-technology-platform, topic>Cloud, software-product>sap-document-management-service, software-product>sap-s-4hana-cloud]
primary_tag: software-product>sap-business-technology-platform
author_name: Vikram Kulkarni
author_profile: https://github.com/Vikramkulkarni01
---

## Prerequisites
 - You've access to the SAP BTP subaccount and have necessary admin privileges.
 - You've access to the SAP S/4HANA Cloud, public edition system.
 - You've access to the Google Workspace Account. For more information, see [Configure Service Account in Google Cloud Platform](https://help.sap.com/docs/IRPA/1154f48dd7ab430ea52badeb4359e4b4/40baf1a31fad4e86892795f7fe59d971.html).

## You will learn
  - How to configure trust between SAP S/4HANA Cloud, public edition and SAP BTP system.
  - How to manage trust configurations between SAP S/4HANA Cloud, public edition and SAP BTP.

---

[ACCORDION-BEGIN [Step 1: ](Download SAML2.0 metadata from SAP S/4HANA cloud)]
1. Log in to SAP S/4HANA Cloud, public edition with the required admin privileges. Search for **Communication Systems**.

    !![Communication Systems Screen](screenshots.png/Communication Systems.png)

2. Find the Communication System by using the filter **Own SAP Cloud System = Yes**.  You can see the list of Communication Systems and choose the relevant one.

    !![OwnSAPCloudSystemYes](screenshots.png/OwnSAPCloudSystemYes.png)

3. Once you open the details of Communication System, Click on **Download SAML2.0 Metadata**. An **`*.xml`** file gets downloaded to your local system.

    !![Download XML file](screenshots.png/DownloadXMLfile.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create trust configuration)]
1. Log on to your BTP Subaccount and navigate to the **Trust Configuration** option in the left side menu and click **New Trust Configuration**.

    !![NewTrustConfiguration](screenshots.png/NewTrustConfiguration.png)

2. In the **New Trust Configuration** window that opens, upload the **`SAML2.Metadata.xml`** that you downloaded in the previous step (Reference: `Step 1.3`), and enter the name of your choice. Click on **Parse** and **Save**.  

    !![SAML_Metadata](screenshots.png/SAML_Metadata.png)

3. Verify the trust configuration by clicking on the recently created trust configuration in the above step (Reference: `Step 2.2`).

    !![ShowDetailsIssuer](screenshots.png/ShowDetailsIssuer.png)

4. Click on **Show Details** and ensure that the *Subject* and *Issuer* provided are correct.

    !![ShowDetailsIssuer2](screenshots.png/ShowDetailsIssuer2.png)

    !![ShowDetailsIssuer3](screenshots.png/ShowDetailsIssuer3.png)


[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Add users in SAP BTP)]

1. Navigate back to the SAP BTP Cockpit home screen and go to the **Users** tab. Click **Create**.

    In the **New User** dialog, enter the **Username**, select the newly created **Identity Provider**, add the email address of the user, and click **Create**.

    !![NewUser](screenshots.png/NewUser.png)

    >**IMPORTANT**: The e-mail address of the user must be identical to the one used in the SAP S/4HANA system. The mail address can be identified using the Maintain Business User or Manage Workforce option.

2. Select the newly created user from the list and click on **Assign Role Collection**.

    !![AssignRoleCollections](screenshots.png/AssignRoleCollections.png)

3. Assign the user role collection of the **SAP Document Management Service, Integration Option** (For example, `SDM_roles` or the role collection that you created) which is defined in the subaccount. For more information, see the 3rd step in this tutorial [Create a Service Instance and then a Service Key of SAP Document Management Service, Integration Option](btp-sdm-gwi-create-serviceinstance).

    !![SDM_RoleCollections](screenshots.png/SDM_RoleCollections.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Download SAML metadata from SAP BTP cockpit)]

1. In the same subaccount, navigate to the **Trust Configuration** and click **SAML Metadata**. A metadata file gets downloaded to your local system.

    !![SAML_Metadata_download](screenshots.png/SAML_Metadata_download.png)

2. Go to the file in your explorer and right-click on the downloaded file in your local system from the previous step.  Open it with any editor (like **Notepad, Notepad++, Code, Sublime Text, etc.**) scroll down to the bottom of the file to get the token endpoint and copy the URL that is located at the string:

    ```JSON
      <md:AssertionConsumerService Binding="urn:oasis:names:tc:SAML:2.0:bindings:URI" Location="https://example.com"index="1"/>
    ```
    !![AssertionConsumerService](screenshots.png/AssertionConsumerService.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Test yourself)]

[VALIDATE_2]

[DONE]
[ACCORDION-END]


---
