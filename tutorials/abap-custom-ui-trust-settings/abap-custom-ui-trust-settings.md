---
parser: v2
auto_validation: true
primary_tag: topic>abap-development
tags: [  tutorial>beginner, topic>abap-development, topic>abap-extensibility  ]
time: 25
author_name: Ulrike Liebherr
author_profile: https://github.com/Liebherr
---

# Set Up Trust Between Identity Authentication and SAP Business Technology Platform
<!-- description --> Set up trust between SAP Cloud Identity Services - Identity Authentication and SAP Business Technology Platform for secure communication via SAML 2.0.

## Prerequisites
**Authorizations**: Your user needs
- Administrator access to your **SAP Business Technology Platform** subaccount
- Administrator access to your **SAP Cloud Identity Services - Identity Authentication tenant**

## Glossary
**Identity**: individual people, but also computers, services, computational entities like processes and threads, or any group of such things
**Identity Provider**: system entity that creates, maintains, and manages identity information for identities
**Identity Authentication**: process of authenticating an identity
**SAP Cloud Identity Services - Identity Authentication**: SAP's solution to enable identity authentication, abbreviated as **IAS**
**SAP Cloud Identity Services - Identity Authentication tenant**: a customer's instance of the service
**SAP Cloud Identity Services - Identity Authentication console**: Web application to configure your tenant

## You will learn
- How to set up SAP Business Technology Platform Subaccount for secure communication (with Security Assertion Markup Language = SAML 2.0)
- How to set up SAP Business Technology Platform Subaccount on SAP Cloud Identity Services - Identity Authentication for secure communication
- How to get necessary information from your SAP Business Technology Platform Subaccount and your SAP Cloud Identity Services - Identity Authentication tenant to set up the mutual trust between them

## Intro
>Be aware that in case of an integration with S/4HANA Cloud the used Identity Authentication for the Subaccount should be the very same as the one used for the S/4HANA Cloud system.
Your S/4HANA Cloud system you got already delivered by SAP with a configured trust between it and your SAP Cloud Identity Services - Identity Authentication tenant. Now you will configure the trust between that and your SAP Business Technology Platform subaccount on your own.
![S/4HANA Cloud and SAP Business Technology Platform Subaccount share same Identity Provider](trust_IAS_SCP.png)
## Additional Information
- **Documentation:** [SAP Cloud Identity Services - Identity Authentication](https://help.sap.com/viewer/6d6d63354d1242d185ab4830fc04feb1/Cloud/en-US/d17a116432d24470930ebea41977a888.html)
- **SAP S/4HANA Cloud Release** (tutorial's last update): 1911
---

### Enter trust management of subaccount

Enter the SAP Business Technology Platform subaccount as an administrator and expand the **Security** area to open Trust Management by clicking the **Trust** section.

![Enter SAP Business Technology Platform Subaccount](sapcp_trust_mgmt.png)


### Set subaccount as service provider

To enable secure (Security Assertion Markup Language = SAML 2.0) communication the SAP Business Technology Platform Subaccount has to be set up as Service Provider.

Being in the trust management, click **Edit** to change the default Local Service Provider.

![Edit local service provider](sapcp_LclSP_edit_default.png)

Change and add following information to your local provider:

| ------------------------------------------- | ------------------------------------------- |
|           **Configuration Type**            |                    `Custom`                   |
|           **Local Provider Name**           | `<platform region s URL>/<subaccount name>` (set automatically) |
|          **Principal Propagation**          |                 `Enabled`                 |
|          **Force Authentication**           |               `Disabled`            |

Click **Generate Key Pair**

![Generate Key Pair for and save Local Service Provider](sapcp_LclSP_genKeyPair_save.png)

**Save** your changes.


### Get metadata of subaccount

To set up the trust from Identity Authentication to the Subaccount soon you need the subaccount's metadata.
Download the metadata by clicking **Get Metadata**.

![Get metadata of subaccount](sapcp_getSubaccountMetadata.png)


### Enter Identity Authentication Administration Console

Open the SAP Cloud Identity Services - Identity Authentication Administration Console with its URL which follows the pattern:

`https://<YOUR_TENANTS_ID>.accounts.ondemand.com/admin`

Tenant ID is an automatically generated ID by the system. The first administrator created for the tenant receives an activation e-mail with a URL in it. This URL contains the tenant ID.

SAP Cloud Identity Services - Identity Authentication Administration Console entry screen looks (depending on authorizations) like that
![Enter SAP Cloud Identity Services - Identity Authentication administration console](IAS_entryScreen.png)


### Add subaccount as application

The subaccount is represented in SAP Identity Authentication Service as Application.
Choose **Applications & Resources** and go to **Applications**. Click **+ Add** on the left hand panel to enter the name of your SAP Business Technology Platform subaccount. Save your changes.

![Add subaccount as application](IAS_addApplication.png)


### Configure application's trust with subaccount

Click on the newly created application on the left side and then on **Trust**. Choose **SAML 2.0 Configuration**.

![Configure application' s SAML 2.0 trust with subaccount](IAS_openSamlConfig.png)

Upload the metadata XML file of your SAP Business Technology Platform subaccount. By this service provider metadata upload, the needed properties are gotten from the XML file. Save the configuration settings.

![Upload subaccount' s metadata](IAS_uploadSubaccountMetadata.png)


### Set Subject Name Identifier


Now you have to configure which attribute is used to identify users during `SAML2.0` secure communication. By default this is **`User ID`**, but as S/4HANA Cloud by default works with **`Login Name`** it shall be switched to that.

**Procedure**

Still being in your application's Trust settings, select **Subject Name Identifier**.

![Open Subject Name Identifier configuration](IAS_openSubjectNameID_attributeConfig.png)

Under Basic Configuration choose **Login Name** from the dropdown list for the attribute and save your changes.

![Set Login Name as application' s Subject Name Identifier](IAS_subjectNameID_attribute_setLoginName.png)


### Configure application' s Default Identity Provider

As in most common use case the SAP Cloud Identity Services - Identity Authentication does not act as Identity Provider itself but as proxy for a probably already existing corporate identity provider this has to be set now.

In your application's Trust settings switch to **Conditional Authentication** and select it.

![Open application' s identity provider configuration](IAS_openIdP_config.png)

Select your corporate Identity Provider as **Default Identity Provider** and click **Save**.

![Set identity provider](IAS_setCorporateIdP_asIdP.png)


### Get metadata of Identity Authentication tenant

To set the Identity Authentication tenant as trusted identity provider in the SAP Business Technology Platform subaccount next, you need to get its metadata first.

Open the metadata XML by entering your tenant's web address for it which follows this pattern:

``` URI
https://<YOUR_TENANTS_ID>.accounts.ondemand.com/saml2/metadata
```

![Save metadata of identity authentication Tenant](IAS_getMetadata.png)

Save that XML to a file.


### Add IAS as subaccount's trusted identity provider

Switch back to your SAP Business Technology Platform cockpit and go to your trust settings.

Choose **Application Identity Provider** to add a trusted identity provider.

![Add subaccount' s trusted identity provider](sapcp_addTrustedIdP.png)

Upload metadata XML file of your identity authentication tenant in the **Metadata File** field. **Save** your changes.

![Upload identity tenant' s metadata as trusted identity provider](sapcp_addTrustedIdP_metadataUpload_save.png)


### Test yourself



