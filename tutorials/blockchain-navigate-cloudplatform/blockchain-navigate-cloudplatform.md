---
title: Navigate to Blockchain Services on SAP Cloud Platform
description: Register for an SAP Cloud Platform trial account, understand the structure of the Cloud Foundry environment, and access blockchain services.
time: 15
auto_validation: true
tags: [tutorial>beginner, products>sap-cloud-platform, topic>cloud, topic>blockchain]
primary_tag: products>sap-cloud-platform
author_name: Brian McKellar
author_profile: https://github.com/BrianMcKellar
---

## Details
### You will learn
  - An overview of SAP Cloud Platform, its environments, and wider infrastructure
  - An understanding of how blockchain services are represented on SAP Cloud Platform
  - The difference between global accounts, subaccounts, and spaces on SAP Cloud Platform
  - How to navigate to SAP Cloud Platform Service Marketplace and locate blockchain services

---
[ACCORDION-BEGIN [Step 1: ](Navigate to SAP Cloud Platform)]

SAP Cloud Platform is an open platform as a service for developing cloud business applications, such as SAP's blockchain services, in a fully provisioned environment. SAP Cloud Platform is effectively an umbrella over several cloud environments, providing the infrastructure for SAP and SAP's customers for enabling best-of-breed cloud experience.

![Image depicting SCP Overview](08--SCP-Overview.png)

SAP Cloud Platform supports the following environments: Neo, Cloud Foundry, Kubernetes, and ABAP.

- **Neo**: Custom SAP developed cloud infrastructure for running cloud-centric Java applications
- **Cloud Foundry**: Environment for scalable web applications running in the cloud
- **Kubernetes**: For hosting container landscapes
- **ABAP**: Complete ABAP systems running in a cloud environment

To achieve a worldwide footprint, SAP Cloud Platform uses different infrastructure providers:

- SAP own computer centers
- Amazon AWS
- Microsoft Azure
- Google GCP
- Alibaba

SAP's blockchain services are available in SAP Cloud Platform's Cloud Foundry environment. This environment offers an initial free 30 day trial period, with the possibility to extend this to 60 and 90 days if required. By registering for a trial account you are able to test our Hyperledger Fabric, MultiChain, Quorum, and Blockchain Application Enablement services.

For further details about trial accounts, read the SAP Cloud Platform [documentation](https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/8ed4a705efa0431b910056c0acdbf377.html#046f127f2a614438b616ccfc575fdb16.html)

To register for a trial account, navigate to SAP Cloud Platform [trial page](https://account.hanatrial.ondemand.com/#/home/welcome) and click **Register**.

![Image depicting SAP cloud platform trial page](01--TrialPage.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Register for trial account)]

Enter your personal information, set a password, and agree to the **Terms & Conditions**, before then clicking **Register**

![Image depicting SAP cloud platform trial page](02--Register.png)

An email containing an activation link is sent to your supplied email address. Within this email click **Click here to activate your account**.

![Image depicting email activation](04--EmailActivate.png)

Your account is now activated, giving you trial access to the SAP Cloud Platform for an initial 30 days.

![Image depicting email activation](05--AccountActivated.png)

Click **Continue**, taking you back to the SAP Cloud Platform home page.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Access Cloud Foundry trial environment)]

Click **Cloud Foundry Trial**.

![Image depicting trial selection](06--Trial.png)

You can then select which region you wish to run the trial environment from and click **OK**. The blockchain services are available in the following environments, so select the one relevant to you:

- Europe / Frankfurt (EU10)
- US East / Virginia (US10)

In our example we are using the Frankfurt / Europe (EU10) region.

![Image depicting region selection](07--Region.png)

After selecting your region, the SAP Cloud Platform automatically activates the creation of your global account, subaccount, and space.

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Understand SAP Cloud Platform structure)]

With a normal SAP Cloud Platform global account, subaccounts and spaces are created manually, enabling you to define your account structure as you wish. At this stage it is important to understand how this structure works when accessing the blockchain services. Here you have:

- **Global Account**: These often represent an entire company and can be viewed as an empty shell used for billing, recording data consumption, and grouping resources together. They can't be used to directly run applications or services, however.

- **Subaccount**: This is the selection of a specific environment, hosted at a specific provider, in a specific region. As such, global accounts can house one to many sub-accounts at any one time.

- **Space**: These allow organizations to divide their sub-accounts into specific teams or projects, helping to map the structure of their organization into clearly defined areas.

Before advancing to your space, we will look at blockchain entitlements.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](View blockchain entitlements)]

All SAP Cloud Platform global accounts are allocated a number of resources to use, including the distribution of memory between applications and services (such as the Hyperledger Fabric service). This process is known as enabling and assigning entitlements, something which is handled manually by global account administrators using a paid SAP Cloud Platform account.

With a trial account in the Cloud Foundry environment, you are automatically allocated the following blockchain entitlements (with links to further documentation provided):

| Service  | Available Service Plans   |
|---|---|
| [Blockchain Application Enablement](https://help.sap.com/viewer/p/BLOCKCHAIN_APPLICATION_ENABLEMENT)  | 2 x  Proof-of-Service, 2 x Proof-of-History, 2 x Timestamp  |
| [Hyperledger Fabric](https://help.sap.com/viewer/2280c19ea8414e4f8d85d272e97e5a08/BLOCKCHAIN/en-US)  | 1 x dev, 1 x channel |
| [MultiChain](https://help.sap.com/viewer/15cb4580694c4d119793f0d3e9b8a32b/BLOCKCHAIN/en-US/0183c6479c47427ab6257bd37ab8bee3.html)  | 1 x trial |
| [Quorum](https://help.sap.com/viewer/91bdd2defc9a469694b0b508b5c8c32f/BLOCKCHAIN/en-US) | 1 x dev |

Click **Go to Space** to open your SAP Cloud Platform space.

![Image depicting region selection](08--Info.png)

To view your blockchain entitlements from here, click on your **Global Account** (in our example this is "Account Name") and then **Overview**.

![Image depicting navigation to Globl Account](10--Navigate.png)

Then scroll down to your **Quota** section.

![Image depicting space selection](09--SpaceEntitlements.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Navigate to blockchain service)]

After viewing your entitlements, you should now navigate to the blockchain service.

Click **Spaces**.

![Image depicting space selection](11--Spaces.png)

Select your space:

![Image depicting space selection](12--Spaces.png)

Open the **SAP Cloud Platform - Service Marketplace**.

![Image depicting space selection](13--Spaces.png)


Blockchain services on SAP Cloud Platform are available within the Service Marketplace as several services, each of which can be provisioned individually as required.

At a technology level, there are services for Hyperledger Fabric, MultiChain, and Quorum. Any of these services can be used to provision the relevant blockchain technology. In addition, SAP offers a set of blockchain agnostic services, which are grouped within the Blockchain Application Enablement service. These services are for example a timestamp service, or integration with HANA database.

Scroll down to your required blockchain service.

![Image depicting space selection](14--Spaces.png)

[DONE]
[ACCORDION-END]
---
