---
title: Set Up SAP Data Hub, Trial Edition 2.5
description: Create a solution instance of SAP Data Hub, trial edition 2.5.
auto_validation: true
primary_tag: products>sap-data-hub
tags: [  tutorial>beginner, topic>big-data, products>sap-data-hub, products>sap-vora ]
---

## Details
### You will learn  
During this tutorial, you will learn how to create a solution instance of SAP Data Hub, trial edition. SAP Data Hub, trial edition is provisioned via SAP Cloud Appliance Library. It runs in your account on Cloud Providers that you choose from GCP, AWS or Azure. Please note here in this tutorial GCP refers to Google Cloud platform, AWS refers to Amazon Web Services and Azure refers to Microsoft Azure.

### Time to Complete
**100 Min**

---

[ACCORDION-BEGIN [Step 1: ](Create Cloud Account or Project)]
Running SAP Data Hub, trial edition requires you to have access to an Amazon Web Services account, a Google Cloud Platform project or an Azure Subscription.

If you do not yet have an account on Google Cloud Platform, Amazon Web Services or Azure, then create one.

For Google Cloud Platform create a corresponding Google Cloud Platform project. Moreover you also need a so-called service account which is used by SAP Cloud Appliance Library while accessing Google Cloud Platform. You find more information about projects and service accounts in the Google Cloud Platform documentation.

* Google Cloud Platform Projects: <https://cloud.google.com/storage/docs/projects>
* Google Cloud Platform Service Accounts: <https://cloud.google.com/iam/docs/service-accounts>

For  Amazon Web Services you need to get access to AWS Management Console for the Amazon Web Services account under certain ID. After getting access you need to also have Access Key and Secret Key as a basis to connect from CAL to AWS backend and spin up the SAP Data Hub 2.5 instance

* Amazon Web Services Account : <https://aws.amazon.com/premiumsupport/knowledge-center/create-and-activate-aws-account/>
* Amazon Web Services Understanding and Getting Your Security Credentials : <https://docs.aws.amazon.com/general/latest/gr/aws-sec-cred-types.html#access-keys-and-secret-access-keys>

For Microsoft Azure you need to create a corresponding Azure subscription using your Microsoft Azure account. And then using the Subscription ID provided to connect CAL from Azure.

* Getting your Microsoft Azure account and unique subscription ID : <https://wiki.scn.sap.com/wiki/display/SAPCAL/FAQ+-+Specific+questions+for+Microsoft+Azure>


The necessary policies for Amazon Web Services account, Google Cloud Platform roles for the service account and Microsoft Azure account are described in the [Getting Started with SAP Data Hub, trial edition] (https://caldocs.hana.ondemand.com/caldocs/help/8772c957-0de5-459b-b98a-27180932f0da_Getting_Started_Guide_v28.pdf) guide (chapter 2.1).

For Google Cloud Platform, you need to enable certain APIs which are also described in the [Getting Started with SAP Data Hub, trial edition] (https://caldocs.hana.ondemand.com/caldocs/help/8772c957-0de5-459b-b98a-27180932f0da_Getting_Started_Guide_v28.pdf) guide (chapter 2.1).

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Link project to SAP Cloud Appliance Library)]
Link your cloud providers to SAP Cloud Appliance Library. This step is described in the SAP Cloud Appliance Library [documentation](https://calstatic.hana.ondemand.com/res/docEN/042bb15ad2324c3c9b7974dbde389640.html).

Open the SAP Cloud Appliance Library in your web browser using the following link: <https://cal.sap.com>.

If you are a first-time user of SAP Cloud Appliance Library, familiarize yourself with its basic concepts and how to work with the user interface by reading the [documentation](https://lkgstatic.hana.ondemand.com/res/~1522937040047~/docEN/6381cffb595143db8d4d7314afa0ae65.html).

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create solution instance)]
Next, you can create a solution instance of SAP Data Hub, trial edition via SAP Cloud Appliance Library. The necessary steps to do so are described in the [Getting Started with SAP Data Hub, trial edition] (https://caldocs.hana.ondemand.com/caldocs/help/8772c957-0de5-459b-b98a-27180932f0da_Getting_Started_Guide_v28.pdf) guide (chapter 2.3).

The creation of the solution instance takes around 60~90 minutes according to different cloud provider.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Connect to SAP Data Hub Launchpad)]
After the solution instance is up and running, you can finally connect to it.

Therefore you first maintain your local host file as described in the [Getting Started with SAP Data Hub, trial edition] (https://caldocs.hana.ondemand.com/caldocs/help/8772c957-0de5-459b-b98a-27180932f0da_Getting_Started_Guide_v28.pdf) guide (chapter 3.2).

To access the SAP Data Hub Launchpad in AWS or GCP or Azure you need go through the chapters 3.3 and 3.4 as described in the [Getting Started with SAP Data Hub, trial edition] (https://caldocs.hana.ondemand.com/caldocs/help/8772c957-0de5-459b-b98a-27180932f0da_Getting_Started_Guide_v28.pdf) guide.

Enter **DEFAULT** as the **Tenant**, `DATAHUB` as **Username** and the password which you have selected during system setup as **Password** to logon to the Launchpad. The system displays the **Application Launchpad** page. If you see an error message that the web browser is not able to verify the security certificate, you need to confirm this error message (because the appliance uses self-signed certificates).

![picture_01](./datahub-trial-v2-setup_01.png)

In the SAP Data Hub Launchpad, find and open the link **Modeler**. Copy the URL from the webpage address bar and paste it in the frame below and click on **Validate**.

[VALIDATE_1]

[ACCORDION-END]

---
