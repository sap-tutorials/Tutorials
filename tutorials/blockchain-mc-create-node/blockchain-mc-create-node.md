---
title: Create a MultiChain Trial Node
description: Create a MultiChain trial node on SAP Cloud Platform and access your dashboard.
auto_validation: true
time: 10
tags: [tutorial>beginner, topic>blockchain, products>sap-cloud-platform, topic>cloud]
primary_tag: topic>blockchain
author_name: José Prados
author_profile: https://github.com/joseprados

---

## Details
### You will learn
  - How to provision a MultiChain trial node on SAP Cloud Platform
  - How to access your MultiChain dashboard

---

[ACCORDION-BEGIN [Step 1: ](Understand MultiChain on SAP Cloud Platform)]

The MultiChain service on SAP Cloud Platform lets you create, delete, monitor, and maintain individual MultiChain nodes and then connect them to a blockchain network.

This service is supported by Small, Medium, Large, and Connect Your Own Network productive service plans.

In addition to productive service plans, we also offer a Trial service plan on the trial landscape. This plan comes with the following conditions:

- One trial service instance per SAP Cloud Platform Global Account

- MultiChain Service instances expire after 14 days, including the loss of the blockchain data (if not synced to a wider network), wallet data (public and private keys), and external IP addresses used to connect to other nodes.

- An existing service instance, whether expired or not, must be deleted before a new service instance can be provisioned within the Global Account.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Open the MultiChain service)]

Once on the SAP Cloud Platform Service Marketplace, locate and open the MultiChain service by clicking the relevant service tile.

![Image depicting SAP Cloud Platform marketplace](01--ServiceMarketplace.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create MultiChain instance)]

Once in the MultiChain service, you will see a service description and the available plans. In this tutorial you will create a trial node. This allows you to provision a single testing node for a 14 day period. These 14 days run concurrently with your 30-day global account trial on SAP Cloud Platform, rather than activating an extension or superseding your global account trial terms.

Click the **Instances** tab on the side menu, opening an overview of available MultiChain instances in your subaccount.

![Image depicting MultiChain Service dashboard](02--Instances.png)

Once on your MultiChain instances overview, click **New Instance** to open the service instance wizard.

![Image depicting MultiChain service instances overview](03--Create-Instances.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Choose service instance settings)]

Navigate through the service instance wizard, selecting the following settings:

Field | Value
:------|:--------
**Plan**  | `trial`
**Name** | `MultiChain`

After selecting the settings, click **Finish**.

![Image depicting Multichain service instance wizard](04--Create-Instances-Window.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Access MultiChain dashboard)]

Your trial node is provisioned (which may take a few moments) and is displayed on the overview of available service instances.

![Image depicting MultiChain node provisioned](05--Confirmation.png)

Click the **Dashboard** icon, opening your MultiChain dashboard.

![Image depicting MultiChain node provisioned](06--Dashboard.png)

[VALIDATE_1]
[ACCORDION-END]
