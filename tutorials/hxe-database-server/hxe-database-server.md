---
title: SAP HANA, express edition - Server Only Deployment Options
description: Learn the options to deploy the database-only version of SAP HANA, express edition in a local computer.
time: 5
author_name: Thomas Jung
author_profile: https://github.com/jung-thomas
tags: [ tutorial>beginner, products>sap-hana\,-express-edition]
primary_tag: products>sap-hana\,-express-edition
---

## Prerequisites  
 - You must register for the product before downloading SAP HANA, express edition. Follow the steps in [Register for SAP HANA, express edition](hxe-ua-register).

## Details
### You will learn  
  - How to deploy locally the database-only version of SAP HANA, express edition

Watch the following video to learn more about the server-only option of SAP HANA, express edition.

<iframe width="560" height="315" src="https://www.youtube.com/embed/FeA2w6Psjto" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

Choose one of the steps below for a local deployment of SAP HANA, express edition.

---

[ACCORDION-BEGIN [Step 1: ](Check RAM memory in your laptop)]

>To deploy locally, you must have 8GB RAM, but 12GB is recommended.

Not sure how much RAM you have?

 - How to check RAM in a [Windows computer](https://developers.sap.com/tutorials/hxe-ram-disk-ms.html)
 - How to check RAM in a [MAC computer](https://developers.sap.com/tutorials/hxe-ram-disk-mac.html)

> ###  Not enough RAM or space on disk?
If you do not have enough RAM, we would recommend SAP HANA Cloud trial which is a completely free, hosted and cloud managed solution. [Deploy SAP HANA Cloud trial](hana-cloud-deploying)

[ACCORDION-END]

[ACCORDION-BEGIN [Option 1: ](Virtual Machine on a Windows or Mac)]

***This is the easiest option.***

SAP HANA, express edition runs on certain distributions of Linux. You can use a Virtual Machine to run on a Windows or Mac PC.

See [Install SAP HANA 2.0, express edition on a Preconfigured Virtual Machine](group.hxe-install-vm).

[ACCORDION-END]


[ACCORDION-BEGIN [Option 2: ](Docker container on a Linux operating system)]

***This option runs on Linux operating systems only.***

Follow these instructions to pull and run the Docker container in any of the supported Linux distributions: [Installing SAP HANA, express edition with Docker](hxe-ua-install-using-docker)


[ACCORDION-END]

[ACCORDION-BEGIN [Option 3: ](Manual installation using Binary Installer)]

***This option requires a minimum level of expertise with Linux operating systems.***

This option allows for some customization of the operating system and the platform.

See [Install SAP HANA, express edition on a Native Linux Machine](group.hxe-install-binary).

[ACCORDION-END]



---
