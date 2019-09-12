---
title: Install Optional Packages
description:   Once SAP HANA, express edition (server only) is installed and running, use the server's built-in command line Download Manager to download optional installation packages directly to your system.  
author_name: John Currie
author_profile: https://github.com/JCurrie27
primary_tag: products>sap-hana\,-express-edition
tags: [ tutorial>beginner, products>sap-hana\,-express-edition ]
time: 20
---

<!-- loio6bb4c7e861654519bb922e7e80a77a0b -->

## Prerequisites
 - **Tutorials:**  You have completed [Test the Installation](hxe-ua-test-binary) 

## Details
### You will learn
 You will learn how to use the built-in command line Download Manager to install optional installation packages for your native Linux installation. 

---

This tutorial assumes you already installed SAP HANA, express edition.

Using the server's built-in command line Download Manager to download optional installation packages directly to your server is the fastest and easiest way to install optional packages, as it saves you from having to transfer files from your client machine to the server.

[ACCORDION-BEGIN [Step 1: ](Choose optional installation packages)]

Decide what optional installation packages you want to install.

|Optional Installation Package|Description|
|-----------------------------|-----------|
|Text analysis files for additional languages|Downloads `additional_lang.tgz`. For languages other than English and German, this package is required for the HANA Text Analysis function. (The text analysis files for English and German are already included in the server.)|
|SAP HANA streaming analytics|Downloads `hsa.tgz`, which contains SAP HANA streaming analytics.|
|SAP HANA External Machine Learning Library|Downloads `eml.tgz`. The SAP HANA External Machine Learning Library is an application function library (AFL) supporting the integration of `Google TensorFlow`, as an external machine learning framework, with SAP HANA, express edition.|
|SAP HANA Automated Predictive Library|Downloads `apl.tgz`. SAP HANA Automated Predictive Library (APL) is an application function library which exposes the data mining capabilities of the Automated Analytics engine in SAP HANA, express edition through a set of functions.|
|Clients|Downloads client packages and the SAP HANA Machine Learning Python API.|
|SAP HANA smart data integration|SAP HANA smart data integration provides functionality to access source data, and to provision, replicate, and transform that data in SAP HANA on premise, or in the cloud.|

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Locate the package-specific installation tutorials)]

Optional installation packages have their own tutorials. Use this table to find the installation tutorial you're looking for:

|Optional Installation Package|Installation Tutorial|
|-----------------------------|---------------------|
|Text analysis files for additional languages|[Install the Optional Text Analysis Files Package for SAP HANA, express edition](hxe-ua-text-analysis-binary)|
|SAP HANA streaming analytics|[Get started with SAP HANA streaming analytics for SAP HANA, express edition](https://developers.sap.com/group.sds-hxe-get-started.html)|
|SAP HANA External Machine Learning Library|[Install the Optional SAP HANA External Machine Learning Library Package for SAP HANA, express edition](hxe-ua-eml-binary)|
|SAP HANA Automated Predictive Library|[Install the Optional SAP HANA Automated Predictive Library Package for SAP HANA, express edition](hxe-ua-apl-binary)|
|Clients| [Install the SAP HANA, express edition Clients](https://developers.sap.com/group.hxe-install-clients.html) |
|SAP HANA smart data integration| [Install the Optional SAP HANA Smart Data Integration Package for SAP HANA, express edition](https://developers.sap.com/tutorials/hxe-ua-sdi-binary.html) |

[DONE]

[ACCORDION-END]


