---
parser: v2
author_name: John Currie
author_profile: https://github.com/JCurrie27
primary_tag: products>sap-hana\,-express-edition
tags: [ tutorial>beginner, products>sap-hana\,-express-edition ]
time: 20
---

# Install Optional Packages
<!-- description -->   Once SAP HANA, express edition (server and applications) is installed and running, use the server's built-in command line Download Manager to download optional installation packages directly to your VM.  

<!-- loio6bb4c7e861654519bb922e7e80a77a0b -->

## Prerequisites
 - **Tutorials:**  You have completed [Start SAP HANA, express edition Server](hxe-ua-getting-started-vm) 

## You will learn
 You will learn how to use the VM (server plus applications) image's built-in command line Download Manager to install optional installation packages. 

---

## Intro
This tutorial assumes you already installed SAP HANA, express edition.

It is possible to download optional installation packages during the server installation process – at the step when you download the OVA file – but SAP recommends you use this tutorial to install optional packages **after** you've installed, started, and tested SAP HANA, express edition.

Using the server's built-in command line Download Manager to download optional installation packages directly to your VM is the fastest and easiest way to install optional packages, as it saves you from having to transfer files from your client machine to the VM.

### Choose optional installation packages


Decide what optional installation packages you want to install.

|Optional Installation Package|Description|
|-----------------------------|-----------|
|Text analysis files for additional languages|Downloads `additional_lang.tgz`. For languages other than English and German, this package is required for the HANA Text Analysis function. (The text analysis files for English and German are already included in the server.)|
|SAP Enterprise Architecture Designer|Downloads `eadesigner.tgz`. SAP EA Designer lets you capture, analyze, and present your organization's landscapes, strategies, requirements, processes, data, and other artifacts in a shared environment.|
|SAP HANA streaming analytics|Downloads `hsa.tgz`, which contains SAP HANA streaming analytics.|
|SAP HANA Interactive Education|Downloads `shine.tgz`. SHINE makes it easy to learn how to build applications on SAP HANA Extended Application Services Advanced Model (XSA).|
|SAP HANA External Machine Learning Library|Downloads `eml.tgz`. The SAP HANA External Machine Learning Library is an application function library (AFL) supporting the integration of `Google TensorFlow`, as an external machine learning framework, with SAP HANA, express edition.|
|SAP HANA Automated Predictive Library|Downloads `apl.tgz`. SAP HANA Automated Predictive Library (APL) is an application function library which exposes the data mining capabilities of the Automated Analytics engine in SAP HANA, express edition through a set of functions.|
|Clients|Downloads client packages and the SAP HANA Machine Learning Python API.|
|SAP HANA smart data integration|SAP HANA smart data integration provides functionality to access source data, and to provision, replicate, and transform that data in SAP HANA on premise, or in the cloud.|


### Locate the package-specific installation tutorials


Optional installation packages have their own tutorials. Use this table to find the installation tutorial you're looking for:

|Optional Installation Package|Installation Tutorial|
|-----------------------------|---------------------|
|Text analysis files for additional languages|[Install the Optional Text Analysis Files Package for SAP HANA, express edition](hxe-ua-text-analysis-vm)|
|SAP Enterprise Architecture Designer| [Install the Optional SAP Enterprise Architecture Designer Package for SAP HANA, express edition](hxe-ua-installing-eads-vm) |
|SAP HANA streaming analytics|[Get started with SAP HANA streaming analytics for SAP HANA, express edition](https://developers.sap.com/group.sds-hxe-get-started.html)|
|SAP HANA Interactive Education| [Install the Optional SAP HANA Interactive Education Package for SAP HANA, express edition](hxe-ua-shine-vm) |
|SAP HANA External Machine Learning Library|[Install the Optional SAP HANA External Machine Learning Library Package for SAP HANA, express edition](hxe-ua-eml-vm)|
|SAP HANA Automated Predictive Library|[Install the Optional SAP HANA Automated Predictive Library Package for SAP HANA, express edition](hxe-ua-apl-vm)|
|Clients| [Install the SAP HANA, express edition Clients](https://developers.sap.com/group.hxe-install-clients.html) |
|SAP HANA smart data integration| [Install the Optional SAP HANA Smart Data Integration Package for SAP HANA, express edition](https://developers.sap.com/tutorials/hxe-ua-sdi-vm.html) |



