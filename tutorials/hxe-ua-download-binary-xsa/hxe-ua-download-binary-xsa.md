---
title: Download the Installer Files
description: Use the Download Manager to download SAP HANA 2.0, express edition installation packages.
primary_tag: products>sap-hana\,-express-edition
tags: [ tutorial>beginner, products>sap-hana\,-express-edition ]
---

<!-- loioc259de83c51343f7968739cf9be8da8f -->

## Prerequisites
 - **Proficiency:** Beginner
 - **Tutorials:** 

## Details
### You will learn
You will learn how to download the binary image of SAP HANA 2.0, express edition and optional additional components.

### Time to Complete
2 min

---

[ACCORDION-BEGIN [Step 1: ](Select your platform and image.)]

In the Download Manager, in the *Platform* pull-down, select the platform you are installing on. In the *Image* pull-down, select Binary Installer.

Click *Browse* and select a directory for your downloaded files.![loio470c3ab0f8524c43a42d1ee61647e398_LowRes](loio470c3ab0f8524c43a42d1ee61647e398_LowRes.png)

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Select the core packages to download.)]

Select the *Server only installer* package and, if you want to install XSA, the *Applications* package.

-   *Server only installer* downloads `hxe.tgz`, which contains the SAP HANA 2.0, express edition server with Application Function Library. This file is necessary for installing SAP HANA 2.0, express edition.
-   *Applications* downloads `hxexsa.tgz`, which contains XS Advanced, Web IDE, and SAP HANA Cockpit.

    > Note:
    > Some additional components require this package. These components are marked *XSA only* in the list of packages below.
    > 
    > 


> Note:
> SAP plans to remove SAP HANA extended application services, classic model (XSC) and the corresponding SAP HANA Repository with the next major product version of SAP HANA.
> 
> These components will be removed:
> 
> -   SAP HANA extended application services, classic model
> 
> -   SAP HANA Repository (XS classic)
> 
> -   SAP HANA Studio (Development, Modeling, and Administration perspectives)
> 
> -   SAP HANA Web-based Development Workbench (XS classic)
> 
> 
> SAP strongly advises you to plan the transition of existing content and applications from XSC to SAP HANA extended application services, advanced model (XS Advanced).
> 
> 

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Select additional packages.)]

|Package|Details|
|-------|-------|
| *Getting Started with SAP HANA, express edition (Binary Installer Method)* |Downloads a pdf version of this documentation.|
| *Text analysis files for additional languages* |For languages other than English and German, these files are required for the HANA Text Analysis function. (The text analysis files for English and German are already included in the *Server only* and *Applications* packages.)|
| *SAP Enterprise Architect Designer (XSA only)* |Downloads `eadesigner.tgz`. Extract this in the same directory as `hxe.tgz` and `hxexsa.tgz` to include EA Designer in your installation process.|
| *SAP HANA streaming analytics* |Downloads `hsa.tgz`, which contains SAP HANA streaming analytics. Extract this in the same directory as `hxe.tgz` and `hxexsa.tgz` to include streaming analytics in your installation process.|
| *SAP HANA streaming analytics studio plug-in* |Downloads `hsa_plugin.zip`, which contains an Eclipse plugin for creating and deploying streaming analytics projects.|
| *SAP HANA Interactive Education (XSA only)* |Downloads `shine.tgz`. Extract this in the same directory as `hxe.tgz` and `hxexsa.tgz` to include SHINE in your installation process.|
| *SAP HANA External Machine Learning Library* |Downloads `eml.tgz`, which contains the SAP HANA External Machine Learning Library. The SAP HANA External Machine Learning Library is an application function library (AFL) supporting the integration of Google TensorFlow, as an external machine learning framework, with SAP HANA, express edition.|
| *SAP HANA Automated Predictive Library* |Downloads `apl.tgz`, which contains the SAP HANA Automated Predictive Library. The SAP HANA Automated Predictive Library is an application function library which exposes the data mining capabilities of the Automated Analytics engine in SAP HANA through a set of functions.|
| *Clients (Linux X86/64)* |Downloads `clients_linux_x86_64.tgz`. Each clients package downloads an archive containing client-tools bundles for the listed platform. Use the client packages to access developed SAP HANA 2.0, express edition applications from a client PC. The client machine does not require a SAP HANA installation to install and run the clients. **Tip:** After you develop an application using SAP HANA 2.0, express edition, install Download Manager to a client machine and download the *Clients* package to that client machine. You can then use the clients to connect to -- and test -- your HANA applications, emulating a customer.|
| *Clients (Linux PPC/Little Endian)* |Downloads `clients_linux_ppc64le.tgz`. Each clients package downloads an archive containing client-tools bundles for the listed platform. Use the client packages to access developed SAP HANA 2.0, express edition applications from a client machine. The client machine does not require a SAP HANA installation to install and run the clients. **Tip:** After you develop an application using SAP HANA 2.0, express edition, install Download Manager to a client machine and download the *Clients* package to that client machine. You can then use the clients to connect to -- and test -- your HANA applications, emulating a customer.|
| *Clients (Windows)* |Downloads `clients_windows.zip`. Each clients package downloads an archive containing client-tools bundles for the listed platform. Use the client packages to access developed SAP HANA 2.0, express edition applications from a client machine. The client machine does not require a SAP HANA installation to install and run the clients. **Tip:** After you develop an application using SAP HANA 2.0, express edition, install Download Manager to a client machine and download the *Clients* package to that client machine. You can then use the clients to connect to -- and test -- your HANA applications, emulating a customer.|
| *Clients (Mac)* |Downloads `clients_mac.tgz`. Each clients package downloads an archive containing client-tools bundles for the listed platform. Use the client packages to access developed SAP HANA 2.0, express edition applications from a client machine. The client machine does not require a SAP HANA installation to install and run the clients. **Tip:** After you develop an application using SAP HANA 2.0, express edition, install Download Manager to a client machine and download the *Clients* package to that client machine. You can then use the clients to connect to -- and test -- your HANA applications, emulating a customer.|
| *SAP HANA smart data integration* |Downloads `sdi.tgz`. SAP HANA smart data integration provides functionality to access source data, and to provision, replicate, and transform that data in SAP HANA on premise, or in the cloud.|
| *SAP HANA smart data integration - Data Provisioning Agent (Linux X86/64)* |Downloads `dpagent_linux_x86_64.tgz`. The Data Provisioning Agent provides secure connectivity between the SAP HANA database and your adapter-based sources.|

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Download the files.)]

Click the *Download* button.

Your download is complete when a pop-up message appears confirming successful download. Make sure you wait for this message before accessing the downloaded files.

[ACCORDION-END]


