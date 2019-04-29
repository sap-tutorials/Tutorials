---
title: Machine Requirements
description: Check if your machine has the recommended software and hardware to successfully install and run SAP HANA 2.0, express edition. The Server + Applications package requires more RAM than the server-only package.
author_name: Aaron Patkau
author_profile: https://github.com/aptk001
primary_tag: products>sap-hana\,-express-edition
tags: [ tutorial>beginner, products>sap-hana\,-express-edition ]
time: 5
---

<!-- loioc3807913b0a340a99822bf0d97a01da6 -->

## Prerequisites

## Details
### You will learn
Check if your machine has the recommended software and hardware to successfully install and run SAP HANA 2.0, express edition (with the Applications package).

---

> Note:
> Additional documentation, including downloadable PDF manuals, is available on the [SAP Help Portal](https://help.sap.com/viewer/p/SAP_HANA,_EXPRESS_EDITION).
> 
> 

[ACCORDION-BEGIN [Step 1: ](Ensure that your system meets the hardware requirements.)]

Check if your system has the required hardware to successfully install and run SAP HANA 2.0, express edition.

|Hardware|Details|
|--------|-------|
|RAM|Server + Applications: 24 GB minimum (32 GB recommended)|
|HDD|120 GB HDD recommended|
|Cores|2 cores (4 recommended)|

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Ensure that your system meets the software requirements.)]

Check if your system has the required software to successfully install and run SAP HANA 2.0, express edition.

-   **Java Runtime Environment (JRE) 8 or Higher**: If you are planning to use the SAP HANA 2.0, express edition Download Manager for Windows or Linux, you need the 64-bit JRE. If you are planning to use the platform-independent Download Manager, you can use either the 32- or 64-bit versions. You can download the SAP JVM (64-bit) from [https://tools.hana.ondemand.com/#cloud](https://tools.hana.ondemand.com/#cloud).
-   **Operating System**:

    -   SUSE Linux Enterprise Server for SAP Applications, 12.1, 12.2, 12.3 (SPS 02 Rev 23 or higher)
    -   SUSE Linux Enterprise Server for SAP Applications, IBM Power Systems ( I t - "Little Endian"), 12.1, 12.2

        > Note:
        > To install on SLES for SAP 12.1, the `libgcc_s1` and `libstdc++6` packages must be version 6.2 or newer. To update these packages, register your system with SUSE and run `zypper install libgcc_s1 libstdc++6`. For registration instructions, see the SUSE Linux Enterprise Server 12 documentation.
        > 
        > 

    -   Red Hat Enterprise Linux for SAP Applications 7.2, 7.3 (SPS 02 Rev 21 or higher), 7.4 (SPS 02 Rev 23 or higher)
    -   Red Hat Enterprise Linux for SAP Applications for Power 7.3 (SPS 02 Rev 21 or higher), 7.4 (SPS 02 Rev 23 or higher)

        > Note:
        > To install on RHEL for SAP, first install the `compat-sap-c++-6` package using the following command: `yum install compat-sap-c++-6`.
        > 
        > 

    > Note:
    > The following components are not available on IBM Power Systems:
    > 
    > -   SAP Enterprise Architecture Designer
    > -   SAP HANA streaming analytics
    > -   SAP HANA External Machine Learning AFL
    > 


[DONE]

[ACCORDION-END]


