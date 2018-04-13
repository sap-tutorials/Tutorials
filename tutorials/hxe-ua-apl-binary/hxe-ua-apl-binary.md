---
title: Install the Optional SAP HANA Automated Predictive Library Package for SAP HANA, express edition
description: SAP HANA Automated Predictive Library (APL) is an application function library which exposes the data mining capabilities of the Automated Analytics engine in SAP HANA, express edition through a set of functions.
primary_tag: products>sap-hana\,-express-edition
tags: [ tutorial>beginner, products>sap-hana\,-express-edition ]
---

<!-- loio31a2f9637e5747298b29c2960d2c286c -->

## Prerequisites
 - **Proficiency:** Beginner
 - **Tutorials:**  

## Details
### You will learn
You will learn how to download and install the `apl.tgz` SAP HANA Automated Predictive Library (APL) package package.

### Time to Complete
3 min

---

SAP HANA Automated Predictive Library is a separate download in the Download Manager. Use the Download Manager to download the SAP HANA Automated Predictive Library package, `apl.tgz`.

[ACCORDION-BEGIN [Step 1: ](Download `apl.tgz`.)]

Download `apl.tgz` using the built-in Download Manager. Enter:

```bash
/usr/sap/HXE/home/bin/HXEDownloadManager_linux.bin linuxx86_64 installer apl.tgz
```

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Extract `apl.tgz`.)]

```bash
tar -xvzf apl.tgz
```

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Run the installation script.)]

As the `sid``adm` user, run:

```bash
<extracted_path>/HANA_EXPRESS_20/install_apl.sh
```

[ACCORDION-END]


