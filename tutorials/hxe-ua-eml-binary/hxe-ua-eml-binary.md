---
title: Install the Optional SAP HANA External Machine Learning Library Package for SAP HANA, express edition (Native Linux Machine)
description: The SAP HANA External Machine Learning Library is an application function library (AFL) supporting the integration of Google TensorFlow, as an external machine learning framework, with SAP HANA, express edition.
author_name: John Currie
author_profile: https://github.com/JCurrie27
primary_tag: products>sap-hana\,-express-edition
tags: [ tutorial>beginner, products>sap-hana\,-express-edition ]
time: 3
---

<!-- loioeafe436a2fa34b13908fc0661ff5b8c9 -->

## Prerequisites
 - **Tutorials:**  You have completed [Test the Installation](hxe-ua-test-binary). 

## Details
### You will learn
You will learn how to download and install the `eml.tgz` SAP HANA External Machine Learning Library package.

---

[ACCORDION-BEGIN [Step 1: ](Download `eml.tgz`)]

Use the Download Manager to download HANA External Machine Learning AFL, `eml.tgz`.

> Note:
> If you're using Download Manager (GUI Mode) make sure you're using the latest version.
> 
> 

Navigate to `/usr/sap/HXE/home/bin`:

```bash
/usr/sap/HXE/home/bin
```

Enter the following command:

```bash
./HXEDownloadManager_linux.bin linuxx86_64 installer eml.tgz
```

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Extract `eml.tgz`)]

Extract `eml.tgz`:

```bash
tar -xvzf eml.tgz
```

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Run the installation script)]

As the <sid>`adm` user, run:

```bash
<extracted_path>/HANA_EXPRESS_20/install_eml.sh
```

For more information on the SAP HANA External Machine Learning Library, see the [SAP HANA documentation collection](https://help.sap.com/viewer/p/SAP_HANA_PLATFORM).

[DONE]

[ACCORDION-END]


