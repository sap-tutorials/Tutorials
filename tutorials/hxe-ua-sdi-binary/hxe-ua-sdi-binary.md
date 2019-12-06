---
title: Install the Optional SAP HANA Smart Data Integration Package for SAP HANA, express edition (Native Linux Machine)
description: Install SAP HANA smart data integration on an SAP HANA, express edition system.
author_name: John Currie
author_profile: https://github.com/JCurrie27
primary_tag: products>sap-hana\,-express-edition
tags: [ tutorial>beginner, products>sap-hana\,-express-edition ]
time: 30
---

<!-- loio7621f586085b4a93898290e1571e560a -->

## Prerequisites
 - **Tutorials:**  You have completed [Test the Installation](http://developers.sap.com/tutorials/hxe-ua-test-binary.html)  

## Details
### You will learn
How to install and run the optional SAP HANA Smart Data Integration Package for SAP HANA, express edition.

---

This installs the Data Provisioning Server on SAP HANA, express edition, and deploys the data provisioning delivery unit that enables monitoring and other capabilities.

[ACCORDION-BEGIN [Step 1: ](Download sdi.tgz)]

Download `sdi.tgz` using the built-in Download Manager. Enter:

```bash
HXEDownloadManager_linux.bin linuxx86_64 installer sdi.tgz
```

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Extract sdi.tgz)]

Extract `sdi.tgz`:

```bash
tar -xvzf sdi.tgz
```

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Run the installation script)]

As the <sid>`adm` user, run:

```bash
HANA_EXPRESS_20/install_sdi.sh
```

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Next steps)]

To use adapters other than the OData adapter, you will also need to install the Data Provisioning Agent.

[DONE]

[ACCORDION-END]
