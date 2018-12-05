---
title: Install the Optional SAP HANA Smart Data Integration Package for SAP HANA, express edition
description: Install SAP HANA smart data integration on an SAP HANA, express edition system.
primary_tag: products>sap-hana\,-express-edition
tags: [ tutorial>beginner, products>sap-hana\,-express-edition ]
---

<!-- loio7621f586085b4a93898290e1571e560a -->

## Prerequisites
 - **Proficiency:** Beginner
 - **Tutorials:**  You have completed [Test the Installation](http://developers.sap.com/tutorials/hxe-ua-test-binary.html)  

## Details
### You will learn
How to install and run the optional SAP HANA Smart Data Integration Package for SAP HANA, express edition.

### Time to Complete
30 min

---

This installs the Data Provisioning Server on SAP HANA, express edition, and deploys the data provisioning delivery unit that enables monitoring and other capabilities.

[ACCORDION-BEGIN [Step 1: ](Download sdi.tgz)]

Download `sdi.tgz` using the built-in Download Manager. Enter:

```bash
HXEDownloadManager_linux.bin linuxx86_64 installer sdi.tgz
```

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Extract sdi.tgz)]

Extract `sdi.tgz`:

```bash
tar -xvzf sdi.tgz
```

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Run the installation script)]

As the `sid``adm` user, run:

```bash
HANA_EXPRESS_20/install_sdi.sh
```

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Next steps)]

To use adapters other than the OData adapter, you will also need to install the Data Provisioning Agent.

[ACCORDION-END]


