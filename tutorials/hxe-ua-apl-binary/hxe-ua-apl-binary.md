---
title: Install the Optional SAP HANA Automated Predictive Library Package for SAP HANA, express edition (Native Linux Machine)
description: SAP HANA Automated Predictive Library (APL) is an application function library which exposes the data mining capabilities of the Automated Analytics engine in SAP HANA, express edition through a set of functions.
author_name: John Currie
author_profile: https://github.com/JCurrie27
primary_tag: products>sap-hana\,-express-edition
tags: [ tutorial>beginner, products>sap-hana\,-express-edition ]
time: 3
---

<!-- loio31a2f9637e5747298b29c2960d2c286c -->

## Prerequisites
 - **Tutorials:**  You have completed [Test the Installation](hxe-ua-test-binary). 

## Details
### You will learn
You will learn how to download, extract, and install the `apl.tgz` SAP HANA Automated Predictive Library (APL) package.

---

SAP HANA Automated Predictive Library is a separate download. Use the commands in this procedure to download the SAP HANA Automated Predictive Library package `apl.tgz` using the built-in Download Manager (console mode).

[ACCORDION-BEGIN [Step 1: ](Download apl.tgz)]

Use the built-in Download Manager (console mode) to download the SAP HANA Automated Predictive Library package, `apl.tgz`.

Navigate to `/usr/sap/HXE/home/bin`:

```bash
cd /usr/sap/HXE/home/bin
```

If your installation is on `x86_64`, enter the following command:

```bash
./HXEDownloadManager_linux.bin linuxx86_64 installer apl.tgz
```

If your installation is on `PowerPC`, enter the following command:

```bash
java -jar HXEDownloadManager.jar linuxppc64le installer apl.tgz
```

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Navigate to the download directory)]

```bash
cd ~/Downloads
```

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Extract apl.tgz)]

```bash
tar -xvzf apl.tgz
```

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Run the installation script)]

As the `<sid>adm` user, run:

```bash
sudo <extracted_path>/HANA_EXPRESS_20/install_apl.sh
```

[DONE]

[ACCORDION-END]


