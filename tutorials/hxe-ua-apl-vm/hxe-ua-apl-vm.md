---
title: Install the Optional SAP HANA Automated Predictive Library Package for SAP HANA, express edition (Preconfigured VM)
description: SAP HANA Automated Predictive Library (APL) is an application function library which exposes the data mining capabilities of the Automated Analytics engine in SAP HANA, express edition through a set of functions.
author_name: John Currie
author_profile: https://github.com/JCurrie27
primary_tag: products>sap-hana\,-express-edition
tags: [ tutorial>beginner, products>sap-hana\,-express-edition ]
time: 3
---

<!-- loio31a2f9637e5747298b29c2960d2c286c -->

## Prerequisites
 - **Tutorials:**  You have completed [Start SAP HANA, express edition Server](hxe-ua-getting-started-vm). 

## Details
### You will learn
You will learn how to download, extract, and install the `apl.tgz` SAP HANA Automated Predictive Library (APL) package in your preconfigured VM.

---

SAP HANA Automated Predictive Library is a separate download. Use the commands in this procedure to download the SAP HANA Automated Predictive Library package `apl.tgz` using the built-in Download Manager (console mode).

[ACCORDION-BEGIN [Step 1: ](Run the memory management script)]

Run the `hxe_gc` memory management script to free up available VM memory.

1.   In your VM, log in as `hxeadm` and enter: 

    ```bash
    cd /usr/sap/HXE/home/bin
    ```

2.   Execute: 

    ```bash
    hxe_gc.sh
    ```

3.   When prompted for System database user (SYSTEM) password, enter the New HANA database master password you specified during SAP HANA, express edition installation 

    The cleanup process runs. The command prompt returns when the cleanup process is finished.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Download apl.tgz)]

In your VM, download `apl.tgz` using the built-in Download Manager. From the same directory where you ran `hxe_gc` (`/usr/sap/HXE/home/bin`) enter:

```bash
HXEDownloadManager_linux.bin linuxx86_64 vm apl.tgz
```

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Extract apl.tgz)]

In your VM, extract `apl.tgz`.

```bash
tar -xvzf apl.tgz
```

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Run the installation script)]

As the `hxeadm` user, run the installation script as root:

```bash
sudo <extracted_path>/HANA_EXPRESS_20/install_apl.sh
```

[DONE]

[ACCORDION-END]


