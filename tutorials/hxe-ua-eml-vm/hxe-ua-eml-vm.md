---
title: Install the Optional SAP HANA External Machine Learning Library Package for SAP HANA, express edition (Preconfigured VM)
description: The SAP HANA External Machine Learning Library is an application function library (AFL) supporting the integration of Google TensorFlow, as an external machine learning framework, with SAP HANA, express edition.
author_name: John Currie
author_profile: https://github.com/JCurrie27
primary_tag: products>sap-hana\,-express-edition
tags: [ tutorial>beginner, products>sap-hana\,-express-edition ]
time: 3
---

<!-- loioeafe436a2fa34b13908fc0661ff5b8c9 -->

## Prerequisites
 - **Tutorials:**  You have completed [Start SAP HANA, express edition Server](hxe-ua-getting-started-vm). 

## Details
### You will learn
You will learn how to download and install the `eml.tgz` SAP HANA External Machine Learning Library package in your preconfigured VM.

---

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

3.   When prompted for System database user (SYSTEM) password, enter the New HANA database master password you specified during SAP HANA, express edition installation. 

    The cleanup process runs. The command prompt returns when the cleanup process is finished.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Download `eml.tgz`)]

In your VM, download `eml.tgz` using the built-in Download Manager. From the same directory where you ran `hxe_gc` (`/usr/sap/HXE/home/bin`) enter:

```bash
HXEDownloadManager_linux.bin linuxx86_64 vm eml.tgz
```

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Extract `eml.tgz`)]

In your VM, extract `eml.tgz`.

```bash
tar -xvzf eml.tgz
```

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Run the installation script.)]

As the <hxeadm> user, run:

```bash
<extracted_path>/HANA_EXPRESS_20/install_eml.sh
```

[DONE]

[ACCORDION-END]


