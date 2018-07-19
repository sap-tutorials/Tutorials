---
title: Install the Optional SAP HANA Interactive Education Package for SAP HANA, express edition
description: SAP HANA Interactive Education (SHINE) makes it easy to learn how to build applications on SAP HANA Extended Application Services Advanced Model (XSA).
primary_tag: products>sap-hana\,-express-edition
tags: [ tutorial>beginner, products>sap-hana\,-express-edition ]
---

<!-- loio5a9927fcad7c436d9fb8a36062acc1b8 -->

## Prerequisites
 - **Proficiency:** Beginner
 - **Tutorials:**  You have completed [Start SAP HANA, express edition Server](http://www.sap.com/developer/tutorials/hxe-ua-getting-started-vm.html)  

## Details
### You will learn
You will learn how to download, install, and configure the `shine.tgz` SAP HANA Interactive Education package.

### Time to Complete
15 min

---

SHINE is a separate download in the Download Manager. To use SHINE, you need the Server + Applications Virtual Machine (`hxexsa.ova`) package. Use the Download Manager to download the SAP HANA Interactive Education package, `shine.tgz`.

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

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Download `shine.tgz`)]

In your VM, download `shine.tgz` using the built-in Download Manager. From the same directory where you ran `hxe_gc` (`/usr/sap/HXE/home/bin`) enter:

```bash
HXEDownloadManager_linux.bin linuxx86_64 vm shine.tgz
```

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Extract `shine.tgz`)]

In your VM, extract `shine.tgz`:

```bash
tar -xvzf shine.tgz
```

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Run the installation script)]

As the `hxeadm` user, run:

```bash
<extracted_path>/HANA_EXPRESS_20/install_shine.sh
```

[ACCORDION-END]


