---
title: Installing the Optional Data Warehousing Foundation Package for SAP HANA, express edition (Preconfigured VM)
description: Download the `Data Warehousing Foundation` package in the Download Manager.
author_name: John Currie
author_profile: https://github.com/JCurrie27
primary_tag: products>sap-hana\,-express-edition
tags: [ tutorial>beginner, products>sap-hana\,-express-edition ]
time: 15
---

<!-- loiocfbb826828fd4342a9ec06f90a5dd11a -->

## Prerequisites
 - **Tutorials:**  You have completed [Start SAP HANA, express edition Server](hxe-ua-getting-started-vm)

## Details
### You will learn
You will learn how to download and install the `dwf.tgz` Data Warehousing Foundation package.

---

[ACCORDION-BEGIN [Step 1: ](Run the memory management script)]

Run the `hxe_gc` memory management script to free up available VM memory

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

[ACCORDION-BEGIN [Step 2: ](Download dwf.tgz)]

In your VM, download `dwf.tgz` using the built-in Download Manager. From the same directory where you ran `hxe_gc` (`/usr/sap/HXE/home/bin`) enter:

```bash
HXEDownloadManager_linux.bin linuxx86_64 vm dwf.tgz
```

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Navigate to the Downloads directory)]

In your VM, enter:

```bash
cd /usr/sap/HXE/home/Downloads
```

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](View the contents of the Downloads folder to confirm dwf.tgz exists)]

Enter:

```bash
ls
```

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Extract the file)]

In your VM, enter:

```bash
tar -xvzf dwf.tgz
```

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Navigate to the HANA_EXPRESS_20 directory)]

In your VM, enter:

```bash
cd HANA_EXPRESS_20
```

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Run the installation script)]

As the `hxeadm` user, run:

```bash
HANA_EXPRESS_20/install_dwf.sh
```

[DONE]

[ACCORDION-END]
