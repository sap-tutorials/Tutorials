---
title: Install the Optional SAP HANA Automated Predictive Library Package for SAP HANA, express edition
description: SAP HANA Automated Predictive Library (APL) is an application function library which exposes the data mining capabilities of the Automated Analytics engine in SAP HANA, express edition through a set of functions.
primary_tag: products>sap-hana\,-express-edition
tags: [ tutorial>beginner, products>sap-hana\,-express-edition ]
---

<!-- loio31a2f9637e5747298b29c2960d2c286c -->

## Prerequisites
 - **Proficiency:** Beginner
 - **Tutorials:**  You have completed [Start SAP HANA, express edition Server](http://www.sap.com/developer/tutorials/hxe-ua-getting-started-vm.html)  

## Details
### You will learn
You will learn how to download and install the `apl.tgz` SAP HANA Automated Predictive Library (APL) package.

### Time to Complete
3 min

---

SAP HANA Automated Predictive Library is a separate download in the Download Manager. Use the Download Manager to download the SAP HANA Automated Predictive Library package, `apl.tgz`.

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

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Download apl.tgz)]

In your VM, download `apl.tgz` using the built-in Download Manager. From the same directory where you ran `hxe_gc` (`/usr/sap/HXE/home/bin`) enter:

```bash
HXEDownloadManager_linux.bin linuxx86_64 vm apl.tgz
```

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Extract apl.tgz)]

In your VM, extract `apl.tgz`.

```bash
tar -xvzf apl.tgz
```

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Run the installation script)]

As the `hxeadm` user, run the installation script as root:

```bash
<extracted_path>/HANA_EXPRESS_20/sudo ./install_apl.sh
```

[ACCORDION-END]


