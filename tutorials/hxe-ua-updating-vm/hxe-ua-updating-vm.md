---
title: Updating SAP HANA, express edition (Virtual Machine)
description: Update your SAP HANA 2.0, express edition VM installation when new patches are released.
primary_tag: products>sap-hana\,-express-edition
tags: [  tutorial>beginner, products>sap-hana\,-express-edition ]
---

## Prerequisites  
 - **Proficiency:** Beginner


## Next Steps
 - Select a tutorial from the [Tutorial Navigator](http://www.sap.com/developer/tutorial-navigator.html) or the [Tutorial Catalog](http://www.sap.com/developer/tutorials.html)

## Details
Update SAP HANA 2.0, express edition when new patches are released.

### Time to Complete
**15 Min**

---

[ACCORDION-BEGIN [Step 1: ](Download Update Files)]

Using the Download Manager, select Binary Installer and download the Server only package (`hxe.tgz`). If the installation you are updating has the Applications package, download the Applications package (`hxexsa.tgz`) as well.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Copy Packages to the Virtual Machine)]

Using an FTP client, copy the packages from your local machine to the virtual machine.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Extract the Packages)]

Login as `<sid>adm` and extract the contents of the packages.

Example:

```
su -l <sid>adm
tar -zxf <downloaded_path>/hxe.tgz
tar -zxf <downloaded_path>/hxexsa.tgz
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Update the Server Installation)]

Navigate to the directory where you extracted the packages:

```
cd <extract_path>/HANA_EXPRESS_20/
```

As the root user, run the upgrade script to update the server:

```
sudo ./hxe_upgrade.sh
```

Follow the prompts to complete the server update.

>**Note**
>`hxe_upgrade.sh` detects the Server-only and Applications packages. The script will upgrade the server and XSA (if available).

>**Note**
> Only upgrading from SAP HANA, express edition 2.0 SP 00 to SAP HANA, express edition 2.0 SP 01 is supported.

>**Note**
> If you receive the error message
> ```
    Installation of archive file(s) '/usr/sap/HXE_2_SP1/HANA_EXPRESS_20/DATA_UNITS/HANA_COCKPIT_20/XSACCOCKPIT02_5.zip' failed
  ```
> see troubleshooting topic [Update Fails with Error Message](https://www.sap.com/developer/how-tos/2016/09/hxe-ua-troubleshooting.html).

[DONE]
[ACCORDION-END]

---

## Next Steps
- Select a tutorial from the [Tutorial Navigator](http://www.sap.com/developer/tutorial-navigator.html) or the [Tutorial Catalog](http://www.sap.com/developer/tutorials.html)
