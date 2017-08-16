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
Update SAP HANA 2.0, express edition when new support packages and patches are released. Upgrading is supported only for SAP HANA, express edition 2.0 (SP 00 onward). Upgrading from SAP HANA, express edition 1.0 SP 12 is not supported.

> Note:
> If you are updating to SAP HANA, express edition 2.0 SP 02, the `libgcc_s1` and `libstdc++6` packages must be version 6.2 or newer. To update these packages, register your system with SUSE and run `zypper install libgcc_s1 libstdc++6`. For registration instructions, see the SUSE Linux Enterprise Server 12 documentation.

### Time to Complete
**15 Min**

---

[ACCORDION-BEGIN [Step 1: ](Download Updated Packages)]

Using the VM's built-in Download Manager (console mode), download the **binary** server-only package (`hxe.tgz`). If the installation you are updating has the Applications package, download the applications package (`hxexsa.tgz`) as well.

```
HXEDownloadManager_linux.bin linuxx86_64 installer hxe.tgz hxexsa.tgz
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Extract the Packages)]

Login as `hxeadm` and extract the contents of the packages.

Example:

```
su -l <sid>adm
tar -zxf <downloaded_path>/hxe.tgz
tar -zxf <downloaded_path>/hxexsa.tgz
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Update the Server Installation)]

Navigate to the directory where you extracted the packages:

```
cd <extract_path>/HANA_EXPRESS_20/
```

> Note:
> If the update you are applying includes the Applications package, increase your allocated memory by 3GB and run `./hxe_gc.sh`. Follow the prompts and then continue with the procedure.

.

As the root user, run the upgrade script to update the server:

```
sudo ./hxe_upgrade.sh
```

Follow the prompts to complete the server update.

>**Note**
>`hxe_upgrade.sh` detects the Server-only and Applications packages. The script will upgrade the server and XSA (if available).


[DONE]
[ACCORDION-END]

---

## Next Steps
- Select a tutorial from the [Tutorial Navigator](http://www.sap.com/developer/tutorial-navigator.html) or the [Tutorial Catalog](http://www.sap.com/developer/tutorials.html)
