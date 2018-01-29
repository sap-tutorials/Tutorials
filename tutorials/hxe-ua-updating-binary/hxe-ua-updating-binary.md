---
title: Updating SAP HANA, express edition (Binary Installer)
description: Update your SAP HANA 2.0, express edition binary installation when new patches are released.
primary_tag: products>sap-hana\,-express-edition  
tags: [  tutorial>beginner, products>sap-hana\,-express-edition   ]
---

## Prerequisites  
 - **Proficiency:** Beginner


## Next Steps
 - Select a tutorial from the [Tutorial Navigator](https://www.sap.com/developer/tutorial-navigator.html) or the [Tutorial Catalog](https://www.sap.com/developer/tutorial-navigator.tutorials.html)

## Details
Update SAP HANA 2.0, express edition when new patches are released.

> Note:
> If you are updating to SAP HANA, express edition 2.0 SP 02, the `libgcc_s1` and `libstdc++6` packages must be version 6.2 or newer. To update these packages, register your system with SUSE and run `zypper install libgcc_s1 libstdc++6`. For registration instructions, see the SUSE Linux Enterprise Server 12 documentation.

### Time to Complete
**15 Min**

---

[ACCORDION-BEGIN [Step 1: ](Download Update Files)]

Login as `<sid>adm`. Depending on your current version of SAP HANA, express edition, you will either use the built-in update utility, or download the through the Download Manager. Your primary choice should be the built-in update utility.

* Check the update utility. It can be found in the `<sid>adm/bin` directory. For `Linux x86_64`, use `HXECheckUpdate_liux.bin`, and for `Linux PPC64`, use `HXECheckUpdate.jar`. Run one of these to check for, and download, the latest files. Follow the prompts to download the new files. By default, they will be downloaded to `/usr/sap/<sid>/home/Downloads`. The downloaded files will be `hxe.tgz`, and `hxexsa.tgz` for Applications if applicable.

* If you do not have the update utility, use the Download Manager, select Binary Installer and download the latest files for `hxe.tgz`, and `hxexsa.tgz` for Applications if applicable.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Extract the Packages)]

Extract the contents of the packages.

Example:

```bash
su -l <sid>adm
tar -zxf <downloaded_path>/hxe.tgz
tar -zxf <downloaded_path>/hxexsa.tgz
```


[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Update the Server Installation)]

Navigate to the directory where you extracted the packages:

```bash
cd <extract_path>/HANA_EXPRESS_20/
```

> Note:
> If the update you are applying includes the Applications package, increase your allocated memory by 3GB and run `./hxe_gc.sh`. Follow the prompts and then continue with the procedure.

.

As the root user, run the upgrade script to update the server:

```bash
sudo ./hxe_upgrade.sh
```

Follow the prompts to complete the server update.

>**Note**
>`hxe_upgrade.sh` detects the Server-only and Applications packages. The script will upgrade the server and XSA (if available).

>**Note**
> Upgrading is supported only for SAP HANA, express edition 2.0 SP 00 and newer.


[ACCORDION-END]

---

## Next Steps
- Select a tutorial from the [Tutorial Navigator](https://www.sap.com/developer/tutorial-navigator.html) or the [Tutorial Catalog](https://www.sap.com/developer/tutorial-navigator.tutorials.html)
