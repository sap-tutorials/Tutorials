---
title: Updating SAP HANA, express edition (Virtual Machine)
description: Update your SAP HANA 2.0, express edition VM installation when new patches are released.
primary_tag: products>sap-hana\,-express-edition
tags: [  tutorial>beginner, products>sap-hana\,-express-edition ]
time: 15
---

## Details
Update SAP HANA 2.0, express edition when new support packages and patches are released. Upgrading is supported only for SAP HANA, express edition 2.0 (SP 00 onward). Upgrading from SAP HANA, express edition 1.0 SP 12 is not supported.

> If you are updating to SAP HANA, express edition 2.0 SP 02, the `libgcc_s1` and `libstdc++6` packages must be version 6.2 or newer. To update these packages, register your system with SUSE and run `zypper install libgcc_s1 libstdc++6`. For registration instructions, see the SUSE Linux Enterprise Server 12 documentation.


---

[ACCORDION-BEGIN [Step 1: ](Download Updated Packages)]

Log in as `hxeadm`. Depending on your version of SAP HANA, express edition, you will either use the built-in update utility, or download the new versions of SAP HANA, express edition through the Download Manager. Your primary choice should be the built-in update utility.

* Check the VM's built-in update utility. It can be found in the `home/bin` directory. Follow the prompts to download the new files. By default, they will be downloaded to `/usr/sap/HXE/home/Downloads`. The downloaded files will be `hxe.tgz`, and `hxexsa.tgz` for Applications if applicable.

* Check the VM's built-in update utility found in the `~/home/bin` directory. Depending on your VM, use either `HXECheckUpdate_linux.bin` for `Linux x86-64`, and `HXECheckUpdate.jar` for `Linux PPC64`. Follow the prompts to download the new files. By default, they will be downloaded to `~/usr/sap/HXE/home/Downloads`. The downloaded files will be `hxe.tgz`, and `hxexsa.tgz` for Applications if applicable.

    ```
    HXECheckUpdate_linux.bin -a
    ```

* If you do not have the update utility, use the Download Manager. Select **Binary Installer** and download the latest files for `hxe.tgz` (server-only), and `hxexsa.tgz` (Applications) if applicable.


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

>`hxe_upgrade.sh` detects the Server-only and Applications packages. The script will upgrade the server and XSA (if available).



[DONE]

[ACCORDION-END]

---
