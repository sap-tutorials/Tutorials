---
title: Updating SAP HANA, express edition (Binary Installer)
description: Update SAP HANA 2.0, express edition when new patches are released.
primary_tag: products>sap-hana\,-express-edition 
tags: [  tutorial>beginner, products>sap-hana\,-express-edition  ]
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

[ACCORDION-BEGIN [Step 1: ](Download update files)]

1. Using the download manager, select Binary Installer and download the Server only package. If the installation you are updating has the Applications package, download the Applications package as well.

2. Extract the contents of the packages.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Update the server installation)]

1. Navigate to the following directory:

    ```
    cd <download_path>/HANA_EXPRESS_20/DATA_UNITS/HDB_SERVER_LINUX_X86_64
    ```

2. As the root user, run one of the following commands to begin the SAP HANA server update:

    - If AFL is installed:

    ```
    sudo ./hdbupd --ignore=check_min_mem,check_plugin_dependencies
    ```
    - If AFL is not installed:

    ```
    sudo ./hdbupd --ignore=check_min_mem
    ```

3. Follow the prompts to complete the server update.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Update the applications package)]

1. Navigate to the following directory:

    ```
    cd <download_path>/HANA_EXPRESS_20/DATA_UNITS/HDB_SERVER_LINUX_X86_64
    ```

2. As the root user, run the following command to update XSA applications:

    ```
    sudo ./hdblcm --action=update --components=xs --xs_components=all --configfile=configurations/auto_install.cfg --component_medium=<download_path>/HANA_EXPRESS_20
    ```

3. Follow the prompts to complete the XSA update.

4. Run `hxe_optimize.sh` as `<sid>adm` user.

    >**Note:**
    > `hxe_optimize.sh` is located in `<download_path>/HANA_EXPRESS_20`

[DONE]
[ACCORDION-END]

---

## Next Steps
- Select a tutorial from the [Tutorial Navigator](http://www.sap.com/developer/tutorial-navigator.html) or the [Tutorial Catalog](http://www.sap.com/developer/tutorials.html)
