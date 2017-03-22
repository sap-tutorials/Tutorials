---
title: Configure SAP HANA 2.0, express edition Security
description: Your SAP HANA, express edition installation has several preconfigured security settings. Before using SAP HANA, express edition, complete these security tasks.
primary_tag: products>sap-hana\,-express-edition
tags: [  tutorial>beginner, products>sap-hana\,-express-edition ]
---
## Prerequisites  
 - **Proficiency:** Beginner
 - **Tutorials:** [Start Using SAP HANA 2.0, express edition](http://www.sap.com/developer/tutorials/hxe-ua-getting-started-vm.html)

## Next Steps
 - Select a tutorial from the [Tutorial Navigator](http://www.sap.com/developer/tutorial-navigator.html) or the [Tutorial Catalog](http://www.sap.com/developer/tutorials.html)

## Details
### You will learn  
How to perform security tasks to ensure that your security settings are not known outside your organization.

### Time to Complete
**5 Min**.

---

## The SAP HANA, express-edition License

Installing SAP HANA 2.0, express edition installs a permanent 32 GB license automatically. No license configuration is required.

## Change the SSFS Master Keys and Root Keys

Every user who downloads SAP HANA, express edition receives the same default encryption settings. Use the **`change_key.sh`** utility to change encryption automatically.

>**Important**: SAP recommends you run **`change_key.sh`** immediately after installation to ensure your security settings are not known outside your organization.

The **`change_key.sh`** utility:

- Changes the secure stores in the file system (SSFS) master keys. The script re-encrypts the master key of the instance SSFS and re-encrypts the system PKI SSFS with a new key.

- Changes the encryption root keys. The script generates new keys, backs them up, and activates them.

Follow this procedure to run the script:

1. At the command prompt, type:
    ```
    /usr/sap/HXE/home/bin/change_key.sh
    ```

2. The script prompts you for:

Prompt         | Description  |
:-------------   | :-------------   |
HANA Instance Number       | Enter the default (90).
System user password  | You specified this password when you were prompted for **HANA database master password**.
Root key backup password            | Enter a strong password. The root key backup password is required to securely back up the root keys and subsequently restore the backed-up root keys during data recovery. For information on root key backup, see [Root Key Backup](https://help.sap.com/saphelp_hanaplatform/helpdata/en/39/730482d6944173b34c660c20963051/content.htm?frameset=/en/b3/0fda1483b34628802a8d62bd5d39df/frameset.htm&current_toc=/en/de/ec02ebbb57101483bdf3194c301d2e/plain.htm&node_id=81).
Root key directory                  | A directory to store the root key backup password securely. Choose a non-temporary directory. For example `/usr/sap/HXE/HDB90`.

Enter **Y** when prompted. The script runs. Wait for the `hxehost:hxeadm>` prompt to return (approximately 30 seconds).

New data will now be encrypted with the new keys.

## Best Practices: Deactivate the SYSTEM user

SYSTEM is the database `superuser` and is not intended for day-to-day activities in production systems. For better security, you can create other database users with only the privileges that they require for their tasks (for example, user administration), then deactivate the SYSTEM user.

1. In a terminal, log in as the `hxeadm` user:

    `sudo su -l hxeadm`

2. Create a new admin user with the USER ADMIN system privilege:

    `/usr/sap/HXE/HDB90/exe/hdbsql -i 90 -d SystemDB -u SYSTEM -p "<SYSTEM-password>" "CREATE USER <admin-username> PASSWORD <admin-password> NO FORCE_FIRST_PASSWORD_CHANGE;"`
    `/usr/sap/HXE/HDB90/exe/hdbsql -i 90 -d SystemDB -u SYSTEM -p "<SYSTEM-password>" "GRANT USER ADMIN TO <admin-username> WITH ADMIN OPTION;"`

2. Use the new admin user to deactivate the SYSTEM user:

    `/usr/sap/HXE/HDB90/exe/hdbsql -i 90 -d SystemDB -u <admin-username> -p "<admin-password>" "ALTER USER SYSTEM DEACTIVATE USER NOW;"`

## Next Steps
 - Select a tutorial from the [Tutorial Navigator](http://www.sap.com/developer/tutorial-navigator.html) or the [Tutorial Catalog](http://www.sap.com/developer/tutorials.html)
