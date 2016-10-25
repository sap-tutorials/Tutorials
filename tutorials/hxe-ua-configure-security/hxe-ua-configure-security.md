---
title: Configure SAP HANA, express edition Security
description: Your SAP HANA, express edition installation has several preconfigured security settings. Before using SAP HANA, express edition, complete these security tasks.
tags: [  tutorial>beginner, products>sap-hana\,-express-edition ]
---
## Prerequisites  
 - **Proficiency:** Beginner
 - **Tutorials:** [Start Using SAP HANA, express edition](http://go.sap.com/developer/tutorials/hxe-ua-getting-started-vm.html)

## Next Steps
 - Select a tutorial from the [Tutorial Navigator](http://go.sap.com/developer/tutorial-navigator.html) or the [Tutorial Catalog](http://go.sap.com/developer/tutorials.html)

## Details
### You will learn  
How to perform security tasks to ensure that your security settings are not known outside your organization. You will learn how to apply the HXE license to ensure your installation keeps working after the default grace period expires.

### Time to Complete
**15-20 Min**.

---

### Apply the HANA, express-edition license key
If you do not apply the license, your SAP HANA, express edition will stop working after the default grace period of 60-90 days. If you have SAP HANA Studio or the HANA Studio eclipse plugin then begin at **Applying the License Key with HANA Studio**. Otherwise proceed to **Applying the License Key with HDBSQL**.

### Applying the License Key with HANA Studio

#### Obtain your hardware key
If you are using the SAP HANA Studio eclipse plugin, you can do the following.

1. Start SAP HANA studio.

2.	On the Systems tab, select SYSTEMDB@HXE (SYSTEM).

3.	View properties for SYSTEMDB@HXE (SYSTEM).

4.	Select License properties. Open the System License tab.

5.	Make a note of the Hardware Key value.

#### Order your license key

1. Go to [SAP Sneak Preview License Key Request](http://sap.com/minisap) page and fill out all required information.

2.	For System ID, select HXE.

3.	For Hardware Key, enter the hardware key value you recorded earlier.

4.	Submit the form. The license key is emailed to you.

5.  Save the license key file to your hard disk under the name `HXE.txt`.

#### Apply the license key

1.	In SAP HANA studio, view properties for SYSTEMDB@HXE (SYSTEM).

2.	Select License properties. Open the System License tab.

3.	Click Delete License Key to delete any existing licenses.

4.	Click Install License Key.

5.	Navigate to your license file and select it.  

    After confirmation, the properties page refreshes with your new license information.

6. Proceed to **Change the SSFS Master Keys**

### Applying the License Key with HDBSQL

#### Obtain your hardware key

1. Login in to your HANA, express edition as `hxeadm`.

2.	Enter the following command:

    ```
    hdbsql -u system -p <your password> -d SystemDB "SELECT HARDWARE_KEY FROM M_LICENSE"
    ```

3.	Copy or otherwise record the value returned for `HARDWARE_KEY`.

#### Order your license key

1. Go to [SAP Sneak Preview License Key Request](http://sap.com/minisap) page and fill out all required information.

2.	For System ID, select HXE.

3.	For Hardware Key, enter the hardware key value you recorded earlier.

4.	Submit the form. The license key is emailed to you.

5.  Save the license key file to your hard disk under the name `HXE.txt`.

#### Apply the license key

1.	Make a directory on your HXE machine to store the license.

    ```
    mkdir ~/license
    ```

        This command will make the directory `/usr/sap/HXE/home/license`

2. If you do not have an SCP client, please download and install one. There are several very good open source `scp` clients available for Windows, Mac and Linux. Copy the file from your hard disk to the `/usr/sap/HXE/home/license` directory.

3. Issue the following command to install the license key.

    ```
    hdbsql -u system -p <password> -n localhost:30013 -m -i <instance number> "SET SYSTEM LICENSE 'cat /usr/sap/HXE/home/license/HXE.txt'`;"
    ```

    **Note**: Make sure the license file string is surrounded by single quotation marks. After the license file string closing single quotation mark, make sure you include the back-tick and semicolon. For `<password>` and `<instance number>` etc., input values matching your SAP HANA, express edition settings.

4.	Confirm that the license key was installed by issuing the following command.

    `hdbsql -u system -p <password> -d SystemDB "select hardware_key, expiration_date from m_licenses"`

 The expiration date should be one year from today.

5. Proceed to **Change the SSFS Master Keys**.


## Change the SSFS Master Keys
The secure stores in the file system (SSFS) used by SAP HANA are protected by unique master keys, generated during installation or update. If you installed HXE from an OVA, then it shares master keys with other HXE systems. We recommend that you change the master keys immediately after setup to ensure that your master keys are not known outside your organization. For more information on changing the master keys, see the [Change the SSFS Master Keys](https://help.sap.com/saphelp_hanaplatform/helpdata/en/58/1593c48739431caaccc3d2ef55c23f/frameset.htm) topic in the *SAP HANA Administration Guide*.

1. Log on to the HANA system as `hxeadm` and shut the system down using the `sapcontrol` program:

    ```
    /usr/sap/hostctrl/exe/sapcontrol -nr 00 -function Stop
    ```

2. Re-encrypt the master key of the instance SSFS:  

    ```
    export RSEC_SSFS_DATAPATH=/usr/sap/HXE/SYS/global/hdb/security/ssfs
    ```

    ```
    export RSEC_SSFS_KEYPATH=/usr/sap/HXE/SYS/global/hdb/security/ssfs
    ```

    ```
    rsecssfx changekey $(rsecssfx generatekey -getPlainValueToConsole)
    ```

3. Add the following entry to the `global.ini` file using a text editor. (HANA, express edition, comes with the `vi` and `vim` text editors.) The `global.ini` file is located here:    
    ```
    /usr/sap/HXE/SYS/global/hdb/custom/config/global.ini
    ```

    Add or edit the cryptography section with the following value.

    `[cryptography]`

    `ssfs_key_file_path = /usr/sap/HXE/SYS/global/hdb/security/ssfs`

4. Re-encrypt the system PKI SSFS with a new key - HDB start:  

    ```
    export RSEC_SSFS_DATAPATH=/usr/sap/HXE/SYS/global/hdb/security/ssfs
    ```

    ```
    export RSEC_SSFS_KEYPATH=/usr/sap/HXE/SYS/global/hdb/security/ssfs
    ```

    ```rsecssfx changekey $(rsecssfx generatekey -getPlainValueToConsole)
    ```

5.	Restart the system:  

    ```
    /usr/sap/hostctrl/exe/sapcontrol -nr 00 -function Start
    ```


## Change the Root Key
SAP HANA generates unique root keys on installation. If you installed HXE from an OVA, then it shares a root key with other HXE systems. We recommend that you change the root key of the internal data encryption service to ensure it is not known outside your organization. For more information on this topic, see the [Change the Root Key of the Internal Data Encryption Service](https://help.sap.com/saphelp_hanaplatform/helpdata/en/8f/bb69c47c224b3292ba078684f176e3/frameset.htm) topic in the *SAP HANA Server Installation and Update Guide*.

1. Log on to the HANA system as **`hxeadm`** and shut the system down using the `sapcontrol` program:  

    ```
    /usr/sap/hostctrl/exe/sapcontrol -nr 00 -function Stop
    ```

2. Generate a new root encryption key using the `hdbnsutil` program:  

    ```
    cd /usr/sap/HXE/HDB00/exe
    ```

    ```
    ./hdbnsutil -generateRootKeys --type=DPAPI
    ```

3. Restart the system:  

    ```
    /usr/sap/hostctrl/exe/sapcontrol -nr 00 -function Start
    ```

4. Reset the consistency information in the SSFS using the `hdbcons` program:  

    ```
    cd /usr/sap/HXE/HDB00/exe
    ```

    ```
    ./hdbcons "crypto ssfs resetConsistency" -e hdbnameserver
    ```

5. After running the `hdbcons` command you have 20 seconds to rerun the command again to completely rewrite `ssfs` consistency information:  

    ```
    ./hdbcons "crypto ssfs resetConsistency" -e hdbnameserver
    ```

6. Change all application keys so that they are encrypted with the new root key by using SAP HANA studio or SAP HANA HDBSQL:  

    ```
    hdbsql -u system -p <YourPassword> -d SystemDB "ALTER SYSTEM APPLICATION ENCRYPTION CREATE NEW KEY"
    ```


## Next Steps
 - Select a tutorial from the [Tutorial Navigator](http://go.sap.com/developer/tutorial-navigator.html) or the [Tutorial Catalog](http://go.sap.com/developer/tutorials.html)
