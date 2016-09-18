---
title: Configure SAP HANA, express edition Security
description: Your SAP HANA, express edition installation has several preconfigured security settings. Before using SAP HANA, express edition, complete these security tasks.
tags: [  tutorial>beginner, topic>HXE, products>sap-hana,-express-edition ]
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
**15 Min**.

---

### Apply the HANA, express-edition license key
If you don’t apply the license, your SAP HANA, express edition will stop working after the default grace period of 60-90 days. If you have SAP HANA Studio or the HANA Studio eclipse plugin then begin at **Using Studio**. Otherwise proceed to **Using HDBSQL**.

### Applying the License Key with HANA Studio

#### Obtain your hardware key
If you are using the SAP HANA Studio eclipse plugin, you can do the following.
1. Start SAP HANA studio.
2.	On the Systems tab, select SYSTEMDB@HXE (SYSTEM).
3.	View properties for SYSTEMDB@HXE (SYSTEM).
4.	Select License properties. Open the System License tab.
5.	Make a note of the Hardware Key value.

#### Order your license key
1. Go to [sap.com/minisap](http://sap.com/minisap) and fill out all required information.
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

    `hdbsql -u system -p <your password> -d SystemDB "SELECT HARDWARE_KEY FROM M_LICENSE"`

3.	Copy or otherwise record the value returned for `HARDWARE_KEY`

#### Order your license key
1. Go to [sap.com/minisap](http://sap.com/minisap) and fill out all required information.
2.	For System ID, select HXE.
3.	For Hardware Key, enter the hardware key value you recorded earlier.
4.	Submit the form. The license key is emailed to you.
5.  Save the license key file to your hard disk under the name `HXE.txt`.

#### Apply the license key

1.	Make a directory on your HXE machine to store the license.

    `mkdir ~/license`

    This command will make the directory `/usr/sap/HXE/home/license`
2. If you do not have an SCP client, please download and install one. There are several very good open source `scp` clients available for Windows, Mac and Linux. Copy the file from your hard disk to the `/usr/sap/HXE/home/license` directory.
3. Issue the following command to install the license key.

    `hdbsql -u system -p HANAhxe2 -d SystemDB -I '/usr/sap/HXE/home/license/HXE_License.txt'`

4.	Confirm that the license key was installed by issuing the following command.

    `hdbsql -u system -p HANAhxe2 -d SystemDB "select hardware_key, expiration_date from m_licenses"`

 The expiration date should be one year from today.
5. Proceed to **Change the SSFS Master Keys**.


### Change the SSFS Master Keys
1. Log on to the HANA system as **`hxeadm`** and shut the system down using the `sapcontrol` program:  
**/`usr/sap/hostctrl/exe/sapcontrol` -nr 00 –function Stop**
2. Re-encrypt the master key of the instance SSFS:  
**`RSEC_SSFS_DATAPATH`=/`usr`/sap/`HXE`/`SYS`/`global`/`hdb`/security/`ssfs`
export `RSEC_SSFS_DATAPATH`
`RSEC_SSFS_KEYPATH=/usr/sap/HXE/SYS/global/hdb/security/ssfs rsecssfx changekey` $(`rsecssfx generatekey -getPlainValueToConsole`)**
3. Add following to the end of /`usr`/sap/`HXE`/`SYS`/`global`/`hdb`/custom/`config`/`global.ini`:  
**[cryptography]
`ssfs_key_file_path = /usr/sap/HXE/SYS/global/hdb/security/ssfs`**
4. Re-encrypt the system PKI SSFS with a new key - HDB start:  
**`RSEC_SSFS_DATAPATH=/usr/sap/HXE/SYS/global/security/rsecssfs/data`  
`export RSEC_SSFS_DATAPATH  RSEC_SSFS_KEYPATH=/usr/sap/HXE/SYS/global/security/rsecssfs/key rsecssfx changekey  $(rsecssfx generatekey -getPlainValueToConsole)`**
5.	Restart the system:  
**/`usr/sap/hostctrl/exe/sapcontrol` -nr 00 -function Start**

For more information, see the [Change the SSFS Master Keys](https://help.sap.com/saphelp_hanaplatform/helpdata/en/58/1593c48739431caaccc3d2ef55c23f/frameset.htm) topic in the *SAP HANA Administration Guide*.

### Change the Root Key
Change the root key of your installation.
1. Log on to the HANA system as **`hxeadm`** and shut the system down using the `sapcontrol` program:  
**/`usr/sap/hostctrl/exe/sapcontrol` –nr 00 –function Stop**
2. Generate a new root encryption key using the **`hdbnsutil`** program:  
**cd /`usr/sap/HXE/HDB00/exe`  
./`hdbnsutil` -`generateRootKeys` --type=`DPAPI`**
3. Restart the system:  
**/`usr/sap/hostctrl/exe/sapcontrol` -nr 00 -function Start**
4. Reset the consistency information in the SSFS using the `hdbcons` program:  
**cd /`usr/sap/HXE/HDB00/exe`
./`hdbcons "crypto ssfs resetConsistency`" -e `hdbnameserver`**
5. You have 20 seconds to rerun the command again to completely rewrite `ssfs` consistency information:  
**./`hdbcons` -e `hdbnameserver` "crypto `ssfs resetConsistency`"**
6. Change all application keys so that they are encrypted with the new root key by using SAP HANA studio or SAP HANA HDBSQL:  
**ALTER SYSTEM APPLICATION ENCRYPTION CREATE NEW KEY**

For more information, see the [Change the Root Key of the Internal Data Encryption Service](https://help.sap.com/saphelp_hanaplatform/helpdata/en/8f/bb69c47c224b3292ba078684f176e3/frameset.htm) topic in the *SAP HANA Server Installation and Update Guide*.

### Deactivate the SYSTEM user
1. Log in as SYSTEM user to create a user with the USER ADMIN system privilege:  
**/`usr`/sap/HXE/HDB00/`exe/hdbsql -d SystemDB` -u SYSTEM -p <SYSTEM_PWD> "CREATE USER `MyAdminUser` PASSWORD <`MyAdminUserPwd`> NO `FORCE_FIRST_PASSWORD_CHANGE`;"  
/`usr`/sap/HXE/HDB00/`exe/hdbsql` -d `SystemDB` -u SYSTEM -p <SYSTEM_PWD> "GRANT USER ADMIN to `MyAdminUser` WITH ADMIN OPTION ;" **
2.	Log in as **`MyAdminUser`** to deactivate SYSTEM:  
**/`usr`/sap/HXE/HDB00/`exe`/`hdbsql` -d `SystemDB` -u `MyAdminUser` -p <`MyAdminUserPwd`> "ALTER USER SYSTEM DEACTIVATE USER NOW;"**

## Next Steps
 - Select a tutorial from the [Tutorial Navigator](http://go.sap.com/developer/tutorial-navigator.html) or the [Tutorial Catalog](http://go.sap.com/developer/tutorials.html)
