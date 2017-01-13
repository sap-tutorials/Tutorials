---
title: Start Using SAP HANA 2.0, express edition (Binary Installer Method)
description: Explore how to connect to the SAP Web IDE for SAP HANA, Express Edition to start developing your project.
tags: [  tutorial>beginner, products>sap-hana\,-express-edition ]
---
## Prerequisites
- **Proficiency:** Beginner
- **Tutorials:** [Installing Binary](http://www.sap.com/developer/tutorials/hxe-ua-installing-binary.html).

## Next Steps
 - Select a tutorial from the [Tutorial Navigator](http://www.sap.com/developer/tutorial-navigator.html) or the [Tutorial Catalog](http://www.sap.com/developer/tutorials.html)

## Details
### You will learn
How to configure the binary install of SAP HANA, express edition.

For troubleshooting information, see [SAP HANA, express edition Troubleshooting](http://www.sap.com/developer/how-tos/2016/09/hxe-ua-troubleshooting.html).
### Time to Complete
**20 Min.**
---
### The SAP HANA, express-edition License

Installing SAP HANA 2.0, express edition installs a permanent 32 GB license automatically. No license configuration is required.

### Test Your Server installation

1. In a terminal, log in as the `<sid>adm` user:

    `sudo su -l <sid>adm`

2. Enter `HDB info`. The following services must be running:
    * `hdbnameserver`
    * `hdbcompileserver`
    * `hdbpreprocessor`
    * `hdbwebdispatcher`
    * `hdbdiserver` (if XSA is installed)

3. If any services are not running, enter `HDB start`. When the prompt returns, the system is started.

### Test XSC and XSA (Applications Package Only)

If you installed the Applications package (`hxexsa.tgz`), test your XS installations.

1. Check that the `XSEngine` is running. Open a browser and enter:   
    ```
    http://<hostname>:80<instance-number>  
    ```

    A success page displays:  

    ![XSEngine Success Page](hxe_xs_success.PNG)

2. As the `<sid>adm` user, log in to XSA services:  
    ```
    xs login -u xsa_admin -p <password>
    ```  

3. View the list of XSA applications. Enter:  
    ```
    xs apps
    ```
    >**Note**: When you run the `xs apps` command for the first time, it may take 1-2 minutes for the system to return the list of XSA applications.

4. Check that the application **`cockpit-admin-web-app`** shows **STARTED** with 1/1 instances in the list of XSA applications.

    Make a note of the URL for `cockpit-admin-web-app`.

    ![XSA apps](hxe_xsa_apps_cockpit.png)

5. Enter the URL for `cockpit-admin-web-app` in a browser. The address is the one that displays in your  **`xs apps`**  command output.  

    Example:  `https://my.hostname:51043`

6. Log in using the `XSA_ADMIN` user.

7. If your site uses a proxy for connecting to HTTP and HTTPS servers, select _Cockpit Settings > Proxy_, then enable **Http(s) Proxy** and set the host, port, and non-proxy hosts.

    >**Tip**: To find your proxy server information, in a terminal, enter `env | grep PROXY`

### Test Web IDE (Applications Package Only)

1. As the `<sid>adm` user, log in to XSA services:  
    ```
    xs login -u xsa_admin -p <password>
    ```  

2. View the status of the `webide` application. Enter:  
    ```
    xs apps | grep webide
    ```

3. Check that the application **`webide`** shows **STARTED** with 1/1 instances in the list of XSA applications.

    Make a note of the URL for `webide`.

4. Test your Web IDE connection. Enter the URL for `webide` in a browser. The address is the one that displays in your  **`xs apps`**  command output.  

    Example:  `https://my.hostname:53075`

5. Log on to Web IDE using the `XSA_DEV` user.

### (Optional) Test your Installation using the HANA Eclipse Plugin

Download and install the HANA Eclipse Plugin on a client machine and connect to SAP HANA, express edition.

1. Download **Eclipse IDE for Java EE Developers** from [http://www.eclipse.org/neon/](http://www.eclipse.org/neon/) to your local file system.

2. Follow the eclipse installer prompts.

3. Launch when prompted, or go to the eclipse folder (example: `C:\Users\<path>\eclipse\jee-neon`) and run the **eclipse** executable file.

4. Follow the tutorial [How to download and install the HANA Eclipse plugin](http://www.sap.com/developer/how-tos/2016/09/hxe-howto-eclipse.html).

### Best Practices: Deactivate the SYSTEM user

SYSTEM is the database superuser and is not intended for day-to-day activities in production systems. For better security, you can create other database users with only the privileges that they require for their tasks (for example, user administration), then deactivate the SYSTEM user.

1. In a terminal, log in as the `<sid>adm` user:

    `sudo su -l <sid>adm`

2. Create a new admin user with the USER ADMIN system privilege:

    `/usr/sap/<SID>/HDB<instance-number>/exe/hdbsql -i <instance-number> -d SystemDB -u SYSTEM -p <SYSTEM-password> "CREATE USER <admin-username> PASSWORD <admin-password> NO FORCE_FIRST_PASSWORD_CHANGE;"`
    `/usr/sap/<SID>/HDB<instance-number>/exe/hdbsql -i <instance-number> -d SystemDB -u SYSTEM -p <SYSTEM-password> "GRANT USER ADMIN TO <admin-username> WITH ADMIN OPTION;"`

2. Use the new admin user to deactivate the SYSTEM user:

    `/usr/sap/<SID>/HDB<instance-number>/exe/hdbsql -i <instance-number> -d SystemDB -u <admin-username> -p <admin-password> "ALTER USER SYSTEM DEACTIVATE USER NOW;"`

### Best Practices: Backups

Make regular data backups to save your work.

For information on data backup, recovery, and log file growth, see the [SAP HANA 2.0 Administration Guide](http://help.sap.com/saphelp_hanaplatform/helpdata/en/00/0ca1e3486640ef8b884cdf1a050fbb/frameset.htm).

## (Optional) Install SAP Enterprise Architecture Designer (Applications Package Only)

>**Note**: Installing additional features requires greater system resources and may impact performance.

If you downloaded the Applications package (`hxexsa.tgz`), the installation file for SAP Enterprise Architecture Designer (SAP EA Designer) is located at `<extracted_path>/HANA_EXPRESS_20/DATA_UNITS/XSA_CONTENT_10/XSACHANAEAD00_0.ZIP`.

SAP EA Designer lets you capture, analyze, and present your organization's landscapes, strategies, requirements, processes, data, and other artifacts in a shared environment. Using industry-standard notations and techniques, organizations can leverage rich metadata and use models and diagrams to drive understanding and promote shared outcomes in creating innovative systems, information sets, and processes to support goals and capabilities.

Install SAP EA Designer in your SAP HANA 2.0, express edition system using the `xs` command line tool.

1. Log in as `<sid>adm`.

    ```
    sudo su -l <sid>adm
    ```

2. Create a text file, copy the following content to it, replacing the variable <tempPwd> with your choice of a temporary administrator password for the first login, and save it as `firstTime.mtaext`:

    ```
    _schema-version: "2.0.0"
    ID: com.sap.hana.eadesigner.ext
    extends: com.sap.hana.eadesigner

    modules:
      - name: eadesigner-backend
      properties:
        ADMIN_PASSWORD: <tempPwd>
    ```    
    >**Note**: Make sure that `properties` is indented two spaces and `ADMIN_PASSWORD` is indented four spaces. Do not use tab.

    >**Note**: If you do not specify this temporary password file in your installation command, the installation will proceed normally, but you will not be able to log into SAP EA Designer. We recommend that your temporary password should contain 8 or more characters including a mix of numbers and uppercase and lowercase letters. Once installation is complete, you should delete this file.

3. Login to the XSA environment with the following command and enter your credentials when prompted:

    ```
    xs login -a https://<HOST>:3<instance-number>30
    ```

4. Install the SAP EA Designer package using the following command, where `firstTime.mtaext` is the file containing the temporary administrator password:

    ```
    xs install XSACHANAEAD00_0.zip –e firstTime.mtaext
    ```

5. When the installation is complete enter the following command to confirm the status of SAP EA Designer:

    ```
    xs apps
    ```

    The output will include all the applications of your organization and space. You should see:   

    - `eadesigner` - The SAP EA Designer application

    - `eadesigner-service` - The SAP EA Designer Node application

    - `eadesigner-backend` - The SAP EA Designer Java application

    - `eadesigner-db` - The SAP EA Designer database creation application. This application will have a state of stopped when the installation is complete.

6. Note the URL for `eadesigner` and enter it in your web browser address bar to go to the SAP EA Designer login screen.

7. Enter the following credentials:

    - User Name - ADMIN

    >**Note**: Account names managed by SAP EA Designer are case-sensitive.

    - Password - Enter the temporary administrator password (<`tempPwd`>) you specified in `firstTime.mtaext`.

    You will be prompted to change the password and then logged in as administrator of SAP EA Designer.     

## (Optional) Install SAP HANA Interactive Education (SHINE)

To install SHINE for XSC, see the [SAP HANA Interactive Education (SHINE) guide](http://help.sap.com/hana/SAP_HANA_Interactive_Education_SHINE_en.pdf).

Installation files for SHINE for **XSC** are located at:
```
<extracted_path>/HANA_EXPRESS_20/DATA_UNITS/HCO_HANA_SHINE
```

To install SHINE for XSA, see the [SAP HANA Interactive Education (SHINE) for SAP HANA XS Advanced Model guide](http://help.sap.com/hana/SAP_HANA_Interactive_Education_SHINE_for_SAP_HANA_XS_Advanced_Model_en.pdf).

If you downloaded the Applications (`hxexsa.tgz`) package, installation files for SHINE for **XSA** are located at:
```
<extracted_path>/HANA_EXPRESS_20/DATA_UNITS/XSA_CONTENT_10
```

## (Optional) Install Text Analysis Files

If you are using SAP HANA 2.0, express edition in a language other than English or German, you can download the **Text analysis files for additional languages** package in the Download Manager. This package contains the text analysis files for the HANA Text Analysis feature for languages other than English or German.

**Prerequisite**: You downloaded the package **Text analysis files for additional languages** using Download Manager.

1. Log in as `<sid>adm`.

2. Navigate to `/hana/shared/<SID>/exe/linuxx86_64/hdb/lexicon`.

3. Extract the contents of `additional_lang.tgz` to this directory:
    ```
    tar -xvzf <download_path>/additional_lang.tgz
    ```

## Next Steps
 - Select a tutorial from the [Tutorial Navigator](http://www.sap.com/developer/tutorial-navigator.html) or the [Tutorial Catalog](http://www.sap.com/developer/tutorials.html)
