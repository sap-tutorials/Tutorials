---
title: Start Using SAP HANA 2.0, express edition (Binary Installer Method)
description: Explore how to connect to the SAP Web IDE for SAP HANA, Express Edition to start developing your project.
primary_tag: products>sap-hana\,-express-edition  
tags: [  tutorial>beginner, products>sap-hana\,-express-edition   ]
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

[ACCORDION-BEGIN [Info: ](The SAP HANA, express edition License)]

Installing SAP HANA 2.0, express edition installs a permanent 32 GB license automatically. No license configuration is required.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Test Your Server Installation)]

1. In a terminal, log in as the `<sid>adm` user:

    ```
    sudo su -l <sid>adm
    ```

2. Enter `HDB info`. The following services must be running:
    * `hdbnameserver`
    * `hdbcompileserver`
    * `hdbwebdispatcher`
    * `hdbdiserver` (if XSA is installed)

3. If any services are not running, enter `HDB start`. When the prompt returns, the system is started.

4. Check that the `XSEngine` is running. Open a browser and enter:

    ```bash
    http://<hostname>:80<instance-number>  
    ```

    A success page displays:  

    ![XSEngine Success Page](hxe_xs_success.PNG)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Test XSA (Applications Package Only))]

If you installed the Applications package (`hxexsa.tgz`), test your XSA installation.

1. As the `<sid>adm` user, log in to XSA services:  

    ```bash
    xs login -u xsa_admin -p "<password>"
    ```  

2. View the list of XSA applications. Enter:  

    ```bash
    xs apps
    ```

    >**Note**: When you run the `xs apps` command for the first time, it may take 1-2 minutes for the system to return the list of XSA applications.

3. Check that the application **`cockpit-admin-web-app`** shows **STARTED** with 1/1 instances in the list of XSA applications.

    >**Note** Normally it only takes a few minutes for XSA services to start. However. depending on your machine, it can take over 30 minutes for XSA services to begin. If the service doesn't show STARTED and doesn't show 1/1 instances, keep waiting until the service is enabled.

    Make a note of the URL for `cockpit-admin-web-app`.

    ![XSA apps](hxe_xsa_apps_cockpit.png)

4. Enter the URL for `cockpit-admin-web-app` in a browser. The address is the one that displays in your **`xs apps`**  command output.  

    Example:  `https://my.hostname:51043`

5. Log in using the `XSA_ADMIN` user.

6. If your site uses a proxy for connecting to HTTP and HTTPS servers, select _Cockpit Settings > Proxy_, then enable **Http(s) Proxy** and set the host, port, and non-proxy hosts.

    >**Tip**: To find your proxy server information, in a terminal, enter `env | grep PROXY`

    >**Note**: If you are using HANA Cockpit to register a resource, both HANA Cockpit and the SAP HANA, express edition need to be from the same release. SAP does not recommend using a newer HANA Cockpit to register an older version of SAP HANA, express edition.


[ACCORDION-END]

[ACCORDION-BEGIN [Best Practice: ](Deactivate the SYSTEM user)]

SYSTEM is the database superuser and is not intended for day-to-day activities in production systems. For better security, you can create other database users with only the privileges that they require for their tasks (for example, user administration), then deactivate the SYSTEM user.

1. In a terminal, log in as the `<sid>adm` user:

    ```bash
    sudo su -l <sid>adm
    ```

2. Create a new admin user with the USER ADMIN system privilege:

    ```bash
    /usr/sap/<SID>/HDB<instance-number>/exe/hdbsql -i <instance-number> -d SystemDB -u SYSTEM -p "<SYSTEM-password>" "CREATE USER <admin-username> PASSWORD <admin-password> NO FORCE_FIRST_PASSWORD_CHANGE;"

    /usr/sap/<SID>/HDB<instance-number>/exe/hdbsql -i <instance-number> -d SystemDB -u SYSTEM -p "<SYSTEM-password>" "GRANT USER ADMIN TO <admin-username> WITH ADMIN OPTION;"
    ```

2. Use the new admin user to deactivate the SYSTEM user:

    ```bash
    /usr/sap/<SID>/HDB<instance-number>/exe/hdbsql -i <instance-number> -d SystemDB -u <admin-username> -p "<admin-password>" "ALTER USER SYSTEM DEACTIVATE USER NOW;"
    ```


[ACCORDION-END]

[ACCORDION-BEGIN [Best Practice: ](Backups)]

Make regular data backups to save your work.

For information on data backup, recovery, and log file growth, see the [SAP HANA 2.0 Administration Guide](https://help.sap.com/viewer/6b94445c94ae495c83a19646e7c3fd56/2.0.00/en-US).

[ACCORDION-END]

[ACCORDION-BEGIN [Best Practice: ](Set the Global Memory Allocation Limit)]

Set how much memory SAP HANA, express edition utilizes by modifying the `global_allocation_limit` parameter in the `global.ini` file.

The unit for `global_allocation_limit` is MB. The default value is 0, which sets the maximum memory to the minimum of your machine limit and license limit. If the machine size is less than 16 GB, the maximum memory is set to 16 GB.

If you set `global_allocation_limit` to a non-zero value, SAP HANA, express edition will use that value as maximum memory.

>**Note**: Do not set `global_allocation_limit` to a value above the limit of your license. This can cause the database to lock down.

[ACCORDION-END]

[ACCORDION-BEGIN [Optional: ](Test Your Installation Using the HANA Eclipse Plugin)]

Download and install the HANA Eclipse Plugin on a client machine and connect to SAP HANA, express edition.

Download **Eclipse IDE for Java EE Developers** from [http://www.eclipse.org/neon/](http://www.eclipse.org/neon/) to your local file system.

Follow the eclipse installer prompts.

Launch when prompted, or go to the eclipse folder (example: `C:\Users\<path>\eclipse\jee-neon`) and run the **eclipse** executable file.

Follow the tutorial [How to download and install the HANA Eclipse plugin](http://www.sap.com/developer/how-tos/2016/09/hxe-howto-eclipse.html) to connect to your SAP HANA, express edition client machine.


[ACCORDION-END]

[ACCORDION-BEGIN [Optional: ](Install SAP Enterprise Architecture Designer (Applications Package Only))]


**Note**: Installing additional features requires greater system resources and may impact performance.

If you downloaded the Applications package (`hxexsa.tgz`), and the SAP Enterprise Architecture Designer (SAP EA Designer) package (`eadesigner.tgz`), the installation file for SAP EA Designer is located at `<extracted_path>/HANA_EXPRESS_20/install_eadesigner.sh/`.

SAP EA Designer lets you capture, analyze, and present your organization's landscapes, strategies, requirements, processes, data, and other artifacts in a shared environment. Using industry-standard notations and techniques, organizations can leverage rich metadata and use models and diagrams to drive understanding and promote shared outcomes in creating innovative systems, information sets, and processes to support goals and capabilities.

Install SAP EA Designer in your SAP HANA 2.0, express edition system using the `install_eadesigner.sh` install script.

1. Log in as `<sid>adm`.

    ```
    sudo su -l <sid>adm
    ```
2. Run the following to install SAP EA Designer:

    ```
    <extracted_path>/HANA_EXPRESS_20/install_eadesigner.sh
    ```

3. When the installation is complete enter the following command to confirm the status of SAP EA Designer:

    ```
    xs apps
    ```

    The output will include all the applications of your organization and space. You should see:   

    - `eadesigner` - The SAP EA Designer application

    - `eadesigner-service` - The SAP EA Designer Node application

    - `eadesigner-backend` - The SAP EA Designer Java application

    - `eadesigner-db` - The SAP EA Designer database creation application. This application will have a state of stopped when the installation is complete.

4. Note the URL for `eadesigner` and enter it in your web browser address bar to go to the SAP EA Designer login screen.

5. Enter the following credentials:

    - User Name - ADMIN

    >**Note**: Account names managed by SAP EA Designer are case-sensitive.

    - Password - Enter the password you created when you installed SAP EA Designer.

    You are prompted to change the password. You are logged in as administrator of SAP EA Designer.


[ACCORDION-END]

[ACCORDION-BEGIN [Optional: ](Install SAP HANA Interactive Education (SHINE))]

SAP HANA Interactive Education (SHINE) makes it easy to learn how to build applications on SAP HANA Extended Application Services Advanced Model.

SHINE is provided as an optional component for SAP HANA, express edition. Download the SHINE installation file, `shine.tgz`, from the SAP HANA, express edition Download Manager. This download includes installation files for installing SHINE on XSC and XSA.

#### Install SHINE for XSC

Installation files for SHINE for **XSC** are located at:

```
<extracted_path>/HANA_EXPRESS_20/DATA_UNITS/HCO_HANA_SHINE
```

To install SHINE for XSC, see the [SAP HANA Interactive Education (SHINE) guide](https://help.sap.com/hana/SAP_HANA_Interactive_Education_SHINE_en.pdf).

>**Note:** The HANA `JDBC` port number for SAP HANA, express edition is different than the default port number `30015` mentioned in the SHINE guide. You need to update the port parameter for the resources `CrossSchemaSys` and `CrossSchemaSysBi` in the `mtaext` file to `3<instance-number>13`.  

#### Install SHINE for XSA

Installation files for SHINE for **XSA** are located at:
```
<extracted_path>/HANA_EXPRESS_20/DATA_UNITS/XSA_CONTENT_10
```

To install SHINE for XSA, run the following as `<sid>adm`:

```
<extracted_path>/HANA_EXPRESS_20/install_shine.sh
```


[ACCORDION-END]

[ACCORDION-BEGIN [Optional: ](Installing SAP HANA External Machine Learning Library)]

The SAP HANA External Machine Learning Library is an application function library (AFL) supporting the integration of Google `TensorFlow`, as an external machine learning framework, with SAP HANA, express edition.

1. Use the Download Manager to download HANA External Machine Learning AFL, `eml.tgz`.

2. Extract `eml.tgz`.

3. As `<sid>adm`, run:

    ```bash
    <extracted_path>/HANA_EXPRESS_20/install_eml.sh
    ```

    For more information on the SAP HANA External Machine Learning Library, see the SAP HANA documentation collection.    



[ACCORDION-END]

[ACCORDION-BEGIN [Optional: ](Install Text Analysis Files)]

If you are using SAP HANA 2.0, express edition in a language other than English or German, you can download the **Text analysis files for additional languages** package in the Download Manager. This package contains the text analysis files for the HANA Text Analysis feature for languages other than English or German.

**Prerequisite**: You downloaded the package **Text analysis files for additional languages** using Download Manager.

1. Log in as `<sid>adm`.

2. Navigate to `/hana/shared/<SID>/global/hdb/custom/config/lexicon`.

3. Extract the contents of `additional_lang.tgz` to this directory:

    ```
    tar -xvzf <download_path>/additional_lang.tgz
    ```


[ACCORDION-END]

[ACCORDION-BEGIN [Optional: ](Installing SAP Web IDE for SAP HANA)]

SAP Web IDE for SAP HANA is a browser-based integrated development environment for the development of applications comprised of extensive SAP HANA data models, business logic, and web-based `UIs`.

Before installing the Web IDE, you must have installed the Applications package.

1. Use the Download Manager to download the Web IDE installation package, `webide.tgz`.

2. Extract the contents of `webide.tgz`:

    ```bash
    tar -xvzf <download_path>/webide.tgz
    ```

3. Navigate to the `HANA_EXPRESS_20` directory where you extracted the files and run `install_webide.sh` as the `<sid>adm` user:

    ```bash
    <extracted_path>/HANA_EXPRESS_20/install_webide.sh
    ```

>**Note:** The console may display some jargon during `npm` package manager installation. This is a known issue and will be fixed in an upcoming release.   

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Test Web IDE (Applications Package Only))]

1. As the `<sid>adm` user, log in to XSA services:  

    ```bash
    xs login -u xsa_admin -p "<password>"
    ```  

2. View the status of the `webide` application. Enter:  

    ```bash
    xs apps | grep webide
    ```

3. Check that the application **`webide`** shows **STARTED** with 1/1 instances in the list of XSA applications.

    Make a note of the URL for `webide`.

4. Test your Web IDE connection. Enter the URL for `webide` in a browser. The address is the one that displays in your  **`xs apps`**  command output.  

    Example:  `https://my.hostname:53075`

5. Log on to Web IDE using the `XSA_DEV` user.

[ACCORDION-END]

## Next Steps
 - Select a tutorial from the [Tutorial Navigator](http://www.sap.com/developer/tutorial-navigator.html) or the [Tutorial Catalog](http://www.sap.com/developer/tutorials.html)
