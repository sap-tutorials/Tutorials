---
title: Getting Started with SAP HANA, express edition (Binary Installer Method)
description: Explore how to connect to the SAP Web IDE for SAP HANA, Express Edition to start developing your project.
tags: [  tutorial>beginner, products>sap-hana, products>sap-hana\,-express-edition ]
---
## Prerequisites
- Install SAP HANA, express edition. See [Installing Binary](http://go.sap.com/developer/tutorials/hxe-ua-installing-binary.html).

## Next Steps
 - Select a tutorial from the [Tutorial Navigator](http://go.sap.com/developer/tutorial-navigator.html) or the [Tutorial Catalog](http://go.sap.com/developer/tutorials.html)

## Details
### You will learn
How to configure the binary install of SAP HANA, express edition.

For troubleshooting information, see [SAP HANA, express edition Troubleshooting](http://go.sap.com/developer/how-tos/hxe-ua-troubleshooting.html).
### Time to Complete
**20 Min.**
---
### Record Your System's IP address

1. In a terminal, enter: `/sbin/ifconfig`.
2. The IP address is listed under the `eth0 interface` as `inet addr`. In the following example, the IP address is 10.172.90.53:

  `etho Link encap:Ethernet HWaddr 00:0C:29:06:1F:93 inet addr:10.172.90.53`

### Test Your Server installation

1. In a terminal, enter: `HDB info`. The following services must be running:
  * `hdbnameserver`
  * `hdbcompileserver`
  * `hdbpreprocessor`
  * `hdbwebdispatcher`
  * `hdbdiserver` (if XSA is installed)
  * `XS services` (if XSA is installed)
2. If any services are not running, enter `HDB start`. When the prompt returns, the system is started.

### Test XSC, XSA, and Web IDE

1. Check if XSC is working by going to the following URL:
  `http://<ip-address-of-VM>:8000`
2. Check if XSA is working:
  a. In a terminal, use the following command to log into XSA services:
  `xs login -u XSA_ADMIN -p <password>`
  b. View a list of XSA applications using: `xs apps`
  c. Confirm that `webide` shows in the list of XSA applications. Note the URL for it.
3. Open the URL for Web IDE in a browser to confirm that it loads.

### Install Eclipse and SAP HANA Tools

#### Prerequisites
* Confirm that your machine meets the system requirements for SAP HANA Tools, listed at [https://tools.hana.ondemand.com/#hanatools](https://tools.hana.ondemand.com/#hanatools)


1. Download and install Eclipse Neon from [https://eclipse.org/downloads/](https://eclipse.org/downloads/), selecting one of the following packages:
  * Eclipse IDE for Java EE Developers
  * Eclipse IDE for Java Developers
2. Open Eclipse and select a workspace.
3. Install SAP HANA Tools:
  a. Select _Help > Install New Software_
  b. In the **Work with** field, enter https://tools.hana.ondemand.com/neon and click Add.
  c. In the **Name** field, enter SAP HANA, express edition.
  d. Expand the **SAP HANA Tools** node.
  e. Select **SAP HANA Administrator (Developer Edition)**.
  f. Accept the installation wizard prompts and restart Eclipse.
  g. Select _Window > Perspective > Open Perspective > Other_.
  h. Select the **SAP HANA Administration Console** perspective.
4. Add the SAP HANA, express edition system:
  a. On the **Systems** view, right-click and select **Add System**, or click the **Add System** button.
  b. Enter the following information and click **Next**:
    * Host Name: the IP address for your system
    * Instance Number: 00
    * Mode: Multiple containers, System database
  c. Select Authentication by database user and enter the credentials for your SYSTEM user. Click **Finish** to connect.

### Obtain and Apply a License Key

1. Obtain your hardware key:
  a. In Eclipse, connect to your system.
  b. In the **Systems** view, right-click on your system and select **Properties**
  c. Select **License**, then the **System License** tab.
  d. Make a note of your hardware key.
2. Order your license key:
  a. In a browser, go to [http://www.sap.com/minisap](http://www.sap.com/minisap) and complete the form.
  b. For **System ID**, select **HXE - SAP HANA, express edition (32 GB)**.
  c. For **Hardware Key**, enter the hardware key for your system.
  d. Submit the form. The license key is emailed to you.
3. Apply the license key:
  a. In Eclipse, click **Delete License Key** to remove existing licenses.
  b. Click **Install License Key** and select the license key file.

### Deactivate the SYSTEM user

1. In a terminal, create a new admin user with the USER ADMIN system privilege:
`/usr/sap/HXE/HDB00/exe/hdbsql -d SystemDB -u SYSTEM -p <SYSTEM-password> "CREATE USER <admin-username> PASSWORD <admin-password> NO FORCE_FIRST_PASSWORD_CHANGE;"
/usr/sap/HXE/HDB00/exe/hdbsql -d SystemDB -u SYSTEM -p <SYSTEM-password> "GRANT USER ADMIN TO <admin-username> WITH ADMIN OPTION;"`
2. Use the new admin user to deactivate the SYSTEM user:
`/usr/sap/HXE/HDB00/exe/hdbsql -d SystemDB -u <admin-username> -p <admin-password> "ALTER USER SYSTEM DEACTIVATE USER NOW;"`

## Next Steps
 - Select a tutorial from the [Tutorial Navigator](http://go.sap.com/developer/tutorial-navigator.html) or the [Tutorial Catalog](http://go.sap.com/developer/tutorials.html)
