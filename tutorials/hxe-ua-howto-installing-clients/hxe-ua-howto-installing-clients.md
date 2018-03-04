---
title: How to Install SAP HANA 2.0, express edition Clients
description: Download and install the client tools locally.
primary_tag: products>sap-hana\,-express-edition
tags: [  tutorial>beginner, tutorial>how-to, products>sap-hana\,-express-edition  ]
---
## Prerequisites
 - You (or someone in your organization) has already installed SAP HANA 2.0, express edition on another machine using either the [Installing Binary](http://www.sap.com/developer/tutorials/hxe-ua-installing-binary.html) or [Installing the VM Image](http://www.sap.com/developer/tutorials/hxe-ua-installing-vm-image.html) tutorial.

## Next Steps
 - [View similar How-Tos](http://www.sap.com/developer/tutorials.html) or [View all How-Tos](http://www.sap.com/developer/tutorials.html)

## How-To Details

This How-To describes how to install two different clients: database (HDB) clients and XS clients.

Install the clients if you intend to develop XS applications on a machine that will not have a local SAP HANA 2.0, express edition installation. The clients let you access SAP HANA 2.0, express edition from your client machine.

You can install the clients on the SAP HANA 2.0, express edition server during server installation, but this how-to assumes you are installing the clients on a different machine than the PC where SAP HANA 2.0, express edition is installed.

**You do not have to install clients on the same machine or VM as your SAP HANA 2.0, express edition installation.**

This how-to refers to the laptop with SAP HANA 2.0, express edition as the **server machine**, and your local machine as the **client machine**.

### Time to Complete
**30 Min**.

---

[ACCORDION-BEGIN [Pre-Installation Info: ](SAP HANA HDB Client)]

SAP HANA 2.0, express edition provides the **Reduced SAP Client** package for connecting applications. You can install the client on the server machine during server installation, or on a separate machine as described in this how-to.

#### Available Clients

When you install the SAP HANA HDB Client software package, you install the following clients:

  - SQLDBC
  - ODBC
  - JDBC
  - Python (`PyDBAPI`)
  - Node.js
  - Ruby

[ACCORDION-END]

[ACCORDION-BEGIN [Pre-Installation Info: ](XS CLI Client)]

The XS advanced client-tools bundle **`xs.onpremise.runtime.client_<platform>-<version>.zip`** also includes the archive **``(xs_javascript-1.3.0-bundle.tar.gz)``**, which includes a selection of mandatory Node.js packages developed by SAP for use with the Node.js applications running in the XS advanced run time.

You can use the XS command line client to perform a wide-variety of developer- and administrator-related tasks. For example, in the role of a developer, you can use the XS CLI to connect to the XS advanced run time installed on the server machine, log on as a specific user, and deploy and manage your applications.

[ACCORDION-END]

[ACCORDION-BEGIN [Download: ](Client Packages)]

Install the Download Manager to your client machine and download the client package.

> **Note:** The Download Manager requires Oracle Java SE Runtime Environment 8 (JRE 8) or higher, 64-bit, on your client machine.

1. Save the Download Manager installation file to your client machine and open it. For instructions on downloading and running the Download Manager, see either the [Installing SAP HANA 2.0, express edition (Binary Installer Method)](http://www.sap.com/developer/tutorials/hxe-ua-installing-binary.html) or [Installing SAP HANA 2.0, express edition (Virtual Machine Method)](http://www.sap.com/developer/tutorials/hxe-ua-installing-vm-image.html) tutorials, or go straight to the SAP HANA, express edition [registration](https://www.sap.com/cmp/ft/crm-xu16-dat-hddedft/index.html).

2. In Download Manager, in the **Image** pull-down, select either  **Virtual Machine** or **Binary Installer**.

    ![Download Manager](download_manager_clients_2sp02.png)

3. Click **Browse** and select a directory where your client package will be saved.

4. Select the **Clients** package that matches the machine you will be installing the clients on. Clear the *Select* boxes of all other packages.

5. Click **Download**. The **`hdb_client_<OS>.tgz`** file, or **`clients_windows.zip`** for Windows, downloads to your save directory.

6. Extract the compressed clients file:

    - For Windows and Mac machines, use a compression utility.

    - For Linux, navigate to the directory in which you wish to extract the client files and use the tar command.

    ```bash
    cd <preferred_filepath>
    sudo tar <download_filepath>/clients_<OS>.zip
    ```

    These files are extracted:

    **`clients_linux_x86_64.tgz`**

    - `hdb_client_linux_x86_64.tgz`

    - `xs.onpremise.runtime.client_linuxx86_64.zip`

    **`clients_linux_ppc64le.tgz`**

    - `hdb_client_linux_ppc64le.tgz`

    - `xs.onpremise.runtime.client_linuxppc64le.zip`

    **`clients_windows.zip`**

    - `hdb_client_windows_x86_32.zip`

    - `hdb_client_windows_x86_64.zip`

    - `xs.onpremise.runtime.client_ntamd64.zip`

    **`clients_mac.tgz`**

    - `hdb_client_mac.tgz`

    - `xs.onpremise.runtime.client_darwinintel64.zip`

[ACCORDION-END]

[ACCORDION-BEGIN [Install SAP HANA HDB Client: ](Windows)]

To install the SAP HANA client on a Windows machine, use either a graphical user interface or a command line.

1. Using a compression utility, extract `hdb_client_windows_x86_32.zip` or `hdb_client_windows_x86_64`. (These files are located in the `clients_windows.zip` file you downloaded earlier.)

    The following file path is created:

    ```
    hdb_client_windows\HDB_CLIENT_WINDOWS_X86_32
    ```

    or

    ```
    hdb_client_windows\HDB_CLIENT_WINDOWS_X86_64
    ```

2. In file explorer go to the `HDB_CLIENT_WINDOWS_X86_32` or `HDB_CLIENT_WINDOWS_X86_64` folder.

    Or:

    Open a command prompt and navigate to `HDB_CLIENT_WINDOWS_X86_32` or `HDB_CLIENT_WINDOWS_X86_64`.

3. In file explorer, double-click:

    - **`hdbsetup.exe`** - GUI installation

    - **`hdbinst.exe`** - Command line installation

    Or from a command prompt:

    Call the program `hdbsetup` (GUI installation) or `hdbinst` (command line installation) by entering one of the following commands:

    Option        | Description
    :---------    | :--------
    GUI           | `hdbsetup [-a client]`
    Command Line  | `hdbinst [-a client] [<option list>]`

4. Follow the instructions displayed by the installation tool.

5. Add the installation path to the PATH environment variable. For information on setting environment variables, see the documentation for your operating system.

[ACCORDION-END]

[ACCORDION-BEGIN [Install SAP HANA HDB Client: ](Mac)]

To install the SAP HANA client on a Mac machine, do the following:

1. Navigate to the directory where you wish to unpack the `hdb_client_mac.tgz` files.

    ```bash
    cd <your_destination>
    ```

2. Unpack `hdb_client_linux_mac.tgz`:

    ```bash
    sudo tar -xvzf <unzipped_filepath>/hdb_client_mac.tgz
    ```

    The directory `HDB_CLIENT_MACOS` is created.

3. Move into the `HDB_CLIENT_MACOS` directory and run `hdbinst`.

    ```bash
    cd HDB_CLIENT_MACOS
    sudo ./hdbinst
    ```

    Follow the onscreen instructions to install the SAP HANA client.

[ACCORDION-END]

[ACCORDION-BEGIN [Install SAP HANA HDB Client: ](Linux)]

To install the SAP HANA client on a Linux machine, do the following:

1. Navigate to the directory where you wish to unpack the `hdb_client_linux_x86_64.tgz` files.

    ```bash
    cd <your_destination>
    ```

2. Unpack `hdb_client_linux_x86_64.tgz`:

    ```bash
    sudo tar -xvzf <unzipped_filepath>/hdb_client_linux_x84_64.tgz
    ```

    The directory `HDB_CLIENT_LINUX_X86_64` is created.

3. Move into the `HDB_CLIENT_LINUX_X86_64` directory and run `hdbinst`.

    ```bash
    cd HDB_CLIENT_LINUX_X86_64
    sudo ./hdbinst
    ```

    Follow the onscreen instructions to install the SAP HANA client.

[ACCORDION-END]

[ACCORDION-BEGIN [Install SAP HANA HDB Client: ](PowerPC)]

To install the SAP HANA client on a Linux PowerPC machine, do the following:

1. Navigate to the directory where you wish to unpack the `hdb_client_linux_ppc64le.tgz` files.

    ```bash
    cd <your_destination>
    ```

2. Unpack `hdb_client_linux_ppc64le.tgz`:

    ```bash
    sudo tar -xvzf <unzipped_filepath>/hdb_client_linux_ppc64le.tgz
    ```

    The directory `HDB_CLIENT_LINUX_PPC64LE` is created.

3. Move into the `HDB_CLIENT_LINUX_PPC64LE` directory and run `hdbinst`.

    ```bash
    cd HDB_CLIENT_LINUX_PPC64LE
    sudo ./hdbinst
    ```

    Follow the onscreen instructions to install the SAP HANA client.

[ACCORDION-END]

[ACCORDION-BEGIN [Post-Installation Info: ](SAP HANA HDB Client)]

#### Logging the Installation

The SAP HANA client installation is automatically logged by the system. The log files are stored at `%TEMP%\hdb_client_<time_stamp>` for Windows and `/var/temp/hdb_client_<time_stamp>` for Linux.

#### Connect SAP HANA HDB Client to SAP HANA 2.0, express edition

Now that you've installed the SAP HANA HDB Client, connect to a SAP HANA 2.0, express edition system.

See these `How-Tos`:

- [Connect to SAP HANA, express edition using JDBC](http://www.sap.com/developer/how-tos/2016/08/hxe-connect-hxe-using-jdbc.html)

- [How to connect to SAP HANA database server in Python](http://www.sap.com/developer/how-tos/2016/08/hxe-python-connection.html)

#### Uninstall the SAP HANA HDB Client

Each installation has its own uninstallation tool. Use the `hdbuninst` command to uninstall the client software from your command prompt.

```bash
sudo <unzipped_filepath>/HDB_CLIENT_<version>/hdbuninst
```

Follow the onscreen instructions to uninstall the SAP HANA Client.

[ACCORDION-END]

[ACCORDION-BEGIN [Install: ](XS CLI Client)]

Install the command line client for XS advanced on your client machine.

In this procedure, you learn how to use the XS CLI client to connect to SAP HANA.

#### XS CLI Client Prerequisites

- XS advanced is installed on the SAP HANA 2.0, express edition machine (the server host).

- You have access to the Internet from your client machine.

- You have logon access to the SAP HANA 2.0, express edition database with the privileges to create SAP HANA users (for example, SYSTEM).

#### Installation Instructions

1. Using a compression utility, extract either `xs.onpremise.runtime.client_ntamd64.zip` for Windows, `xs.onpremise.runtime.client_darwinintel64.zip` for Mac, `xs.onpremise.runtime.client_linuxx86_64.zip` for Linux, or `xs.onpremise.runtime.client_linuxppc64le.zip` for PowerPC.

    The system will create this folder:

    ```
    xs.onpremise.runtime.client_<version>
    ```

2.  Check that the XS advanced run time is installed and available on the server machine.

    Enter the following URL in your Web browser:

    ```bash
    https://<hana_hostname>:3<instance>30/v2/info
    ```

    For example, the SAP HANA instance "90" on the host `xsa.acme.com`:

    ```
    https://xsa.acme.com:39030/v2/info  
    ```

    The response displayed in the Web browser is a JSON string with details that indicate a successful connection to the XSA controller. This connection test is important as you want to make sure the connection exists before you attempt it from within the API command.

3. Check that the XS client for XS advanced is installed and available.

    The XS client tools are required to connect to the XS advanced run time on SAP HANA and deploy your XS advanced applications.

    On your client machine, open a command window and run the following commands:

    >**Note** Linux users should run this command as `<sid>adm`.

    ```bash
    xs help  
    xs -v
    ```

    If you see output indicated **Client version**, XS is installed.

4. Connect to the XS advanced controller on the SAP HANA 2.0, express edition server.   

    Specify the URL of the API end point on the SAP HANA server you want to connect to:

    ```bash
    xs api https://<hostname>:3<instance_number>30
    ```

    > **Note**: This command may fail due to a missing SSL certificate. This note shows you how to set up a certificate on the client so it can connect to the server. Copy the correct certificate to your client from the SAP HANA 2.0, express edition server.

    > Open a command session on the server machine or open a PuTTY session to the server machine. From the command prompt, login as sudo and navigate to the certificate. The certificate `default.root.crt.pem` is typically located here:

    ```bash
    <installation_path>/<SID>/xs/controller_data/controller/ssl-pub/router
    ```

    > For example, where `<installation_path>` = `/hana/shared` and `<SID>` = `HDB`:

    ```
    /hana/shared/HDB/xs/controller_data/controller/ssl-pub/router/default.root.crt.pem
    ```

    > Copy the certificate to a folder on the server where you can easily access it.

    > Using an FTP client or the `scp` command, send a copy of the certificate from your server machine to a safe location on your client machine.

    >FTP example:

    ```
    /<path>/default.root.crt.pem
    ```

    >`scp` example:

    ```
    scp <server_machine_user>@<ip_address_server>:<file_destination>/default.root.crt.pem <client_machine_user>@<ip_address_client>:<your_desired_filepath>/
    ```

    > Exit your FTP and PuTTY sessions and return to your client machine. Try the previous command again, but use the **`-cacert`** command and specify the local certificate you just copied.

    ```bash
    xs api https://<hostname>:3<instance_number>30 -cacert "<copied_filepath>/default.root.crt.pem"
    ```


5. From the client machine, log on to the XS advanced run time.

    You log on to the SAP HANA instance specified in the API end point set in a previous step. SAP HANA provides a default user `XSA_ADMIN` with administrator permissions; you can use this user ID to test the logon. However, it is recommended to create a new user with more limited permissions, which you can use to log on for developer tasks.  

    ```bash
    xs login -u XSA_ADMIN -p "<password>"
    ```

    >**Note:** The password is assigned to the XSA_ADMIN user during SAP HANA 2.0, express edition installation.

6. Test your connection to XSA. Run the following command to view XS applications on the SAP HANA 2.0, express edition server.

    ```bash
    xs apps
    ```

[ACCORDION-END]

<!--
[ACCORDION-BEGIN [Info: ](Logging the Installation)]

The SAP HANA client installation is automatically logged by the system. The log files are stored at `%TEMP%\hdb_client_<time_stamp>` for Windows and `/var/temp/hdb_client_<time_stamp>` for Linux.

[ACCORDION-END]

[ACCORDION-BEGIN [Info: ](Connect SAP HANA HDB Client to SAP HANA 2.0, express edition)]

Now that you've installed the SAP HANA HDB Client, connect to an SAP HANA 2.0, express edition system.

See these `How-Tos`:

- [Connect to SAP HANA, express edition using JDBC](http://www.sap.com/developer/how-tos/2016/08/hxe-connect-hxe-using-jdbc.html)

- [How to connect to SAP HANA database server in Python](http://www.sap.com/developer/how-tos/2016/08/hxe-python-connection.html)

[ACCORDION-END]

[ACCORDION-BEGIN [Info: ](Uninstall the SAP HANA HDB Client)]

Each installation has its own uninstallation tool. Use the `hdbuninst` command to uninstall the client software from your command prompt.

```
sudo <unzipped_filepath>/HDB_CLIENT_<version>/hdbuninst
```

Follow the onscreen instructions to uninstall the SAP HANA Client.

[ACCORDION-END]

-->

## Next Steps
- [View similar How-Tos](http://www.sap.com/developer/tutorials.html) or [View all How-Tos](http://www.sap.com/developer/tutorials.html)
