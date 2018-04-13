---
title: Installing SAP HANA HDB Client (Windows)
description: Install the client package if you intend to develop XS applications on a machine that will not have a local SAP HANA 2.0, express edition installation.
primary_tag: products>sap-hana\,-express-edition
tags: [ tutorial>beginner, products>sap-hana\,-express-edition ]
---

<!-- loio45dafcf13cb6489cb25149a2a62a442e -->

## Prerequisites
 - **Proficiency:** Beginner
 - **Tutorials:**

## Details
### You will learn
How to install the SAP HANA client on a Windows machine, using either a graphical user interface or a command line.

### Time to Complete
10 min

---

The `server machine` in these instructions refers to the laptop on which SAP HANA 2.0, express edition is installed, while `client machine` refers to your local machine. You do not need to install the two on same machine or VM.

The clients let you access SAP HANA 2.0, express edition, from your client machine. This is the Reduced SAP Client package.

The clients included with the SAP HANA HDB client software package are:

-   SQLDBC

-   ODBC

-   JDBC

-   Python (`PyDBAPI`)

-   Node.js

-   Ruby


To install the SAP HANA HDB client on a Windows machine, use either a graphical user interface or a command line.

[ACCORDION-BEGIN [Step 1: ](Download the client package.)]

Install the Download Manager to your client machine and download the client package.

1.  Save the Download Manager installation files to your client machine and open it.

2.  In Download Manager, in the `Image` menu, select either `Virtual Machine` or `Binary Installer`.

3.  Click `Browse` and select a directory where your client package will be saved.

4.  Select the `Clients` package that matches the machine you will be installing the clients on. Clear the Select boxes of all other packages.

5.  Click `Download`. The `hdb_client_<OS>.tgz` file (`clients_windows.zip` for Windows) downloads to your save directory.

6.  Extract the compressed clients file:

    -   For Windows and Mac machines, use a compression utility.

    -   For Linux, navigate to the directory in which you wish to extract the client files and use the `tar` command:

```bash
cd <preferred_filepath>
sudo tar <download_filepath>/clients_<OS>.zip
```

This extracts the following files and their contents:

`clients_linux_x86_64.tgz`

-   `hdb_client_linux_x86_64.tgz`

-   `xs.onpremise.runtime.client_linuxx86_64.zip`


`clients_linux_ppc64.le.tgz`

-   `hdb_client_linux_ppc64le.tgz`

-   `xs.onpremise.runtime.client_linuxx86_64.zip`


`clients_windows.zip`

-   `hdb_client_windows_x86_32.tgz`

-   `hdb_client_windows_x86_64.tgz`

-   `xs.onpremise.runtime.client_ntamd64.zip`


`clients_mac.tgz`

-   `hdb_client_mac.tgz`

-   `xs.onpremise.runtime.client_darwinintel64.zip`


[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Install the SAP HANA HDB client.)]

SAP HANA HDB client installation supports both GUI and console methods.

1.  Use a compression utility to extract the downloaded files `hdb_client_windows_x86_32.zip` or `hdb_client_windows_x86_64.zip` for 32-bit and 64-bit installations respectively.

    The following file path is created:

    ```bash
    hdb_client_windows/HDB_CLIENT_WINDOWS_X86_32
    ```

    or

    ```bash
    hdb_client_windows/HDB_CLIENT_WINDOWS_X86_64
    ```

2.  Navigate to the `HDB_CLIENT_WINDOWS_86_32` or `HDB_CLIENT_WINDOWS_X86_64` folder.

3.  In the file explorer, double-click:

    -   `hdbsetup.exe` - GUI installation

    -   `hdbinst.exe` - Command line installation

    Or from a command prompt:

    Call the program `hdbsetup` (GUI installation) or `hdbinst` (command line installation) by entering one of the following commands:

    -   GUI - `hdbsetup [-a client]`

    -   Command Line - `hdbinst [-a client] [`option list`]`

    Follow the onscreen prompts displayed by the installation tool.

4.  Add the installation path to the PATH environment variable. For information on setting environments variables, see the documentation for your operating system.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Log the installation.)]

The system automatically logs the SAP HANA HDB client installation. The log files are stored at `%TEMP%\hdb_client_<time_stamp>` for Windows and `/var/temp/hdb_client_<time_stamp>` for Linux.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Connect to SAP HANA, express edition.)]

Connect to a SAP HANA 2.0, express edition system using either JDBC or Python.

See these `Tutorials`:

-   [Connect to SAP HANA, express edition using JDBC](http://www.sap.com/developer/tutorials/hxe-connect-hxe-using-jdbc.html)

-   [Connect to SAP HANA, express edition using Python](http://www.sap.com/developer/tutorials/hxe-python-connection.html)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Uninstall the SAP HANA HDB client.)]

Each installation has its own uninstallation tool. Use the `hdbuninst` command to uninstall the client software from your command prompt.

```bash
sudo <unzipped_filepath>/HDB_CLIENT_<version>/hdbuninst
```

Follow the instructions on the screen to uninstall the SAP HANA HDB client.

[ACCORDION-END]
