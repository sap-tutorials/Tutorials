---
title: Installing SAP HANA HDB Client (PowerPC)
description: Install the client package if you intend to develop XS applications on a machine that will not have a local SAP HANA 2.0, express edition installation.
primary_tag: products>sap-hana\,-express-edition
tags: [ tutorial>beginner, products>sap-hana\,-express-edition ]
---

<!-- loiobda302faf36e442c8934f7245b924986 -->

## Prerequisites
 - **Proficiency:** Beginner
 - **Tutorials:** You have completed [Start SAP HANA, express edition Server (VM installations)](http://www.sap.com/developer/tutorials/hxe-ua-getting-started-vm.html) or [Test the Installation (Native Linux installations)](http://www.sap.com/developer/tutorials/hxe-ua-test-binary.html)

## Details
### You will learn
How to install the SAP HANA client on a PowerPC machine, using either a graphical user interface or a command line.

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

To install the SAP HANA client on a Linux PowerPC machine, do the following:

1.  Go to the directory where you wish to unpack the `hdb_client_linux_x86_64.tgz` files:

    ```bash
    cd <your_destination>
    ```

2.  Unpack the file:

    ```bash
    sudo tar -xvzf <unzipped_filepath>/hdb_client_linux_ppc64le.tgz
    ```

    The directory `HDB_CLIENT_LINUX_X86_64` is created.

3.  Navigate to the `HDB_CLIENT_LINUX_X86_64` directory and run `hdbinst` to start the installer:

    ```bash
    cd HDB_CLIENT_LINUX_PPC64LE
    sudo ./hdbinst
    ```

    Follow the instructions on the screen to install the SAP HANA HDB client.


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


