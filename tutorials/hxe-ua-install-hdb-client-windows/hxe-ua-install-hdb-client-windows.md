---
title: Installing SAP HANA HDB Client (Windows)
description: Install the client package if you intend to develop XS applications on a machine that will not have a local SAP HANA 2.0, express edition installation.
author_name: Adrian Plata
author_profile: https://github.com/aplata-sap
primary_tag: products>sap-hana\,-express-edition
tags: [ tutorial>beginner, products>sap-hana\,-express-edition ]
time: 10
---

<!-- loio45dafcf13cb6489cb25149a2a62a442e -->

## Prerequisites
 - **Tutorials:** You have completed [Start SAP HANA, express edition Server (VM installations)](https://developers.sap.com/tutorials/hxe-ua-getting-started-vm.html) or [Test the Installation (Native Linux installations)](https://developers.sap.com/tutorials/hxe-ua-test-binary.html)

## Details
### You will learn
How to install the SAP HANA client on a Windows machine, using either a graphical user interface or a command line.

---

The `server machine` in these instructions refers to the laptop on which SAP HANA 2.0, express edition is installed, while `client machine` refers to your local machine. You do not need to install the two on the same machine or VM.

The clients let you access SAP HANA 2.0, express edition, from your client machine. This is the Reduced SAP Client package.

The clients included with the SAP HANA HDB client software package are:

-   JDBC

-   ODBC

-   SQLDBC

-   `ODBO/MDX`

-   Python (`PyDBAPI`)

-   `ADO.NET`


To install the SAP HANA HDB client on a Windows machine, use either a graphical user interface or a command line.

[ACCORDION-BEGIN [Step 1: ](Download the client package)]

Install the Download Manager to your client machine and download the client package.

1.  Save the Download Manager installation files to your client machine and open it. For instructions on downloading and running the Download Manager, see either the [Installing SAP HANA 2.0, express edition (Binary Installer Method)](https://developers.sap.com/tutorials/hxe-ua-installing-binary.html) or [Installing SAP HANA 2.0, express edition (Virtual Machine Method)](https://developers.sap.com/tutorials/hxe-ua-installing-vm-image.html) tutorials, or go straight to the SAP HANA, express edition [registration page](https://www.sap.com/cmp/ft/crm-xu16-dat-hddedft/index.html).

2.  In Download Manager, in the `Image` menu, select either `Virtual Machine` or `Binary Installer`.

3.  Click `Browse` and select a directory where your client package will be saved.

4.  Select the `Clients (Windows)` package. Clear the Select boxes of all other packages.

5.  Click `Download`. The file `clients_windows.zip` downloads to your save directory.

6.  Use a compression utility to extract the compressed clients file.

    This extracts the following files and their contents:

    -   `hana_ml- <version>.tar.gz`

    -   `hana.ml.r- <version>.tar.gz`

    -   `hdb_client_windows_x86_32.zip`

    -   `hdb_client_windows_x86_64.zip`

    -   `xs.onpremise.runtime.client_ntamd64.zip`


[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Install the SAP HANA HDB client)]

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

    -   Command Line - `hdbinst [-a client] [<option list>]`

    Follow the onscreen prompts displayed by the installation tool.

4.  Add the installation path to the PATH environment variable. For information on setting environments variables, see the documentation for your operating system.


[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Log the installation)]

The system automatically logs the SAP HANA HDB client installation. The log files are stored at `%TEMP%\hdb_client_<time_stamp>`.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Connect to SAP HANA, express edition)]

Connect to a SAP HANA 2.0, express edition system using either JDBC or Python:

[Use Clients to Query an SAP HANA Database](https://developers.sap.com/mission.hana-cloud-clients.html)


[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Uninstall the SAP HANA HDB client)]

Each installation has its own uninstallation tool. Use the `hdbuninst` command to uninstall the client software from your command prompt.

```bash
<unzipped_filepath>/HDB_CLIENT_<version>/hdbuninst
```

Follow the instructions on the screen to uninstall the SAP HANA HDB client.

[DONE]

[ACCORDION-END]
