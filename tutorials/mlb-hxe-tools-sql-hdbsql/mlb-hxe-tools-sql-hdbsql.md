---
title: Execute SQL on SAP HANA, express edition using the HDBSQL tool
description: Provide details on the installation the SAP HANA client and instruction to connect a SAP HANA, express edition instance using the HDBSQL tool.
primary_tag: products>sap-hana\, express-edition
tags: [  tutorial>intermediate, products>sap-hana\, express-edition ]
---
## Prerequisites  
- Proficiency: intermediate

### You will learn

In this tutorial, you will install the SAP HANA client for SAP HANA, express edition.

Then, you will learn how to connect your SAP HANA, express edition instance using the SAP HANA HDBSQL tool.

SAP HANA HDBSQL is a command line tool for executing commands on SAP HANA databases.

## Details

### Time to Complete
**10 Min**.

[ACCORDION-BEGIN [Step 1: ](Download the SAP HANA client)]

After registering to download [SAP HANA, express edition](https://www.sap.com/developer/topics/sap-hana-express.html), use the **Download Manager** to retrieve the client package for the system you will connect from.

The package names are:

 - Linux (x86/64) : `clients_linux_x86_64.tgz`
 - Windows: `clients_windows.zip`

The downloaded archive for the client package contains both the ***SAP HANA HDB Client*** and the ***SAP HANA XS CLI***.

The ***SAP HANA HDB Client*** software package includes the following connectivity/drivers:

 - SQLDBC
 - ODBC
 - JDBC
 - Python (`PyDBAPI`)
 - Node.js
 - Ruby

Here you will install the ***SAP HANA HDB Client*** only.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Install the SAP HANA client)]

#### **Connecting from Linux environments**

First, you need to extract the contents of the downloaded package:

```bash
tar -xvzf /opt/hxe/clients_linux_x86_64.tgz -C /opt/hxe
```

The following files will be extracted:

 - ***`hdb_client_linux_x86_64.tgz`*** : the *SAP HANA HDB Client* software package
 - ***`xs.onpremise.runtime.client_linuxx86_64.zip`*** : the *SAP HANA XS CLI* software package

You need now to decompress the *SAP HANA HDB Client* package executing the following command:

```bash
tar -xvzf /opt/hxe/hdb_client_linux_x86_64.tgz -C /opt/hxe/installer
```

And now you can run the installer program executing the following commands:

```bash
sudo su -l hxeadm

cd /opt/hxe/installer/HDB_CLIENT_LINUX_X86_64
./hdbinst

exit
```

Accept the prompts default values to configure your installation:

 - Installation Path : `/usr/sap/hdbclient`

Once the installation is completed, you should get the following elements in your console:

```
Installation done
```

#### **Connecting from Windows environments**

First, you need to extract the contents of the downloaded package using your favorite archive manager (for example `7zip`, `WinZip` or `WinRar`).

The following files will be extracted:

 - ***`hdb_client_windows_x86_32.zip`*** : the *SAP HANA HDB Client* software package for Windows 32 bits platforms
 - ***`hdb_client_windows_x86_64.zip`*** : the *SAP HANA HDB Client* software package for Windows 64 bits platforms
 - ***`xs.onpremise.runtime.client_ntamd64.zip`*** : the *SAP HANA XS CLI* software package

You need now to decompress the *SAP HANA HDB Client* package for your target platform.

And now you can run the installer program executing the `hdbsetup.exe` application located in the `HDB_CLIENT_WINDOWS_X86_64` directory.

Accept the prompts default values to configure your installation:

 - Installation Path : `C:\Program Files\sap\hdbclient`

Once the installation is completed, you should get the following elements in your console:

```
Installation finished successfully
```

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Locate the HDBSQL tool)]

The HDBSQL tool, which is installed as part of the SAP HANA client, is located at (unless specified otherwise during the installation):

 - on Linux and UNIX platforms `/usr/sap/hdbclient/`
 - on Microsoft Windows platforms: `C:\Program Files\SAP\hdbclient\`

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Details about the HDBSQL tool)]

The HDBSQL tools support an interactive mode, a non-interactive mode and a file mode.

HDBSQL provide a large number of features like variable substitution, use of input and output files, formatting options etc.

You can check the [SAP HANA Administration Guide](https://help.sap.com/viewer/6b94445c94ae495c83a19646e7c3fd56/2.0.02/en-US/c22c67c3bb571014afebeb4a76c3d95d.html) for more details.

#### **Connection details**

HDBSQL support both the use of instance number or the port number to connect to your instances:

```
hdbsql -i <instance nr> -n <host> -u <user> -p <password>
```
or
```
hdbsql -n <host>:<port> -u <user> -p <password>
```

#### **Password details**

If you prefer not to input your password as a command line parameter, you can omit the parameter and you will be prompted for it.

Alternatively, you cab use a **Secure User Store** (`a.k.a.` ***`hdbuserstore`***).

When using the ***`hdbuserstore`***, your credentials (including your user name and server details) are saved and secured in a store that can used directly from the HDBSQL tool but not only.

#### **SQL commands**

As stated previously, you use an interactive mode, where you will type/paste your command interactively.

You can also pass the SQL as part of the command:

```
hdbsql -n <host>:<port> -u <user> -p <password> "SQL statement"
```
or using an input SQL file:
```
hdbsql -n <host>:<port> -u <user> -p <password> -I "SQL file full path""
```

All the available options are listed in the [SAP HANA HDBSQL Options](https://help.sap.com/viewer/6b94445c94ae495c83a19646e7c3fd56/2.0.02/en-US/c24d054bbb571014b253ac5d6943b5bd.html) documentation.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Test your connections)]

#### **Connect to the SYSTEM Database**

Open a terminal console and execute the following command:

```shell
cd <SAP HANA Client installation directory>

hdbsql -n <host>:39013 -u SYSTEM -p <password> "SELECT DATABASE_NAME, ACTIVE_STATUS, RESTART_MODE FROM M_DATABASES ORDER BY 1;"
```

Make sure you adjust the SAP HANA Client installation directory, the host and password details.

The console output should look like this:

```
DATABASE_NAME,ACTIVE_STATUS,RESTART_MODE
"HXE","YES","DEFAULT"
"SYSTEMDB","YES","DEFAULT"

2 rows selected (overall time 116.252 msec; server time 376 usec)
```

#### **Connect to the HXE tenant**

Open a terminal console and execute the following command:

```shell
cd <SAP HANA Client installation directory>

hdbsql -n <host>:39015 -u SYSTEM -p <password> "SELECT DATABASE_NAME, ACTIVE_STATUS, RESTART_MODE FROM M_DATABASES ORDER BY 1;"
```

Make sure you adjust the SAP HANA Client installation directory, the host and password details.

The console output should look like this:

```
DATABASE_NAME,ACTIVE_STATUS,RESTART_MODE
"HXE","YES","DEFAULT"

1 row selected (overall time 51.027 msec; server time 312 usec)
```

[ACCORDION-END]
