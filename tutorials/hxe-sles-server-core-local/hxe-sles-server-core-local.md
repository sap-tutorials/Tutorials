---
title: Install SAP HANA 2.0, express edition on Red Hat Enterprise Linux (Server only)
description: Learn how to prepare your Red Hat Enterprise Linux system to install and test SAP HANA, express edition
primary_tag: products>sap-hana\,-express-edition  
tags: [  tutorial>intermediate, products>sap-hana\,-express-edition]
---

### You will learn
In this tutorial, you will install the dependencies needed and configure your Red Hat Enterprise Linux system for the setup and execution of SAP HANA, express edition.

Then you will learn how to download the binary installer archives of SAP HANA 2.0, express edition, decompress and install it on your server, including some additional packages for your SAP HANA 2.0, express edition installation.

For troubleshooting information, see [SAP HANA, express edition Troubleshooting](https://www.sap.com/developer/how-tos/2016/09/hxe-ua-troubleshooting.html).

> ### **Note:**
>**This tutorial was build and tested using SAP HANA, express edition 2.0 SPS02 revision 22 and SUSE Linux Enterprise Server for SAP Applications, 12.2 for `x86-64` on an Intel NUC**.

&nbsp;

## Details

### Time to Complete
**90 Min**

[ACCORDION-BEGIN [Step 1: ](Verify your Machine Requirements)]

Before you get started, you should check if your machine meet the recommended software and hardware to successfully install and run SAP HANA 2.0, express edition.

> ### **Note: Supported Platform Disclaimer**
>
>The operating systems officially supported by ***SAP HANA, express edition 1.0 SPS12*** include:
>
> - SUSE Linux Enterprise for SAP Applications, 11.4, 12.0, 12.1
> - Red Hat Enterprise Linux 7.2
>
>The operating systems officially supported by ***SAP HANA, express edition 2.0*** include:
>
> - SUSE Linux Enterprise Server for SAP Applications, 12.1, 12.2
> - SUSE Linux Enterprise Server for SAP Applications, IBM Power Systems (`ppc64le` - "Little Endian"), 12.1, 12.2
> - Red Hat Enterprise Linux for SAP Applications 7.2, 7.3
> - Red Hat Enterprise Linux for SAP Applications for Power 7.3 (SAP HANA 2.0, express edition SPS 02 Rev 21 or higher required)
>
>Please refer to the version of the [binary installation guide](https://help.sap.com/viewer/32c9e0c8afba4c87814e61d6a1141280/2.0.02/en-US) for the latest updates.
>
>SAP Community members have been successful in running HXE on newer or other Linux operating systems that are not formally supported by SAP, such as Ubuntu, openSUSE and Fedora.
>
However, SAP is not committing to resolving any issues that may arise from running SAP software on any unsupported platforms.
>

&nbsp;

Your server will need the following:

Hardware | Details
---------|-----------------
RAM      | 16 GB minimum (24 GB recommended)<br><br> <blockquote> <b>Note:</b> If you are installing on a system with 16 GB of RAM, increase the amount of swap space to at least 32 GB.</blockquote>
HDD      | 120 GB HDD recommended
Cores    | 2 cores (4 recommended)

---

### <b>Check the RAM</b>

---

To check the amount of RAM memory available on your system you can execute the following from your server terminal console:

```bash
free -m
```

And the output will look like this:

```
        total   used   free   shared  buff/cache   available
Mem:     7808   7265    133       23         409         235
Swap:    8064   3627   4437
```

---

### <b>Check the HDD (Hard Drive Disk)</b>

---

To check the amount of disk space available on your system, you can execute the following from your server terminal console:

```bash
df --block-size=1G
```

Here it is important to consider that the software will be installed in the `/usr/sap` directory.

---

### <b>Check the CPU</b>

---

To check the number of CPU available on your system, you can execute the following from your server terminal console:

```bash
grep -c ^processor /proc/cpuinfo
```

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Prepare Your System)]

---

### <b>Java Version</b>

---

One the requirement to install SAP HANA, express edition is a 64-bit Java Runtime Environment (JRE) 8 or Higher, especially if you are planning to use the SAP HANA 2.0, express edition ***Download Manager***.

To check if Java is installed, you can run the following command from your terminal console:

```bash
java -version
```

which should return:

```
java version "1.8.0_xx"
Java(TM) SE Runtime Environment (build 1.8.0_xx-yyy)
```

If you don't have it yet installed, you can check the following link for download link and installation instructions : <a href="https://tools.hana.ondemand.com/#cloud" target="new">https://tools.hana.ondemand.com/#cloud</a>

Using the RPM option is most likely the easiest, as you will have to simply run the following command from your terminal console (where **<version>** needs to be adjusted based on the downloaded version):

```
sudo rpm -ivh <rpm directory>/sapjvm-<version>-linux-x64.rpm
```

Then you will need to update the "alternatives" and enable your flavor of java using the following commands:

```bash
sudo update-alternatives --install "/usr/bin/java" "java" "/usr/java/sapjvm_8_latest/bin/java" 1
sudo update-alternatives --set java /usr/java/sapjvm_8_latest/bin/java
```

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Reboot your system)]

Some of the changes applied requires a restart of the system to fully take effect, you must reboot your system before proceeding with the installation.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Register for SAP HANA, express edition)]

Go to the registration page at https://www.sap.com/sap-hana-express published on the SAP site.

Alternately, you can go to the SAP HANA, express edition launch page at https://www.sap.com/developer/topics/sap-hana-express.html published on SAP site and click the Register and download SAP HANA, express edition download manager link.

Complete the registration form and click the Register button. The Registration Success page displays. You will also receive an email indicating successful registration.

> **Note:**
>If you have an SAP login, click the Login icon at the top of the page to populate the registration form automatically.

&nbsp;

![Registration Page](01.png)

The **Registration Success** page displays. (You will also receive an email indicating successful registration.)

![Registration Success page](02.png)

On the **Registration Success** page, under **1A. ON-PREMISE INSTALLATION**, you will get the different download manager options listed:

 - ***Linux*** (`HXEDownloadManager_linux.bin`)

	 This standard Linux executable allows you to run the download manager directly from your Linux server machine, using either the ***GUI Mode*** (assuming you have a X11 properly configured) or the ***Console Mode***

 - ***Windows*** (`HXEDownloadManager_win.exe`)

 This standard Windows executable allows you to run the download manager directly from your Window machine, using only the ***GUI Mode***. Then you will need to transfer the downloaded archives to you target server.

 - ***Platform-independent*** (`HXEDownloadManager.jar`)

 Allows you to run the download manager directly from any platform (as it is a Java based program), using either the ***GUI Mode*** or the ***Console Mode***.

 To run the Platform-independent download manager, you will need to execute the following:

```
java -jar <download manager path>/HXEDownloadManager.jar
```

> **Note:** If you have a Mac, or another type of machine, click "Platform-independent" for a platform-independent download manager.

&nbsp;

You will find next the instructions for both the ***GUI Mode*** and the ***Console Mode***. It is however, recommended that you use the ***Console Mode*** directly from the RHEL server.

Save the ***Platform-independent*** **Download Manager** file directly on your RHEL system.

> **Note:** If your RHEL system is not connected to the internet, then you have no choice but to save the Download Manager locally, download the packages locally then transfer them to your RHEL system.

&nbsp;

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5:](Download Your Packages)]

As stated in the previous section you can download the installation packages either using a *GUI Mode* or a *Console Mode*.

You will find below the instructions for both. It is however, recommended that you use the ***Console Mode*** directly from the RHEL server.

> **Note:** Again, if your RHEL system is not connected to the internet, then you have no choice but to save the Download Manager locally, download the packages locally (using either the *GUI Mode* or a *Console Mode*) then transfer them to your RHEL system.

&nbsp;

---

### <b>Using the Console Mode</b>

---

> **Note:** Before, run running the Download Manager in console, make sure to close any open GUI Mode running instances.

&nbsp;

Open a command prompt or a terminal console at the location where you saved the *Platform-independent* download manager file.

You can display the command help using the `-h` argument like this:

```
java -jar <download manager path>/HXEDownloadManager.jar -h
```

The command syntax is:

```bash
HXEDownloadManager [( [-h|-X] | [-d <save_directory>] [--ph <proxy_host>] [--pp <proxy_port>] <platform> <image> <file>... )]
```

> **Note:** You must include an argument with each command. If you call the Download Manager without an argument, it opens in GUI mode.

&nbsp;

You will need to download the following packages:

- *Server Only Installer*
- *Client for Linux X86/64*
- *SAP HANA External Machine Learning AFL*
- *Text analysis files for additional languages*

Execute the following commands to create the directory where you download the installer packages, set the permissions to allow all required access, and finally run the download manager (***make sure to update the download manger path***):

```bash
sudo mkdir /opt/hxe
sudo chmod a+rwx /opt/hxe

cd <download manager path>

java -jar HXEDownloadManager.jar linuxx86_64 installer \
  -d /opt/hxe \
  --ph proxy \
  --pp 8080 \
  hxe.tgz \
  clients_linux_x86_64.tgz \
  eml.tgz \
  additional_lang.tgz
```

The archive packages will be downloaded in the ***`/opt/hxe`***.

For more information about the other downloadable package, please refer to the ***Appendix*** section.

---

### <b>Using the GUI Mode</b>

---

> **Note:** To run the download manager suing the GUI Mode, it assumes that your RHEL system is configured with a *Desktop* environment like GNOME or KDE.

&nbsp;

Start the download manager.

![Download Manager](03.png)

In the **Platform** pull-down, select **Linux/`x86-64`** (unless your target system is **Linux/Power (little endian)**.)

In the **Image** pull-down, select **Binary Installer**.

Click **Browse** and select where you want your downloads to be saved.

Then select the following packages:

- *Server Only Installer*
- *Client for Linux X86/64*
- *SAP HANA External Machine Learning AFL*
- *Text analysis files for additional languages*

Click the **Download** button to being your download.

Your download is complete when a pop-up message appears confirming successful download. Make sure you wait for this message before accessing the downloaded files.

Now that the file are downloaded, you can transfer them to the ***`/opt/hxe`*** directory on your target RHEL system.

Make sure you update the read/write/execute permission on the `/opt/hxe` directory using the following command:

```bash
sudo chmod a+rwx /opt/hxe
```

For more information about the other downloadable package, please refer to the ***Appendix*** step.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 6:](Install SAP HANA, express edition)]

---

### <b>Install the Server package</b>

---

You need to extract the contents of the downloaded archive packages using the following command:

```bash
mkdir /opt/hxe/installer
tar -xvzf /opt/hxe/hxe.tgz -C /opt/hxe/installer
```

> **Note:** the `hxe.tgz` package must be downloaded and extracted first whatever the installation you are planning as it include the *core* data units to be installed.

&nbsp;

Then, you will need to navigate to the directory where the archives were extracted and run the setup.:

From your server terminal console, execute the following commands:

```bash
cd /opt/hxe/installer
sudo ./setup_hxe.sh
```

Accept the prompts default values (unless no value is provided) to configure your installation:

|--------------------------|---------------------------------------|
| Installer root directory | `/opt/hxe/installer/HANA_EXPRESS_20`
| SAP HANA system ID       | HXE
| HANA instance number     | 90
| local host name          | should display your local host name
| HDB master password      | you will need to conform to the *Master password policy* as described bellow

Once the installation is completed, you should get the following elements in your console:

```
  ##################################
  # Summary after execution        #
  ##################################
  Server Installation...(OK)
  XSC Installation...(OK)
  HXE Optimization...(OK)
```

After the installation is completed, it is recommended to run the ***Memory Management Script*** as described in the ***Best Practice*** to release all unused resources and free up some memory.

```bash
sudo su -l hxeadm

cd /usr/sap/HXE/home/bin
./hxe_gc.sh

exit
```

> **Note: Master password policy**
> The master password you will specify is used for the `<sid>adm` and `sapadm` OS users, the telemetry technical user, and the HANA SYSTEM user.
>
> SAP HANA, express edition requires a very strong password that complies with these rules:
>
> - At least 8 characters
> - At least 1 uppercase letter
> - At least 1 lowercase letter
> - At least 1 number
> - Can contain special characters, but not _&grave;_ (backtick), _&#36;_ (dollar sign),  _&#92;_ (backslash), _&#39;_ (single quote), or _&quot;_ (double quotes)
> - Cannot contain dictionary words
> - Cannot contain simplistic or systematic values, like strings in ascending or descending numerical or alphabetical order

&nbsp;

---

### <b>Install the SAP HANA HDB Client for Linux</b>

---

The downloaded archive for the client package contains both the ***SAP HANA HDB Client*** and the ***SAP HANA XS CLI***.

Here you will only install the ***SAP HANA HDB Client***.

The ***SAP HANA HDB Client*** software package includes the following connectivity/drivers:

 - SQLDBC
 - ODBC
 - JDBC
 - Python (`PyDBAPI`)
 - Node.js
 - Ruby

Then, you need to extract the contents of `clients_linux_x86_64.tgz` into the ***`/opt/hxe`*** directory using the following command:

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

---

### <b>Install SAP HANA External Machine Learning AFL</b>

---

You need to extract the contents of the downloaded archive packages using the following command:

From your server terminal console, execute the following command:

```bash
tar -xvzf /opt/hxe/eml.tgz -C /opt/hxe/installer
```

You need now to execute the following commands:

```bash
sudo su -l hxeadm

cd /opt/hxe/installer/HANA_EXPRESS_20
./install_eml.sh

exit
```

Once the installation is completed, you should get the following elements in your console:

```
Installation done
```

After the installation is completed, it is recommended to run the ***Memory Management Script*** as described in the ***Best Practice*** to release all unused resources and free up some memory.

```bash
sudo su -l hxeadm

cd /usr/sap/HXE/home/bin
./hxe_gc.sh

exit
```

---

### <b>Install the additional Text Analysis files</b>

---

If you want to use the SAP HANA, express edition **Text Analysis** feature in a language other than English or German, you will need to install the **Text analysis files for additional languages** package.

You should be prompted for the master password provided during the installation process.

Then, extract the contents of `additional_lang.tgz` to the ***`lexicon`*** directory using the following command:

```bash
sudo tar -xvzf /opt/hxe/additional_lang.tgz -C /hana/shared/HXE/global/hdb/custom/config/lexicon
```

[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Test Your Installation)]

---

### <b>Running processes</b>

---

By default, when the server installation is completed, all required process should be started.

Here are the steps to verify that all the required processes are up and running.

You need now to execute the following commands:

```bash
sudo su -l hxeadm

HDB info | grep -e hdbnameserver -e hdbcompileserver -e hdbindexserver -e hdbdiserver -e hdbwebdispatcher
```

The output should look like this:

```
hxeadm    71252  71235 55.2 7982280 5377200      \_ hdbnameserver
hxeadm    71347  71235  2.1 1580412 257312      \_ hdbcompileserver
hxeadm    71374  71235 12.8 5321396 2845732      \_ hdbindexserver -port 39003
hxeadm    71516  71235  1.6 1588012 260768      \_ hdbdiserver
hxeadm    71518  71235  1.9 1880592 279788      \_ hdbwebdispatcher
```

The following services must be running:

  * `hdbnameserver`
  * `hdbcompileserver`
  * `hdbindexserver`
  * `hdbdiserver`
  * `hdbwebdispatcher`

If any of the above services is not listed, you must start your instance executing the following command:

```bash
HDB start
```

When the prompt returns, the system is started.

---

### <b>SAP HANA Client</b>

---

You can check the following links to verify some of the connectivity options available for the ***SAP HANA HDB Client***:

 - <a href="https://www.sap.com/developer/how-tos/2016/08/hxe-connect-hxe-using-jdbc.html" target="new">JDBC</a>
 - <a href="https://www.sap.com/developer/how-tos/2016/08/hxe-python-connection.html" target="new">Python</a>
 - ODBC (content coming soon!)
 - SQLDBC (content coming soon!)
 - Node.js (content coming soon!)
 - Ruby (content coming soon!)

 ---

### <b>SAP HANA External Machine Learning Library (EML)</b>

---

The SAP HANA External Machine Learning Library is an application function library (AFL) supporting the integration of Google `TensorFlow`, as an external machine learning framework, with SAP HANA, express edition.

Now, you can test that the installation was successful using a `hdbsql` command.

You need now to execute the following commands with the default installation values::

```bash
sudo su -l hxeadm

cd /usr/sap/HXE/HDB90/exe

./hdbsql \
	-i 90 \
	-d SystemDB \
	-u SYSTEM \
	-p "<master-password>" \
	"SELECT * FROM SYS.AFL_AREAS WHERE AREA_NAME = 'EML';"
```
You will need to replace `<master-password>` by the master password as provided during the installation.

You should receive a result similar to this:

```
AREA_OID,SCHEMA_NAME,AREA_NAME,CREATE_TIMESTAMP
159136,"_SYS_AFL","EML","<installation date>"
```

---

### <b>XS Engine</b>

---

You can check that the `XSEngine` is up and running using a browser using the following URL pattern with the default installation values, you can use:

  <a href="http://<hostname>:8090" target="new">http://&lt;hostname&gt;:8090</a>

or

  <a href="https://<hostname>:4390" target="new">https://&lt;hostname&gt;:4390</a>

A success page displays:

![Success Page](07-xsengine.png)

If you can't connect to the page you can verify that the port is in LISTEN mode using the following command:

```bash
netstat -ano | grep -e 8090 -e 4390 | grep LISTEN
```

And the expected result should be:

```
tcp   0   0 0.0.0.0:8090    0.0.0.0:*     LISTEN   off (0.00/0/0)
tcp   0   0 0.0.0.0:4390    0.0.0.0:*     LISTEN   off (0.00/0/0)
```

If the result is empty, then one of the required services is not up and running else your firewall is blocking the communication.

[ACCORDION-END]

[ACCORDION-BEGIN [Best Practice: ](Deactivate the SYSTEM user)]

The SYSTEM user is the database "super-user" and is not intended for day-to-day activities in production systems.

For better security, you can create other database users with only the privileges that they require for their tasks (for example, user administration), then deactivate the SYSTEM user.

You need now to execute the following commands to create a new admin user with the USER ADMIN system privilege:

```bash
sudo su -l hxeadm

cd /usr/sap/HXE/HDB90/exe

./hdbsql \
	-i 90 \
	-d SystemDB \
	-u SYSTEM \
	-p "<master-password>" \
	"CREATE USER <admin-username> PASSWORD <admin-password> NO FORCE_FIRST_PASSWORD_CHANGE;"

./hdbsql \
	-i <instance-number> \
	-d SystemDB \
	-u SYSTEM \
	-p "<master-password>" \
	"GRANT USER ADMIN TO <admin-username> WITH ADMIN OPTION;"
```
You will need to replace `<master-password>` by the master password as provided during the installation.

> ### **Warning**: do not run the next command until you have successfully created an alternate admin user.

&nbsp;

Now that the new admin user is created, you can deactivate the existing SYSTEM user using:

```bash
./hdbsql \
  -i <instance-number> \
  -d SystemDB \
  -u SYSTEM \
  -p "<master-password>" \
	"ALTER USER SYSTEM DEACTIVATE USER NOW;"
```

[ACCORDION-END]

[ACCORDION-BEGIN [Best Practice: ](Backups)]

It a good practice to make regular data backups and save your work.

For information on data backup, recovery, and log file growth, see the <a href="https://help.sap.com/viewer/6b94445c94ae495c83a19646e7c3fd56/2.0.00/en-US" target="new">SAP HANA 2.0 Administration Guide</a>.

[ACCORDION-END]

[ACCORDION-BEGIN [Best Practice: ](Set the Global Memory Allocation Limit)]

You can set how much memory SAP HANA, express edition utilizes by modifying the `global_allocation_limit` parameter in the `global.ini` file.

The measure unit for the `global_allocation_limit` parameter is MB.

The default value is 0, which sets the maximum memory to the minimum of your machine limit and license limit.

If the machine size is less than 16 GB, the maximum memory is set to 16 GB.

If you set `global_allocation_limit` to a non-zero value, the server instance will use that value as maximum memory.

Here an example command to alter the `global_allocation_limit` value to 16 GB (to be executed as `hxeadm` user):

```bash
cd /usr/sap/HXE/HDB90/exe
./hdbsql \
	-i 90 \
	-d SystemDB \
	-u SYSTEM \
	-p "<master_password>" \
  "alter system alter configuration ('global.ini','SYSTEM') set ('memorymanager', 'global_allocation_limit') = '16384' with reconfigure;"
```

> **Note:** Do not set `global_allocation_limit` to a value above the limit of your license. This may cause the database to lock down.

&nbsp;

[ACCORDION-END]

[ACCORDION-BEGIN [Best Practice: ](Run the Memory Management Script)]

On a regular basis, it is recommended to use the memory management / Garbage collector script to free up available memory using the following commands:

```bash
sudo su -l hxeadm

cd /usr/sap/HXE/home/bin
./hxe_gc.sh
```

When prompted for System database user (SYSTEM) password, enter the master password you specified during SAP HANA, express edition installation.

The command prompt returns when the cleanup process is finished, and the memory usage before and after the process are displayed.

[ACCORDION-END]

[ACCORDION-BEGIN [Best Practice: ](Text Analysis As Embedded Preprocessor)]

If your tables do not use a full text index, or if your tables use a full text index but contain very little data, you can save about 120 MB of memory if you turn off the standalone text analysis preprocessor, and activate the embedded text analysis preprocessor.

Stop the standalone preprocessor:

```bash
cd /usr/sap/HXE/HDB90/exe
./hdbsql \
	-i 90 \
	-d SystemDB \
	-u SYSTEM \
	-p "<master_password>" \
  "alter system alter configuration ('daemon.ini','SYSTEM') set ('preprocessor','instances') = '0' with reconfigure;"
```

Start the embedded preprocessor:

```bash
cd /usr/sap/HXE/HDB90/exe
./hdbsql \
	-i 90 \
	-d SystemDB \
	-u SYSTEM \
	-p "<master_password>" \
  "alter system alter configuration ('preprocessor.ini','SYSTEM') set ('general','embedded') = 'true' with reconfigure;"
```

[ACCORDION-END]

[ACCORDION-BEGIN [Best Practice: ](Firewall Configuration)]

SAP HANA, express edition, just like SAP HANA, will use a series of port based on the current instance id (the default value is ***90*** starting version 2.0).

Here a description of the port used (Source <a href="https://help.sap.com/viewer/ports" target="new">https://help.sap.com/viewer/ports</a>):

Name                     | Default | Range                      | Rule              | Comments (Explanation of Table Headings)
-------------------------|---------|----------------------------|-------------------|--------------------
Host Agent               | 1128    | 1128                       | 1128              | SAP Host Agent with SOAP/HTTP
Host Agent               | 1129    | 1129                       | 1129              | SAP Host Agent with SOAP/HTTPS
HTTPS                    | 4300    | <nobr>4300 – 4399</nobr>   | 43&lt;NN&gt;      | SAP Web Dispatcher
HTTP                     | 8000    | <nobr>8000 – 8099</nobr>   | 80&lt;NN&gt;      | SAP Web Dispatcher
`indexserver`            | 30013   | <nobr>30013 – 39913</nobr> | 3&lt;NN&gt;13     | SQL/MDX access port for standard access to the system database of a multitenant system.
`indexserver`            | 30015   | <nobr>30015 – 39915</nobr> | 3&lt;NN&gt;15     | SQL/MDX access port for standard access to the tenant database of a multitenant system (automatically created).
`statisticsserver`       | 30017   | <nobr>30017 – 39917</nobr> | 3&lt;NN&gt;17     | Applicable when run as a separate service (default is embedded).
HTTP(S)                  | 30030   | <nobr>30030 – 39930</nobr> | 3&lt;NN&gt;30     | In an XSA runtime environment and port routing used, allow data access connection to the `xscontroller-managed` Web Dispatcher
HTTP(S)                  | 30032   | <nobr>30032 – 39932</nobr> | 3&lt;NN&gt;32     | In an XSA runtime environment and port routing used, allow data access connection to the `xscontroller-managed` Web Dispatcher
HTTP(S)                  | 30033   | <nobr>30033 – 39933</nobr> | 3&lt;NN&gt;33     | Single port for all SAP HANA XSA application and services when routing is done by host names instead of ports. For more information see SAP Note 2245631.
`indexserver`            | None    | <nobr>30041– 39998</nobr>  | 3&lt;NN&gt;41 – 3&lt;NN&gt;98 | SQL/MDX access port for standard access to the tenant databases of a multitenant system. Port assigned automatically from available port at creation time.
SOAP/HTTP                | 50013   | <nobr>50013 – 59913</nobr> | 5&lt;NN&gt;13     | Instance Agent
SOAP/HTTPS               | 50014   | <nobr>50014 – 59914</nobr> | 5&lt;NN&gt;14     | ‌Instance Agent
HTTP(S)                  | None    | <nobr>51000 – 51500</nobr> | 51000 – 51500     | In an XSA runtime environment and port routing used, port range used for the connection from the `xscontroller-managed` Web Dispatcher to the `xscontroller` for application instances access

> **Note**: &lt;NN&gt; represent the instance id of your SAP HANA, expression edition. The default value is <b>90</b>.

&nbsp;

Therefore, if your instance number is ***90***, here is an example for the firewall configuration.

You can execute the following commands:

```bash
sudo echo "<?xml version=\"1.0\" encoding=\"utf-8\"?>" > /etc/firewalld/services/hana.xml
sudo echo "<service>" >> /etc/firewalld/services/hana.xml
sudo echo "	<short>SAP HANA</short>" >> /etc/firewalld/services/hana.xml
sudo echo "	<description>Firewall rules for SAP HANA</description>" >> /etc/firewalld/services/hana.xml
sudo echo "	<port port=\"1128\" protocol=\"tcp\"/>" >> /etc/firewalld/services/hana.xml
sudo echo "	<port port=\"1129\" protocol=\"tcp\"/>" >> /etc/firewalld/services/hana.xml
sudo echo "	<port port=\"4390\" protocol=\"tcp\"/>" >> /etc/firewalld/services/hana.xml
sudo echo "	<port port=\"8090\" protocol=\"tcp\"/>" >> /etc/firewalld/services/hana.xml
sudo echo "	<port port=\"30000-39999\" protocol=\"tcp\"/>" >> /etc/firewalld/services/hana.xml
sudo echo "	<port port=\"50000-59999\" protocol=\"tcp\"/>" >> /etc/firewalld/services/hana.xml
sudo echo "</service>" >> /etc/firewalld/services/hana.xml

sudo chmod 0640 /etc/firewalld/services/hana.xml

sudo systemctl start firewalld
sudo systemctl enable firewalld

sudo firewall-cmd --zone=public --add-service=hana --permanent
sudo firewall-cmd --reload
```

You can check the service status using the following command where `hana` should be listed:

```bash
sudo firewall-cmd --list-services
```
[ACCORDION-END]

[ACCORDION-BEGIN [Appendix: ](Download Manager)]

---

### <b>Using the GUI Mode</b>

---

When using the GUI Mode, you will be able to download the following packages.

The table below provide more details around each package.

<style>
  th, td {
      border: 1px solid black;
      vertical-align:top;
  }
</style>

<table>
  <tr>
    <th style="width: 30%">Package</th>
    <th>Details</th>
  </tr>
  <tr>
    <td><nobr><b>Getting Started Guide</b></nobr></td>
    <td>
      User manual in PDF format
    </td>
  </tr>
  <tr>
    <td><nobr><b>Server only installer (Required)</b></nobr></td>
    <td>
      Downloads <code>hxe.tgz</code>; the SAP HANA 2.0, express edition server with Application Function Library.
      </br>
      <b>This file is necessary for installing SAP HANA 2.0, express edition.</b>
    </td>
  </tr>
  <tr>
    <td><b>Applications</b></td>
    <td>
      Downloads the optional package <code>hxexsa.tgz</code>; XSA and SAP HANA cockpit.
      </br>
      <b>The "Server only" package is required to install this package.</b>
    </td>
  </tr>
  <tr>
    <td><b>Text analysis files for additional languages</b></td>
    <td>
      For languages other than English and German, these files are required for the HANA Text Analysis function.
      <br>
      The text analysis files for English and German are already included in the Server only and Applications packages.
      <br>
      <b>The "Server only" package is required to install this package.</b>
    </td>
  </tr>
  <tr>
    <td><b>SAP Enterprise Architect Designer</td>
    <td>Downloads <code>eadesigner.tgz</code></td>
  </tr>
  <tr>
    <td><b>SAP HANA streaming analytics</b></td>
    <td>
      Downloads <code>hsa.tgz</code>, which contains SAP HANA streaming analytics.
      <br>
      <b>The "Applications" package is required to install this package.</b>
    </td>
  </tr>
  <tr>
    <td><b>SAP HANA streaming analytics studio plug-in</b></td>
    <td>
      Downloads <code>hsa_plugin.zip</code>, which contains an Eclipse plugin for creating and deploying streaming analytics projects.
    </td>
  </tr>
  <tr>
    <td><b>SAP HANA Interactive Education (SHINE)</b></td>
    <td>
      Downloads <code>shine.tgz</code>.
      <br>
      <b>The "Applications" package is required to install this package.<b>
    </td>
  </tr>
  <tr>
    <td><b>SAP HANA External Machine Learning AFL</b></td>
    <td>
      Downloads <code>eml.tgz</code>, which contains the SAP HANA External Machine Learning Library.
      <br>
      The SAP HANA External Machine Learning Library is an application function library (AFL) supporting the integration of Google TensorFlow, as an external machine learning framework, with SAP HANA, express edition.
      <br>
      <b>The "Server only" package is required to install this package.</b>
    </td>
  </tr>
  <tr>
    <td>
      <b>Clients</b>:
      <br>
      <ul>
        <li>Linux X86/64</li>
        <li>Linux PPC/Little Endian</li>
        <li>Windows</li>
        <li>Mac</li>
      </ul>
    </td>
    <td>
      Downloads: <br>
      <ul>
        <li><code>clients_linux_x86_64.tgz</code></li>
        <li><code>clients_linux_ppc64le.tgz</code></li>
        <li><code>clients_windows.zip</code></li>
        <li><code>clients_mac.tgz</code></li>
      </ul>
      Each clients package downloads an archive containing client-tools bundles for the listed platform. Use the client packages to access developed SAP HANA 2.0, express edition applications from a client PC.
      <br>
      The client machine does not require a SAP HANA installation to install and run the clients.
      <br>
      The package includes:
      <ul>
        <li>
          <b><code>hdb_client_xxxx.tgz</code></b> - Reduced HANA client for the selected operating system. Contains the HANA client package, drivers, and required licenses.</li>
        <li>
          <b><code>xs.onpremise.runtime.client_xxxxx.zip</code></b> Command-line tools for the selected operating system that enable access to (and control of) the SAP HANA XS advanced run-time environment.
        </li>
      </ul>
      <blockquote>
        <b>Tip</b>
        <br>
        After you develop an application using SAP HANA 2.0, express edition, install Download Manager to a client machine and download the Clients package to that client machine. You can then use the clients to connect to -- and test -- your HANA applications, emulating a customer.
      </blockquote>
    </td>
  </tr>
  <tr>
    <td><b>SAP HANA smart data integration</b></td>
    <td>
      Downloads <code>sdi.tgz</code>.
      <br>
      SAP HANA smart data integration provides functionality to access source data, and to provision, replicate, and transform that data in SAP HANA on premise, or in the cloud.
      <br>
      <b>The "Server only" package is required to install this package.</b>
    </td>
  </tr>
  <tr>
    <td><b>SAP HANA smart data integration - Data Provisioning Agent</b></td>
    <td>
      Downloads <code>dpagent_linux_x86_64.tgz</code>.
      <br>
      The Data Provisioning Agent provides secure connectivity between the SAP HANA database and your adapter-based sources.
      <br>
      Download this and "Server only" package to install this additional component.
      <br>
      <b>This package is only available for Linux X86/64 environments</b>
    </td>
  </tr>
  <tr>
    <td><b>SAP Web IDE for SAP HANA</b></td>
    <td>
      Downloads <code>webide.tgz</code>.
      <br>
      SAP Web IDE for SAP HANA is a browser-based integrated development environment for the development of SAP HANA-based applications comprised of extensive SAP HANA data models, business logic, and web-based UIs.
      <b>The "Applications" package is required to install this package.</b>
    </td>
  </tr>
</table>

---

### <b>Using the Console Mode</b>

---

Here is the complete list of command arguments with the Console Mode:

| Argument                             | Description  |
| ------------------------------------ | -------------|
| `-h`                                 | Print this help.|
| `-x`                                 | Print extended help.|
| <nobr>`-d <save_directory>`</nobr>   | Directory where to save the download file. Default is `%USERPROFILE%\Downloads` on Windows; `~/Downloads` on Linux.|
| <nobr>`--ph <proxy_host>`</nobr>     | Proxy host name or IP address.|
| <nobr>`--pp <proxy_port>`</nobr>     | Proxy host name or IP address.|
| <nobr>`<platform>`</nobr>            | HANA platform. Valid values are `linuxx86_64`, `linuxppc64le`.|
| <nobr>`<image>`</nobr>               | Type of image to download. Valid values for `linuxx86_64` platform are: `vm`, `installer`. Valid values for `linuxppc64le` platform are: `installer`.|
| `<file>`                             | File(s) to download.|

And the complete list of <file> values for the `linuxx86_64` platform:

| Values                                                     |    Description    |
| ---------------------------------------------------------- |-------------------|
| `Getting_Started_HANAexpress_Binary_Installer.pdf`         | User manual in PDF format|
| <nobr>`hxe.tgz`</nobr>                                     | Downloads `hxe.tgz`; the server only binary installer|
| <nobr>`hxexsa.tgz`</nobr>                                  | Downloads `hxexsa.tgz`; the application binary installer, which requires the `hxe.tgz` binary installer to run|
| <nobr>`additional_lang.tgz`</nobr>                         | Downloads `additional_lang.tgz`. For languages other than English and German, this package is required for the HANA Text Analysis function. (The text analysis files for English and German are already included in the `hxe.tgz` and `hxexsa.tgz` packages.)|
| <nobr>`eadesigner.tgz`</nobr>                              | Valid only with `hxexsa.tgz`. SAP EA Designer lets you capture, analyze, and present your organization's landscapes, strategies, requirements, processes, data, and other artifacts in a shared environment|
| <nobr>`hsa.tgz`</nobr>                                     | Downloads SAP HANA streaming analytics.|
| <nobr>`hsa_plugin.zip`</nobr>                              | Downloads the Eclipse plugin for creating and deploying streaming analytics projects.|
| <nobr>`shine.tgz`</nobr>                                   | Valid only with `hxexsa.tgz`. SAP HANA Interactive Education (SHINE) makes it easy to learn how to build applications on SAP HANA Extended Application Services Advanced Model (XSA).|
| <nobr>`eml.tgz`</nobr>                                     | Downloads HANA Extended Machine Learning AFL.|
| <nobr>`clients_linux_x86_64.tgz`</nobr>                    | Client download package for Linux machines (x86/64 architectures). Use the client packages to access developed SAP HANA, express edition applications from a client PC.|
| <nobr>`clients_linux_ppc64le.tgz`</nobr>                   | Client download package for Linux machines (little endian on Power architectures). Use the client packages to access developed SAP HANA, express edition applications from a client PC.|
| <nobr>`clients_windows.zip`</nobr>                         | Client download package for Windows machines. Use the client packages to access developed SAP HANA, express edition applications from a client PC.|
| <nobr>`clients_mac.tgz`</nobr>                             | Client download package for Mac. Use the client packages to access developed SAP HANA, express edition applications from a client PC.|
| <nobr>`sdi.tgz`</nobr>                                     | SAP HANA smart data integration download package. SAP HANA smart data integration provides functionality to access source data, and to provision, replicate, and transform that data in SAP HANA on premise, or in the cloud.|
| <nobr>`dpagent_linux_x86_64.tgz`</nobr>                    | SAP HANA smart data integration - Data Provisioning Agent (Linux X86/64) download package. The Data Provisioning Agent provides secure connectivity between the SAP HANA database and your adapter-based sources.|
| <nobr>`webide.tgz`</nobr>                                  | SAP Web IDE for SAP HANA is a browser-based integrated development environment for the development of SAP HANA based applications comprised of extensive SAP HANA data models, business logic, and web-based UI's.|

[ACCORDION-END]
