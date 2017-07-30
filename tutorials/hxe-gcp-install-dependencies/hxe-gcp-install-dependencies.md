---
title: HANA Express Edition - Install Dependencies for Ubuntu on Google Cloud Platform
description: Install dependencies in your Ubuntu OS image on Google Cloud Platform before installing HANA Express Edition
primary_tag: products>sap-hana\,-express-edition  
tags: [  tutorial>beginner, topic>cloud, products>sap-hana\,-express-edition   ]

---

## Prerequisites  
 - [HANA Express Edition - Create and Access Instance on Google Cloud Platform - Server + XS Advanced](http://www.sap.com/developer/tutorials/hxe-gcp-create-instance-access.html)

## Next Steps
 - [HANA Express Edition - Extract installation Files and Configure Firewall Rules](http://www.sap.com/developer/tutorials/hxe-gcp-extract-files-configure-firewall.html)

## Disclaimer
SAP HANA, express edition (HXE) is officially supported on SLES and RHEL. SAP Community members have been successful in running HXE on other Linux operating systems that are not formally supported by SAP, such as Ubuntu, openSUSE and Fedora. SAP is not committing to resolving any issues that may arise from running HXE on these platforms.

## Details
### You will learn  
In this tutorial, you will install the dependencies needed for the setup and execution of HANA Express Edition in your Ubuntu OS image on the Google Cloud Platform.

### Time to Complete
**15 Min**

---

[ACCORDION-BEGIN [Step 1: ](Install or check openssl)]

Enter the following command to make sure openSSL is up to date or continue installation:
```
apt-get install openssl
```

You will probably get a message indicating it is already installed:
![SSL installation](6.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 2: ](Install cracklib)]

Enter the following command to install `cracklib`:
```
apt-get install libpam-cracklib
```
At any time during the installation of dependencies, you may be prompted about using more disk space. Make sure to answer `Y`:

![Confirm disk usage](7.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Install libtool)]

Enter the following command to install `libtool`:
Example:

```
apt-get install libltdl7
```

![Install libtool](8.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Install the AIO library)]

Enter the following command to install AIO:

```
apt-get install libaio1
```
Example:

![Install libaio](9.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](For XSA installation, install unzip)]

If you are planning on installing the server and XS Advanced applications, you will also need to install unzip:
```
apt-get install unzip
```
Example:

![Install libtool](10.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](For HANA 2.0 SPS01: Create and alias for chkconfig and install curl)]

This is a command that is not available in Ubuntu. You will download its equivalent and create an alias so it can be called by the installation script.

```
apt-get install sysv-rc-conf
```
Example:

![Install chkconfig](16.png)

And set the alias for the installer:

```
alias chkconfig='sysv-rc-conf'
```

![Alias](17.png)

Also for SAP HANA 2.0 SPS01, install curl:

```
apt-get install curl
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Update the hostname)]

Google Cloud Platform will overwrite your hosts file every time you reboot the virtual machine. HANA Express Edition relies on the hostname to execute most of its services, so it is essential that you use something that can be accessed from outside the server.

You can do this manually by changing the configuration in the file /etc/hosts every time you reboot the machine. Fortunately, there is a script to place at startup that can do this automatically.

>Note that the hostname has to match the name of the VM instance.

Go to your home directory and create a file:

```
nano ./set_hostname.sh
```
Paste the following into the editor:
```
#! /bin/bash
hostname  <<your hostname>>
```

Example:
![XSA hostname](11.png)

Or for the server-only:
![Server only hostname](12.png)

Press `Ctrl-O` and `Ctrl-X` to save and exit.

Grant permissions for all users on the script with command `chmod`:

```
chmod 777 ./set_hostname.sh
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Configure the script to set the hostname at startup in Google Cloud Platform)]

Go to the VM instances, click on your instance:
![Click on the Instance](13.png)

And then click on Edit:

![Click on Edit](14.png)

Scroll down to the custom metadata section and add:
- `startup-script` in **Key**
- `/home/your_home_directory/set_hostname.sh` in **Value**

For example:

![Configure startup script in Google Cloud platform to setup the hostname](15.png)

**Reboot** your Virtual Machine and check that your hostname has been set automatically with command `hostname`.

[DONE]
[ACCORDION-END]


## Next Steps
- [Extract Files and Configure Firewall](http://www.sap.com/developer/tutorials/hxe-gcp-extract-files-configure-firewall.html)
