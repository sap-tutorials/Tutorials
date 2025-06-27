---
parser: v2
primary_tag: products>sap-hana\, express-edition
tags: [  tutorial>beginner, tutorial>how-to, products>sap-hana\,-express-edition  ]
author_name: John Currie
author_profile: https://github.com/JCurrie27
---
# SAP HANA 2.0, express edition Troubleshooting
<!-- description --> Troubleshoot common installation issues.

## Prerequisites  
- **Setup:** You are following the instructions to install SAP HANA 2.0, express edition in the [Installing SAP HANA 2.0, express edition (Binary Installer Method)](http://developers.sap.com/tutorials/hxe-ua-installing-binary.html) tutorial.


## How-To Details
Perform these steps to resolve issues when installing SAP HANA 2.0, express edition.
## Time to Complete
**20 Min**.

---

### HDB Daemon not Running

**Issue:** You are installing SAP HANA, express edition on a Linux server using `hdblcm`. You receive this error:  

**Cannot start system.  
Start instance 00 on host '`hxehost.localdomain.com`' failed.  
FAIL: process `hdbdaemon` HDB Daemon not running.**

**Solution:** Use `zypper` to check the `util-linux`, `util-linux-systemd` and `uuidd` packages to make sure they are at these versions:  

`zypper info util-linux util-linux-systemd uuidd`

The results need to show that you have at least the following versions installed:
`util-linux`: `util-linux-2.25-22.1`  
`uuidd` : `uuidd-2.25-22`  
`util-linux-systemd`: 2.25-22.1  

If you are missing any of the packages, or if the versions are outdated, install them using the `zypper` install command.

Check that socket activation is enabled and started. In a shell enter:

`systemctl` status `uuidd.socket`

If the status is inactive, start socket activation:

`systemctl` start `uuidd.socket`

### SAP HANA XS Applications Run Error

**Issue:** You are trying to run a SAP HANA service on your SAP HANA 2.0, express edition installation and are receiving an error.

**Solution:** Log in to your SAP HANA 2.0, express edition installation as `hxeadm`.

```
sudo su -l <sid>adm
```

Check which services are enabled on your machine:

```
xs apps
```

This operation may take 1-2 minutes to return the list of apps. You should see the following:

![XS Apps](hxe_xsa_webide.PNG)

If the service you're trying to use is shown as `STOPPED`, start it:

```
xs start <app>
```

It may take a few minutes for the system to get started. Run `xs apps` again to see if the app has started and that under `instances` the app shows `1/1`.

### Download Manager Shows Error

**Issue:** You have received the following error: `Failed to concatenate downloaded files`

You are downloading packages using the Download Manager. The Status area and Progress Detail area show the error `Failed to concatenate downloaded files`.

**Solution:** Check the log file for details. The log file is in the Temp directory:

Linux: **`/tmp/hxedm[yymmdd].log`**

Windows: **`%TEMP%\hxedm_[yymmdd].log`**

If the log indicates a simple issue such as lack of disk space or file permissions, fix the problem and download again.

If the problem is less obvious, do the following:

Go to the **Save directory**. Delete all downloaded files, including incomplete download files. Download again.

or

Change the **Save directory**. Download again.     

### Locate Download Manager Log File

**Issue:** You are downloading packages using the Download Manager when you terminate Download Manager before download completes, or Download Manager quits unexpectedly.

**Solution:** Check the log file for details. The log file is in the Temp directory:

Linux: **`/tmp/hxedm[yymmdd].log`**

Windows: **`%TEMP%\hxedm_[yymmdd].log`**

### Memory Limit Exceeded - No Additional Local Capacity

**Issue:** You have exceeded the 32GB memory limit of SAP HANA, express edition.

**Solution:** Consider migrating to SAP HANA Cloud.  Visit the [SAP HANA Cloud](https://www.sap.com/canada/products/technology-platform/hana.html) homepage to view more details about SAP HANA Cloud.

### Local Memory Usage Exceeded

**Issue:** You have reached the memory limit on your local SAP HANA, express edition, but have not hit the 32GB memory limit

**Solution:** Run garbage cleanup or stop unnecessary or extraneous SAP HANA, express edition processes.

To run garbage collection, run:

```bash
cd /usr/sap/HXE/home/bin
./hxe_gc.sh
```

Log into XSA

```bash
xs login
```

Follow the prompts to log in.

See which XS apps are running:

```bash
xs apps
```

To stop any XS apps you do not wish to have running, run:

```bash
xs stop <app>
```

This will stop the XS app and free any resources it may have been using.

### Local Disk Space Full

**Issue:** Your local machine has run out of disk space.

**Solution:** You have a few options. You can physically install more disk space, or free up additional disk space by uninstalling and deleting unnecessary files and programs.

You can also move your SAP HANA, express edition installation to a cloud provider. Visit the [SAP HANA, express edition](https://developers.sap.com/topics/hana.html) homepage to view cloud offerings.

