---
title: SAP HANA 2.0, express edition Troubleshooting
description: Troubleshoot common installation issues.
primary_tag: products>sap-hana\, express-edition
tags: [  tutorial>beginner, tutorial>how-to, products>sap-hana\,-express-edition  ]
---
## Prerequisites  
- **Setup:** You are following the instructions to install SAP HANA 2.0, express edition in the [Installing SAP HANA 2.0, express edition (Binary Installer Method)](http://developers.sap.com/tutorials/hxe-ua-installing-binary.html) tutorial or [Installing SAP HANA 2.0, express edition (Virtual Machine Method)](http://developers.sap.com/tutorials/hxe-ua-installing-vm-image.html) tutorial.


## How-To Details
Perform these steps to resolve issues when installing SAP HANA 2.0, express edition.

### Time to Complete
**20 Min**.

---

[ACCORDION-BEGIN [Issue &#151; ](HDB Daemon not Running)]

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

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Issue &#151; ](Virtual Machine: Checking Resource Usage)]

**Issue:** You are having memory issues on your VM and want to check resource usage.

**Solution:** If you have HANA studio, right-click on the system and select **Configuration and Monitoring > Open Administration** and check the Overview and Landscape tabs for anything in red.

If you don't have HANA Studio, run the following queries in `hdbsql` to view SAP HANA resource usage:

`select service_name, round(effective_allocation_limit/1024/1024/1024, 1) as MemLimit, round(total_memory_used_size/1024/1024/1024,1) as MemUsed from m_service_memory;`

If the `MemUsed` is close to the `MemLimit`, you may encounter problems allocating memory.

Alternatively, you can run the Linux `free` command at the command line to see free resources:

`free -g`

The key number is in the second row (-/+ buffers/cache) in the **free** column. If this number is low, (e.g. 0 GB) you may have run out of memory when performing your recent operation.

You can also run the following command to see if you are running out of disk space on the VM's `filesystem`:  

`df -h`

Look for the **Use%** for the `/dev/sda1 filesystem`. If it is down to just a few GB, you may have run out of disk space when performing your recent operation.


[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Issue &#151;](SAP HANA XS Applications Run Error)]

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

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Issue &#151;](Download Manager Shows Error)]

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

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Issue &#151;](Locate Download Manager Log File)]

**Issue:** You are downloading packages using the Download Manager when you terminate Download Manager before download completes, or Download Manager quits unexpectedly.

**Solution:** Check the log file for details. The log file is in the Temp directory:

Linux: **`/tmp/hxedm[yymmdd].log`**

Windows: **`%TEMP%\hxedm_[yymmdd].log`**

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Issue &#151;](Unable to Obtain an IPv4 Address in VMware)]

**Issue:** You are unable to obtain an `IPv4` `hxehost` IP address. You are using a `VMware` `hypervisor`.

`VMware` defaults to `bridged` networking. You may need to adjust `VMware's` network adapter settings in certain circumstances.

If you are behind a proxy or a firewall, your institution's network may prevent `VMware` from assigning an `IPv4` address when you attempt to locate your `hxehost` `IP` address.

**Solution:**

1. In `VMware`, change your network adapter settings from `Bridged` to `NAT`.

2. Wait a few minutes.

3. At the command prompt, enter `sudo ifconfig` to see if an `IPv4` address is now assigned. You do not need to restart your VM.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Issue &#151;](VMware Fusion on Mac OS X: hxexsa.ova Installation Fails)]

**Issue:** You use `VMware` Fusion on `Mac OS X`. You import and start `hxexsa.ova`. You receive an error message.

This error displays: "`XSA` cockpit apps failed to start at this point of time. Please retry by running `hxe_cockpit.sh` script"

**Solution:**

- Shut down the instance.

- Re-import `hxexsa.ova`.

- Start SAP HANA 2.0, express edition installation again, and this time choose to run `XSA` configuration in the background.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Issue &#151;](Upgrade Script Hangs While Upgrading VM Installation)]

**Issue:** When you run `hxe_upgrade.sh`, you notice the upgrade hangs.

**Solution:** The VM is low on memory. Run the `hxe_gc` memory management script.

1. Open a new terminal to your VM.

2. Run the memory management script.

    The `hxe_gc` memory management script frees up available VM memory.

    - In your VM, log in as `hxeadm` and enter:

    ```
    cd /usr/sap/HXE/home/bin
    ```

    - Execute:

    ```
    hxe_gc.sh
    ```

    - When prompted for System database user (SYSTEM) password, enter the New HANA database master password you specified during SAP HANA, express edition installation.

    The cleanup process runs. The command prompt returns when the cleanup process is finished.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Issue &#151;](Memory Limit Exceeded - Additional Local Capacity)]

**Issue:** You have exceeded the 32GB memory limit of SAP HANA, express edition and you wish to use more memory on your local machine.

**Solution:** SAP HANA, express edition is limited to 32GB of memory. If you've exceeded this limit and wish to use additional memory for your local installation, purchase a license. Go to the [SAP HANA, express edition](https://www.sapstore.com/solutions/99055/SAP-HANA%2C-express-edition) license page to purchase a license.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Issue &#151;](Memory Limit Exceeded - No Additional Local Capacity)]

**Issue:** You have exceeded the 32GB memory limit of SAP HANA, express edition and you wish to use more memory but you do not have any more available on your local machine.

**Solution:** SAP HANA, express edition is limited to 32GB of memory. If you've exceeded this limit and wish to use additional memory but do not have any additional memory for your local installation, consider migrating to a cloud installation. Visit the [SAP HANA, express edition](https://developers.sap.com/topics/hana.html) homepage to view cloud offerings.

Additionally, you can add more memory to your local installation and then purchase a license. Go to the [SAP HANA, express edition](https://www.sapstore.com/solutions/99055/SAP-HANA%2C-express-edition) license page to purchase a license.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Issue &#151;](Local Memory Usage Exceeded)]

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

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Issue &#151;](Local Disk Space Full)]

**Issue:** Your local machine has run out of disk space.

**Solution:** You have a few options. You can physically install more disk space, make more disk available to your VM (for VM installations), or free up additional disk space by uninstalling and deleting unnecessary files and programs.

You can also move your SAP HANA, express edition installation to a cloud provider. Visit the [SAP HANA, express edition](https://developers.sap.com/topics/hana.html) homepage to view cloud offerings.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Issue &#151;](Local Disk Space Threshold Limited)]

**Issue:** Your local machine has limited how much disk space your Virtual Machine can use for SAP HANA, express edition.

**Solution:** If your virtual machine is limiting how much disk space SAP HANA, express edition is allowed to use, and you have exceeded that amount, visit the virtual machine's documentation to increase the virtual machine's disk space threshold.

You can also move your SAP HANA, express edition installation to a cloud provider. Visit the [SAP HANA, express edition](https://developers.sap.com/topics/hana.html) homepage to view cloud offerings.


[DONE]

[ACCORDION-END]
