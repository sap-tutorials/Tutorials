---
title: Installing SAP HANA Streaming Analytics for SAP HANA, Express Edition
description: Install the SAP HANA client package and SAP HANA streaming analytics on an SAP HANA, express edition system.
primary_tag: products>sap-hana\,-express-edition
tags: [  tutorial>beginner, products>sap-hana-smart-data-streaming, products>sap-hana\,-express-edition   ]
---

## Prerequisites  
- **Proficiency:** Beginner
- **Tutorials:** [Installing SAP HANA 2.0, express edition (Virtual Machine Method)](http://www.sap.com/developer/tutorials/hxe-ua-installing-vm-image.html) and [Start Using SAP HANA 2.0, express edition (Virtual Machine Method)](http://www.sap.com/developer/tutorials/hxe-ua-getting-started-vm.html) or [Installing SAP HANA 2.0, express edition (Binary Installer Method)](http://www.sap.com/developer/tutorials/hxe-ua-installing-binary.html) and [Start Using SAP HANA 2.0, express edition (Binary Installer Method)](http://www.sap.com/developer/tutorials/hxe-ua-getting-started-binary.html)
- **Additional Information:** For more information about sizing requirements for streaming analytics projects, see the [Sizing and Configuration Guidelines document](https://www.sap.com/documents/2017/01/783a6b39-a47c-0010-82c7-eda71af511fa.html).


## Next Steps
- [Installing and Configuring the Streaming Studio Plugin](http://www.sap.com/developer/tutorials/hxe-ua-streaming-plugin.html)

## Details
Install the SAP HANA client package and SAP HANA streaming analytics on an SAP HANA, express edition system.

### Time to Complete
**15 Min**

---

[ACCORDION-BEGIN [Step 1: ](Download the streaming analytics installation package)]

Use the SAP HANA, express edition Download Manager to download the streaming analytics installation package, `hsa.tgz`.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Copy hsa.tgz to the virtual machine)]

If you are installing streaming analytics on an SAP HANA, express edition virtual machine, locate the download package.

| If you downloaded using...        | Then do this...  |
| ---------------- | -------------|
| The VM's built-in Download Manager (Console Mode)           | Locate ` hsa.tgz ` in the VM's Save Directory (`~/Downloads` by default.) |
| The Download Manager (GUI Mode) on your laptop            | Transfer ` hsa.tgz ` from your laptop's Save Directory to `~/Downloads` on your VM.|


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Extract hsa.tgz)]

Extract the contents of `hsa.tgz`:

```
tar -xvzf <download_path>/hsa.tgz
```

>**Tip:**
> You may have to give these files run permissions. Example:

> ```bash
> chmod -R 777 <download_path>/hsa.tgz
> ```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Edit the /etc/hosts file)]

If you are installing streaming analytics on an SAP HANA, express edition virtual machine, edit the `/etc/hosts` file on the VM and modify the `hxehost.localdomain.com   hxehost` line to have your VM's IP address:

```
<VM_IP_address>  hxehost.localdomain.com   hxehost
```    

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Run the installer)]

Navigate to the `HANA_EXPRESS_20` directory where you extracted the files and run `install_hsa.sh` as the root user:

```
cd <extracted_path>/HANA_EXPRESS_20
sudo ./install_hsa.sh
```

Follow the prompts to configure your installation.

>**Note:**
> The system database user (SYSTEM) password you enter during installation is used for the `SYS_STREAMING` and `SYS_STREAMING_ADMIN` users.

[DONE]
[ACCORDION-END]


---

## Next Steps
- [Install and Configure the Streaming Studio Plugin](http://www.sap.com/developer/tutorials/hxe-ua-streaming-plugin.html)
