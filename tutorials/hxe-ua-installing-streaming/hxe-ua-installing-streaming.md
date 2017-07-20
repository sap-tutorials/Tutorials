---
title: Installing SAP HANA Smart Data Streaming for SAP HANA, Express Edition
description: Install the SAP HANA client package and SAP HANA smart data streaming on an SAP HANA, express edition system.
primary_tag: products>sap-hana\,-express-edition
tags: [  tutorial>beginner, products>sap-hana-smart-data-streaming, products>sap-hana\,-express-edition   ]
---

## Prerequisites  
- **Proficiency:** Beginner
- **Tutorials:** [Installing SAP HANA 2.0, express edition (Virtual Machine Method)](http://www.sap.com/developer/tutorials/hxe-ua-installing-vm-image.html) and [Start Using SAP HANA 2.0, express edition (Virtual Machine Method)](http://www.sap.com/developer/tutorials/hxe-ua-getting-started-vm.html) or [Installing SAP HANA 2.0, express edition (Binary Installer Method)](http://www.sap.com/developer/tutorials/hxe-ua-installing-binary.html) and [Start Using SAP HANA 2.0, express edition (Binary Installer Method)](http://www.sap.com/developer/tutorials/hxe-ua-getting-started-binary.html)


## Next Steps
- [Installing and Configuring the Streaming Studio Plugin](http://www.sap.com/developer/tutorials/hxe-ua-streaming-plugin.html)

## Details
Install the SAP HANA client package and SAP HANA smart data streaming on an SAP HANA, express edition system.

### Time to Complete
**15 Min**

---

[ACCORDION-BEGIN [Step 1: ](Download the smart data streaming installation package)]

Use the Download Manager to download the smart data streaming installation package, `sds.tgz`.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Copy sds.tgz to the virtual machine)]

If you are installing smart data streaming on an SAP HANA, express edition virtual machine, copy `sds.tgz` to the virtual machine.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Extract sds.tgz)]

Extract the contents of `sds.tgz`:

```
tar -xvzf <download_path>/sds.tgz
```

>**Tip:**
> You may have to give these files run permissions. Example:

> ```bash
> chmod -R 777 <download_path>/sds.tgz
> ```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Edit the /etc/hosts file)]

If you are installing smart data streaming on an SAP HANA, express edition virtual machine, edit the `/etc/hosts` file on the VM:

1. Enter the `/etc/hosts` file editor as `sudo` using the `vi` editor.

    ```
    sudo vi /etc/hosts
    ```

2. Press `i` to edit the file and modify the `hxehost.localdomain.com   hxehost` line to have your VM's IP address:

    ```
    <VM_IP_address>  hxehost.localdomain.com   hxehost
    ```    

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Run the installer)]

Navigate to the `HANA_EXPRESS_20` directory where you extracted the files and run `install_sds.sh` as the root user:

```
cd <extracted_path>/HANA_EXPRESS_20
sudo ./install_sds.sh
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Configure your installation)]

Follow the prompts to configure your installation.

>**Note:**
> The system database user (SYSTEM) password you enter during installation is used for the `SYS_STREAMING` and `SYS_STREAMING_ADMIN` users.

[DONE]
[ACCORDION-END]


---

## Next Steps
- [Install and Configure the Streaming Studio Plugin](http://www.sap.com/developer/tutorials/hxe-ua-streaming-plugin.html)
