---
parser: v2
primary_tag: products>sap-hana\,-express-edition
tags: [  tutorial>beginner, products>sap-hana-streaming-analytics, products>sap-hana\,-express-edition   ]
---

# Installing SAP HANA Streaming Analytics for SAP HANA, Express Edition
<!-- description --> Install the SAP HANA client package and SAP HANA streaming analytics on an SAP HANA, express edition system.

## Prerequisites  
- **Proficiency:** Beginner
- **Tutorials:** [Installing SAP HANA 2.0, express edition (Virtual Machine Method)](https://developers.sap.com/tutorials/hxe-ua-installing-vm-image.html) and [Start Using SAP HANA 2.0, express edition (Virtual Machine Method)](https://developers.sap.com/tutorials/hxe-ua-getting-started-vm.html) or [Installing SAP HANA 2.0, express edition (Binary Installer Method)](https://developers.sap.com/tutorials/hxe-ua-installing-binary.html) and [Start Using SAP HANA 2.0, express edition (Binary Installer Method)](https://developers.sap.com/tutorials/hxe-ua-getting-started-binary.html)


## Next Steps
- [Installing and Configuring the Streaming Studio Plugin](https://developers.sap.com/tutorials/hxe-ua-streaming-plugin.html)


## Intro
Install the SAP HANA client package and SAP HANA streaming analytics on an SAP HANA, express edition system.
## Time to Complete
**15 Min**

---

### Download `hsa.tgz` using the built-in Download Manager.


Navigate to `/usr/sap/HXE/home/bin`:

```bash
/usr/sap/HXE/home/bin
```

Enter the following command:

```bash
HXEDownloadManager_linux.bin linuxx86_64 installer hsa.tgz
```


### Navigate to the `Downloads` directory.


Enter:

```bash
cd /usr/sap/HXE/home/Downloads
```


### View the contents of the `Downloads` directory to confirm `hsa.tgz` exists.


Enter:

```bash
ls
```


### Extract the file.


Enter:

```bash
tar -xvzf hsa.tgz
```


### Navigate to the `HANA_EXPRESS_20` directory.


Enter:

```bash
cd HANA_EXPRESS_20
```


### Edit the /etc/hosts file.


If you are installing streaming analytics on an SAP HANA, express edition virtual machine, edit the `/etc/hosts` file on the VM and modify the `hxehost.localdomain.com   hxehost` line to have your VM's IP address:

```
<VM_IP_address>  hxehost.localdomain.com   hxehost
```    


### Run the installation script.


Navigate to the `HANA_EXPRESS_20` directory where you extracted the files and run `install_hsa.sh` as the root user:

```
cd <extracted_path>/HANA_EXPRESS_20
sudo ./install_hsa.sh
```

Follow the prompts to configure your installation.

>**Note:**
> The system database user (SYSTEM) password you enter during installation is used for the `SYS_STREAMING` and `SYS_STREAMING_ADMIN` users.


