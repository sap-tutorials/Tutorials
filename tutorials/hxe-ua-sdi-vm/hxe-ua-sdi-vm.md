---
parser: v2
author_name: John Currie
author_profile: https://github.com/JCurrie27
primary_tag: products>sap-hana\,-express-edition
tags: [ tutorial>beginner, products>sap-hana\,-express-edition ]
time: 30
---

# Install the Optional SAP HANA Smart Data Integration Package for SAP HANA, express edition (Preconfigured VM)
<!-- description --> Install SAP HANA smart data integration on an SAP HANA, express edition system.

<!-- loio7621f586085b4a93898290e1571e560a -->

## Prerequisites
 - **Tutorials:**  You have completed [Start SAP HANA, express edition Server](http://developers.sap.com/tutorials/hxe-ua-getting-started-vm.html)  

## You will learn
How to install and run the optional SAP HANA Smart Data Integration Package for SAP HANA, express edition.

---

## Intro
This installs the Data Provisioning Server on SAP HANA, express edition, and deploys the data provisioning delivery unit that enables monitoring and other capabilities.

### Run the memory management script


Run the `hxe_gc` memory management script to free up available VM memory

1.   In your VM, log in as `hxeadm` and enter:

    ```bash
    cd /usr/sap/HXE/home/bin
    ```

2.   Execute:

    ```bash
    hxe_gc.sh
    ```

3.   When prompted for System database user (SYSTEM) password, enter the New HANA database master password you specified during SAP HANA, express edition installation

    The cleanup process runs. The command prompt returns when the cleanup process is finished.


### Download sdi.tgz


In your VM, download `sdi.tgz` using the built-in Download Manager. From the same directory where you ran `hxe_gc` (`/usr/sap/HXE/home/bin`) enter:

```bash
HXEDownloadManager_linux.bin linuxx86_64 vm sdi.tgz
```


### Extract sdi.tgz


In your VM, extract `sdi.tgz`:

```bash
tar -xvzf sdi.tgz
```


### Run the installation script


As the `hxeadm` user, run:

```bash
HANA_EXPRESS_20/install_sdi.sh
```


### Next steps


To use adapters other than the OData adapter, you will also need to install the Data Provisioning Agent.

