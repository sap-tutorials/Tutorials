---
parser: v2
author_name: John Currie
author_profile: https://github.com/JCurrie27
primary_tag: products>sap-hana\,-express-edition
tags: [ tutorial>beginner, products>sap-hana\,-express-edition ]
time: 15
---

# Installing the Optional Data Warehousing Foundation Package for SAP HANA, express edition (Native Linux Machine)
<!-- description --> Download the `Data Warehousing Foundation` package in the Download Manager.

<!-- loiocfbb826828fd4342a9ec06f90a5dd11a -->

## Prerequisites
 - **Tutorials:**  You have completed [Test the Installation](hxe-ua-test-binary)

## You will learn
You will learn how to download and install the `dwf.tgz` Data Warehousing Foundation package.

---

### Download dwf.tgz


Download `dwf.tgz` using the built-in Download Manager. Enter the following command:

-   `x86_64:`

```bash

./HXEDownloadManager_linux.bin linuxx86_64 installer dwf.tgz
```

-   `PowerPC:`

    ```bash
    HXEDownloadManager_linux.bin linuxx86_64 installer dwf.tgz
    ```



### Navigate to the Downloads directory


Enter:

```bash
cd /usr/sap/HXE/home/Downloads
```


### View the contents of the Downloads folder to confirm dwf.tgz exists


Enter:

```bash
ls
```


### Extract the file


Enter:

```bash
tar -xvzf dwf.tgz
```


### Navigate to the HANA_EXPRESS_20 directory


Enter:

```bash
cd HANA_EXPRESS_20
```


### Run the installation script


As the <sid>`adm` user, run:

```bash
HANA_EXPRESS_20/install_dwf.sh
```

