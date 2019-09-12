---
title: Download the Installer Files
description: Use the Download Manager in console mode to download the SAP HANA 2.0, express edition server only installer, XSA applications installer, and additional installation packages.
author_name: Aaron Patkau
author_profile: https://github.com/aptk001
primary_tag: products>sap-hana\,-express-edition
tags: [ tutorial>beginner, products>sap-hana\,-express-edition ]
time: 2
---

<!-- loio605bef28cbed45a58c53d00c2c1375a1 -->

## Prerequisites
Close the Download Manager if it is running in GUI mode.


## Details
### You will learn
You will learn how to download the binary image of SAP HANA 2.0, express edition, plus the Applications package, and optional additional components.

---

> Note:
> The Download Manager for Windows (`HXEDownloadManager_win.exe`) runs in asynchronous mode, and console mode is not available. If you are a Windows user, download the platform-independent Download Manager (`HXEDownloadManager.jar`) to use console mode.
> 
> 

[ACCORDION-BEGIN [Step 1: ](Open a command prompt)]

Open a command prompt at the location where you saved the Download Manager file (`HXEDownloadManager.jar` or `HXEDownloadManager_linux.bin`).

> Note:
> If you've already installed SAP HANA express edition, you can run the built-in download manager from any directory as the <sid>`adm` user.
> 
> 

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Display the command help using the `-h` argument)]

Linux `x86_64` Download Manager example:

```bash
HXEDownloadManager_linux.bin -h
```

Platform-independent Download Manager example:

```bash
java -jar HXEDownloadManager.jar -h
```

> Note:
> You must include an argument with each command. If you call the Download Manager without an argument, it opens in GUI mode.
> 
> 

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Familiarize yourself with the command syntax, and the command arguments)]

Command syntax is:

```bash
HXEDownloadManager [( [-h|-X] | [-d <save_directory>] [--ph <proxy_host>] [--pp <proxy_port>] <platform> <image> <file>... )]
```

Command arguments are:

|Argument|Description|
|--------|-----------|
|`-h`|Print this help|
|`-x`|Print extended help|
|`-d <save_directory>`|Directory where to save the download file. Default is `%USERPROFILE%\Downloads` on Windows; and `~/Downloads` on Linux.|
|`--ph <proxy_host>`|Proxy host name or IP address.|
|`--pp <proxy_port>`|Proxy port.|
|`<platform>`|HANA platform. Valid values are `linuxx86_64`, `linuxppc64le`.|
|`<image>`|Type of image to download. Valid values for `linuxx86_64` platform are: `vm`, `installer`. Valid values for `linuxppc64le` platform are: `installer`.|
|`<file>`|File(s) to download.|

Valid <file> values:

|Package|Description|
|-------|-----------|
| `Getting_Started_Binary_Installer.pdf` |User manual in PDF format: *Getting Started with SAP HANA, express edition (Binary Installer Method).* |
| `hxe.tgz` |Downloads `hxe.tgz`, which contains the SAP HANA 2.0, express edition server with Application Function Library. This file is necessary for installing SAP HANA 2.0, express edition.|
| `hxexsa.tgz` |Downloads `hxexsa.tgz`, which contains XS Advanced, Web IDE, and SAP HANA Cockpit. Valid only when downloaded with `hxe.tgz`.|
| `additional_lang.tgz` |Downloads `additional_lang.tgz`. For languages other than English and German, this package is required for the HANA Text Analysis function. (The text analysis files for English and German are already included in the `hxe.tgz` and `hxexsa.tgz` packages.)|
| `eadesigner.tgz` |Valid only with `hxexsa.tgz`. SAP EA Designer lets you capture, analyze, and present your organization's landscapes, strategies, requirements, processes, data, and other artifacts in a shared environment.|
| `hsa.tgz` |Downloads SAP HANA streaming analytics.|
| `hsa_plugin.zip` |Downloads the Eclipse plugin for creating and deploying streaming analytics projects.|
| `shine.tgz` |Valid only with `hxexsa.tgz`. SAP HANA Interactive Education (SHINE) makes it easy to learn how to build applications on SAP HANA Extended Application Services Advanced Model (XSA).|
| `eml.tgz` |Downloads HANA Extended Machine Learning AFL.|
| `apl.tgz` |Downloads SAP HANA Automated Predictive Library.|
| `clients_linux_x86_64.tgz` |Client download package for Linux machines (x86/64 architectures). Use the client packages to access developed SAP HANA, express edition applications from a client PC.|
| `clients_linux_ppc64le.tgz` |Client download package for Linux machines (little endian on Power architectures). Use the client packages to access developed SAP HANA, express edition applications from a client PC.|
| `clients_windows.zip` |Client download package for Windows machines. Use the client packages to access developed SAP HANA, express edition applications from a client PC.|
| `clients_mac.tgz` |Client download package for Mac. Use the client packages to access developed SAP HANA, express edition applications from a client PC.|
| `sdi.tgz` |SAP HANA smart data integration download package. SAP HANA smart data integration provides functionality to access source data, and to provision, replicate, and transform that data in SAP HANA on premise, or in the cloud.|
| `dpagent_linux_x86_64.tgz` |SAP HANA smart data integration - Data Provisioning Agent (Linux X86/64) download package. The Data Provisioning Agent provides secure connectivity between the SAP HANA database and your adapter-based sources.|

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Download one or more files)]

If you want applications like XS Advanced, Web IDE, and SAP HANA Cockpit, download both `hxe.tgz` and `hxexsa.tgz`. If you want optional packages like SAP HANA streaming analytics (`hsa.tgz`), download them now as well.

This example uses the Linux Download Manager `HXEDownloadManager_linux.bin`. It specifies a proxy host, proxy port, and downloads *Getting Started with SAP HANA, express edition (Binary Installer Method)* and the SHINE package (`shine.tgz`).

```bash
HXEDownloadManager_linux.bin --ph proxy.yourcompany.corp --pp 8080 linuxx86_64 installer Getting_Started_Binary_Installer.pdf shine.tgz
```

This example uses the platform-independent Download Manager `HXEDownloadManager.jar`.

It downloads *Getting Started with SAP HANA, express edition (Binary Installer Method)* and `hxexsa.tgz`.

```bash
java -jar HXEDownloadManager.jar linuxx86_64 installer Getting_Started_Binary_Installer.pdf hxexsa.tgz
```

[DONE]

[ACCORDION-END]


