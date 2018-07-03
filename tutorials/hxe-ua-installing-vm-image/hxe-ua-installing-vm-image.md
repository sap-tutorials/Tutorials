---
title: Pre-Installation Tasks
description: The virtual machine (VM) installation method is the simplest SAP HANA 2.0, express edition on-premise installation method for compatible Windows, OS X, and Linux laptops. Perform these pre-installation tasks first, before you register.
primary_tag: products>sap-hana\,-express-edition
tags: [ tutorial>beginner, products>sap-hana\,-express-edition ]
---

<!-- loio6d73440459d4476c8f21bb852d7f36a4 -->

## Prerequisites
 - **Proficiency:** Beginner
 - **Tutorials:**  Open the [Install SAP HANA 2.0, express edition on a preconfigured virtual machine](https://www.sap.com/developer/groups/hxe-install-vm.html) tutorial group or the [Install SAP HANA 2.0, express edition on a preconfigured virtual machine (with SAP HANA XS Advanced)](https://www.sap.com/developer/groups/hxe-install-vm.html) tutorial group. 

## Details
### You will learn
You'll decide whether the virtual machine installation method is best for you, you'll learn the hardware and software requirements, and you'll install a hypervisor.

### Time to Complete
5 min

---

These installation tutorials are also available as a [video](https://www.sap.com/assetdetail/2016/09/d2900513-8a7c-0010-82c7-eda71af511fa.html).

[ACCORDION-BEGIN [Step 1: ](Understand the Virtual Machine installation method.)]

The SAP HANA 2.0, express edition VM image is platform-independent. You can install it to a Windows, OS X, or Linux machine, provided your laptop meets the storage and memory prerequisites. Choose the VM installation method if you want the simplest `on-premise` installation experience.

> Note:
> If you need a custom on-premise setup, use the Binary Installer Method, which is for Linux machines – running specific installations – that meet certain storage and memory prerequisites. See [Install SAP HANA, express edition on a native Linux machine](https://www.sap.com/developer/groups/hxe-install-binary.html) or [Install SAP HANA, express edition on a native Linux machine (with SAP HANA XS Advanced)](https://www.sap.com/developer/groups/hxe-install-binary-xsa.html).
> 
> 

The Virtual Machine method installs:

-   A VM running `SUSE Linux Enterprise Server (SLES) for SAP Applications 12 SP2`.

-   An SAP HANA 2.0, express edition instance on the VM, preconfigured and ready to start.


You can download two different installation packages depending on your requirements:

-   A `server-only virtual machine package`: the server plus XSC, and the Application Function Library (AFL).

-   A `server + applications virtual machine package`: the server and XS Advanced, Web IDE, and SAP HANA Cockpit. This package requires more RAM.


> Note:
> SAP HANA 2.0, express edition is officially supported on SLES. SAP Community members have been successful in running SAP HANA, express edition on other Linux operating systems that are not formally supported by SAP, such as `Ubuntu`, `openSUSE`, `Fedora`, and `RedHat`. SAP is not committing to resolving any issues that may arise from running SAP HANA, express edition on these platforms.
> 
> 

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Note changes from version 1.0.)]

If you're familiar with the older 1.0 SPS 12 version, note this important change:

-   The instance number has changed from 00 to 90.


See the [release notes](https://www.sap.com/developer/topics/sap-hana-express.release-notes.html) for information on what's new and changed in this release of SAP HANA 2.0, express edition.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Ensure your laptop meets the software requirements.)]

Check if your laptop has the recommended software to successfully install and run the SAP HANA 2.0, express edition VM package.

|Requirement|Details|
|-----------|-------|
|Java Runtime Environment 8|The Download Manager requires Java SE Runtime Environment 8 (JRE 8) or higher. You can download the SAP JVM (64-bit) from [https://tools.hana.ondemand.com/#cloud](https://tools.hana.ondemand.com/#cloud).|

> Note:
> If you plan to use the SAP HANA, express edition Download Manager for Windows or Linux, you need the **64-bit JRE**. If you are planning to use the platform-independent Download Manager, you can use either the **32-** or **64-bit** JRE.
> 
> 

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Ensure your laptop meets the hardware requirements.)]

Check if your laptop has the recommended software to successfully install and run the SAP HANA 2.0, express edition VM package.

|Requirement|Details|
|-----------|-------|
|RAM - Server-only virtual machine|8 GB RAM minimum. (If you add additional components, or run heavy processing loads, you will need to increase your RAM.)|
|RAM - Server plus applications virtual machine|16 GB RAM minimum. 24 GB RAM recommended.|
|HDD|120 GB HDD recommended.|
|Cores|2 cores (4 recommended).|
|Hardware Virtualization|(Intel processors only) For Intel processors, virtualization is a BIOS setting known as either Intel Virtualization Technology or Intel VT. If virtualization is turned off on your virtualization-capable machine, consult documentation from your machine vendor on how to enable virtualization technology (or Intel VT) in the BIOS.|

Go to [Determine If Your Processor Supports Intel Virtualization Technology](https://www.intel.com/content/www/us/en/support/articles/000005486/processors.html) to determine if your processor is capable of supporting virtualization.

> Note:
> Concerned about memory? The memory consumption of each optional component is listed in the[SAP HANA 2.0, express edition Sizing Guide (Virtual Machine Method)](https://help.sap.com/viewer/DRAFT/9e4243e92f244537b2164a57a405a9fd/latest/en-US).
> 
> 

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Install a hypervisor.)]

Hypervisors are software products used for creating and running virtual machines. Install a supported hypervisor on your laptop if you don't have one already. SAP HANA 2.0, express edition has been tested on these hypervisors:

-   `VMware Workstation Player 12.x` - [https://my.vmware.com/web/vmware/downloads](https://my.vmware.com/web/vmware/downloads)

-   `VMware Workstation Pro 12.x` - [https://my.vmware.com/web/vmware/downloads](https://my.vmware.com/web/vmware/downloads)

-   `VMware Fusion 8.x` - [https://my.vmware.com/web/vmware/downloads](https://my.vmware.com/web/vmware/downloads)
-   `Oracle VirtualBox` (`https://www.virtualbox.org`)

Example installation procedure for `VMware Workstation Player 12.x`:

-   Download `VMware Workstation Player`. Ensure you're downloading the correct version for your machine.

-   Run the installer.

-   Register `VMware Workstation Player` when prompted, and follow the setup instructions.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Go to the next tutorial.)]

1.   If you don't have the parent tutorial group open already, open the [Install SAP HANA 2.0, express edition on a preconfigured virtual machine](https://www.sap.com/developer/groups/hxe-install-vm.html) tutorial group or the [Install SAP HANA 2.0, express edition on a preconfigured virtual machine (with SAP HANA XS Advanced)](https://www.sap.com/developer/groups/hxe-install-vm.html) tutorial group. 

2.   Click the next tutorial in the sequence. 

[ACCORDION-END]


