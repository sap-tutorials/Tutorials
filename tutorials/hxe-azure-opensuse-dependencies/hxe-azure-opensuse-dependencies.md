---
title: SAP HANA, express edition, install dependencies for openSUSE
description: Install the despendencies for openSUSE Linux before you install SAP HANA, express edition
primary_tag: products>sap-hana\,-express-edition
tags: [  tutorial>beginner, products>sap-hana\,-express-edition ]
---

## Prerequisites  
 - **Proficiency:** Beginner | Intermediate | Advanced
 - **Tutorials:** [Installing SAP HANA, express edition, on Microsoft Azure and openSUSE](http://www.sap.com/developer/tutorials/hxe-azure-open-suse.html)
 - [**OPTIONAL** - Setup PuTTY and WinSCP to access your HANA Express Edition instance on Google Cloud Platform](http://www.sap.com/developer/tutorials/hxe-gcp-setup-putty-winscp-windows.html)  


## Next Steps
 - [Extract the binaries and execute the installation script](http://www.sap.com/developer/tutorials/hxe-azure-opensuse-extract-install.html)


## Details
### You will learn  
This tutorial will guide you through the installation of the dependencies needed for SAP HANA, express edition, to run on Open SUSE.

### Time to Complete
**15 Min**

---


[ACCORDION-BEGIN [Step 1: ](Update repositories)]

Begin by updating repositories. In a command line (using PuTTY or any other SSH client), enter the following commands:

```ssh
sudo su –
yast
```

Choose `Software`->`Online Update`

![YaSt update](13.png)

Wait for `YaST` to initialize, refresh and check for available updates:

![YaSt update](14.png)

And accept using **F10**

![YaSt update](15.png)

Once installation is complete, press **F** to finish:

![YaSt update](16.png)

>Note: Restart the system if necessary.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Install libtool)]

Back in the initial screen, use go into `Software -> Software Management`

![YaSt libtool](17.png)

Enter `libtool` and press Enter

![YaSt libtool 2](18.png)

Press enter on `libltdl7` and then F10 to accept

![YaSt libtool 2](19.png)

[DONE]
[ACCORDION-END]



## Next Steps
- [Extract the binaries and execute the installation script](http://www.sap.com/developer/tutorials/hxe-azure-opensuse-extract-install.html)
