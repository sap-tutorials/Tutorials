---
title: Test SAP HANA, express edition
description: Test your XSC, XSA, SAP Web IDE, and Cockpit installations.
primary_tag: products>sap-hana\,-express-edition
tags: [ tutorial>beginner, products>sap-hana\,-express-edition ]
---

<!-- loio0377017816dc46b09db7b2e13bfabc0a -->

## Prerequisites
 - **Proficiency:** Beginner
 - **Tutorials:** 

## Details
### You will learn
You'll learn how to connect to the server from applications, and check if the applications installed correctly.

### Time to Complete
10 min

---

> Note:
> Make sure you edited your `/etc/hosts` file before starting this procedure.
> 
> 

[ACCORDION-BEGIN [Step 1: ](Test XSC.)]

Check that the XSEngine is running. From your host OS (not the VM guest) open a browser and enter:

```bash
http://<hxehost IP address>:8090
```

You recorded the IP address earlier. A success page displays. This indicates that XSC is running:

![loio511f9acd6591413db454e05b8dc8368c_HiRes](loio511f9acd6591413db454e05b8dc8368c_HiRes.png)

> Note:
> SAP plans to remove SAP HANA extended application services, classic model (XSC) and the corresponding SAP HANA Repository with the next major product version of SAP HANA.
> 
> These components will be removed:
> 
> -   SAP HANA extended application services, classic model
> 
> -   SAP HANA Repository (XS classic)
> 
> -   SAP HANA Studio (Development, Modeling, and Administration perspectives)
> 
> -   SAP HANA Web-based Development Workbench (XS classic)
> 
> 
> SAP strongly advises you to plan the transition of existing content and applications from XSC to SAP HANA extended application services, advanced model (XS Advanced).
> 
> 

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Test XSA.)]

1.   Go back to your VM. Log in to XSA services: 

    ```bash
    xs-admin-login
    ```

2.   When prompted for the `XSA_ADMIN` password, enter the master password. 

    You specified this password when you were prompted for the HANA database master password earlier in this tutorial group.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Test SAP Web IDE.)]

1.   View the list of XSA applications. Enter: 

    ```bash
    xs apps
    ```

    > Note:
    > When you run the `xs apps` command for the first time, it may take 1-2 minutes for the system to return the list of XSA applications.
    > 
    > 

2.   Check that the application `webide` shows `STARTED` in the list of XSA applications, and has 1/1 instances. (If the list shows 0/1 in the instance column, the application is not started.) 

    > Note:
    > Normally it only takes a few minutes for XSA services to start. However. depending on your machine, it can take over 30 minutes for XSA services to begin. If the service doesn't show `STARTED` and doesn't show `1/1` instances, keep waiting until the service is enabled.
    > 
    > 

    Make a note of the URL for `webide`.

    ![loio5edd67a000a745cdb47d3a00973d0632_HiRes](loio5edd67a000a745cdb47d3a00973d0632_HiRes.png)

    > Note:
    > The command `xs apps | grep webide` returns the `webide` row only.
    > 
    > 

3.   Test your Web IDE connection. Enter the URL for Web IDE in a browser on your laptop. 

    ```bash
    Example: https://hxehost:53075
    ```

4.   Log on to Web IDE using the `XSA_DEV` user. 

    You specified this password when you were prompted for `HANA database master password` earlier in this tutorial.

    If you are prompted to change your password, follow the instructions.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Test Cockpit.)]

1.   Go back to your VM. Check that the application `cockpit-admin-web-app` shows `STARTED` in the list of XSA applications and has 1/1 instances. 

    > Note:
    > Normally it only takes a few minutes for XSA services to start. However. depending on your machine, it can take over 30 minutes for XSA services to begin. If the service doesn't show `STARTED` and doesn't show `1/1` instances, keep waiting until the service is enabled.
    > 
    > 

    Make a note of the URL for `cockpit-admin-web-app`.

    ![loiodc4420a9bb9a4602a96b839b266dbd71_HiRes](loiodc4420a9bb9a4602a96b839b266dbd71_HiRes.png)

    > Note:
    > The command `xs apps | grep cockpit-admin-web-app` returns the `cockpit-admin-web-app` row only.
    > 
    > 

2.   Check that the application `xsa-admin` shows `STARTED` in the list of XSA applications and has 1/1 instances. 

    > Note:
    > Normally it only takes a few minutes for XSA services to start. However. depending on your machine, it can take over 30 minutes for XSA services to begin. If the service doesn't show `STARTED` and doesn't show `1/1` instances, keep waiting until the service is enabled.
    > 
    > 

    Make a note of the URL for `xsa-admin`.

    ![loio2345350639c34cf3be9bd4bea51d5517_HiRes](loio2345350639c34cf3be9bd4bea51d5517_HiRes.png)

    > Note:
    > The command `xs apps | grep xsa-admin` returns the `xsa-admin` row only.
    > 
    > 

3.   In a browser on your laptop, enter the `cockpit-admin-web-app` URL you noted earlier. 

The Cockpit log in page displays.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ]((Optional) Test with HANA Eclipse Plugin.)]

Download and install the HANA Eclipse Plugin to your host OS (not the VM guest) and connect to SAP HANA 2.0, express edition.

1.   Download Eclipse IDE for Java EE Developers from Eclipse, for [Neon](http://www.eclipse.org/neon/) or [Mars](http://www.eclipse.org/mars/) releases, to your local file system. 

2.   Follow the eclipse installer prompts. 

3.   Launch when prompted, or go to the eclipse folder (example: `C:\Users\<path>\eclipse\jee-neon`) and run the eclipse executable file. 

4.   Follow the tutorial [How to download and install the HANA Eclipse plugin](http://www.sap.com/developer/tutorials/hxe-howto-eclipse.html). 

[ACCORDION-END]


