---
title: Test SAP HANA, express edition
description: Test your XSA, SAP Web IDE, Cockpit, and XSC installations.
author_name: John Currie
author_profile: https://github.com/JCurrie27
primary_tag: products>sap-hana\,-express-edition
tags: [ tutorial>beginner, products>sap-hana\,-express-edition ]
time: 10
---

<!-- loio0377017816dc46b09db7b2e13bfabc0a -->

## Prerequisites
 - **Tutorials:** You have completed [Edit the Hosts File](http://www.sap.com/developer/tutorials/hxe-ua-hosts.html)

## Details
### You will learn
You'll learn how to connect to the server from applications, and check if the applications installed correctly.

---

> Note:
> Make sure you edited your `/etc/hosts` file before starting this procedure.
>
>

[ACCORDION-BEGIN [Step 1: ](Test XSA)]

1.   In your VM, log in to XSA services:

    ```bash
    xs-admin-login
    ```

2.   When prompted for the `XSA_ADMIN` password, enter the master password

    You specified this password when you were prompted for the HANA database master password earlier in [Start SAP HANA, express edition Server](https://www.sap.com/developer/tutorials/hxe-ua-getting-started-vm-xsa.html).

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ]((Optional) Turn on XSA messaging)]

If you want the XSA messaging service, issue these commands to start the messaging service applications:

```bash
xs start messaging-service-hub
xs start messaging-service-node
xs start messaging-service-broker

```

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Test SAP Web IDE)]

1.   Display the status and URL for the application `webide`. Enter:

    ```bash
    xs apps | grep webide
    ```

    ![loio5e7c5697cabd467daa1bd966ab372ae8_LowRes](loio5e7c5697cabd467daa1bd966ab372ae8_LowRes.png)

2.   Check that the application `webide` shows `STARTED` in the list of XSA applications, and has 1/1 instances. (If the output shows 0/1 in the instance column, the application is not started.)

    > Note:
    > Normally it only takes a few minutes for XSA services to start. However. depending on your machine, it can take over 30 minutes for XSA services to begin. If the service doesn't show `STARTED` and doesn't show `1/1` instances, keep waiting until the service is enabled.
    >
    >

    Make a note of the URL for `webide`.

3.   Test your Web IDE connection. Enter the URL for Web IDE in a browser on your laptop.

    ```bash
    Example: https://hxehost:53075
    ```

    ![loiocf1862543d7b495a87d621cf3680580f_LowRes](loiocf1862543d7b495a87d621cf3680580f_LowRes.png)

4.   For *HANA Username*, enter `XSA_DEV`.

    You specified this password when you were prompted for `HANA database master password` in [Start SAP HANA, express edition Server](https://www.sap.com/developer/tutorials/hxe-ua-getting-started-vm-xsa.html).

    Web IDE displays.![loioc06ad641e44240b99da7bd10cd275377_LowRes](loioc06ad641e44240b99da7bd10cd275377_LowRes.png)

    If you are prompted to change your password, follow the instructions.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Test Cockpit)]

1.   Go back to your VM. Enter:

    ```bash
    xs apps | grep cockpit-admin-web-app
    ```

2.   Check that the application `cockpit-admin-web-app` shows `STARTED` in the list of XSA applications and has 1/1 instances.

    ![loio26bc3a5355cf49a49ca8f213fbf28a97_LowRes](loio26bc3a5355cf49a49ca8f213fbf28a97_LowRes.png)

    > Note:
    > Normally it only takes a few minutes for XSA services to start. However. depending on your machine, it can take over 30 minutes for XSA services to begin. If the service doesn't show `STARTED` and doesn't show `1/1` instances, keep waiting until the service is enabled.
    >
    >

    Make a note of the URL for `cockpit-admin-web-app`.

3.   Check that the application `xsa-cockpit` shows `STARTED` in the list of XSA applications and has 1/1 instances. Enter:

    ```bash
    xs apps | grep xsa_cockpit
    ```

    > Note:
    > Normally it only takes a few minutes for XSA services to start. However. depending on your machine, it can take over 30 minutes for XSA services to begin. If the service doesn't show `STARTED` and doesn't show `1/1` instances, keep waiting until the service is enabled.
    >
    >

    ![loio8a040521474e4cdf9a8e70b953a50c4d_LowRes](loio8a040521474e4cdf9a8e70b953a50c4d_LowRes.png)

4.   In a browser on your laptop, enter the `cockpit-admin-web-app` URL you noted earlier.

    The Cockpit log in page displays.

    ![loio241f11bd7ea6480faf28d40428aac80f_LowRes](loio241f11bd7ea6480faf28d40428aac80f_LowRes.png)

5.   For *HANA Username*, enter `XSA_ADMIN`.

6.   For *HANA Password*, enter the master password.

    You specified this password when you were prompted for `HANA database master password` earlier in this tutorial.

    Cockpit displays:![loio90debd0143f4416799956f26a580c289_LowRes](loio90debd0143f4416799956f26a580c289_LowRes.png)

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ]((Optional) Test XSC)]

Check that the XSEngine is running. From your host OS (not the VM guest) open a browser and enter:

```bash
http://<hxehost IP address>:8090
```

You recorded the IP address earlier in [Start SAP HANA, express edition Server](https://developers.sap.com/tutorials/hxe-ua-getting-started-vm-xsa.html). A success page displays. This indicates that XSC is running:

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

[ACCORDION-BEGIN [Step 6: ](Next steps)]

-   **Drivers and connectors for Python, Node.js, .NET, Java and others**: Write your [first application using advanced analytics](https://developers.sap.com/mission.xsa-analytics-advanced.html) or check sample applications using different drivers and languages [such as Node.js and Python](https://developers.sap.com/mission.xsa-analytics-advanced.html).

-   **Database clients**: Use a SQL client and the JDBC drivers to [connect to the database](https://developers.sap.com/tutorials/hxe-cj1-download-sql-client.html). Alternatively, use a plugin for Eclipse to connect to your SAP HANA, express edition system database instance. See [How to download and install the HANA Eclipse plugin](https://developers.sap.com/tutorials/hxe-howto-eclipse.html).

-   **Learn XS Advanced basics**: Explore the basics of XS Advanced tools, such as the administration cockpit or SAP Web IDE for SAP HANA. This tutorial includes a step for mapping the tenant database to the development space. See [SAP HANA XS Advanced, explore the basic tools](https://developers.sap.com/tutorials/xsa-explore-basics.html).


[ACCORDION-END]
