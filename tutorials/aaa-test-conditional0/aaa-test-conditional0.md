---
title: Edit the Hosts File (8)
description: The `hxehost` IP address is private to the VM. In order for applications on your laptop (like your web browser) to access `hxehost`, add the `hxehost` IP address to your laptop's hostname map.
author_name: John Currie
author_profile: https://github.com/JCurrie27
primary_tag: products>sap-hana\,-express-edition
tags: [  tutorial>beginner, products>sap-hana\,-express-edition   ]
time: 2
---

## Prerequisites
 - **Tutorials:** You have completed [Start SAP HANA, express edition Server](hxe-ua-getting-started-vm)

## Details
### You will learn
You'll learn how to configure your host machine to communicate with the server. OK.

[Test link](https://localhost:4004/testLink.html)
[Test link 3](https://localhost/testLink)
[Test link 4](https://localhost/testLink.html)
test
---

[ACCORDION-BEGIN [Step 1: ](Update etc/hosts)]

[SAP](mission.cp-starter-ibpm-employeeonboarding)

!![test](Noborder.png)

!![Link in other folder](..\abap-connectivity-daemon-mqtt-bridge\add-authorized-programs.png)

AAAAAAAAAAAAAAAAA

<rac-support@sap.com>

![Link text e.g., Destination screen](HXE_Hosts_Windows_0.png)

![Link text e.g., Destination screen](https://images.pexels.com/photos/67636/rose-blue-flower-rose-blooms-67636.jpeg?auto=compress&cs=tinysrgb&h=750&w=1260)

<https://localhost:4000>


[OPTION BEGIN [Windows]]

If you installed the VM installation package to a Windows machine, follow this step to update the `etc/hosts` file.

1.   On your Windows laptop, navigate to `C:\Windows\System32\drivers\etc`

2.   In `Administrator` mode, open `hosts` in Notepad. See your operating system Help for information on opening applications in Administrator mode

3.   In a new uncommented row, add the IP address and `hxehost`. Save your changes

    Spacing is important. Make sure your hosts file in Notepad looks like this image.

    ![HXE_Hosts_Windows_0](HXE_Hosts_Windows_0.png)

    > Note:
    > If the VM is restarted and assigned a new IP, you'll need to update the Hosts file.
    >
    >

[OPTION END]


[OPTION BEGIN [Mac and Linux]]
If you installed the VM installation package to a Mac or Linux machine, follow this step to update the `etc/hosts` file.

1.   On your Mac or Linux machine, start the Terminal application

2.   Enter the following command:

    ```bash
    sudo sh -c 'echo <hxehost IP address> hxehost >> /etc/hosts'
    ```

    > Note:
    > If the VM is restarted and assigned a new IP, you will need to update the Hosts file.
    >
    >


[OPTION END]

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Test borders)]
### #EEEEEE

![Link text e.g., Destination screen](EEEEEE.png)

### #AAAAAA


|  Field Name     | Value
|  :------------- | :-------------
|  Name           | **`Northwind`**
|  Type           | **`HTTP`**
|  Description    | **`Northwind OData Service`**
|  URL          | **`http://services.odata.org`**
|  Proxy Type   | **`Internet`**
|  Authentication | **`NoAuthentication`**

![Link text e.g., Destination screen](AAAAAA.png)

### #888888

This is some text before.

![Link text e.g., Destination screen](888888.png)

This is some text before.

### #444444

![Link text e.g., Destination screen](444444.png)

This is some text before.

[DONE]
[ACCORDION-END]
