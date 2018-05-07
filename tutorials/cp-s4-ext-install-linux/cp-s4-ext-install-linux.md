---
title: Install openSUSE in VirtualBox Virtual Machine
description: Install openSUSE in a VirtualBox Virtual Machine (VM), which is used as operating system in other consecutive tutorials.
primary_tag: products>sap-s-4hana
tags: [ tutorial>intermediate, products>sap-s-4hana ]
---

## Prerequisites
 - **Proficiency:** Intermediate
 - **Tutorials:** [Create a trial account on SAP Cloud Platform](https://www.sap.com/developer/tutorials/hcp-create-trial-account.html)
 -  **System Requirements:**
   * 64 bit Windows / Mac OS
   * 8 GB RAM (better: 16 GB RAM)
   * 100 GB available disk space

## Next Steps
 - [Install SAP NetWeaver in openSUSE](https://www.sap.com/developer/tutorials/cp-s4-ext-install-nw.html)

## Details
### You will learn
In this tutorial, you will install and set up a Linux operating system (openSUSE) in a VirtualBox virtual machine container. This can then be used as host operating system for installing an SAP NetWeaver system.

### Time to Complete
**30 Mins**

---

[ACCORDION-BEGIN [Preparation Steps](&nbsp;)]

To enable also users working with Windows or Mac OS X operation system to install `SAP NetWeaver AS ABAP`, we describe here how to make use of `Oracle VM VirtualBox` (VirtualBox), where the ABAP system then is installed and run on a Linux Virtual Machine (VM).

> **Hint:** If you have already a 64 bit Linux OS then you might be able to skip this tutorial and begin directly with [Install SAP NetWeaver in openSUSE](https://www.sap.com/developer/tutorials/cp-s4-ext-install-nw.html). However, please note that we haven't tested or documented this.

**System Requirements:**

Make sure that you have on your PC/Laptop or Mac:

1.  At least **8 GB of RAM** (better: 16 GB).

2.  At least **100 GB of free disk space**.

The tutorial has been tested to work for Windows and on Mac OS X.

As the some of the installation files are very large (> several GB) we recommend to start the download first for all files, before you start the installation process.

### Download Oracle VM VirtualBox

1.  Download the latest released version of `Oracle VM VirtualBox` (in short VirtualBox) from [https://www.virtualbox.org/wiki/Downloads](https://www.virtualbox.org/wiki/Downloads) for your host operating system (Host OS).

2.  Click on e.g. **`x86/amd64`** link to start the download of VirtualBox for Windows OS.

### Download Linux OS openSUSE

1.  Download `Linux OS openSUSE Leap` (DVD Image, `x86_64`) to your local machine from [https://software.opensuse.org/distributions/leap](https://software.opensuse.org/distributions/leap). (Ensure that you do not choose the **Network** installation files.)

    > **Hint:** This tutorial is based on `Linux OS openSUSE Leap` **`42.1`**, which is not the latest openSUSE release. Therefore the look of your installation might be slightly different from the provided screenshots.

2.  On the opened download page click **Direct Link** link to start the download.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 1:](Install Linux VM in VirtualBox)]

### Install VirtualBox

1.  Use the VirtualBox installer you downloaded before in step 1.

2.  You can keep all default settings of the installation wizard.

3.  After installation, you can start the `VirtualBox Manager` as follows:
    -   On a Windows host:  Open the standard `Programs` menu and click on the item in the `VirtualBox` group.
    -   On a Mac OS X host: In the `Finder`, double-click on the **VirtualBox** item in the `Applications` folder (You may want to drag this item onto your Dock).

    ![Virtual Box](./images/w1-u6-s2/pic01--virtualbox.png)

### Create new Linux VM in VirtualBox

1.  Open `VirtualBox Manager` application

2.  Click **New** button to create a new Linux Virtual Machine (VM) image with the following settings:
      - Name: **`SAP NetWeaver 7.50 SP02`**
      - Type: **`Linux`** (Operating System)
      - Version: **`openSUSE (64-bit)`**
      - Click **Next**

    ![New VM 1](./images/w1-u6-s2/pic02--new-vm.png)

3.  On the next displayed wizard page:
      - Change `Memory size` to **`6 GB`** (6144 MB).
      - Click **Next**

    ![New VM 2](./images/w1-u6-s2/pic03--new-vm.png)

4.  On the next displayed wizard page:
      - Make sure that **`Create a virtual hard disk now`** is selected.
      - Click **Create**

    ![New VM 3](./images/w1-u6-s2/pic04--new-vm.png)

5.  On the next opened wizard page:
      - Make sure that **`VDI`** (Default) is selected as `Hard disk file type`.
      - Click **Next**

    ![New VM 4](./images/w1-u6-s2/pic05--new-vm.png)

6.  On the next displayed wizard page:
    - Make sure that **`Dynamically allocated`** (Default) is selected as `Storage on physical hard disk`.
    - Click **Next**

    ![New VM 5](./images/w1-u6-s2/pic06--new-vm.png)

7.  On the next displayed wizard page:
    - Change `File size` to **`80 GB`** and keep the `File location`
    - Click **Create** to create **`a new VM entry`** with the specified settings.

    ![New VM 6](./images/w1-u6-s2/pic07--new-vm.png)

> **Result:** A new VM entry with name `SAP NetWeaver 7.50 SP02` is created on the left side of the VirtualBox Manager.
>
>    ![New VM 7](./images/w1-u6-s2/pic08--new-vm.png)

### Set boot installation file for Linux VM

1.  Right-click on the new VM entry `SAP NetWeaver 7.50 SP02` to open the context menu.

2.  Choose **Settings...** to open a settings dialog.

3.  Select **Storage** from the navigation.

4.  In the `Storage Tree` section select the **Empty** node of `Controller:IDE`.

    ![Boot file 1](./images/w1-u6-s2/pic09--boot-file.png)

5.  In the `Attributes` section on the right side click on the **disk icon** to open a context menu.

6.  Click on option **Choose Virtual Optical Disk File...** to open a file browser.

    ![Boot file 2](./images/w1-u6-s2/pic10--boot-file.png)

7.  Select the openSUSE Linux OS `.iso` file from your local storage that you downloaded in step 1.

    ![Boot file 3](./images/w1-u6-s2/pic11--boot-file.png)

8.  Back on the `Settings` dialog click **OK** to save the settings.

    ![Boot file 4](./images/w1-u6-s2/pic12--boot-file.png)

> **Result:** The VM has been prepared to boot from the specified `openSUSE DVD Image` file as soon as it is started as described in the next section.

### Install openSUSE Linux OS on the VM

1.  In `VirtualBox Manager` select `SAP NetWeaver 7.50 SP02` VM node and click **Start** button to start the new VM. It opens a new window `Oracle VM VirtualBox` and starts the virtual machine.

    ![Install Linux 1](./images/w1-u6-s2/pic13--install-linux.png)

    > **Note:** If you have never worked with a Virtual Machine before, you have to imagine that a VM acts like a computer inside of your operating system. The window that now opened acts like a computer monitor for this emulated machine.

2.  Quickly use your keyboard **DOWN key** to select the **Installation** menu item, otherwise it will automatically select the first menu item.

    ![Install Linux 2](./images/w1-u6-s2/pic14--install-linux.png)

3.  Hit the **RETURN** key to start the VM OS installation of openSUSE Linux.

4.  During initialization, click the icon of the **mouse pointer integration** message so that this message is never displayed again.

      ![Install Linux 3](./images/w1-u6-s2/pic15--install-linux.png)

5.  In the installation wizard, set the OS language to **English** and the keyboard language corresponding to the keyboard layout that you are using. You should check that your keyboard layout is correct by typing some special characters (e.g. `ä`, `ß`, `z`, `a`, `>`) in **Keyboard Test** area.

      ![Install Linux 4](./images/w1-u6-s2/pic16--install-linux.png)

6.  Click **Next** to accept license and press **Next** in the screen with **Installation Options** by leaving the defaults.

      ![Install Linux 5](./images/w1-u6-s2/pic17--install-linux.png)

7.  In the next screen with `Suggested Partitioning` click on **Edit Proposal Settings**.

      ![Install Linux 6](./images/w1-u6-s2/pic18--install-linux.png)

    -   For the field **File System for Root partitioning**, choose **`Ext4`** from the drop-down box.
    -   Uncheck **Propose Separate Home Partition**. This option is needed so that openSUSE Linux OS installation creates only one drive, instead of two drives (`Home` and `Extension`) where `Home` would have insufficient space to install the SAP NetWeaver backend system.
    -   Choose **OK** to save settings.

8. Click **Next**.

    ![Install Linux 7](./images/w1-u6-s2/pic19--install-linux.png)

9. Adjust `Region` and `Timezone` and press **Next**.

    ![Install Linux 8](./images/w1-u6-s2/pic20--install-linux.png)

10. In Desktop Selection, choose **KDE Desktop**.

    ![Install Linux 9](./images/w1-u6-s2/pic21--install-linux.png)

11. Click on **Next**.

12. Provide User's Full Name, Username and Master Password and click **Next**.

    > **Note:** Regarding the `Master Password` we assume that you might want to use the same password throughout the tutorial, because it might be easier to remember. The SAP NetWeaver requires a certain password complexity. Therefore, it makes sense that you use at least 8 characters, one number and one capital letter to form your password, e.g. `Appl1ance`.

    ![Install Linux 10](./images/w1-u6-s2/pic22--install-linux.png)

13. In the `Installation Settings` wizard step, scroll down to find **Firewall and SSH** settings:
    -   Click on the **disable** link to disable the **Firewall**.
    -   Click on the **enable** link to enable the **SSH service**.

    ![Install Linux 11](./images/w1-u6-s2/pic23--install-linux.png)

14. Click on **Install**.

    ![Install Linux 12](./images/w1-u6-s2/pic24--install-linux.png)

15. After confirming that all settings are correct, click on **Install**.

    ![Install Linux 13](./images/w1-u6-s2/pic24b--install-linux.png)

16. After the installation finished (about 15-20 minutes), the VM will reboot automatically.

17. The VM will come up with default start option **Boot from hard Disk**. If you do nothing it will
    automatically use this option after 60 seconds (you may also hit RETURN key to speed this up).

      ![Reboot Linux 1](./images/w1-u6-s2/pic25--reboot-linux.png)

18. Next the **openSUSE Leap &lt;version>** is displayed as default boot option and will be used automatically after a few seconds.

    ![Reboot Linux 2](./images/w1-u6-s2/pic26--reboot-linux.png)

19. openSUSE might want to install quite some updates. It is not necessary to install these, so you may skip this step.

20. Eventually you will reach the logon screen for your openSUSE installation. (The logon page/lock screen will also come up after certain time of not working with the running VM). Enter your **`password`** and click **Unlock** to enter the KDE desktop.

    > **Note:** This password is also the password of the `root` user, which you have to enter frequently later in this tutorial. The `root` user has advanced administration privileges in a Linux OS.

    ![Reboot Linux 3](./images/w1-u6-s2/pic27--reboot-linux.png)

21. Close the **Desktop Folder** window: Hover over the window and click the **X** icon (with tooltip `Remove`) in the lower right corner of the window.

    ![Reboot Linux 4](./images/w1-u6-s2/pic28--reboot-linux.png)

22. **For Laptop/Notebook/MacBook Users:** To keep the Linux VM session alive when not connected to a power supply you must change the default `Energy Saving` system settings.

    > **Warning:** The default power setting is that the server will suspend your session after 10 minutes of inactivity when your laptop is running on a battery. This stops any access to the SAP system and also shuts down the network connection making the VM inaccessible.

23. On the KDE desktop open the `Application` menu (similar to the Windows `Start` menu) in the bottom left corner. In the search field enter the query string **power** and click on the menu entry  **Energy Saving**.

    ![Reboot Linux 5](./images/w1-u6-s2/pic31--install-linux-power.png)

24. In the `Energy Saving` dialog select tab **On Battery**. Disable the checkbox **Suspend session**.

25. Select tab **On Low Battery**. Again disable the checkbox **Suspend session**.

26. Click on **Apply** and close the dialog with **OK**.

      ![Reboot Linux 6](./images/w1-u6-s2/pic32--install-linux-power.png)

### Create VM Snapshot
Having successfully installed and set up your openSUSE Linux operating system, you should create a **VM snapshot** of this initial state. You may revert to this snapshot, in case something should go wrong or break in the VM.

1.  In the running `Oracle VM VirtualBox` window menu: Choose **Machine** | **Take Snapshot...**.

    ![Snapshot 1](./images/w1-u6-s2/pic29--snapshot.png)

2.  In the opened window enter **Initial Installation Snapshot** and click **OK**.

    ![Snapshot 2](./images/w1-u6-s2/pic30--snapshot.png)

> **Result:** The VM snapshot `Initial Installation Snapshot` has been created and saved.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Configure VM - Proxy, Internet, shared Clipboard)]

### Configure Internet Proxy (optional)

In case you are using a proxy to connect to the internet, you have to configure it as follows; otherwise you may skip this section.

#### VM VirtualBox Setting

1.  In the running `Oracle VM VirtualBox` window menu: Choose **Input** | **Keyboard** | **Keyboard Settings...**.

    ![Proxy Config 1](./images/w1-u6-s3/pic01--proxy-config.png)

2.  In the opened `Preferences` window select `Proxy` from the navigation:
      - Select **Manual Proxy Configuration** option.
      - Enter your **Host** and **Port** to the corresponding input fields (e.g. `proxy.mycompany.com` and `8080`).
      - Click **OK** to save the proxy settings.

    ![Proxy Config 2](./images/w1-u6-s3/pic02--proxy-config.png)

#### YaST Proxy Setting

YaST is the installation and configuration tool for openSUSE. It can be used to configure your entire system, network, system services and security settings. It may also install new software packages from the internet. Hence you might need to set up a proxy for YaST.

1.  In the running `Oracle VM VirtualBox` open the KDE **Application Menu** by clicking on the **gear with K** icon in the lower left corner.

    ![Proxy Config 3](./images/w1-u6-s3/pic03--proxy-config.png)

2.  Navigate to **System** | **YaST** and click on the `YaST` item.

3.  Enter your **root password** to open the `YaST Control Center`.

    ![Proxy Config 4](./images/w1-u6-s3/pic04--proxy-config.png)

4.  Enter **`proxy`** as search term into the **Search** field.

    ![Proxy Config 5](./images/w1-u6-s3/pic05--proxy-config.png)

5.  On the right side, select the **Proxy** icon in the `Network Services` category.

6.  On the `Proxy Configuration` page:
    -   Select **Enable Proxy** checkbox.
    -   Enter **HTTP Proxy URL** value, e.g. `http://proxy.mycompany.corp:8080`
    -   Select  **Use the Same Proxy for All Protocols** checkbox.
    -   Click **Test Proxy Settings** to verify the proxy settings.

    ![Proxy Config 6](./images/w1-u6-s3/pic06a--proxy-config.png)

7.  A `Proxy settings work successfully` popup should appear. Confirm with **OK**.

    ![Proxy Config 7](./images/w1-u6-s3/pic06b--proxy-config.png)

8.  Click **OK** to save the proxy settings.

    ![Proxy Config 8](./images/w1-u6-s3/pic06c--proxy-config.png)

9.  Confirm the `Successfully Saved` dialog with **OK**.

#### Firefox Proxy Setting

1.  In the running `Oracle VM VirtualBox` open the KDE **Application Menu** by clicking on the **gear with K** icon in the lower left corner.

    ![Test Proxy 1](./images/w1-u6-s3/pic07--test-proxy.png)

2.  Navigate to **Internet** | **Firefox** and click on the item to open the `Firefox` web browser.

    ![Test Proxy 2](./images/w1-u6-s3/pic08--test-proxy.png)

3.  Open **Firefox menu** in the upper right corner and click on **Preferences**.

    ![Test Proxy 3](./images/w1-u6-s3/pic09--test-proxy.png)

4.  On the opened **Preferences** page:
    -   Choose **Advanced** item from left side icon bar.
    -   On the `Advanced` page click **Network** tab.
    -   Click **Settings...** button

    ![Test Proxy 4](./images/w1-u6-s3/pic10--test-proxy.png)

5.  On the opened **Connection Settings** dialog:
    -   Select **Manual Proxy configuration** option
    -   Enter your **HTTP Proxy** and **Port** value (e.g. `proxy` and `8080`)
    -   Select **Use this proxy server for all protocols** option
    -   Click **OK** to save the proxy settings.

    ![Test Proxy 5](./images/w1-u6-s3/pic11--test-proxy.png)

### Test Internet Connection

1.  In the running `Oracle VM VirtualBox` open the KDE **Application Menu** by clicking on the **gear with K** icon in the lower left corner.

    ![Test Proxy 6](./images/w1-u6-s3/pic07--test-proxy.png)

2.  Navigate to **Internet** | **Firefox** and click on the item to open the `Firefox` web browser.

    ![Test Proxy 7](./images/w1-u6-s3/pic08--test-proxy.png)

3.  You should now see a working openSUSE default web page.

### Enable bi-directional shared clipboard

Before you continue with the ABAP installation specific steps, you should enable the bi-directional shared clipboard as follows. This option allows you to use clipboard contents (copy & paste) from your host OS (Windows / Mac OS X) in the VM (openSUSE) and vice-versa. This should make things much easier when you copy and paste contents from this tutorial into the Linux console.

1.  In the running `Oracle VM VirtualBox` window menu: Open **Devices** | **Shared Clipboard** | **Bidirectional**.

    ![Clipboard](./images/w1-u6-s3/pic12--clipboard.png)

2.  Click on the **Bidirectional** item so that it is enabled.

> **Hint:** With this enabled `bi-directional` option you can now copy and paste from your host operating system to the VM image window and vice versa. This is very helpful as you can copy e.g. needed command lines from the tutorial description (web page on your host OS) into a bash window of your VM window.

[ACCORDION-END]

You have now successfully completed this tutorial and set up openSUSE in a VirtualBox VM. You can now proceed with installing a SAP NetWeaver system in this VM, as described in this next tutorial: [Install SAP NetWeaver in openSUSE](https://www.sap.com/developer/tutorials/cp-s4-ext-install-nw.html)
