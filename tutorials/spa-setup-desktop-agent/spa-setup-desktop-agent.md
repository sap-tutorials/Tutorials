---
author_name: Chaitanya Priya Puvvada
author_profile: https://github.com/chaitanya-priya-puvvada
title: Install and Set Up the Desktop Agent
description: Install and Set Up the Desktop Agent to run your automation
auto_validation: true
time: 15
tags: [ tutorial>beginner, software-product>sap-business-technology-platform]
primary_tag: software-product>sap-process-automation
---

## Details
The Desktop Agent is a component of SAP Intelligent Robotic Process Automation that is installed locally on user desktops. It executes automation projects that launch and run applications of various kinds, read information from screens, enter data, click options, and process data.

## Prerequisites
 - A Windows PC
 - If you are using a MAC, please install a VDI

### You will learn

  - How to install the Desktop Agent
  - How to register a tenant in the Desktop Agent

---

[ACCORDION-BEGIN [Step 1: ](About the Desktop Agent)]

The Desktop Agent is an on-premise component of SAP Process Automation that is installed locally on user desktops. It executes automation projects that launch and run applications of various kinds, read information from screens, enter data, click options, and process data.

Projects are assigned to tenants running on a Desktop Agent. You can see what your Desktop Agent is doing at all times thanks to the convenient system tray, or `Systray`, that is always accessible while your Desktop Agent is ready or active.

When it is installed, the Desktop Agent is configured to start at Windows logon automatically. You should not change this setting because your Agent might be assigned background (unattended) jobs at any time.

The very first time you launch the Desktop Agent on your workstations, you will be prompted to log in. If you are unsure of your login details, please ask an administrator.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Install the Desktop Agent)]

When you install the Desktop Agent, this will automatically set up the SAP Intelligent RPA web browser extension for Google Chrome and Edge.

>To prevent issues during the installation, please close all the Chrome tabs opened on your machine. The setup program is provided in the form of an industry standard Windows MSI installer. You can download it from the SAP Software Center.

>The minimum version of the Desktop Agent supported by SAP Process automation is **2.0.20**.

1. Download the **latest version**  of the [MSI file execution](https://tools.hana.ondemand.com/#cloud).

    > MSI version will be updated for every new release. Always download the latest version.

    !![Link text e.g., Destination screen](tools.png)

2. Select **Next** to begin the installation process.

    !![Desktop Agent Installation](02-desktop-agent-installation.png)

3. In the **Installation type** pop-up, select the **Desktop Agent** option.

    !![Desktop Agent Installation](03-desktop-agent-installation.png)

4. Optional: choose **Browse** to change the destination of the installation folder.

    !![Desktop Agent Installation](04-desktop-agent-installation.png)

5. Select **Install** to start the installation.

    !![Desktop Agent Installation](05-desktop-agent-installation.png)

6. The installation process may take a few seconds to complete. An authorization request to bring change to the computer might appear, choose **OK** if so.

    !![Desktop Agent Installation](06-desktop-agent-installation.png)

    !![Desktop Agent Installation](06-desktop-agent-installation2.png)

7. Once the installation has been successfully completed, choose **Finish** and launch your Desktop Agent.

    !![Desktop Agent Installation](07-desktop-agent-installation.png)

    >Microsoft Edge and Chromium are used for the rendering of the Desktop Agent. If you use Edge, the Edge 'WebView2' component is mandatory: if not already installed on your machine, please install Edge WebView2 from the Microsoft website.

    A Google Chrome extension and an Internet Explorer add-on are installed when you install the Desktop Agent but you have to enable them (at least the Google Chrome extension).

8. Launch your Google Chrome then select the "Enable extension" button.

    !![Desktop Agent Installation](enable-extension.png)

9. If the message and question (above) do not appear, open this URL in your Google Chrome: chrome://extensions/ and enable the "SAP Intelligent RPA Extension" if it is disabled.

    !![Desktop Agent Installation](chrome-extensions.png)

    >You now need to register the Desktop Agent to finalize the installation.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Register the Desktop Agent Tenant)]

Once you've completed the installation steps of the SAP Robotic Process Automation setup wizard, you need to register your agent and connect it to a SAP Process Automation tenant in order to execute automations.

> Once the Desktop Agent is installed, the icon will be available on your System Tray.

1. Select the Desktop Agent icon, choose the 3 dots and select **Tenants**.

    !![Desktop Agent Installation](08-desktop-agent-installation.png)

2.  Choose **Add**.

    !![Desktop Agent Installation](09-desktop-agent-installation.png)

3.  On the SAP Process Automation homepage, go to **Settings**, choose **Agents List** and then select the **Register new agent...** button on the right-hand side of the screen.

    !![Tenant Registration](10-tenant-registration.png)

4. When the pop-up opens, select **Copy and Close**.

    !![Tenant Registration](11-tenant-registration.png)

5. Then paste the URL into the **Domain** field on your **Desktop Agent Tenant Registration** window and choose **Save**.

    !![Tenant Registration](12-tenant-registration.png)

6. Select the tenant, choose **Activate** and confirm.

    !![Tenant Registration](13-tenant-registration.png)

7. Log in to your tenant with your user name or e-mail and password.

    !![Tenant Registration](14-tenant-registration.png)

8. The Agent should be in **Idle** state, waiting to start a project.

    !![Tenant Registration](15-tenant-registration.png)

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Add your agent)]

1. Navigate back to the **Application Development**.

2. Select **Settings**.

3. Choose **Agents Management** then **Add Agent**.

5. Select your agent.

6. Choose **Add agent**.

    !![Settings Agents Management](01-Settings-Agent-Management-Add-Agent-selected.png)

7. Your agent is now added and ready to run.

    !![Settings Agent add](01-Settings-Agent-Management-Add-Agent-Added.png)

[DONE]
[ACCORDION-END]



---
