---
author_name: Chaitanya Priya Puvvada
author_profile: https://github.com/chaitanya-priya-puvvada
title: Install and Setup the Desktop Agent
description: Install and Setup the Desktop Agent to run your automation
auto_validation: true
time: 15
tags: [ tutorial>beginner, software-product>sap-business-technology-platform]
primary_tag: software-product>sap-process-automation
---

## Details
### You will learn

  - How to install Desktop Agent
  - How to register tenant in Desktop Agent

---

[ACCORDION-BEGIN [Step 1: ](Install the Desktop Agent)]

Among the SAP Process Automation, the Desktop Agent is an On-Premise component. Installing the Desktop Agent automatically installs the SAP Intelligent RPA web browser extension for Google Chrome and Edge.

>To prevent issues during the installation, please close all the Chrome tabs open on your machine. The setup program is provided in the form of an industry standard Windows MSI installer. You can download it from the SAP Software Center.

>The minimum version of the Desktop Agent supported by SAP Process automation is **2.0.20**.

1. Launch the MSI file execution.

2. Select **Next** to begin the installation process.

    !![Desktop Agent Installation](02-desktop-agent-installation.png)

3. On the **Installation type** pop-up, select the **Desktop Agent** option.

    !![Desktop Agent Installation](03-desktop-agent-installation.png)

4. Optional: Choose **Browse** to change the destination of the installation folder.

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

[ACCORDION-BEGIN [Step 2: ](Register the Desktop Agent Tenant)]

Once you've completed the installation steps of the SAP Robotic Process Automation setup wizard, you need to register your agent and connect it to an SAP Process Automation tenant in order to execute automations.

>Once the Desktop Agent is installed, the icon will be available on your System Tray.

1. Select the Desktop Agent icon, choose the 3 dots and select **Tenants**.
    !![Desktop Agent Installation](08-desktop-agent-installation.png)

2.  Choose **Add**
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



---
