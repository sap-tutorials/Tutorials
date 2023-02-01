---
parser: v2
author_name: Chaitanya Priya Puvvada
author_profile: https://github.com/chaitanya-priya-puvvada
keywords: RPA
auto_validation: true
time: 15
tags: [ tutorial>beginner, software-product>sap-business-technology-platform]
primary_tag: software-product>sap-intelligent-robotic-process-automation
---

# Install SAP Intelligent RPA On-Premise Components
<!-- description --> Install and configure essential development tools required to set up and execute an automation.

## Prerequisites
- [Subscribe to SAP Intelligent RPA Service in SAP BTP](irpa-setup-1-booster-subscription)
- [Technical prerequisites and system requirements](https://help.sap.com/viewer/6b9c8e86a0be43539b670de962834562/Cloud/en-US/0061438816a34fa78b77c99852318c70.html)


## You will learn
- How to use tools required for execution of your automation
- How to enable the SAP Intelligent RPA Browser Extension
- How to configure SAP Intelligent RPA Cloud Factory
---

### Download the SAP Intelligent RPA MSI

The Desktop Agent is a component of SAP Intelligent Robotic Process Automation that is installed locally on user desktops. It executes automation projects that launch and run applications of various kinds.

Download the latest version of the SAP Intelligent RPA MSI from [SAP Development tools](https://tools.hana.ondemand.com/#cloud).

<!-- border -->![Desktop Agent installation](Step1-Desktop Agent Installation.png)


### Install components

1. Double-click on the MSI, and choose **Next**.

2. Choose the components to install and choose **Next** for the following steps.

3. Wait for the installation to complete.

4. Choose **Finish**.

  ![Desktop Agent Installation Progress](Step2-AgentInstallationProgress.png)

> See section [Installing SAP Intelligent RPA On-Premise Components](https://help.sap.com/viewer/6b9c8e86a0be43539b670de962834562/Cloud/en-US/c76545a9a5d1496db5d28039908cb28a.html) in the SAP Intelligent RPA documentation for more details.


### Register Desktop Agent Tenant

1. In the Windows search bar, search for **Desktop Agent** and choose it.

    <!-- border -->![DesktopAgent Search](Step3-AgentSearch.PNG)

2. Register the tenant in the Desktop Agent.

3. Enter a name for your tenant.

4. Copy the domain of the Cloud Factory URL.

    > This is the URL you saved in the previous tutorial, obtained after subscribing to SAP Intelligent RPA.

4. Paste it in the domain area.

5. Choose **Finish**.

   <!-- border -->![Agent Configuration](Step3-Agent Configuration.PNG)

Log in with your email account and password which you have used to subscribe for SAP BTP.


### Activate Desktop Agent Tenant

1. Choose **Tenants**.

    <!-- border -->![Tenants](Step4-Tenants.png)

    A list of tenants is displayed.

2. Choose **Activate** to activate the trial account.

    <!-- border -->![Tenant Activation](Step4-TenantActivation.png)

3. Open **SAP Intelligent RPA Cloud Factory** URL in a browser window.

4. Choose the **Agents** tab and check that your agent is listed with status **Idle**.

    <!-- border -->![Agent Idle](AgentIdle.png)


### Enable SAP Intelligent RPA Browser Extension

> Installing SAP Intelligent RPA on-premise components automatically installs the web browser extension for Google Chrome/Edge.

Navigate to the extensions of the Google Chrome Browser and enable the **SAP Intelligent RPA Extension**.

  <!-- border -->![RPA Extension](Step6-RPABrowserExtension.png)


### Configure Cloud Factory: Create an agent group

Once connected to the Cloud Factory, you need to declare your Desktop Agent in an agent group.

> Agent groups allow you to organize your agents in a parent-child relationship, using either:

>- The **computer name** on which the agent is installed.

>- The **login name** of the person who will connect to the agent


1. Create an agent group, as follows:

    - Choose the dropdown menu of the **Agents** tab and select the **Agent groups** tab.

    - Choose **New Agent Group**.

    - In the **Name** field, enter a name for your agent group.

    - Under **Type**, select **Login**.

    - Choose **Create**.

    <!-- border -->![Create Agent Group](Step5-create-agentgroup.png)

2. Add a node to the agent group, as follows:

    - In the agent group details view, choose the `+` button to add a new node to the group.

    - Set **All** as the name.

    - Choose **Create**.

    <!-- border -->![Add Agent group](step5-add-node-group.png)

3. Prepare to add an agent to the agent group, as follows:

    - Choose the new created node to select it.

    - Choose the **+** button to add a new agent.

    - Enter your login in the **Name** field.

    - Enter a name for the new agent in the **Label** field.

    - Choose **Create**.

    - Choose **Save**.

    <!-- border -->![Add new agent](step5-add-new-agent.png)


### Configure Cloud Factory: Create an environment

> An environment represents the functional landscape in which SAP Intelligent RPA is deployed.

1. Choose the **Environments** tab.

2. Choose **New Environment**.

3. In the **New Environment** popup, enter the following information:

    |  Field Name     | Value
    |  :------------- | :-------------
    |  Name           | A name for the environment
    |  Description    | A description for the environment
    |  Type          | **Test**

6. Choose **Create**

  <!-- border -->![Create Environment](step5-create-environment.png)


### Configure Cloud Factory: Add agent to environment

> An agent is a local component consisting of a computer system (PC, desktop or server) and a user session that executes an automation scenario.

1. In your new environment page, choose the **Agents** section.

2. Choose **+ Add Agent**.

3. Choose **Agent groups**.

4. Select your agent group.

5. Under **Agent group node**, select the **All** node.

6. Choose **Add agent group node** to add the new environment.

    <!-- border -->![Add Agent](step5-add-agent.png)

You have successfully installed and configured the Desktop Agent, completed the settings in SAP Intelligent RPA Cloud factory and ready to design your automation.


---
