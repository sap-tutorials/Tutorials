---
parser: v2
auto_validation: true
time: 20 minutes
tags: [software-product>sap-fiori, software-product>sap-fiori-tools, tutorial>beginner, software-product>sap-fiori, software-product>sap-business-application-studio, software-product-function>sap-cloud-application-programming-model, software-product>sap-business-technology-platform]
primary_tag: software-product>sap-fiori
---
# Prepare Your Development Environment for SAP Fiori Elements
<!-- description --> Set up your development environment with SAP Business Application Studio to create an SAP Fiori elements application based on the SAP Cloud Application Programming Model.

## Prerequisites
- You need a trial account on SAP Cloud Platform. If you don't have one, follow the instructions in: [Get a Free Trial Account on SAP Cloud Platform](hcp-create-trial-account).
- Ensure that you have started SAP Business Application Studio in your SAP Cloud Platform trial account. For detailed instructions: [Set Up SAP Business Application Studio for Development](appstudio-onboarding).

## You will learn
- How to set up SAP Business Application Studio for SAP Fiori elements application development
- How to create a project containing the service needed for generating your sample application

## Intro
Click [here](https://cap.cloud.sap/docs/about/) for more information about the SAP Cloud Application Programming Model.

---

### Create development space


1. On the SAP Business Application Studio start page, click **Create Dev Space**.

2. On the following page, enter a name for your new development space and choose the application type **Full Stack Cloud Application** from the list.

    Click **Create Dev Space**.

    <!-- border -->![Start the Dev Space](create-dev-space-BAS.png)

    Your development space is now ready to use. Wait until the status has changed from **STARTING** to **RUNNING**. After the initial creation this is done automatically.

    >In case your development space was stopped, you can restart it by clicking the start button (for example after a longer idle time).

3. Open the development space by clicking on its name.


&nbsp;

### Clone the demo service


Once you are in the development space, you will see a **Welcome** page from which you can create the application project.

1. Copy the following GitHub repository URL into your clipboard:

    ```URL
    https://github.com/SAP-samples/fiori-elements-incident-management.git
    ```

2. Click the link **Clone from Git**.

    <!-- border -->![Click on link "Clone from Git"](click-clone-from-git.png)

    Paste the repository link into the input field and press **Enter**.

    <!-- border -->![Enter the github repository URL](enter-github-repository.png)

3. Wait until the cloning has finished. When you see a toast message in the lower right corner, click **Open** to open the project.

    You see your project in the explorer panel as shown in the image below:

    <!-- border -->![Explorer service structure](explorer-project-tree.png)


### Complete the service


1. From the menu **Terminal**, select **New Terminal**

    Ensure that your terminal prompt shows **fiori-elements-incident-management**. At the terminal prompt, enter **`npm install `** and press **Enter**. This command will download and install all necessary modules from the npm package repository required to run the SAP Fiori elements application.

    <!-- border -->![Enter npm install](enter-npm-install.png)

2. After the installation is complete, enter **`npm i @sap/cds-dk -g`** at the terminal prompt.

    This will start the command line client and development toolkit for the SAP Cloud Application Programming Model. You will need this client to run your SAP Fiori elements application in the tutorial [Generate a Fiori elements application](fiori-tools-cap-create-application). For more information about the command line client, refer to the documentation pages of [SAP Cloud Application Programming Model](https://cap.cloud.sap/docs/get-started/).

Your development environment is now ready.

In the next tutorial, you will create an SAP Fiori elements application and run it inside the SAP Business Application Studio.

<!---
Comment needed for md update. Can be deleted next time
-->


### Test yourself



