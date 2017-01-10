---
title: Hide view element from a standard SAP Fiori app
description: This tutorial shows you how to hide a view element from a standard SAP Fiori app.
tags: [  tutorial>beginner, topic>cloud, topic>sapui5, products>sap-web-ide ]
---
## Prerequisites  
 - **Proficiency:** Beginner
 - **Tutorials:** [Getting started with the SAP Fiori, Demo Cloud Edition](http://www.sap.com/developer/tutorials/hcp-fiori-cloud-edition-start.html)

## Next Steps
 - [Add extended app to SAP Fiori launchpad](http://www.sap.com/developer/tutorials/hcp-fiori-cloud-edition-launchpad.html)

## Details
### You will learn  
In this tutorial you will learn how to hide a view element from a standard SAP Fiori app. The app that you will extended in this tutorial is "My Leave Requests". The key steps are:

- Start the extension using the extension pane in SAP Web IDE 
- Locate the view element to hide
- Test the app

### Time to Complete
**10 Min**.

---

1. Please make sure that you have worked through the tutorial [Getting started with the SAP Fiori, Demo Cloud Edition](http://www.sap.com/developer/tutorials/hcp-fiori-cloud-edition-start.html). Log into the SAP Fiori Demo Cloud Edition and locate the group **Human Capital Management**. Click on the **My Leave Requests** tile.

    ![SAP Fiori launchpad group Human Capital Management](Launchpag-My-Leave-Requests.png)

2. Open the Standard app and familiarize yourself with how it appears. To start extending the app click on the gear wheel icon on the top right beside your name and choose **Develop Apps**:

    ![Options menu](8.png)

3. You will be forwarded to a screen called "Create App Extension" where you find instructions for the next steps. Click the **Launch SAP Web IDE** button. You may have to click this button twice as the new tab may remain empty on the first attempt.

    ![Create App Extension](Create-App-Extension.png)

4. You have to login to the SAP HANA Cloud Platform. Please provide your credentials and then click **Login**.

    ![Login to the SAP HANA Cloud Platform](Login-to-SAP-HANA-Cloud-Platform.png)


5. Click **OK** to accept the project name.

    ![Extension Project Name](Extension-Project-Name.png)

6. The SAP Web IDE is launched with your extension project created.

    ![Project in SAP Web IDE](Project-in-SAP-Web-IDE.png)

7. With your extension project folder selected, the graphical extensibility pane is the easiest way to preview the app and extend it. Start it via **Tools > Extensibility Pane**. 

    ![Extensibility Pane](Extensibility-Pane.png)

8. In order to select the view/control to extend, change from **Preview Mode** to **Extensibility Mode**.

    ![Switch to Extensibility Mode](Switch-to-Extensibility-Mode.png)

9. You'll see that hovering over the views/controls on the left highlights them in sync in the Outline pane on the right.

    ![Outline](Outline.png)

10. Clicking a control in the graphical pane or in the Outline pane will change its color in the graphical pane to pink and highlights it in the Outline pane in blue. You can continue to hover over the graphical pane.

    ![Control selected](Control-selected.png)

11. There are several ways to extend a control. You will use the Outline pane to make the change. Select the **Legend** either in the graphical pane or the Outline pane. Go to the Outline pane, **right click** the blue highlighted item `LRS4_LEGEN` and select **Hide Control**

    ![Hide Control](Hide-Control.png)

12. You receive a notification that the change was implemented. Click **Refresh** to view the change. 

    ![Control was hidden](Control-was-hidden.png)

13. Note that the legend is now gone from the graphical pane, but still present (listed as hidden) in the outline pane.

    ![legend is gone from the graphical pane but present in the outline pane](Outline-shows-hidden-status.png)

14. An interesting feature of the Web IDE is to view a list of controls that are extended. Select **Show extended elements** in the Outline Pane filter pulldown menu. 

    ![Show extended elements](Show-extended-elements.png)

15. You will see that the legend you just hid is the only control shown.

    ![Show extended elements details](Show-extended-elements-details.png)

Congratulations, you've successfully hid a view element from a SAP Standard Fiori app.

## Next Steps
 - [Add extended app to SAP Fiori launchpad](http://www.sap.com/developer/tutorials/hcp-fiori-cloud-edition-launchpad.html)
