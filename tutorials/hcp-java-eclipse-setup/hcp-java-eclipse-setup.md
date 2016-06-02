---
title: Configuring Eclipse with SAP HANA Cloud Platform Tools for Java
description: A step-by-step procedure to configure the Eclipse IDE with for Java development on SAP HANA Cloud Platform
tags: [  tutorial>beginner, topic>cloud, topic>java, products>sap-hana-cloud-platform ]
---
## Prerequisites  
 - **Proficiency:** Beginner | Intermediate | Advanced
 - **Tutorials:** [Sign up for an account on HANA Cloud Platform](http://go.sap.com/developer/tutorials/hcp-create-trial-account.html)

## Next Steps
 - Developing and Deploying a basic Java application on SAP HANA Cloud Platform (coming soon)

## Details
### You will learn  
In this tutorial you'll learn everything you need to know to setup your local development environment based on Eclipse. The main steps are:

 - Downloading Eclipse
 - Installing SAP HANA Cloud Platform Tools for Java in your Eclipse IDE
 - Installing the SAP HANA Cloud Platform Software Development Kit

### Time to Complete
**15 Min**.

---

1. To make use of the SAP HANA Cloud Platform Tools for Java you first need to have a supported version of Eclipse installed on your computer. The Eclipse Mars version is recommended. 

    [Open the Download site for Eclipse](http://eclipse.org/downloads) and click on the **Eclipse IDE for Java EE Developers** link

    ![Eclipse download page](jav100-1-find-eclipse-mars.png) 

2. Choose the operating system that you will use to run Eclipse and choose the download site:

    ![Eclipse download page](jav100-1-choose_os.png) 

 
3. Choose the preferred download site and start the download.

4. Once the download has finished extract the archive to a local folder of your choice (e.g. `c:\dev\eclipse`).

 


5. Click on the **eclipse** executable file to start the Eclipse IDE.

    ![Start Eclipse](jav100-1-start_eclipse.png)


6. Eclipse will first show you a **Workspace Launcher** dialog to choose your workspace. Replace the suggested workspace path with `c:\dev\eclipse_workspace`. Confirm with **OK**.

    ![Create Eclipse workspace](jav100-1-create_workspace.png)


7. Close the Eclipse **Welcome Page**.

    ![Close welcome page](jav100-1-close_welcome.png)



8. Now that you have installed Eclipse, you need to install the SAP HANA Cloud Platform tools for Java. This is done following the standard approach of Eclipse to install plugins. 

    From the Eclipse menu, choose **Help > Install New Software...**
    
    ![Start software install](jav100-1-start_install.png)
    
9. Copy the URL `https://tools.hana.ondemand.com/mars` and paste it in the **Work with **field and then press the **Enter** (or **Return**) key.

    ![Add update site](jav100-1-add_update_site.png)
    
10. Select **SAP HANA Cloud Platform Tools** and click **Next**.

    > Note: If Eclipse is not able to find the tools then please check your network settings. You might need to configure a proxy, in particular if you are working from a corporate network. How-to setup a proxy in Eclipse is explained in the [Installing SAP Development Tools for Eclipse](https://help.hana.ondemand.com/help/frameset.htm?76137a37711e1014839a8273b0e91070.html) section of the official online documentation.

    ![Choose HCP Tools](jav100-1-choose_sap_hana_cloud_platform_tools.png)


11. On the **Install Details** page click **Next**.

    ![Install details](jav100-1-install_details.png)


12. Read and accept the license agreement and choose **Finish**. The installation will now start.

    ![Install details](jav100-1-accept_license.png)


13. During the installation, a **Security Warning** dialog box will appear stating that you are installing software which contains unsigned content. Confirm with **OK** to continue the installation.

    ![Confirm security warning](jav100-1-confirm_security_warning.png)
    
14. At the end of the installation, you will be asked to restart Eclipse. Confirm the dialog with **Yes** to restart Eclipse immediately.

    ![Restart Eclipse](jav100-1-restart_eclipse.png)

15. After Eclipse restarts, make sure to close the Eclipse **Welcome Page**. You now have the SAP HANA Cloud Platform Tools for Java installed in Eclipse.

    ![Restart Eclipse](jav100-1-close_welcome_2.png)

16. The next step is to download and install the SAP HANA Cloud Platform SDK, which contains all the required artifacts to setup a local development environment as well as the [console client](https://help.hana.ondemand.com/help/frameset.htm?76132306711e1014839a8273b0e91070.html) used to interact with your cloud account.

    The SDK comes in different flavors:

    - Java Web: Provides a lightweight runtime supporting a subset of the standard Java EE APIs (Servlet, JSP, JSTL, EL). Currently there is a `1.x` and a `2.x` version of this runtime available
    - Java EE 6 Web Profile: Provides certified support for the whole Java EE 6 Web Profile APIs
 
    For this tutorial you will use the `Java Web SDK 1.x` version of the SDK. To install it on your system do the following:

    Open <https://tools.hana.ondemand.com#cloud>. Make sure you are on the Cloud tab of the page.
    
    ![Cloud tools page](jav100-1-open_tools_page.png)


17. Choose the `Java Web (neo-java-web-sdk.X.X.X)` (use the latest version) for download.

    > Note: The version of the Java Web SDK shown on this screenshot might be lower than the one that you will actually download. This is OK. The procedure should work with any higher version as well. Just make sure that you always use the SDK version that you actually downloaded in the following steps.

    ![Choose SDK](jav100-1-choose-sdk-197.png) 

18. Before you are allowed to download the SDK you have to read and agree to the SAP Developer License agreement. After accepting the license by clicking **I Have Read and Agree** the download of the SDK will start.

    ![Accept SDK License](jav100-1-accept_license_2.png)  

19. Once the download has finished extract the archive to a local folder of your choice. It is recommended to place the folder in `c:\dev` and to name it like the just downloaded file, e.g. `c:\dev\neo-java-web-sdk-1.63.20.7`

    > Note: To familiarize yourself further with the content of the SDK, especially note the location of the [console client](https://help.hana.ondemand.com/help/frameset.htm?76132306711e1014839a8273b0e91070.html) within the tools directory and the provided samples in a respective samples directory. Also, you might be interested to learn about the [supported APIs](https://help.hana.ondemand.com/help/frameset.htm?e836a95cbb571014b3c4c422837fcde4.html).
    
    
20. The next thing to do is to configure the SAP HANA CLoud Platform Tools for Java so they make use of the just downloaded SDK. From the Eclipse IDE main menu, choose **Window > Preferences**.

    ![Open preferences](jav100-1-open_preferences.png) 

21. Choose **Server > Runtime Environment**. Click the **Add...** button to open the **New Server Runtime** dialog.

    ![Open runtime environment](jav100-1-open_runtime_env.png)
    
22. Select **SAP > Java Web** as the Server Runtime Environment and click **Next**.

    ![Choose runtime](jav100-1-choose_server_runtime.png)

23. Provide the folder to which you have extracted the SDK by clicking the **Browse...** button and choosing the respective folder, e.g. `c:\dev\neo-java-web-sdk-1.63.20.7`. Click on **Finish**.

    ![Provide SDK](jav100-1-provide_sdk.png)
    
24. With this a new Java Web runtime is now available that will be used for your SAP HANA Cloud Platform projects. You can now close the dialog by clicking **OK** 

    ![Java Web Added](jav100-1-javaweb_added.png)
    
Congratulations: You have now installed the SAP HANA Cloud Platform Tools for Java and are ready to start with your Java project on SAP HANA Cloud Platform.





### Related Information
 - [Eclipse Downloads](http://www.eclipse.org/downloads)
 - (Tools) [Installing the SDK](https://tools.hana.ondemand.com/#cloud)
 - (Online documentation) [Installing Eclipse](https://help.hana.ondemand.com/help/frameset.htm?761374e5711e1014839a8273b0e91070.html)
 - (Online documentation) [Installing the SDK](https://help.hana.ondemand.com/help/frameset.htm?7613843c711e1014839a8273b0e91070.html)
 - (Online documentation) [Installing SAP Development Tools for Eclipse](https://help.hana.ondemand.com/help/frameset.htm?76137a37711e1014839a8273b0e91070.html)
 - (Online documentation) [Setting Up the Runtime Environment](https://help.hana.ondemand.com/help/frameset.htm?7613f000711e1014839a8273b0e91070.html)

## Next Steps
 - Developing and Deploying a basic Java application on SAP HANA Cloud Platform (coming soon)
