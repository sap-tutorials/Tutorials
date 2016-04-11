---
title: Connecting SAP Web IDE to the Hybrid App Toolkit
description: Start the Hybrid App Toolkit and test the connection with SAP Web IDE
tags: [  tutorial:interest/cloud, tutorial:interest/mobileDevices, tutorial:product/hcp, tutorial:product/mobile, tutorial:product/sapui5_web_ide ]
---
## Prerequisites  
 - **Proficiency:** Intermediate
 - **Tutorials:** 
   - [Configure an SAP Web IDE project for hybrid builds](http://go.sap.com/developer/tutorial-contribution/hcpms-webide-hybrid-config.html)
   - You must have completed the download and [installation procedure](https://help.hana.ondemand.com/webide_hat/frameset.htm?d2865598e67f4ddabc79e5943352b0a1.html) for the Hybrid App Toolkit

## Next Steps
 - [Build and run a hybrid app with SAP Web IDE and Hybrid App Toolkit](http://go.sap.com/developer/tutorial-contribution/hcpms-webide-hybrid-build.html)

## Details
### You will learn  
In this tutorial, will start the Hybrid App Toolkit Connector and test the communication between it and SAP Web IDE. You must have completed the download and installation process for the Hybrid App Toolkit before starting this section. 

### Time to Complete
**< 5 Min**.

---

1. Open a Terminal or command window and navigate to the HAT directory.

2. Start the HAT server using one of the methods below:

    - On Windows, double-click `run.cmd`
    - On a Mac, execute the `./run.sh` command
    
    > Note: On a Mac, you may need to change file permissions `sudo chmod +x ./run.sh`. You may also need to change the owner by running `sudo chown –R <your user> .` from inside the HAT directory

3. Enter your key store password (same password you used when you installed HAT) when prompted. Your terminal/command window will look similar to the picture below if HAT started properly.

    ![HAT server running](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcpms-webide-hat-connection/3.png)

4. To test the connection between HAT and your Web IDE instance, open Web IDE and select the Web IDE menu option: **Tools > Preferences**. 


5. Select **Hybrid Application Toolkit** in the left-hand navigation bar and confirm that the **Port** number shown is the same that HAT is listening on (shown in the output in step 2).

    ![HAT preferences tab](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcpms-webide-hat-connection/5.png)

6. Click on Test Connection button.  The test results are displayed below the Test Connection button.

    ![HAT test successful](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcpms-webide-hat-connection/6.png)

7. Go back to the main project view in Web IDE by selecting the menu option: **Tools > Development**.

 
## Troubleshooting
If the connection fails, the likely causes are a browser security exception or a misconfiguration. Work through the steps below in order, reload Web IDE and retest the connection after each step.  

1. Determine if you need to add an exception to your browser settings by opening a browser tab to: <https://localhost:9010> and follow the prompts needed to add the exception (typically this is to acknowledge the risk, add exception, and confirm exception).

2. Open the `SAP_HAT_local-1.xx.x/config.json` file and confirm that the `webIdeHosts` URL is correct, and the `port` number and `apiKey` match what shows up on in the Web IDE preferences view.

    ```xml
    "webIdeHosts" : [ "https://webide-<p-numbertrial>.dispatcher.hanatrial.ondemand.com" ],
	"port" : 9010,
	"apiKey" : "X2gr91J4ihu60pN8kwbV7",
	```
 
## Next Steps
 - [Build and run a hybrid app with SAP Web IDE and Hybrid App Toolkit](http://go.sap.com/developer/tutorial-contribution/hcpms-webide-hybrid-build.html)
