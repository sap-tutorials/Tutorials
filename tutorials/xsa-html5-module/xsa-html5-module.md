---
title: SAP HANA XS Advanced, Creating an HTML5 Module
description: Part 2 of 3, Create your first HTML5 module for HTML5 content within your XSA application
tags: [products>sap-hana, topic>big-data, tutorial>beginner ]

---

## Prerequisites  
 - **Proficiency:** Beginner
 - **Tutorials:** [SAP HANA XS Advanced Connecting to SAP Web IDE for SAP HANA](http://go.sap.com/developer/tutorials/xsa-connecting-webide.html)

## Next Steps
 - [SAP HANA XS Advanced Creating an HDI Module](http://go.sap.com/developer/tutorials/xsa-hdi-module.html)


## Details
### You will learn  
You will now create the HTML5 module to add basic web based content to your XSA application.


### Time to Complete
**15 Min**.

---

1. Launch the SAP Web IDE for SAP HANA at the following URL in your web browser. The `hostname` of course is the hostname of the SAP HANA Developer Edition that you created in the previous tutorial. Remember for XSA you will need to use the hostname and not the IP address of the server, instructions are found on the server landing page itself.

    `http://<hostname>:53075/`

    User: `WORKSHOP_01`
    Password: `HanaRocks2016`

    or you can use

    User: `CODEJAMMER`
    Password: `CodeJam2016`

    ![Login](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/xsa-html5-module/1.png)

2. We will begin by opening your existing project `DEV602`

3. Now create the HTML5 module to host and serve out the front end content. Begin by selecting your project and then choosing `New -> HTML5 Module` 

    ![New Module](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/xsa-html5-module/2.png)

4. Name the module `web`. Press Next. Then press Finish.

    ![Module web](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/xsa-html5-module/3.png)

5. This not only adds the necessary metadata to the web folder but also maintains the module entry in the project’s `mta.yaml` file within the project root.

    ![Module web](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/xsa-html5-module/4.png)

6. You need to add `dev602_01-uaa` as a dependent service/resource to the `mta.yaml` file. This is because you have user authentication required to access your application. Save your file. Remember the YAML format is very position sensitive so no tabs and be careful of how many spaces you use.

    ```
	ID: dev602
	description: DEV602 Group 01 First XS Advanced App
	version: 0.0.1
	
	modules:
	- name: web
	  type: html5
	  path: web
	  requires:
	      - name: dev602_01-uaa
	      
	resources:
	  - name: dev602_01-uaa
	    type: com.sap.xs.uaa
	    parameters:
	      config_path: ./xs-security.json  

    ```


7. In future, adding the resource declaration to the MTA descriptor will allow the tools to automatically provision these resources at runtime. But this functionality is not yet provided for UAA services.  Therefore you have to use the XS command line client tool to perform this step. From a command line type the following to connect to remote XSA server.
	
	```
	 xs login -a http://<hostname>:30030 -o hanapm -s SAP -u WORKSHOP_01 -p HanaRocks2016 
	```

If you have used the CODEJAMMER user then the command would look different.    

	```
	 xs login -a http://<hostname>:30030 -o hanapm -s SAP -u CODEJAMMER -p CodeJam2016 
	```
    
    ![Command line XS Login](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/xsa-html5-module/5.png)

8. To create the UAA service issue the following command:

	```
	xs create-service xsuaa default dev602_01-uaa
	```

    ![Command line UAA](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/xsa-html5-module/6.png)


9. The web folder in your project contains the resources that will be served out by this HTML5 module. This HTML5 module manages all HTML/client side UI resources (in the resources folder) and performs the task of reverse proxy for all other internal services. This way you have a single HTTP endpoint and avoid any CORS issues. The Add Module wizard already placed a simple index.html with `Hello World` in the resources folder. 

    ![Default Hello World](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/xsa-html5-module/7.png)

10. The HTML5 module is configured via the file `xs-app.json`. In this file we can map the routes to destinations we defined in the `mta.yaml`. We can also set authentication and other options. Go ahead and change the `authenticationMethod` to route. 

	```
	{
		"welcomeFile": "index.html",
		"authenticationMethod": "route",
		"routes": [ ]
	}
	```

11. Your initial development is done and you are ready to deploy your application onto the XS Advanced server. Highlight the web folder and press Run. This will perform a build, then deploy the service onto the server.  If successful it will open a new browser tab to the default page of this web service. 

    ![Run your application](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/xsa-html5-module/8.png)

12. It can take a minute or two for the first build/deploy/run operation to complete.  Upon completion you should see that the service status has changed to Running and there is a hyper link to the run logs.  

    ![Application executing](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/xsa-html5-module/9.png)

13. Switch your browser tab and you should see the authentication prompt for your application. Login with the same user credentials you used to log into the SAP Web IDE for SAP HANA. Provided you are not extremely quick in which case your browser may still hold your session and you may not need to log in again. Authentication at the XS level is now done by referencing a user stored. This can be configured to be the HANA database or it can be an external user directory. 

    ![Login](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/xsa-html5-module/1.png)

14. After successful authentication, you should see your `index.html` with the Hello World button. 

    ![Index page loaded](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/xsa-html5-module/10.png)



## Next Steps
 - [SAP HANA XS Advanced Creating an HDI Module](http://go.sap.com/developer/tutorials/xsa-hdi-module.html)
