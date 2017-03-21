---
title: End-to-End Weather App Scenario Part 2
description: Adding a simple web page to a Java app
tags: [ products>sap-cloud-platform, topic>cloud, topic>java, tutorial>intermediate]
---

## Prerequisites  
 - [End-to-End Weather App Scenario Part 1](http://www.sap.com/developer/tutorials/hcp-java-weatherapp-part1.html)

## Next Steps
 - [End-to-End Weather App Scenario Part 3](http://www.sap.com/developer/tutorials/hcp-java-weatherapp-part3.html)

## Details
### You will learn  
In this tutorial you will learn how to add a basic HTML page to a Java app.


### Time to Complete
**5 min**

---

1. Navigate to the **`webapp`** node in the Project Explorer and create a new HTML page by selecting the respective context menu entry: **New > HTML File**.

    Choose the name `index.html` and click on **Finish**.

    ![Creating a new HTML file](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part2/e2e_02-1.png)

2. Add the following line of code in between the opening and the closing <body> tag:

    ```html
    <p>Hello World!</p>
    ```

    ![Editing the body of the HTML file](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part2/e2e_02-2.png)

3. Save your changes and publish the update project to the local server by selecting the respective context menu on the server in the Server view.

4. Once the publishing is done you can go back to the internal browser window and remove the `/hello` part of the URL and hit the **Enter** key. Now, you should see the same "Hello World!" message for the URL <http://localhost:8080/weatherapp/> as well. The reason for this is the `<welcome-file-list>` in the `web.xml` (located in the `WEB-INF` directory underneath the `webapp` folder).

    ![Run the java app, but use the base URL, not the servlet](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part2/e2e_02-4.png)

5. For security reasons and for the sake of housekeeping you should remove all <welcome-file> entries except for the `index.html` entry from the `web.xml` file.

    ![Editing web.xml to remove welcom-file entries](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part2/e2e_02-5.png)


## Next Steps
 - [End-to-End Weather App Scenario Part 3](http://www.sap.com/developer/tutorials/hcp-java-weatherapp-part3.html)
