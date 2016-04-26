---
title: Internet of Things (IoT) Using the Tessel to post data
description: Part 5 of 8, Modify your Tessel code to now post data to SAP HANA
tags: [products>sap-hana, topic>big-data, topic>internet-of-things, tutorial>beginner ]

---

## Prerequisites  
 - **Proficiency:** Beginner
 - **Tutorials:** [Internet of Things (IoT) Check your data](http://go.sap.com/developer/tutorials/iot-part4-checking-data.html)


## Next Steps
 - [Internet of Things (IoT) Explore the SAP HCP IoT Services](http://go.sap.com/developer/tutorials/iot-part6-hcp-services.html)

## Details
### You will learn  
Now it’s time to modify a one of your existing Tessel module applications to insert data to HANA directly.
For this part you are going to add two JavaScript functions to either the `climate.js` or `ambient.js` files created in Part 1. The first function you will insert will perform an “HTTP POST” just like you did manually using Postman

### Time to Complete
**10 Min**.

---

1. Open `climate.js` or `ambient.js` (based on which module you have) in your editor. At the top of your existing code insert the following function:

    ```javascript
    // Connect to SAP HANA
    var http = require('http');
    var httpOptions = {
    host: 'XX.XX.XX.XX',
    port: 80,
    path: '/CODEJAMMER/johndoe/myiot/mydata.xsodata/DATA', method: 'POST',
    headers: {
                'Authorization': 'Basic Q09ERUpBTU1FUjpDb2RlSmFtMjAxNQ==',
                'Content-Type': 'application/json'
             }
    };
    ```

    ![HTTP request](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part5-inserting-tessel-data/p5_1.png)

2. The function creates an http object with the proper hostname or IP address of your server and the location of your XSODATA file you created. Insert the IP address for your server in this line: `host: 'XX.XX.XX.XX',`

    ![hostname](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part5-inserting-tessel-data/p5_2.png)

3. One difference between this and “Postman” is that you need to add in your “Authorization” line. In “Postman” you entered the username and password and refreshed the headers. This calculated the authorization hash. Switch back to your Postman window and copy the entire **Authorization** value from the header tab.

    ![basic authorization fron previous example](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part5-inserting-tessel-data/p5_3.png)

4. Paste the string you copied into the header section of the new function. Depending on your username and password, it may be the same as shown here.

    ![basic authorization](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part5-inserting-tessel-data/p5_4.png)

5. Update the `path` in your `httpOptions` variable file to match your name (e.g. replace `johndoe` in the path with your name).

    ![package specification](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part5-inserting-tessel-data/p5_5.png)

6. At the bottom of your JavaScript file, insert the function below. This function that takes your JSON formatted information and sends it via the HTTP connection you established.

    ```javascript
    // Generate JSON output and send over POST request to HANA
    // Brightness field added if the ambient module is used in the future
function updateHANA(temp,humid) {
        // catch HTTP request errors
        var req = http.request(httpOptions).on('error', function(err){ console.error(err); });
        var jsonData = {
            "ID": "1",
            "BRIGHTNESS": "27",
            "TEMPERATURE": temp,
            "HUMIDITY": humid,
        };
        var strData = JSON.stringify(jsonData);
        console.log(strData);
        req.write(strData);
        req.end();
    };
    ```

    ![function to update HANA](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part5-inserting-tessel-data/p5_6.png)
7. To invoke the new functions in your code you will insert a call to the `updateIoT` function where data is sent to the console log for output. For example in `climate.js`, insert the following just after the call to `console.log()`:

 ```javascript
updateIoT(temp.toFixed(4), humid.toFixed(4));
 ```

 ![call to function](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part5-inserting-tessel-data/p5_7.jpg)

8. Change the timeout from 300 to 3000 (3 seconds).

    ```javascript
    setTimeout(loop, 3000);
    ```

    ![loop](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part5-inserting-tessel-data/p5_8.png)

9. Before running your script, you need to connect the Tessel to the Internet. Run the following command line file (in your CMD or Terminal window) replacing `SSID` and `password` with the appropriate Wi-Fi credentials (if you are at home or the office, use what is appropriate):

    ```bash
    tessel wifi -n SSID -p password
    ```

    If you are successful, you should see something like this in the console/terminal:

    ![acquiring IP](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part5-inserting-tessel-data/p5_9.png)

10. Now run your code with the command below. You should see an output in your terminal/command window as shown below. You can refresh the query in the catalog to see the data inserted into HANA.

    ```bash
    tessel run climate.js
    ```
￼
    ![accessing sensor data](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part5-inserting-tessel-data/p5_10.png)
￼

11. In addition to viewing the data directly, you can view the OData service in a browser or application. To view the OData service in the browser, double-click the `mydata.xsodata` file to open it in your editor, then click the green **run** button. This will open a new browser tab to the OData service document.

    ![xsodata](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part5-inserting-tessel-data/p5_11.png)

12. The URL to the service document is shown below (for `johndoe`). To see your data, add `/HISTORY` to see the XML feed or `/HISTORY?$format=json` for JSON format to the end of the URL as shown here:

    - **Service Document:** http://52.90.177.151/CODEJAMMER/johndoe/myiot/mydata.xsodata
    - **XML Version:** http://52.90.177.151/CODEJAMMER/johndoe/myiot/mydata.xsodata/HISTORY
    - **JSON Version:** http://52.90.177.151/CODEJAMMER/johndoe/myiot/mydata.xsodata/HISTORY?$format=json
    The JSON formatted output:

    ![JSON results from xsodata](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part5-inserting-tessel-data/p5_12.png)

13. The XS OData service also allows you to build applications to display the data. You could do this with SAP Web IDE or any number of tools. To keep things simple, you will modify the `index.html` file that was added to your project when you did the **Create Application** step.
    Replace the existing file contents with the HTML/JavaScript contents below to add a table to the page. There is more than one way to do this so feel free to explore and discover!

    ```html
    <!DOCTYPE HTML>
    <html>
    <head>
    <meta http-equiv="X-UA-Compatible" content="IE=edge" />
    <meta charset="UTF-8"/>
    <title>My Sensor Data</title>
    <script id='sap-ui-bootstrap'
    src='/sap/ui5/1/resources/sap-ui-core.js'
    data-sap-ui-theme='sap_goldreflection'
    data-sap-ui-libs='sap.ui.core,sap.ui.commons,sap.ui.table'></script>

    <script language="JavaScript">
        var oModel = new sap.ui.model.odata.ODataModel("/CODEJAMMER/johndoe/myiot/mydata.xsodata/", false);
        var arrayHeaders = new Array();
        var ROW_COUNT = 40;

        oTable = new sap.ui.table.Table("test",{tableId: "tableID", visibleRowCount: ROW_COUNT});
        //Bring the table onto the UI
        oTable.placeAt("sensor_table");
        //Table Column Definitions
        var oMeta = oModel.getServiceMetadata();
        var oControl;
        for ( var i = 0; i < oMeta.dataServices.schema[0].entityType[0].property.length; i++) {
              var property = oMeta.dataServices.schema[0].entityType[0].property[i];

            oControl = new sap.ui.commons.TextField().bindProperty("value",property.name);
            oTable.addColumn(new sap.ui.table.Column({label:new sap.ui.commons.Label({text: property.name}), template: oControl, sortProperty: property.name, filterProperty: property.name, filterOperator: sap.ui.model.FilterOperator.EQ, flexible: true, width: "125px" }));
        }                      
        oTable.setModel(oModel);
        var sort1 = new sap.ui.model.Sorter("ID");
        oTable.bindRows("/DATA",sort1);
    </script>
    </head>
    <body>
       <div id="sensor_table"> </div>
    </body>
    </html>
    ```
    The web view is shown here. Note that since this pages uses the SAPUI5 JavaScript framework, this is now a responsive web app and if you were to open this on your tablet or smartphone you would see a similar view but it would fit better to the screen of your device.

￼    ![data output](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part5-inserting-tessel-data/p5_13.png)


## Next Steps
 - [Next tutorial in the series...](http://go.sap.com/developer/tutorials/hcp-create-destination.html)
