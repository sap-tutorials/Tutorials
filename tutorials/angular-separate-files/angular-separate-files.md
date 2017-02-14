---
title: AngularJS - Separate the JavaScript and CSS files
description: Step #4: Move the CSS and JavaScript code from the HTML page to separate files on the web server.
tags: [  tutorial>beginner, topic>html5, topic>mobile, topic>odata, products>sap-hana-cloud-platform ]
---
## Prerequisites  
 - **Proficiency:** Beginner 
 - **Tutorials** Step #3 [Add the AngularJS JavaScript](http://www.sap.com/developer/tutorials/angular-add-javascript.html)

## Next Steps
 - **Tutorials** Step #5 [Add ODATA to the application](http://www.sap.com/developer/tutorials/angular-add-odata.html)

 
## Details
### You will learn  
In this tutorial series, we will explore another technology for Single Page Application (SPA) development - AngularJS (or just Angular).  Angular is a popular web framework in North America, and is used by many companies for both internal and client-facing systems.  These tutorials will parallel our SAPUI5 tutorials, building a visual interface using Angular, and connecting it to an OData back end service.

### Time to Complete
**15 Min**.

---
#### AngularJS series
**Step 4**: Connect an external OData feed to our application.  While doing that, we will also learn how to use an Angular *factory* to insert functionality in multiple places.  Finally, we will examine the Angular `$resource` factory, which helps us connect to REST resources.

---

### Move the JavaScript to a Separate File

First, let's move the JavaScript and CSS files out of our application, and in to a proper place in our project.

1.  Right click on the folder `HelloAngular` and select **New** --> **Folder**

    ![Add new JS folder](1-1.png)
    
    Name the folder `js`, and click **OK**
    
    ![Name the folder js](1-1b.png)

2.  Next, right click on the new `js` folder, and select **New** --> **File**

    ![Add new javascript file](1-2.png)

    Name this file `main.js` and click **OK**.

    ![Name the file main.js](1-2b.png)

3.  Next, copy this code in to the new `main.js` file.  (It is the same code that is in your `index.html` file.)  Then click **Save**.

    ```javascript
    var testData = [{
    	ProductName: "Test Product 1",
    	QuantityPerUnit: "100 units per box",
    	UnitPrice: "49.75",
    	Discontinued: false
    }, {
    	ProductName: "Test Product 2",
    	QuantityPerUnit: "20 cases per pallet",
    	UnitPrice: "168.77",
    	Discontinued: false
    }, {
    	ProductName: "Test Product 3",
    	QuantityPerUnit: "20 per box, 20 boxes",
    	UnitPrice: "4953.75",
    	Discontinued: false
    }, {
    	ProductName: "Test Product 4",
    	QuantityPerUnit: "65 individually wrapped",
    	UnitPrice: "112.50",
    	Discontinued: true
    }];
    
    angular.module("helloWorld", [])
    	.controller('helloController', helloController);
    
    function helloController($scope) {
    	$scope.productList = testData;
    }
    ```

    ![Copy the javascript to the new file](1-3.png)

4.  Next, we will delete the `<script>` tag out of the `index.html` file.  Select the entire text from `<script>` to `</script>`, and then press the delete key.  Save your file.

    ![Delete the javascript in the HTML file](1-4a.png)

5.  Insert a link to the new file, so that the `index.html` file loads your new JavaScript file.  

    Under the `</style>` tag, add the following HTML
    
    ```html
    <script src="/js/main.js"></script>
    ```

    ![Link the new JavaScript file to the HTML file](1-5.png)
    
6.  Run your application.  The page should look exactly the same, as the JavaScript now runs from an external file.

    ![Run - no changes should appear](run-app.png)

### Move the CSS file to a separate file

Follow the same procedures to move the CSS file

1.  Right click on the folder `HelloAngular` and select **New** --> **Folder**

    ![Add a new CSS folder](2-1.png)
    
    Name the folder `css`, and click **OK**

2.  Next, right click on the new `css` folder, and select **New** --> **File**

    ![Add a new CSS file](2-2.png)

    Name this file `main.css` and click **OK**.

3.  Next, copy this code in to the new `main.css` file.  (It is the same CSS that is in your `index.html` file.)  Then click **Save**.

    ```css
    .vertical-align {
        display: flex;
        align-items: baseline;
    }
    
    .vertical-align .top {
        align-self: baseline;
    }
    
    .vertical-align .center {
        align-self: center;
    }
    
    .available {
        color: green;
        font-weight: bold;
    }
    
    .discontinued {
        color: red;
        font-weight: bold;
    }
    ```

    ![Copy the CSS to the new file](2-3.png)

4.  Next, we will delete the `<style>` tag out of the `index.html` file.  Select the entire text from `<style>` to `</style>`, and then press the delete key.  Save your file.

    ![Delete the CSS in the HTML file](2-4.png)

5.  Insert a link to the new file, so that the `index.html` file loads your new JavaScript file.  

    Under the last `<script>` tag (the one you just inserted, add the following HTML
    
    ```html
    <link href="/css/main.css" rel="stylesheet">
    ```

    ![Link the CSS file to the HTML file](2-5.png)
    
6.  Run your application.  The page should look exactly the same, as the CSS file now loads from an external file.

    ![Run - no changes should appear](run-app.png)



## Next Steps
 - **Tutorials** Step #5 [Add ODATA to the application](http://www.sap.com/developer/tutorials/angular-add-odata.html)
