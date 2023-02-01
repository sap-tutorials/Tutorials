---
parser: v2
author_name: Chaim Bendelac
author_profile: https://github.com/chaim32
keywords: SAP Graph
auto_validation: true
time: 20
tags: [ tutorial>beginner, software-product>sap-business-technology-platform]
primary_tag: software-product>sap-integration-suite
---
# Hello Graph! Write Your First SAP Graph Application
<!-- description --> Get a brief introduction to SAP Graph, including the SAP Graph Navigator and programming basics.

## Prerequisites
 - You will use **Node.js** and **npm** to build this application, and to keep it extremely simple, you will use the **Express** web framework. Not familiar with Node.js and Express? Refer to section [Express/Node introduction](https://developer.mozilla.org/en-US/docs/Learn/Server-side/Express_Nodejs/Introduction), which also explains how to easily set up a Node development environment, whether you use Windows, MacOS or Linux.
 - If you prefer to develop in the cloud without installing anything on your computer, SAP offers a great cloud-based development experience alternative in the form of the SAP Business Application Studio. Follow [this tutorial](appstudio-onboarding) for more details on getting your own instance.
 - The rest of this tutorial assumes that you have installed Node.js and npm on your computer.

## You will learn
  - What SAP Graph entails
  - How to navigate connected data entities in the SAP Graph Navigator
  - `hello world` programming essentials to try out SAP Graph

---

### Introduction


[SAP Graph](https://community.sap.com/topics/graph) is a new unified API for SAP. It addresses run-away API complexity and integration challenges by providing developers a single connected and unified view of all their business data, consolidating the data models of data sources like SAP S/4HANA, SAP Sales Cloud and SAP SuccessFactors into one, unified and connected, data model, representing all the data in a landscape. This is called the Business Data Graph.

<!-- border -->![SAP Graph Overview](SAP-Graph-overview_sm.png)

A data graph is a connected graph, with data entities serving as the nodes of the graph and edges that represent relationships (which are called associations) between those entities. The nodes of the Business Data Graph are the common enterprise business objects, like Customer, Supplier, Product, Sales and Purchase Order, General Ledger, etc.

SAP Graph was productively released for early adopters in December of 2021. Dedicated support for early adopters of this innovative solution is available. [Sign up to be an early adopter](https://influence.sap.com/sap/ino/#campaign/2940) of SAP Graph or contact us at sap.graph@sap.com if you are interested.

---
### Explore SAP Graph on the SAP Graph Navigator


The SAP Graph API is documented in the [SAP Graph Navigator](https://navigator.graph.sap/entities).

1. Open the link to the [SAP Graph Navigator](https://navigator.graph.sap/entities) application to explore the API.

    The index, on the left of the application, shows you over 1,500 entities grouped by data source. The first group of entities are the unified entities. Here is an example of a unified entity, `sap.graph/WorkAssignment`.

    <!-- border -->![WorkAssignment unified entity in SAP Graph Navigator](blog1_navigator_sm.png)

    In this tutorial, you will create a small application that fetches the unified entity `SalesQuote`.

2. Use the SAP Graph Navigator to look up the definition and corresponding attributes for `SalesQuote`.

---
### Prepare package.json


1. Select a folder (directory) where you want to develop your application. In this folder, you will create a small file called `package.json`, with the following content:

    ```JSON
    {
        "name": "hello-graph",
        "version": "0.0.0",
        "private": true,
        "dependencies": {
            "express": "4.17.1",
            "node-fetch": "2.6.1",
            "universal-cookie": "4.0.4"
        }
    }
    ```

2. After saving the file, run the following command on your console:

    ```Shell / Bash
     npm install
    ```

    If your Node.js environment was properly set up, then this command will install a few standard library packages in a new sub-folder called `node_modules`.

3. Now, create another sub-folder, next to `node_modules`, called `src`. This is where you will develop your code.

---
### Prepare graph.js

First, you will create a boilerplate file in the `src` folder, called `graph.js`. This file will contain a class called `Graph`, which provides a nice wrapper for using SAP Graph. A `get()` function will read the data.

1. Paste the following code into the file and save it. As you can see, it is very simple, and takes advantage of the node-fetch package.

    ```JSON
    const fetch = require("node-fetch");
    class Graph {
        async get(req, entity, params) {
            const url = `https://sandbox.api.sap.com/sapgraph/${entity}${params ? `?${params}` : ""}`;
            console.log(url) //for debugging
            const options = {
                method: "get",
                headers:{
                    "Accept": "application/json",
                    "apiKey": "your APIkey"
                }
            };
            const response = await fetch(url, options);
            console.log(`${response.status} (${response.statusText})`) // for debugging
            const json = await response.json();
            return json;
        }
    }

    module.exports = Graph;
    ```

    > Note that the SAP Graph server that uses SAP API Business Hub sandbox is hard-coded. This allows you to focus here on the data access API, without requiring you to first configure your own SAP Graph business data graph and deal with all the complex aspects of security and authentication, which will be the subject of future tutorial parts.

    Since you are accessing the sandbox landscape data via SAP API Business Hub, you will need to insert your API Key (a short string) into the following code. Where do you get this key?

2. Go and login to <https://api.sap.com/settings> and choose **Show API Key** to see and save it.

---
### Write a simple application


Now you are ready to write your first, simple server-side application that uses SAP Graph.

1. Paste the following into a new file, called `hellograph.js`, in the `src` folder:

    ```JSON
    const express = require("express");
    const app = express();
    const Graph = require("./graph");
    const port = 3004;

    const graph = new Graph();

    app.get('/sap*', async (req, res) => {
        const response = await graph.get(req, `${req.url}`, "");
        res.send(`<pre><code>${JSON.stringify(response, null, 2)}</code></pre>`);
    });

    app.listen(port, () => { console.log(`Explore SAP Graph at http://localhost:${port}`) });
    ```

    As mentioned, this code uses a popular Node.js package called **Express**. On line 8, you define the action taken when your server-side app is called from a browser. Here, you simply take the URL that you received (`req.url`), and pass it through, as-is, to SAP Graph. The returned data from SAP Graph is then shown as a raw result in the browser screen.

2. Your server-side app is ready. To run it, use your terminal console to enter:

    ```Shell / Bash
    node hellograph.js
    ```

    The console will show:

    ```Shell / Bash
    Explore SAP Graph at <http://localhost:3004>
    ```

    Now the fun begins. The application expects a well-formed query URL to function properly.

3. Open a browser window or tab, and enter the following query URL:
`http://localhost:3004/sap.graph/SalesQuote?$top=2`

The browser will invoke your application code, which will call SAP Graph to fetch the data, and if all went well, you will see the following output on your screen:

<!-- border -->![Output](blog2_output_sm.png)

This output should look very similar to what you saw in the SAP Graph Navigator for the `SalesQuote` entity in Step 3.

---
### Use SAP Graph securely with real data and authentication


Congratulations, you have made it to the end of this tutorial! You now know how to navigate the connected data entities using SAP Graph Navigator, you've set up a lightweight development environment to test out SAP Graph, and you've verified that you can query unified entities. Continue on with Part 3 of the tutorial series on [blogs.sap.com](https://blogs.sap.com/2021/06/25/part-3-use-sap-graph-securely-with-real-data-authentication/) where you can use SAP Graph securely and with real data.


---
