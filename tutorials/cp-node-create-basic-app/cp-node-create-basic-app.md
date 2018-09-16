---
title: Create a Basic Node.js App
description: Build a basic Node.js app and run it locally on your machine.
auto_validation: true
primary_tag: products>sap-cloud-platform
tags: [ products>sap-cloud-platform, topic>cloud, tutorial>beginner, topic>node-js ]
time: 10
---

## Prerequisites  
- A text editor (e.g., Notepad, Atom, Sublime)
- **Tutorials:** [Sign up](https://www.sap.com/developer/tutorials/hcp-create-trial-account.html) for a free trial account on SAP Cloud Platform
- **Tutorials:**  [Install](https://www.sap.com/developer/tutorials/hcp-cf-getting-started.html) the Cloud Foundry CLI


## Details
### You will learn  
  - How to install Node.js and npm (Node Package Manager)
  - How to create a basic web app
  - How to run an app on your machine

---

[ACCORDION-BEGIN [Step 1: ](Prepare the infrastructure)]

>Node.js is an server-side runtime environment built on Chrome's V8 JavaScript engine. It provides an event-driven, non-blocking (asynchronous) I/O and cross-platform runtime environment. It enables you to build scalable server-side applications using JavaScript and is open-source.
>&nbsp;
>Node.js can be used to build various types of applications like command line applications, web applications, REST API server and many else. It is mostly used to create network programs like web servers. For more information visit the official site at <https://nodejs.org>.

Before you can start building your `Node.js` app, you need to install `npm` and `Node.js`. `npm` is included in the `Node.js` installation.

It is possible to download the libraries and organize the directories on your own and start that way. However, as your project (and list of dependencies) grows, this will quickly become messy. It also makes collaborating and sharing your code much more difficult.

You can either use a package manager (recommended) on your OS or use an installer: **[Download Installer](https://nodejs.org/en/download/)**

### Installation on Windows (Windows 7+ / Windows Server 2003+)

**Install Chocolatey (Windows package manager), if not yet installed**

```:
@powershell -NoProfile -ExecutionPolicy Bypass -Command "iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))" && SET "PATH=%PATH%;%ALLUSERSPROFILE%\chocolatey\bin"
```

**Install Node.js**

```:
choco install nodejs
```

### Installation on Mac

**Install Homebrew (Mac package manager), if not yet installed**

```:
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
```

**Install Node.js**

```:
brew install node
```

[VALIDATE_1]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Verify your installation)]

To verify if the installation was successful, check the `npm` and `node.js` version. Open the command line tool and ask for the version.
```:
node -v
npm -v
```

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create your application)]

1. Create a new dedicated directory for your Node.js application called `nodetutorial` and another directory inside it called `myapp`.  
2. To start the application setup, change to the `myapp` directory and execute `npm init` in the command line. This will walk you through creating a `package.json` file.

    ```:
    npm init
    ```

    Use the values from the table below. If no specific value for a property  is needed at this point (# at the value table below) so just hit enter.

    |  Field Name     | Value
    |  :------------- | :-------------
    |  package        | `myapp`
    |  version        | #
    |  description    | `this is my first node app`
    |  entry point    | `server.js`
    |  test command   | #
    |  git repository | #
    |  keywords       | #
    |  author         | #
    |  license        | #

    Open the `package.json` in your `myapp`. directory and compare it to the shown result.

    ![package json](package-json.png)

    > All npm packages contain a file, usually in the project root, called `package.json`. This file holds various metadata relevant to the project. This file is used to give information to npm that allows it to identify the project as well as handle the project's dependencies.
    >&nbsp;
    > The `package.json` file is located at the root directory of a Node.js project.


3. Create a file called `server.js` in the application folder (which will act as your web server) and copy the following code to this file and save it:

    ```javascript:
    const express = require('express');
    const app = express();

    app.get('/', function (req, res) {
      res.send('Hello World!');
    });

    const port = process.env.PORT || 3000;;
    app.listen(port, function () {
      console.log(`myapp listening on port ${port});
    });
    ```

[VALIDATE_2]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Download dependencies)]

Your application is almost ready. Before you actually can access the web server, there's the need to download the required modules. If you have a look at the `server.js` file you created in the previous step, the `express-module` is used there but not yet available on your machine. Therefore, run the following command to let npm take care of the dependencies:

```:
npm install express
```

You should now have a newly (automatically) created directory `node_modules` in the `myapp` directory, where all the dependency modules are located. The `package.json` will be updated with the installed module(s).

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Start your application)]

Now the application and can be started locally. Simply start it with the following command:

```:
node server.js
```

You should see `myapp listening on port 3000` on your command line if everything went well. You should also get a `Hello World` response when accessing your web server at `http://localhost:3000`.

[DONE]

[ACCORDION-END]


---
