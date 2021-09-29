---
title: Prepare the Local Development Environment
description: Install essential runtimes and tools for local development.
auto_validation: true
primary_tag: products>sap-cloud-platform
tags: [ products>sap-cloud-platform, topic>cloud, tutorial>beginner, topic>node-js ]
time: 20
---



## Details
### You will learn  
  - How to install Node.js and npm (Node Package Manager) as a runtime
  - How to install essential tools like git and Cloud MTA Build Tool (`mbt`)

---

[ACCORDION-BEGIN [Step 1: ](Install the Node.js runtime)]

>Node.js is a server-side runtime environment built on Chrome's V8 JavaScript engine. It provides an event-driven, non-blocking (asynchronous) I/O and cross-platform runtime environment. It enables you to build scalable server-side applications using JavaScript and is open-source.
>&nbsp;
>Node.js can be used to build applications like command-line applications, web applications, REST API servers, and many else. It is mostly used to create network programs like web servers. For more information, visit the official site at <https://nodejs.org>.

Before you can start building your `Node.js` app, you need to install `npm` and `Node.js`. `npm` is included in the `Node.js` installation.

It is possible to download the libraries and organize the directories on your own and start that way. However, as your project (and list of dependencies) grows, this will quickly become messy. It also makes collaborating and sharing your code much more difficult.

We recommend using a package manager on your OS.

[OPTION BEGIN [Windows]]

**Install** the Windows package manager [Chocolatey](https://chocolatey.org/).

```Terminal
@powershell -NoProfile -ExecutionPolicy Bypass -Command "iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))" && SET "PATH=%PATH%;%ALLUSERSPROFILE%\chocolatey\bin"
```

**Install Node.js**

```Terminal
choco install nodejs
```


[OPTION END]
[OPTION BEGIN [Mac]]


**Install** the Mac package manager [Homebrew](https://brew.sh/).

```Terminal
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
```

**Install Node.js**

```Terminal
brew install nodejs
```


[OPTION END]
[OPTION BEGIN [Other]]

**Install** the Node.js from the  [official website](https://nodejs.org/en/download/).

[OPTION END]

[VALIDATE_1]
[ACCORDION-END]
[ACCORDION-BEGIN [Step 2: ](Verify your installation)]

To verify if the installation was successful, check the `npm` and `node.js` version. Open the command line and print for the installed version.

```Terminal
node -v
npm -v
```

[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step 3: ](Install the cloud MTA build tool)]

This tool will create so-called `.mtar` archives, which are deployable packages that contain your entire project. The tool itself is an OS-dependent binary, but you can leverage [npm](https://www.npmjs.com/package/mbt) to install the right version for your OS and append it to the Path variable. Run the following command to install this tool:


```Terminal
npm install -g mbt
```



[OPTION BEGIN [Windows]]

> This tool depends on [GNU make](https://www.gnu.org/software/make/). In case this tool is not installed on your machine, run `choco install make` to install it.

[OPTION END]
[OPTION BEGIN [Mac]]

[OPTION END]
[OPTION BEGIN [Other]]

> This tool depends on [GNU make](https://www.gnu.org/software/make/) that is most likely already installed when you use a Unix-based OS. In case this tool is not installed on your machine, install it from [here](http://ftp.gnu.org/gnu/make/).


[OPTION END]

[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step 4: ](Install git)]

Another tool that you will use often is git. It will allow you to copy (aka clone) existing code project from compatible pages such as <https://github.com>.

Use the package manager from step 1 to install this tool as well.

[OPTION BEGIN [Windows]]

```Terminal
choco install git
```

[OPTION END]
[OPTION BEGIN [Mac]]

```Terminal
brew install git
```


[OPTION END]
[OPTION BEGIN [Other]]

**Install** the git from the  [official website](https://git-scm.com/downloads).


[OPTION END]

[DONE]
[ACCORDION-END]


---
