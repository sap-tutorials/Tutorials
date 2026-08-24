---
parser: v2
auto_validation: true
author_name: Thomas Jung
author_profile: https://github.com/jung-thomas
primary_tag: products>sap-business-technology-platform
tags: [ products>sap-business-technology-platform, topic>cloud, tutorial>beginner, programming-tool>node-js ]
time: 20
---



# Prepare the Local Development Environment
<!-- description --> Install essential runtimes and tools for local development.

## You will learn  
  - How to install Node.js and npm (Node Package Manager) as a runtime
  - How to install essential tools like git and Cloud MTA Build Tool (`mbt`)

---

### Install the Node.js runtime

>Node.js is a server-side runtime environment built on Chrome's V8 JavaScript engine. It provides an event-driven, non-blocking (asynchronous) I/O and cross-platform runtime environment. It enables you to build scalable server-side applications using JavaScript and is open-source.
>&nbsp;
>Node.js can be used to build applications like command-line applications, web applications, REST API servers, and many else. It is mostly used to create network programs like web servers. For more information, visit the official site at <https://nodejs.org>.

Before you can start building your `Node.js` app, you need to install `npm` and `Node.js`. `npm` is included in the `Node.js` installation.

It is possible to download the libraries and organize the directories on your own and start that way. However, as your project (and list of dependencies) grows, this will quickly become messy. It also makes collaborating and sharing your code much more difficult.

We recommend using a package manager on your OS.

[OPTION BEGIN [Windows]]

**Install** the Windows package manager [Chocolatey](https://chocolatey.org/) by running the following in an **administrative** PowerShell:

```Terminal
Set-ExecutionPolicy Bypass -Scope Process -Force; [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; iex ((New-Object System.Net.WebClient).DownloadString('https://community.chocolatey.org/install.ps1'))
```

**Install Node.js** (the current Long-Term Support release)

```Terminal
choco install nodejs-lts
```


[OPTION END]
[OPTION BEGIN [Mac]]


**Install** the Mac package manager [Homebrew](https://brew.sh/).

```Terminal
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

**Install Node.js**

```Terminal
brew install node
```


[OPTION END]
[OPTION BEGIN [Linux]]

**Install Node.js** via your distribution's package manager, or download the current LTS build from the [official website](https://nodejs.org/en/download).

[OPTION END]


### Verify your installation


To verify if the installation was successful, check the `npm` and `node.js` version. Open the command line and print for the installed version.

```Terminal
node -v
npm -v
```

> This tutorial requires **Node.js 20 LTS or later**. If `node -v` reports an older version, install a current Long-Term Support release before continuing.

### Install the cloud MTA build tool


This tool will create so-called `.mtar` archives, which are deployable packages that contain your entire project. The tool itself is an OS-dependent binary, but you can leverage [npm](https://www.npmjs.com/package/mbt) to install the right version for your OS and append it to the Path variable. Run the following command to install this tool:


```Terminal
npm install -g mbt
```



[OPTION BEGIN [Windows]]

> This tool depends on [GNU make](https://www.gnu.org/software/make/). In case this tool is not installed on your machine, run `choco install make` to install it.

[OPTION END]
[OPTION BEGIN [Mac]]

> This tool depends on [GNU make](https://www.gnu.org/software/make/). On macOS it is provided by the Xcode Command Line Tools — run `xcode-select --install` if it is not already installed.

[OPTION END]
[OPTION BEGIN [Linux]]

> This tool depends on [GNU make](https://www.gnu.org/software/make/), which is most likely already installed on a Unix-based OS. If it is missing, install it with your distribution's package manager (for example, `sudo apt install make`).


[OPTION END]

### Install git


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
[OPTION BEGIN [Linux]]

**Install** git with your distribution's package manager (for example, `sudo apt install git`), or download it from the [official website](https://git-scm.com/downloads).


[OPTION END]



---
