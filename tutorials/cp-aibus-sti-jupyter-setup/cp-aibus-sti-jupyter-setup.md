---
title: Set Up Jupyter Notebook for Service Ticket Intelligence
description: Set up a local JupyterLab instance as well as the Jupyter Notebook for Service Ticket Intelligence.
auto_validation: true
time: 10
tags: [tutorial>beginner, topic>machine-learning, topic>artificial-intelligence, topic>cloud, products>sap-business-technology-platform, products>sap-ai-business-services, products>service-ticket-intelligence]
primary_tag: topic>machine-learning
---

## Details
### You will learn
  - How to install Python and Anaconda
  - How to set up a local Jupyter server to run Jupyter notebooks

A Jupyter Notebook is used in the following tutorials to make the steps interactive and easy to follow. For more information on Jupyter Notebooks, see [Jupyter](https://jupyter.org/). The notebooks and datasets used for this tutorial mission are available in this [repository](https://github.com/SAP-samples/service-ticket-intelligence-jupyter-notebook).

---

[ACCORDION-BEGIN [Step 1: ](Install Anaconda)]

First, you need to install Anaconda. Anaconda is a platform that offers tools to process large datasets and is often used by data scientists. When installing Anaconda, the programming language Python is installed as well.

To install Anaconda, head over to the [Anaconda installation guide](https://docs.anaconda.com/anaconda/install/) and look for your operating system. Once there, follow the installation guide.

[OPTION BEGIN [Windows]]

To verify the installation, search for `Anaconda Prompt` in your programs and open it. In the command prompt, enter `conda --version` and the version is printed as in the image below.

[OPTION END]

[OPTION BEGIN [Mac and Linux]]

To verify the installation, open a new Terminal or Command Prompt. In the command prompt, enter `conda --version` and the version is printed as in the image below.

[OPTION END]

![Check Anaconda Version](check-anaconda-version.png)

You have successfully installed Anaconda.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Clone the repository)]

All you will need throughout the tutorial is within this [repository](https://github.com/SAP-samples/service-ticket-intelligence-jupyter-notebook) on GitHub. It includes two Jupyter notebooks, one for each of the use cases of Service Ticket Intelligence, as well as some dataset that can be used. In the following, you will clones this repository to your local computer.

[OPTION BEGIN [Windows]]

Open the `Anaconda Prompt` again in case you have closed it. Navigate into the folder where the repository should be placed using the command `cd <path>` or stay where you are. Then, enter the following command to clone the repository:
```shell
git clone https://github.com/SAP-samples/service-ticket-intelligence-jupyter-notebook
```

[OPTION END]

[OPTION BEGIN [Mac and Linux]]

Open a Terminal or Command Prompt again in case you have closed it. Navigate into the folder where the repository should be placed using the command `cd <path>` or stay where you are. Then, enter the following command to clone the repository:
```shell
git clone https://github.com/SAP-samples/service-ticket-intelligence-jupyter-notebook
```

[OPTION END]

![Clone Repository](clone-repository.png)

Once the repository is cloned, you receive an output as above, saying that it's done. 

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Start Jupyter server)]

To use the Jupyter notebooks, you must have a local instance of the Jupyter server running.

Stay in the Anaconda Prompt (Windows) or the Terminal (Mac and Linux) and navigate into the new folder that was created while cloning the repository by using the command `cd service-ticket-intelligence-jupyter-notebook`. You then move into the folder and your current path will change accordingly.

![Changing Folders](changing-folders.png)

Next, enter the command `jupyter notebook` to start the server.

![Start Server](start-server.png)

Once the server started, the site automatically opens in browser. In case it does not, you can use the URLs that were printed in the command prompt.

With that, you are done with setup and can proceed with the following tutorials.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Add credentials)]

Finally, you need to upload the service key file that you received in [Set Up Account for Service Ticket Intelligence and Get Service Key](cp-aibus-sti-booster-key).

On the website that you opened in the previous step, click **Upload**.

![Upload](upload-key-file.png)

In the dialog that opens, select the file that contains the service key.

![Select File](select-file.png)

Finally, ensure that the file is named `default_key.json` and click **Upload** again.

![Name and Upload](name-and-upload.png)

Now, the file shows in the list of documents and folders. With that, you are ready to proceed with the next tutorial.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Test yourself)]

[VALIDATE_1]
[ACCORDION-END]
