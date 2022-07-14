---
title: Set Up Computer Vision Package for SAP AI Core
description: Set Up Python and install computer vision package and its dependencies like PyTorch and Detectron2. Install the SAP AI Core SDK to interact with SAP AI Core using Python.
auto_validation: true
time: 30
primary_tag: topic>artificial-intelligence
tags: [ tutorial>advanced, topic>artificial-intelligence, tutorial>license]
author_name: Kannan Presanna Kumar
author_profile: https://github.com/kannankumar
---


### You will learn
  - How to install packages for python for computer vision package
  - How to execute python code in Jupyter
  - How to connect to SAP AI Core using the SAP AI Core SDK

## Prerequisites
  - You have completed the [Create Your First Machine Learning Project using SAP AI Core tutorial series](group.ai-core-get-started-basics)
  - You have [provisioned SAP AI Core](https://help.sap.com/viewer/2d6c5984063c40a59eda62f4a9135bee/LATEST/en-US/38c4599432d74c1d94e70f7c955a717d.html)
  - You have [set up your Git Repository with SAP AI Core](https://help.sap.com/viewer/808d9d442fb0484e9b818924feeb9add/LATEST/en-US/3269092e37d141a293f0dbd7eaafc829.html).
  - You have [created Docker registry secret in SAP AI Core](https://help.sap.com/viewer/2d6c5984063c40a59eda62f4a9135bee/LATEST/en-US/b29c7437a54f46f39c911052b05aabb1.html)
  - You have [registered your Object Store secret in SAP AI Core](https://help.sap.com/docs/AI_CORE/2d6c5984063c40a59eda62f4a9135bee/b083d73f672c428faac3048b74733546.html)

### Pre-read

In this tutorial, you create an virtual Python environment with Jupyter notebook. Then you install required Python packages into the virtual Python environment to run computer vision pipelines on SAP AI Core from your Jupyter notebook.

#### For Windows users

Python package `detectron2` currently [only supports Linux or MacOS](https://detectron2.readthedocs.io/en/latest/tutorials/install.html#requirements). For this reason, you are recommended to setup a Linux Virtual Machine (VM) in your Windows OS to use the computer vision package and complete the tutorial. For more information about how to install Linux VM, see [guide to install Ubuntu VM on VirtualBox here](https://ubuntu.com/tutorials/how-to-run-ubuntu-desktop-on-a-virtual-machine-using-virtualbox#1-overview).


---

[ACCORDION-BEGIN [Step 2: ](Install Python)]

Download and Install Python 3.9.X from [python.org v3.9.13](https://www.python.org/downloads/release/python-3913/)

> **CAUTION**: Only python version `3.9` is supported for computer vision package.


`pip` is the Python package installer. It is required to install `sap-computer-vision-package` and other required packages. `pip` is installed along with Python.

Check `pip` presence using the following snippet on your terminal.

```BASH
pip --version
```


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create Python virtual environment)]

Python virtual environment helps you install Python packages inside a sandbox like environment such that you maintain required versions of the packages for your project.

Create and activate virtual environment using the following snippet on your terminal. The keyword `sap_cv_env` is name of your environment.

```BASH
$ python -m venv sap_cv_env
$ source sap_cv_env/bin/activate
```

You may check which python is used by your virtual environment by running the following snippet.

```Shell
$ which python
```

You should get path of the Python executable in use by your virtual environment.

```Shell
/Users/kannan/Documents/tutorials/sap_cv_env/bin/python
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Install SAP AI Core SDK)]

The Python package `ai-core-sdk` is an interface to content packages for computer vision task.

Install `SAP AI Core SDK` package in by running the snippet below in your terminal. The command `pip install <package_name>` download the package from [public python repository]((https://pypi.org/project/ai-core-sdk/).) and installs in your virtual environment.

```BASH
pip install "ai-core-sdk[aicore-content]"
```

!![image](img/install-ai-core-sdk.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 5: ](Install PyTorch and Detectron2)]

You must install packages `Torch` and `detectron2`.

[OPTION BEGIN [Mac OS]]

```BASH
pip install torch==1.10 torchvision
pip install https://github.com/facebookresearch/detectron2/archive/refs/tags/v0.6.zip
```

!![image](img/install-torch-torchvision.png)

!![image](img/install-detectron.png)

[OPTION END]

[OPTION BEGIN [Linux]]

For Linux pre-builds of `detectron2` are available, install using the below snippet.

```BASH
pip install torch==1.10 torchvision
pip install detectron2 -f https://dl.fbaipublicfiles.com/detectron2/wheels/cpu/torch1.10/index.html
```

!![image](img/install-torch-torchvision.png)

!![image](img/install-detectron.png)

[OPTION END]

[OPTION BEGIN [Windows]]

> NOTE: Unfortunately, `detectron2` currently does not support Windows OS. We recommend to use a Linux VM for the tutorial. If you need some assistance check out the [Guide to install Ubuntu VM on VirtualBox here](https://ubuntu.com/tutorials/how-to-run-ubuntu-desktop-on-a-virtual-machine-using-virtualbox#1-overview).

On the Linux Virtual Machine you can follow the installation steps listed under the Linux tab.

[OPTION END]

> **INFORMATION** Detailed instructions on Torch installation can be found [here](https://pytorch.org/get-started/locally/). After the installation of Torch the matching version of `detectron2` must be installed. Please check the [detectron2 installation guide](https://detectron2.readthedocs.io/en/latest/tutorials/install.html) to select the proper version. The package is tested with `detectron2=0.6`.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 6: ](Install computer vision package)]

Paste and run the snippet.

```BASH
pip install sap-computer-vision-package
```

[VALIDATE_7]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 7: ](Configure Metaflow)]

Computer vision package uses Metaflow library for SAP AI Core. Metaflow is installed along with `sap-computer-vision-package`. You are required need to configure Metaflow on your system to set your AWS connection to it.

Paste and run the snippet in your terminal. Provide the details to prompts with your AWS Object Store (example: S3) credentials.

> **CAUTION** For Windows users, run the snippet within your Linux VM.

```BASH
metaflow configure aws
```

After this configuration you should have a Metaflow configuration file (`~/.metaflowconfig/config.json`) at the home directory of your user. The contents of that file should look something like this.

(Template for Metaflow configuration file)

```JSON
{
    "METAFLOW_DEFAULT_DATASTORE": "s3",
    "METAFLOW_DATASTORE_SYSROOT_S3": "s3://<your_objec_store_bucket_name>/path/in/your/bucket"
}
```

Alternatively, you may manually create the configuration file (`~/.metaflowconfig/config.json`) for Metaflow and fill it with your `S3 bucket` using above template.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 8: ](Configure AWS CLI)]

You must ensure your [AWS CLI](https://aws.amazon.com/cli/) is configured for the same bucket you used for Metaflow

If you don't have `awscli` installed, you can [install AWS CLI](https://docs.aws.amazon.com/cli/latest/userguide/getting-started-install.html) and then run the below snippet to configure AWS CLI.

```BASH
aws configure
```

On prompt, provide details of your AWS Object Store (S3) account details similar to the one shown in the screenshot.

!![image](img/aws_configure.png)

For more details about configuring AWS CLI, see [Official AWS user guide for configuring `awscli`](https://docs.aws.amazon.com/cli/latest/userguide/cli-configure-quickstart.html)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Install Jupyter)]
Jupyter is used to execute python code pieces in form of cells.

Run the snippet on your terminal.

```BASH
pip install notebook
```

!![image](img/install-jupyter-notebook.png)

Verify Jupyter installation using the below snippet.

```BASH
jupyter --version
```
Example output.

```BASH
$ jupyter --version

jupyter core     : 4.6.1
jupyter-notebook : 6.0.3
qtconsole        : 4.6.0
ipython          : 7.12.0
ipykernel        : 5.1.4
jupyter client   : 5.3.4
jupyter lab      : 1.2.6
nbconvert        : 5.6.1
ipywidgets       : 7.5.1
nbformat         : 5.0.4
traitlets        : 4.3.3
```

Add the `sap_cv_env` environment to the Jupyter using below snippet.

```BASH
python -m ipykernel install --name=sap_cv_env
```

You should see an output that says that the kernel is installed.

```Shell
Installed kernelspec sap_cv_env in /usr/local/share/jupyter/kernels/sap_cv_env
```

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 10: ](Start Jupyter Notebook)]

On the terminal, navigate to folder which you will be using for this the tutorial (must not be GitHub synced, as in the folder you will store sensitive information). Here my folder is `C:/aicore-test`

Execute the following on terminal.

```BASH
jupyter notebook
```

This would start the Jupyter environment like shown below and automatically open the Jupyter Environment in your default browser.

!![image](img/start-jupyter-notebook.png)

> **IMPORTANT:** DO NOT close the terminal which started the Jupyter notebook while using the notebook. The webpage is just an interface to this terminal.


The Jupyter environment should look similar to the screenshot below.

!![image](img/jupyter-start-screen.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 11: ](Create a Jupyter notebook)]

Click **New > `sap_cv_env (ipykernel)`** on Jupyter webpage (result of starting Jupyter). A new notebook starts with the `sap_cv_env` environment set as the kernel.

!![image](img/jupyter-notebook-start.png)

Click on **Untitled** on the header bar to change the notebook name.

Type in name `Meter Reading using SAP CV Package` in the dialog box that appears. Click **Rename**.

!![image](img/jupyter-notebook-rename.png)

You should see the file `Meter Reading using SAP CV package.ipynb` created in the folder where you started Jupyter session from terminal.

!![image](img/jupyter-notebook-rename-2.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 12: ](Run Python code in Jupyter notebook)]

Type your python codes inside the Jupyter notebook.

```Python
print("Hi, printed using python")
```

!![image](img/jupyter-notebook-execute-cell.png)

1. Write your Python code in the gray box, its called **cell** in Jupyter notebook.

2. Click on the **Run** button on the toolbar to execute the selected cell. Default shortcut (`CTRL` + `Enter`).

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 13: ](Download SAP AI Core service key)]

Get service key for your SAP AI Core. [Read How to create service key for SAP AI Core.](https://help.sap.com/viewer/2d6c5984063c40a59eda62f4a9135bee/LATEST/en-US/7323ff4e37ba41c198b06e9669b80920.html)

> If you have not provisioned SAP AI Core. Read [provisioning steps](https://help.sap.com/viewer/2d6c5984063c40a59eda62f4a9135bee/LATEST/en-US/38c4599432d74c1d94e70f7c955a717d.html) to provision SAP AI Core service.

Here's an example service key file. Verify if your SAP AI Core service file has same name of keys as mentioned here.

```JSON
{
    "clientid": "<YourClientID>",
    "clientsecret": "<YourClientSecret>",
    "url": "https://tutorial.authentication.sap.hana.ondemand.com",
    "identityzone": "tutorial",
    "identityzoneid": "5555a-a2c-4444-2222",
    "appname": "example-cdefg-111-12233!h7777|aicore!1111",
    "serviceurls": {
        "AI_API_URL": "https://api.ai.ml.hana.ondemand.com"
    }
}
```

Download SAP AI Core service key file (JSON).

Save the file in the folder relative to where your Jupyter notebook is located, `aic_service_key.json`.

You will use the saved location to load service key and create AI API client, so ensure the path is correct. After you have added the `aic_service_key.json` file to the correct location, it should look like this in your Jupyter File browser.

!![image](img/jupyter-browser-service-key-location.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 14: ](Connect with SAP AI Core)]

Create the AI API client to connect to SAP AI Core by pasting the code snippet below in a new cell in Jupyter.

```PYTHON
from ai_core_sdk.ai_core_v2_client import AICoreV2Client
import json

# Your service key JSON file relative to this notebook
aic_service_key_path = 'aic_service_key.json'

# Loads the service key file
with open(aic_service_key_path) as ask:
    aic_service_key = json.load(ask)

# Creating an AI API client instance
ai_api_client = AIAPIV2Client(
    base_url = aic_service_key["serviceurls"]["AI_API_URL"] + "/v2/lm", # The present AI API version is 2
    auth_url=  aic_service_key["url"] + "/oauth/token",
    client_id = aic_service_key['clientid'],
    client_secret = aic_service_key['clientsecret']
)
```

And execute the cell. (Click the arrow beside the cell or press `Shift` + `Enter` on keyboard. *Alternatively: **Run** > **Run Cells**)*.

- The code loads file `aic_service_key.json`.
- Creates connection to your SAP AI Core instance via AI API client SDK.
- Stores the connection to AI API client instance variable `ai_api_client`.
- Use this `ai_api_client` variable throughout the tutorial to refer to your connection to SAP AI Core.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 15: ](Check AI API Client connection)]

Check that the AI API is correctly set up using the following snippet.

```PYTHON
ai_api_client.scenario.query('default').count
```
You should see a numeric value (most likely `0`) based on the number of scenarios in your AI Core instance.

!![image](img/jupyter-ai-api-check.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 16: ](Test yourself 2)]

Assuming you have the following file snippet in your SAP AI Core service key *(JSON file)*.

```
{
    ...
    "serviceurls": {
      "AI_API_URL": "https://api.ai.ml.hana.ondemand.com"
}
```
And given that at present the API version for AI API is `v2`.

[VALIDATE_4]
[ACCORDION-END]

---
