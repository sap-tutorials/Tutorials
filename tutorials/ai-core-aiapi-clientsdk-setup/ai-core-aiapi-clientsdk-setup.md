---
title: Install Jupyter and Run SAP AI API Client SDK on Python
description: Learn python installation and Jupyter setup. Invoke SAP AI API Client SDK.
auto_validation: true
time: 15
tags: [ tutorial>license, tutorial>advanced, programming-tool>artificial-intelligence, programming-tool>machine-learning, software-product>sap-business-technology-platform ]
primary_tag: programming-tool>artificial-intelligence
author_name: Dhrubajyoti Paul
author_profile: https://github.com/dhrubpaul
---

## Details
### You will learn
  - How to install packages for python
  - How to execute python code in Jupyter
  - How to connect to SAP AI API Client SDK using python via Jupyter

---

[ACCORDION-BEGIN [Step 1: ](Install python)]

Install python. [Download here](https://www.python.org/downloads/)

`pip` is the python package installer, it will be installed along python.

To check for `pip` after installation completes,
execute the following on terminal *(command prompt)*

```BASH
pip --version
```

!![pip version check](img/jupyter/pip.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Install SAP AI API Client SDK)]

`SAP-AI-API-Client-SDK`  is python package which will enable us to talk to SAP AI API.

| Package | Link |
| --- | --- |
| `SAP-AI-API-Client-SDK` | [Download Here](https://developers.sap.com/trials-downloads.html?search=AI+Core) |

Python packages have format `.whl` or `.tar.gz`, both can be installed in the same way.

Follow the steps to install the same

1. Download `.tar.gz` for `SAP-AI-API-Client-SDK`

2. Install `.tar.gz` using pip. Execute the following on terminal. *(Change the path)*

    ```BASH[1]
    pip install <path_to_your_whl_folder>/ai-api-client-sdk-<version>.tar.gz
    ```
    !![pip ai-api](img/pip/ai-api.png)

Similarly install `PyYaml`, *(No `.tar.gz` or `.whl` download required for `PyYaml` , it would be fetched automatically from public python repository)*.

Execute the following on terminal.

```BASH
pip install PyYaml
```

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Installing Jupyter)]

Jupyter is used to execute python code pieces in form of cells.

Use `pip` to install Jupyter. Execute the following on terminal.

```BASH
pip install notebook
```

Check if Jupyter is correctly installed. Execute the following on terminal.

```BASH
jupyter --version
```

!![Jupyter version check](img/jupyter/jupy.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Start Jupyter Notebook)]

On the terminal, navigate to folder which you will be using for this the tutorial (must not be GitHub synced, as in the folder you will store sensitive information). Here my folder is `C:/aicore-test`

Execute the following on terminal.

```BASH
jupyter notebook
```

!![notebook](img/jupyter/notebook.png)

This will automatically open a webpage in your default browser.

> **IMPORTANT:** DO NOT close the terminal which started the Jupyter notebook while using the notebook. The webpage is just an interface to this terminal.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Create a Jupyter notebook)]

Click **New > Python 3 `(ipykernel)`** on Jupyter webpage (result of starting Jupyter).

!![new notebook](img/jupyter/new.png)

Click on `Untitled` to change the notebook name.

Type `main` in the dialog box that appears. Click `Rename`.

!![name jupyter notebook](img/jupyter/name-notebook.png)


After renaming you can see your folder (where you started Jupyter session from terminal) will have `main.ipynb`.

*(The below screenshot is of the folder from IDE)*

!![notebook preview](img/jupyter/preview.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 6: ](Run Python Code in Jupyter Notebook)]

Type your python codes inside the Jupyter notebook.

```PYTHON
print("Hi, printed using python")
```

!![notebook element](img/jupyter/cell.png)  

- (1) Write you python code in the gray box, its called **cell** in Jupyter notebook.

- (2) Click on he arrow button next to cell, will execute the cell *(python piece of code)*.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Run SAP AI API Client SDK with python)]

Get service key for your SAP AI Core. [Read How to create service key for SAP AI Core.](https://help.sap.com/viewer/2d6c5984063c40a59eda62f4a9135bee/LATEST/en-US/7323ff4e37ba41c198b06e9669b80920.html)

> If you have not provisioned SAP AI Core. Read [provisioning steps](https://help.sap.com/viewer/2d6c5984063c40a59eda62f4a9135bee/LATEST/en-US/38c4599432d74c1d94e70f7c955a717d.html) to provision SAP AI Core service.

Here an example service key file. The content of your SAP AI API service file should look similar to this:

```JSON
{
    "clientid": "ab-cdefg-111-12233!h7777|aicore!1111",
    "clientsecret": "qnwerntny=",
    "url": "https://tutorial.authentication.sap.hana.ondemand.com",
    "identityzone": "tutorial",
    "identityzoneid": "5555a-a2c-4444-2222",
    "appname": "cdefg-111-12233!h7777|aicore!1111",
    "serviceurls": {
        "AI_API_URL": "https://api.ai.ml.hana.ondemand.com"
    }
}
```

Download SAP AI Core service key file (JSON).

Save the file in the folder relative to where your Jupyter notebook is located, inside the `files` folder: `files/aic_service_key.json`.

You will use the saved location to load service key and use with SAP AI API client SDK, so ensure the path is correct.                    

!![aic service key](img/pip/aic_service_key.png)

Now execute the following python code on your Jupyter notebook cell: *(You will get no output)*

```PYTHON
from ai_api_client_sdk.ai_api_v2_client import AIAPIV2Client
import json

# Your service key JSON file relative to this notebook
aic_service_key_path = 'files/aic_service_key.json'

# Loads the service key file
with open(aic_service_key_path) as ask:
    aic_service_key = json.load(ask)

# Creating an SAP AI API client instance
ai_api_client = AIAPIV2Client(
    base_url = aic_service_key["serviceurls"]["AI_API_URL"] + "/v2", # The present SAP AI API version is 2
    auth_url=  aic_service_key["url"] + "/oauth/token",
    client_id = aic_service_key['clientid'],
    client_secret = aic_service_key['clientsecret']
)
```

1. Create a new cell.

    !![new cell](img/jupyter/new-cell.png)

2. Paste code snippet and click the arrow to execute. *(Alternative: **Run** > **Run Cells**)*.

    !![paste code](img/jupyter/paste-code.png)

    - The code will load `aic_service_key.json`
    - It will create connection to your SAP AI Core instance via SAP AI API client SDK.
    - Store the connection to SAP AI API Client SDK instance variable `ai_api_client`.
    - Use this `ai_api_client` variable  throughout the tutorial to refer to your connection to SAP AI API, watch out for **Warning** in the tutorials, where the same variable is referred (used).


[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 8: ](Test yourself)]

Assuming you have the following file snippet in your SAP AI API service key *(JSON file)*.

```
{
    ...
    "serviceurls": {
      "AI_API_URL": "https://api.ai.ml.hana.ondemand.com"
}
```
And given that at present the API version for SAP AI API is `v2`.

What value would you write for the parameter `base_url` in the following code snippet, to create connection to SAP AI Core using SAP AI API Client SDK ?

```PYTHON[2]
my_ai_core_connection = AIAPIV2Client(
    base_url =  # Your response
    ...
)
```

[VALIDATE_1]
[ACCORDION-END]

---
