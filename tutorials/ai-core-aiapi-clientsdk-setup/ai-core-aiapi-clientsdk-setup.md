---
title: Install Jupyter and Run SAP AI API Client SDK on Python
description: Learn python installation and Jupyter setup. Invoke SAP AI API Client SDK.
auto_validation: true
time: 15
tags: [ tutorial>license, tutorial>advanced, topic>artificial-intelligence, topic>machine-learning, products>sap-business-technology-platform ]
primary_tag: topic>artificial-intelligence
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

Python packages have format `.whl` or `.tar.gz`, both can be installed in the same way. `SAP-AI-API-Client-SDK`  is python package which will enable us to talk to SAP AI API.
Follow the steps to install the same

1. Download `.tar.gz` for `SAP-AI-API-Client-SDK` [Download Here](https://developers.sap.com/trials-downloads.html?search=AI+Core)


2. Install `.tar.gz` using pip. Execute the following on terminal. *(Change the path)*

    ```BASH[1]
    pip install <path_to_your_whl_folder>/ai-api-client-sdk-<version>.tar.gz
    ```
    !![pip ai-api](img/pip/ai-api.png)

Similarly install `PyYaml`, (*No manual download required for `PyYaml`*, it would be fetched automatically from public python repository). Execute the following on terminal.


```BASH
pip install PyYaml
```

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Installing Jupyter)]

Jupyter is used to execute python code pieces in form of cells. Use pip to install Jupyter.

Execute the following on terminal.

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

[ACCORDION-BEGIN [Step 4: ](Start Jupyter)]

On the terminal, navigate to folder which you will be using throughout the tutorial. Here my folder is `C:/aicore-test`

Execute the following on terminal.

```BASH
jupyter notebook
```

!![notebook](img/jupyter/notebook.png)

This will automatically open a webpage in your default browser.


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Create a Jupyter notebook)]

On Jupyter page(result of starting Jupyter) opened on your browser. Click **New > Python 3 `(ipykernel)`**

!![new notebook](img/jupyter/new.png)

Double-click on `Untitled`, Change the name to `main`  

!![name jupyter notebook](img/jupyter/name-notebook.png)

After renaming you can see your folder(where you started Jupyter from terminal) will have `main.ipynb`.

*(The below screenshot is of the folder from IDE)*

!![notebook preview](img/jupyter/preview.png)

Type your python codes inside the Jupyter notebook.

```PYTHON
from ai_api_client_sdk.ai_api_v2_client import AIAPIv2Client
```

!![notebook element](img/jupyter/cell.png)  

- (1) Represents cell where your will write code.

- (2) Click on he Arrow button will execute the cell *(python piece of code)*.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Run SAP AI API Client SDK with python)]


Follow the [provisioning steps](https://help.sap.com/viewer/product/AI_CORE/CLOUD/en-US) to get an SAP AI API service instance key.

> If you already have an SAP AI API service instance you can get the service key from your SAP BTP cockpit:
**SAP BTP cockpit > subaccount > Instances and Subscriptions > Instances > Credentials**

The content of your SAP AI API service file should look similar to this:

```JSON
{
    "clientid": "ab-cdefg-111-12233!h7777|aicore!1111",
    "appname": "cdefg-111-12233!h7777|aicore!1111",
    "url": "https://tutorial.authentication.sap.hana.ondemand.com",
    "identityzone": "tutorial",
    "identityzoneid": "5555a-a2c-4444-2222",
    "clientsecret": "qnwerntny=",
    "serviceurls": {
        "ML_API_URL": "https://api.ai.ml.hana.ondemand.com"
    }
}
```

Store your SAP AI API service file inside the `files` folder: `files/aic_service_key.json`, Use it to connect to the SAP AI Core in later steps, so ensure the path is correct.                    

!![aic service key](img/pip/aic_service_key.png)

Now execute the following python code on your Jupyter notebook cell: *(You will get no output)*

```PYTHON
from ai_api_client_sdk.ai_api_v2_client import AIAPIV2Client
import json

aic_service_key_path = 'files/aic_service_key.json'

# NO CHANGES REQURIED BELOW
#
# Loads the service key file
with open(aic_service_key_path) as ask:
    aic_service_key = json.load(ask)

# Creating an api client instance
ai_api_client = AIAPIV2Client(
    base_url = aic_service_key["serviceurls"]["ML_API_URL"] + "/v2",
    auth_url=  aic_service_key["url"] + "/oauth/token",
    client_id = aic_service_key['clientid'],
    client_secret = aic_service_key['clientsecret']
)
```

1. Create a new cell.

    !![new cell](img/jupyter/new-cell.png)

2. Paste the code snippet and click the arrow *(Alternative: **Run** > **Run Cells**)*.

    !![paste code](img/jupyter/paste-code.png)

    - The code will create connection to your SAP AI Core instance using your `aic_service_key.json` and store the connection to SAP AI API Client SDK instance variable `ai_api_client`.
    - Use this `ai_api_client` variable  throughout the tutorial to refer to your connection to SAP AI API, watch out for **Warning** in the tutorials, where the same is referred(used).


[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 7: ](Test yourself)]

Assuming you have the following file snippet in your SAP AI API service key *(JSON file)*.

```
{
    ...
    "serviceurls": {
      "ML_API_URL": "https://api.ai.ml.hana.ondemand.com"
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
