---
parser: v2
auto_validation: true
time: 60
tags: [ tutorial>intermediate, topic>artificial-intelligence, topic>machine-learning, software-product>sap-business-technology-platform, software-product>sap-ai-core ]
primary_tag: software-product>sap-ai-core
author_name: Nora von Thenen
author_profile: https://github.com/noravth
---
# Add Prior Labs TabPFN-3.5 Plus on SAP AI Core to your AI Agent
<!-- description --> In this tutorial you will learn how to build a LangChain agent that uses TabPFN-3.5 Plus to predict delivery delays and on-time delivery status for e-commerce shipments, and then generate an operations summary report using an LLM. TabPFN-3.5 Plus is a tabular foundation model by Prior Labs available through Generative AI Hub on SAP AI Core.

## You will learn
- How to deploy TabPFN-3.5 Plus on SAP AI Core (Generative AI Hub)
- How to call the TabPFN `/predict` endpoint for regression and classification tasks
- How to build a LangChain agent that combines tabular ML predictions with an LLM

## Prerequisites
SAP AI Core setup and basic knowledge: [Initial Setup](https://help.sap.com/docs/AI_CORE/2d6c5984063c40a59eda62f4a9135bee/38c4599432d74c1d94e70f7c955a717d.html)
SAP AI Core instance with **Extended Service Plan** (required for Generative AI Hub) Python 3.10 or higher installed. Basic familiarity with Python and REST APIs.

---

### Architecture Overview

The agent follows a simple three-step flow:

1. The user sends a message asking for predictions and a summary report.
2. The LLM calls the `run_tabpfn_predictions` tool, which hits the TabPFN-3.5 Plus `/predict` endpoint on SAP AI Core — once for regression (`DELAY_DAYS`) and once for classification (`LATE`).
3. The tool result is returned to the LLM, which formats the predictions as a concise logistics operations report.

> Note: TabPFN-3.5 Plus learns directly from the rows you provide — there is no separate training phase. Rows with known values are the context; rows with `[PREDICT]` placeholders are the test set.

---

### Dataset

This tutorial uses the **E-Commerce Delivery & Shipping Dataset 2026**, which contains 50,000 simulated e-commerce orders with logistics attributes such as shipping method, carrier, distance, package weight, warehouse processing time, weather conditions, and delivery outcome.

You can download the dataset from Kaggle: [E-Commerce Delivery and Shipping Data 2026](https://www.kaggle.com/datasets/datascikhan/e-commerce-delivery-and-shipping-data-2026)

The agent uses a representative sample of 50 training rows (every 1,000th order) plus 10 test rows drawn from that dataset. The two prediction targets are:

| Column | Task | Description |
|---|---|---|
| `DELAY_DAYS` | Regression | Number of days an order arrives after its promised delivery date |
| `LATE` | Classification | Whether the order arrived late (`Yes` / `No`) |

---

### Deploy TabPFN-3.5 Plus on SAP AI Core

Before writing any code you need a running deployment of TabPFN-3.5 Plus in your SAP AI Core instance.

Use the following values when creating the deployment:

| Field | Value |
|---|---|
| Scenario | `foundation-models` |
| Executable ID | `prior-labs` |
| Model name | `tabpfn-3.5-plus` |

For step-by-step deployment instructions see [Create a Deployment](https://help.sap.com/docs/sap-ai-core/generative-ai/create-deployment-for-generative-ai-model).

Once the deployment status is **Running**, copy the deployment ID. You will need it in the next step.

---

### Configure environment variables

Create a file named `.env` in your project folder and add the following variables. Never commit this file! Add `.env` to your `.gitignore`. When deploying to Cloud Foundry you can simply bind those variables using [environmental variables](https://docs.cloudfoundry.org/devguide/deploy-apps/environment-variable.html).

```
AICORE_AUTH_URL=https://<your-subdomain>.authentication.<region>.hana.ondemand.com/oauth/token
AICORE_CLIENT_ID=<your-client-id>
AICORE_CLIENT_SECRET=<your-client-secret>
AICORE_BASE_URL=https://api.ai.<region>.aws.ml.hana.ondemand.com
AICORE_RESOURCE_GROUP=default
TABPFN_DEPLOYMENT_ID=<your-tabpfn-deployment-id>
LLM_MODEL=sap/amazon--nova-lite
```

You can find all SAP AI Core credentials in the SAP BTP cockpit under your SAP AI Core service instance's **Service Keys**.

> Note: `LLM_MODEL` is optional. This agent defaults to `sap/amazon--nova-lite` if the variable is not set.

---

### Install Python dependencies

In your terminal/IDE of your choice: Create and activate a virtual environment, then install the required packages:

```
python -m venv .venv
source .venv/bin/activate        # macOS / Linux
.venv\Scripts\activate           # Windows
```

```
pip install langchain langchain-litellm requests pandas python-dotenv
```

---

### Create the Python file

Create a file named `agent-prior-labs.py` in your project folder. Start with the imports and top-level setup:

```python
import json
import os
from pathlib import Path

import pandas as pd
import requests
from dotenv import load_dotenv
from langchain.agents import create_agent
from langchain.tools import tool
from langchain_litellm import ChatLiteLLM

load_dotenv(dotenv_path=Path(__file__).parent / ".env")

TABPFN_DEPLOYMENT_ID = os.environ["TABPFN_DEPLOYMENT_ID"]

model = ChatLiteLLM(model=os.environ.get("LLM_MODEL", "sap/amazon--nova-lite"), temperature=0)
```

A few things worth noting here:

- `load_dotenv` uses an explicit path relative to the script file so it works regardless of which directory you run the script from.
- `TABPFN_DEPLOYMENT_ID` is read at import time so a missing variable fails immediately with a clear error, not silently later.
- `temperature=0` on the LLM makes the report reproducible, given the same TabPFN predictions, the LLM will always produce a similar summary.

---

### Add the authentication helper

SAP AI Core uses OAuth 2.0 client credentials. Add this helper function to your code to get your Bearer token:

```python
def _get_auth_token() -> str:
    response = requests.post(
        os.environ["AICORE_AUTH_URL"],
        data={
            "grant_type": "client_credentials",
            "client_id": os.environ["AICORE_CLIENT_ID"],
            "client_secret": os.environ["AICORE_CLIENT_SECRET"],
        },
        timeout=30,
    )
    response.raise_for_status()
    return response.json()["access_token"]
```

`raise_for_status()` turns any 4xx or 5xx response into an exception immediately rather than letting the script fail later with a confusing JSON parse error.

---

### Prepare the prediction data

Calling TabPFN or SAP-RPT via Generative AI Hub work differently from most ML APIs: you do not need to train the model separately. Instead, you send everything in one request: rows with known labels teach the model on the fly (in context learning), and rows marked with a `[PREDICT]` placeholder become the test set (missing values to predict).

All shipment data lives in a separate file. Download [payload_data.py](payload_data.py) and place it next to `agent-prior-labs.py`. In `agent-prior-labs.py`, import the two names you need:

```python
from payload_data import payload, GROUND_TRUTH
```

`payload_data.py` contains four things:

- `_REGRESSION_TEST_ROWS` — 5 orders whose `DELAY_DAYS` the model predicts
- `_CLASSIFICATION_TEST_ROWS` — 5 orders whose `LATE` label the model predicts
- `GROUND_TRUTH` — the known answers derived from those rows, used in evaluation
- `payload` — the full request body: 50 training rows plus the 10 test rows with `[PREDICT]` already substituted for the target column

**Why define the test rows separately?**

Because they serve double duty. The same row object is used twice: once to inject `[PREDICT]` into the payload so TabPFN treats it as a test row, and once to record the known label in `GROUND_TRUTH` so the evaluation step can compare against the real value.

```python
_REGRESSION_TEST_ROWS = [
    # 5 rows: DELAY_DAYS is the target; one row per shipping method
    {"ORD_ID": "ORD-016662", "CATEGORY": "Toys",                   "SHIP_METHOD": "Economy",       "CARRIER": "BlueRoute",           "DISTANCE_KM": 3119.4, "WEIGHT_KG": 0.60, "ORDER_VALUE": 73.38,   "PROMISED_DAYS": 10, "WAREHOUSE_HRS": 8.9,  "PKG_SIZE": "Medium",   "PRIORITY": "High",   "WEATHER": "Rain",   "SHIP_COST": 58.36,  "DELAY_DAYS": 1, "LATE": "Yes"},
    {"ORD_ID": "ORD-016620", "CATEGORY": "Fashion",                "SHIP_METHOD": "International", "CARRIER": "SpeedyCargo",         "DISTANCE_KM": 5404.7, "WEIGHT_KG": 0.49, "ORDER_VALUE": 216.28,  "PROMISED_DAYS": 13, "WAREHOUSE_HRS": 17.1, "PKG_SIZE": "Small",    "PRIORITY": "Normal", "WEATHER": "Cloudy", "SHIP_COST": 279.40, "DELAY_DAYS": 1, "LATE": "Yes"},
    {"ORD_ID": "ORD-016613", "CATEGORY": "Pet Supplies",           "SHIP_METHOD": "Express",       "CARRIER": "SwiftShip",           "DISTANCE_KM": 3465.5, "WEIGHT_KG": 7.72, "ORDER_VALUE": 11.17,   "PROMISED_DAYS": 4,  "WAREHOUSE_HRS": 3.5,  "PKG_SIZE": "Oversized","PRIORITY": "Urgent", "WEATHER": "Snow",   "SHIP_COST": 301.30, "DELAY_DAYS": 2, "LATE": "Yes"},
    {"ORD_ID": "ORD-016749", "CATEGORY": "Health & Personal Care", "SHIP_METHOD": "Standard",      "CARRIER": "GlobalExpress",       "DISTANCE_KM": 1322.0, "WEIGHT_KG": 0.60, "ORDER_VALUE": 10.61,   "PROMISED_DAYS": 5,  "WAREHOUSE_HRS": 4.0,  "PKG_SIZE": "Medium",   "PRIORITY": "Normal", "WEATHER": "Rain",   "SHIP_COST": 34.17,  "DELAY_DAYS": 1, "LATE": "Yes"},
    {"ORD_ID": "ORD-016869", "CATEGORY": "Electronics",            "SHIP_METHOD": "Same Day",      "CARRIER": "FastTrack Logistics", "DISTANCE_KM": 641.9,  "WEIGHT_KG": 1.71, "ORDER_VALUE": 342.67,  "PROMISED_DAYS": 1,  "WAREHOUSE_HRS": 6.8,  "PKG_SIZE": "Medium",   "PRIORITY": "High",   "WEATHER": "Snow",   "SHIP_COST": 71.77,  "DELAY_DAYS": 0, "LATE": "No"},
]
_CLASSIFICATION_TEST_ROWS = [
    # 5 rows: LATE is the target; 3×Yes / 2×No
    {"ORD_ID": "ORD-000172", "CATEGORY": "Beauty",          "SHIP_METHOD": "Express",       "CARRIER": "SpeedyCargo",   "DISTANCE_KM": 4009.9, "WEIGHT_KG": 0.19, "ORDER_VALUE": 91.73,  "PROMISED_DAYS": 4,  "WAREHOUSE_HRS": 6.5,  "PKG_SIZE": "Small",  "PRIORITY": "High",   "WEATHER": "Rain",   "SHIP_COST": 143.59, "LATE": "Yes"},
    {"ORD_ID": "ORD-000905", "CATEGORY": "Electronics",     "SHIP_METHOD": "International", "CARRIER": "PrimeDelivery", "DISTANCE_KM": 4877.2, "WEIGHT_KG": 2.41, "ORDER_VALUE": 13.36,  "PROMISED_DAYS": 12, "WAREHOUSE_HRS": 5.2,  "PKG_SIZE": "Large",  "PRIORITY": "Normal", "WEATHER": "Cloudy", "SHIP_COST": 383.59, "LATE": "Yes"},
    {"ORD_ID": "ORD-002136", "CATEGORY": "Electronics",     "SHIP_METHOD": "International", "CARRIER": "SpeedyCargo",   "DISTANCE_KM": 1955.8, "WEIGHT_KG": 5.67, "ORDER_VALUE": 792.25, "PROMISED_DAYS": 11, "WAREHOUSE_HRS": 6.8,  "PKG_SIZE": "Large",  "PRIORITY": "High",   "WEATHER": "Rain",   "SHIP_COST": 151.25, "LATE": "Yes"},
    {"ORD_ID": "ORD-000433", "CATEGORY": "Fashion",         "SHIP_METHOD": "Express",       "CARRIER": "PrimeDelivery", "DISTANCE_KM": 1182.6, "WEIGHT_KG": 1.46, "ORDER_VALUE": 20.56,  "PROMISED_DAYS": 3,  "WAREHOUSE_HRS": 12.7, "PKG_SIZE": "Medium", "PRIORITY": "Low",    "WEATHER": "Clear",  "SHIP_COST": 52.79,  "LATE": "No"},
    {"ORD_ID": "ORD-001840", "CATEGORY": "Sports & Fitness","SHIP_METHOD": "Standard",      "CARRIER": "EagleCourier",  "DISTANCE_KM": 1151.8, "WEIGHT_KG": 0.10, "ORDER_VALUE": 10.85,  "PROMISED_DAYS": 5,  "WAREHOUSE_HRS": 20.4, "PKG_SIZE": "Small",  "PRIORITY": "Normal", "WEATHER": "Clear",  "SHIP_COST": 25.12,  "LATE": "No"},
]

GROUND_TRUTH = {
    **{row["ORD_ID"]: {"DELAY_DAYS": row["DELAY_DAYS"]} for row in _REGRESSION_TEST_ROWS},
    **{row["ORD_ID"]: {"LATE": row["LATE"]} for row in _CLASSIFICATION_TEST_ROWS},
}
```

`_REGRESSION_TEST_ROWS` are the test set for the `DELAY_DAYS` regression task. In the payload their `DELAY_DAYS` field is replaced with `"[PREDICT]"`, which tells TabPFN to predict it and excludes them from the regression training set. Because they still carry a known `LATE` value, they also contribute as additional training context for the `LATE` classification task.

`_CLASSIFICATION_TEST_ROWS` are the test set for the `LATE` classification task. Their `LATE` field is replaced with `"[PREDICT]"` in the payload. They have no `DELAY_DAYS` key at all, so `call_tabpfn` will find `r.get("DELAY_DAYS") == None` and exclude them entirely from the regression task — they play no role there.

**The payload structure**

The payload in `payload_data.py` assembles all rows in one structure. The 50 training rows have every column filled in. The test rows are injected with a dict-spread that copies all fields and overrides only the target column:

```python
payload = {
    "prediction_config": {
        # Rows where a target column equals prediction_placeholder become the
        # test set; all other rows with a known value are training data.
        "target_columns": [
            {"name": "DELAY_DAYS", "prediction_placeholder": "[PREDICT]", "task_type": "regression"},
            {"name": "LATE",       "prediction_placeholder": "[PREDICT]", "task_type": "classification"},
        ]
    },
    "rows": [
        # 50 training rows — all columns filled in
        {"ORD_ID": "ORD-000001", ..., "DELAY_DAYS": 5, "LATE": "Yes"},
        # ...
        # Test rows injected via dict-spread — only the target column is overridden
        *[{**r, "DELAY_DAYS": "[PREDICT]"} for r in _REGRESSION_TEST_ROWS],
        *[{**r, "LATE":       "[PREDICT]"} for r in _CLASSIFICATION_TEST_ROWS],
    ],
}
```

`{**r, "DELAY_DAYS": "[PREDICT]"}` copies every field from the row dict and overrides just the target. Because `_CLASSIFICATION_TEST_ROWS` has no `DELAY_DAYS` key at all, those rows are completely invisible to the regression task.

---

### Build the TabPFN prediction function

Now define which columns the model should use as input features. Both target columns — `DELAY_DAYS` and `LATE` — are deliberately excluded:

```python
FEATURE_COLS = ["CATEGORY", "DISTANCE_KM", "WEIGHT_KG", "ORDER_VALUE", "PROMISED_DAYS",
                "WAREHOUSE_HRS", "SHIP_METHOD", "CARRIER", "PKG_SIZE", "PRIORITY", "WEATHER", "SHIP_COST"]
```

`DELAY_DAYS` must not be a feature when predicting `LATE`, because the two are directly linked, a delay of 0 means on-time, any positive value means late. Giving the model `DELAY_DAYS` as input for the classification task would leak the answer. Likewise, `LATE` is excluded from the regression features. Only the operational shipment attributes are passed in.

The core of the integration is `call_tabpfn`. It loops over each target column in `prediction_config`, splits the rows into training and test sets, reshapes the features into the format the API expects, and POSTs a separate request for each task:

```python
def call_tabpfn(payload: dict) -> str:
    try:
        token = _get_auth_token()
        url = f"{os.environ['AICORE_BASE_URL'].rstrip('/')}/v2/inference/deployments/{TABPFN_DEPLOYMENT_ID}/predict"
        headers = {
            "Authorization": f"Bearer {token}",
            "AI-Resource-Group": os.environ.get("AICORE_RESOURCE_GROUP", "default"),
            "Content-Type": "application/json",
        }

        rows = payload["rows"]
        results: dict = {"predictions": {}}

        for target in payload["prediction_config"]["target_columns"]:
            col_name    = target["name"]                    # e.g. "DELAY_DAYS"
            placeholder = target["prediction_placeholder"]  # "[PREDICT]"
            task_type   = target["task_type"]

            # Split: rows with a known value train the model;
            # rows with "[PREDICT]" are what we want predicted.
            # .get() handles classification-only test rows that have no DELAY_DAYS key.
            train_rows = [r for r in rows if r.get(col_name) is not None and r.get(col_name) != placeholder]
            test_rows  = [r for r in rows if r.get(col_name) == placeholder]

            if not test_rows:
                results["predictions"][col_name] = []
                continue

            train_df = pd.DataFrame(train_rows)
            test_df  = pd.DataFrame(test_rows)

            # The API expects features as a column-oriented dict: {"col": [val1, val2, ...]}
            x_train = {col: train_df[col].tolist() for col in FEATURE_COLS}
            x_test  = {col: test_df[col].tolist()  for col in FEATURE_COLS}

            if task_type == "regression":
                request_payload = {
                    "task_config": {
                        "task": "regression",
                        "predict_params": {"output_type": "mean"},
                    },
                    "x_train": x_train,
                    "y_train": train_df[col_name].astype(float).tolist(),
                    "x_test": x_test,
                }
            else:
                request_payload = {
                    "task_config": {
                        "task": "classification",
                        "tabpfn_config": {"n_estimators": 8, "inference_precision": "auto"},
                        "predict_params": {"output_type": "preds"},
                    },
                    "x_train": x_train,
                    "y_train": {col_name: train_df[col_name].tolist()},
                    "x_test": x_test,
                }

            response = requests.post(url, headers=headers, json=request_payload, timeout=300)
            response.raise_for_status()

            predictions = response.json()["prediction"]
            results["predictions"][col_name] = [
                {"ORD_ID": row["ORD_ID"], "SHIP_METHOD": row["SHIP_METHOD"], col_name: p}
                for row, p in zip(test_rows, predictions)
            ]

        return json.dumps(results, indent=2)

    except requests.RequestException as e:
        return f"Error calling TabPFN: {str(e)}"
```

Two things to notice about the request format:

- For **regression**, `y_train` is a plain list of numbers — the model learns to predict a continuous value.
- For **classification**, `y_train` must be a dict keyed by the column name — this tells TabPFN which label column to learn from.

The response always comes back as `{"prediction": [...]}` — a list of values in the same order as the test rows you sent.

---

### Build the agent

A LangChain agent is built from three things: a **language model** (which decides what to do next), **tools** (which the model can call), and a **system prompt** (which defines the model's role and behaviour).

**Tool**

Wrap `call_tabpfn` as a LangChain tool so the agent can call it. The `@tool` decorator makes the function available to the LLM, and the docstring becomes the description the LLM reads when deciding whether to call it:

```python
@tool
def run_tabpfn_predictions() -> str:
    """Run TabPFN-3.5 Plus predictions for shipment delay days (DELAY_DAYS) and on-time status (LATE).

    Returns a JSON string with predictions for all test rows.
    """
    return call_tabpfn(payload)
```

**System prompt**

Define the LLM's role before wiring the agent. The prompt anchors the LLM strictly to the predictions — it should report, not guess:

```python
system_prompt = """You are a senior e-commerce logistics analyst.
You have received TabPFN-3.5 Plus predictions for two missing fields in a shipment dataset:
- DELAY_DAYS (integer): the predicted number of days an order arrives after its promised delivery date.
- LATE (Yes/No): whether each order is predicted to arrive after its promised delivery date.

Your role is to summarise the predictions in a clear operations report:
- List the predicted delivery delay (in days) for each order, grouped by shipping method.
- List the predicted on-time / late status for each order.
- Highlight any patterns (e.g. which shipping methods, carriers, or weather conditions correlate with longer delays).

You rely exclusively on the model predictions provided — never estimate or adjust values yourself."""
```

**Agent**

`create_agent` wires the LLM, tools, and system prompt together in one call:

```python
forecast_agent = create_agent(
    model=model,
    tools=[run_tabpfn_predictions],
    system_prompt=system_prompt,
)
```

**Main**

Invoke the agent with a user message and print the results:

```python
def main():
    result = forecast_agent.invoke(
        {"messages": [{"role": "user", "content": "Run the TabPFN predictions and write a concise logistics operations summary."}]}
    )

    print("\n" + "=" * 50)
    print("E-commerce Logistics Report:")
    print("=" * 50)
    print(result["messages"][-1].content)

    for msg in result["messages"]:
        if hasattr(msg, "content") and isinstance(msg.content, str):
            try:
                data = json.loads(msg.content)
                if "predictions" in data:
                    evaluate_predictions(msg.content)
                    break
            except (json.JSONDecodeError, KeyError):
                pass

if __name__ == "__main__":
    main()
```

---

### Run the agent

Run the script from your project folder:

```
python agent-prior-labs.py
```

The agent prints the raw TabPFN request and response for each target column, followed by the final logistics report. The script also compares predictions against known ground-truth values at the end, printing MAE for `DELAY_DAYS` and accuracy for `LATE`.

```
--- TabPFN request [DELAY_DAYS] ---
URL: https://api.ai.<region>.aws.ml.hana.ondemand.com/v2/inference/deployments/<id>/predict
HTTP 200
...

--- TabPFN request [LATE] ---
HTTP 200
...

==================================================
E-commerce Logistics Report:
==================================================
DELIVERY DELAY PREDICTIONS BY SHIPPING METHOD

Economy
  ORD-016662 (BlueRoute, Rain): 1 day delay

International
  ORD-016620 (SpeedyCargo, Cloudy): 1 day delay
  ...
```

> Note: Response times vary depending on the size of the context dataset. A payload of 60 rows typically completes within 10–60 seconds per target column.

---

### Complete source code

The complete agent script is available at [agent-prior-labs.py](../../agent-prior-labs.py).

A template for all required environment variables is provided in [env.example](../../env.example). Copy it to `.env` and fill in your values before running the agent.
