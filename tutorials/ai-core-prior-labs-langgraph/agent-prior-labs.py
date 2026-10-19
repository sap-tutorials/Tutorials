import json
import os
from pathlib import Path

import pandas as pd
import requests
from dotenv import load_dotenv
from langchain.agents import create_agent
from langchain.tools import tool
from langchain_litellm import ChatLiteLLM

from payload_data import payload, GROUND_TRUTH

load_dotenv(dotenv_path=Path(__file__).parent / ".env")

TABPFN_DEPLOYMENT_ID = os.environ["TABPFN_DEPLOYMENT_ID"]

model = ChatLiteLLM(model=os.environ.get("LLM_MODEL", "sap/amazon--nova-lite"), temperature=0)


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


FEATURE_COLS = [
    "CATEGORY", "DISTANCE_KM", "WEIGHT_KG", "ORDER_VALUE", "PROMISED_DAYS",
    "WAREHOUSE_HRS", "SHIP_METHOD", "CARRIER", "PKG_SIZE", "PRIORITY", "WEATHER", "SHIP_COST",
]


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
            col_name    = target["name"]
            placeholder = target["prediction_placeholder"]
            task_type   = target["task_type"]

            # .get() handles classification-only test rows that have no DELAY_DAYS key
            train_rows = [r for r in rows if r.get(col_name) is not None and r.get(col_name) != placeholder]
            test_rows  = [r for r in rows if r.get(col_name) == placeholder]

            if not test_rows:
                results["predictions"][col_name] = []
                continue

            train_df = pd.DataFrame(train_rows)
            test_df  = pd.DataFrame(test_rows)

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

            print(f"\n--- TabPFN request [{col_name}] ---")
            print(f"URL: {url}")
            response = requests.post(url, headers=headers, json=request_payload, timeout=300)
            print(f"HTTP {response.status_code}")
            response.raise_for_status()

            predictions = response.json()["prediction"]
            results["predictions"][col_name] = [
                {"ORD_ID": row["ORD_ID"], "SHIP_METHOD": row["SHIP_METHOD"], col_name: p}
                for row, p in zip(test_rows, predictions)
            ]

        return json.dumps(results, indent=2)

    except requests.RequestException as e:
        return f"Error calling TabPFN: {str(e)}"


@tool
def run_tabpfn_predictions() -> str:
    """Run TabPFN-3.5 Plus predictions for shipment delay days (DELAY_DAYS) and on-time status (LATE).

    Returns a JSON string with predictions for all test rows.
    """
    return call_tabpfn(payload)


system_prompt = """You are a senior e-commerce logistics analyst.
You have received TabPFN-3.5 Plus predictions for two missing fields in a shipment dataset:
- DELAY_DAYS (integer): the predicted number of days an order arrives after its promised delivery date.
- LATE (Yes/No): whether each order is predicted to arrive after its promised delivery date.

Your role is to summarise the predictions in a clear operations report:
- List the predicted delivery delay (in days) for each order, grouped by shipping method.
- List the predicted on-time / late status for each order.
- Highlight any patterns (e.g. which shipping methods, carriers, or weather conditions correlate with longer delays).

You rely exclusively on the model predictions provided — never estimate or adjust values yourself."""


forecast_agent = create_agent(
    model=model,
    tools=[run_tabpfn_predictions],
    system_prompt=system_prompt,
)


def evaluate_predictions(predictions_json: str) -> None:
    data = json.loads(predictions_json)
    preds = data["predictions"]

    if "DELAY_DAYS" in preds:
        errors = []
        for item in preds["DELAY_DAYS"]:
            truth = GROUND_TRUTH.get(item["ORD_ID"], {}).get("DELAY_DAYS")
            if truth is not None:
                errors.append(abs(float(item["DELAY_DAYS"]) - float(truth)))
        if errors:
            print(f"\nMAE (DELAY_DAYS): {sum(errors) / len(errors):.2f} days")

    if "LATE" in preds:
        correct = 0
        total = 0
        for item in preds["LATE"]:
            truth = GROUND_TRUTH.get(item["ORD_ID"], {}).get("LATE")
            if truth is not None:
                total += 1
                if str(item["LATE"]).strip().lower() == str(truth).strip().lower():
                    correct += 1
        if total:
            print(f"Accuracy (LATE): {correct}/{total} = {correct / total:.0%}")


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
