import logging

from flask import Flask
from flask import request as call_request

from tf_template import Model, TextProcess, AVAILABLE_GPUS

app = Flask(__name__)
app.logger.setLevel(logging.INFO)
# app.logger.addHandler(logging.FileHandler('server.log'))  # Uncomment to save logs to file `server.log`

app_has_run_before: bool = False


@app.before_request
def first_run():
    global app_has_run_before
    if not app_has_run_before:
        app.logger.info(f"Num GPUs Available: {len(AVAILABLE_GPUS)}")

        app.config['text_process'] = TextProcess()
        app.config['model'] = Model()

        app_has_run_before = True


@app.route("/v1/predict", methods=["POST"])
def predict() -> str:
    text_process = app.config['text_process']
    model = app.config['model']

    input_data = dict(call_request.json)
    text = str(input_data['text'])

    app.logger.info(f'Requested text: {text}')
    prediction = model.predict(
        text_process.pre_process(text)
    )

    app.logger.info(f"Prediction: {prediction}")

    return text_process.post_process(prediction)


if __name__ == "__main__":
    app.run(host="0.0.0.0", debug=True, port=9001)

"""
To run and debug locally:
1. Install
   - flask
   - scikit-learn
   - tensorflow==2.10.0

2. Run the server from the project's root directory
$ export SERVE_FILES_PATH=tf_files && python server/serve.py

As an alternative, the server can be run like this:
$ export SERVE_FILES_PATH=../tf_files && gunicorn --chdir server serve:app -b 0.0.0.0:9001

3. Query the endpoint
$ curl --location --request POST 'http://localhost:9001/v1/predict' --header 'Content-Type: application/json' --data-raw '{"text": "A restaurant with great ambiance"}'

4. Result should be
{
  "negative": 0.5039926171302795
}
"""
