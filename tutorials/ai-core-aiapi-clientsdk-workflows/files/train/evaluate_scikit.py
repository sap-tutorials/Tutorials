# -*- coding: utf-8 -*-
"""
Evaluation script for showcasing the two-step workflow
"""
import logging
import numpy as np

from random import choice
from train_scikit import TrainSKInterface

FORMAT = "%(asctime)s:%(name)s:%(levelname)s - %(message)s"
# Use filename="file.log" as a param to logging to log to a file
logging.basicConfig(format=FORMAT, level=logging.INFO)


class EvaluateSKInterface(TrainSKInterface):
    def __init__(self) -> None:
        super().__init__()

    def evaluate_model(self) -> None:
        """
        Evaluate the performance of the trained model
        """
        if self.text_pipeline is None:
            self.get_model()

        prediction = self.text_pipeline.predict(self.val["text"].tolist())
        logging.info(f"-----START EVALUATION-----")
        accuracy = np.mean(prediction == self.val["target"].tolist())
        logging.info(f"Test accuracy: {accuracy}")

        for _ in range(10):
            idx = choice(list(range(len(self.val["text"]))))
            prediction = self.text_pipeline.predict([self.val["text"].iloc[idx]])
            predicted_label = prediction[0]

            logging.info(f"Actual [{self.val['target'].iloc[idx]}]")
            logging.info(f"Predicted [{predicted_label}]")
            logging.info(f" -> {self.val['target'].iloc[idx]==predicted_label}")

        logging.info(f"-----END EVALUATION-----")

    def run_workflow(self) -> None:
        """
        Run the training script with all the necessary steps
        """
        self.read_dataset()
        self.split_dataset()

        self.get_model()
        if self.text_pipeline is None:
            # Train the model if no model is available
            return "Model has not been trained yet!"

        self.evaluate_model()

        return None


if __name__ == "__main__":
    eval_obj = EvaluateSKInterface()
    eval_obj.run_workflow()
