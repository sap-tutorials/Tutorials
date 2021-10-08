# -*- coding: utf-8 -*-
"""
Training script to showcase the end-to-end training and evaluation script.
"""

from os.path import exists
from os import makedirs
from os import environ
from joblib import load, dump
import pandas as pd
import logging

from sklearn.feature_extraction.text import CountVectorizer, TfidfTransformer
from sklearn.pipeline import Pipeline
from sklearn.naive_bayes import MultinomialNB
from sklearn.model_selection import train_test_split

FORMAT = "%(asctime)s:%(name)s:%(levelname)s - %(message)s"
# Use filename="file.log" as a param to logging to log to a file
logging.basicConfig(format=FORMAT, level=logging.INFO)


class TrainSKInterface:
    def __init__(self) -> None:
        # Set the params for the training below
        self.text_pipeline = None
        self.dataset_all = None
        self.train, self.val, self.test = None, None, None
        self.target_classes = None
        self.dataset_name = "travel"
        self.model_name = "classifier_pipeline.pkl"
        self.output_path = environ["OUTPUT_PATH"]
        self.file_name = environ["DATA_SOURCE"]

    def read_dataset(self) -> None:
        """
        Reads the CSV file from path

        Returns:
            None
        """
        self.dataset_all = pd.read_csv(self.file_name, usecols=["text", "target"])

        return None

    def split_dataset(self) -> None:
        """
        Split the dataset into train, validate and test

        Raises:
            Error: if dataset_train and dataset_test are not set

        Returns:
            None
        """
        if self.dataset_all is None:
            raise Exception("Train or test data not set")

        self.target_classes = self.dataset_all["target"].unique()
        self.dataset_all["target"] = self.dataset_all["target"].astype("category")
        self.dataset_all["target"] = self.dataset_all["target"].cat.codes
        self.train, self.val = train_test_split(self.dataset_all, test_size=0.2)

        assert set(self.val["target"].tolist()) == set(
            self.train["target"].tolist()
        ), "Splitting the data did not result"

        return None

    def train_model(self) -> None:
        """
        Train and save the model
        """
        self.text_pipeline = Pipeline(
            [
                ("vect", CountVectorizer()),
                ("tfidf", TfidfTransformer()),
                ("clf", MultinomialNB()),
            ]
        )

        self.text_pipeline.fit(
            self.train["text"].tolist(),
            self.train["target"].tolist(),
        )

        return None

    def save_model(self) -> None:
        """
        Saves the model to the local path
        """
        logging.info(f"Writing tokenizer into {self.output_path}")
        if not exists(self.output_path):
            makedirs(self.output_path)
        # Save the Tokenizer and target classes to pickle file
        with open(f"{self.output_path}/{self.model_name}", "wb") as handle:
            dump([self.text_pipeline, self.target_classes], handle)

        return None

    def get_model(self) -> None:
        """
        Get the model if it is available locally
        """
        if exists(f"{self.output_path}/{self.model_name}"):
            logging.info(f"Loading classifier pipeline from {self.output_path}")
            with open(f"{self.output_path}/{self.model_name}", "rb") as handle:
                [self.text_pipeline, self.target_classes] = load(handle)
        else:
            logging.info(f"Model has not been trained yet!")

        return None

    def infer_model(self) -> str:
        """
        Perform an inference on the model that was trained
        """
        if self.text_pipeline is None:
            self.get_model()

        infer_data = self.val["text"].iloc[10]
        logging.info(f"-----START INFERENCE-----")
        prediction = self.text_pipeline.predict([infer_data])
        predicted_label = self.target_classes[prediction[0]]
        logging.info(f"The input '{infer_data}' was predicted as '{predicted_label}'")
        logging.info(f"-----END INFERENCE-----")

        return predicted_label

    def run_workflow(self) -> None:
        """
        Run the training script with all the necessary steps
        """
        self.read_dataset()
        self.split_dataset()

        self.get_model()
        if self.text_pipeline is None:
            # Train the model if no model is available
            logging.info(f"Training classifier and saving it locally")
            self.train_model()
            self.save_model()

        self.infer_model()

        return None


if __name__ == "__main__":
    train_obj = TrainSKInterface()
    train_obj.run_workflow()
