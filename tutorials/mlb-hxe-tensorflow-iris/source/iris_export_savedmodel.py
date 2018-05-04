from __future__ import absolute_import
from __future__ import division
from __future__ import print_function

import sys, os, shutil, time, urllib

sys.path.append(os.path.expanduser("~") + '/models/samples/core/get_started')
import iris_data
import tensorflow as tf

tf.app.flags.DEFINE_integer('steps'           , 10000           , 'number of training steps.')
tf.app.flags.DEFINE_integer('batch_size'      , 100             , 'batch size.')
tf.app.flags.DEFINE_string ('export_path'     , os.path.expanduser("~") + '/export/iris' , 'export path')
args = tf.app.flags.FLAGS

def main(unused_argv):
  tf.logging.set_verbosity(tf.logging.INFO)
  session = tf.InteractiveSession()

  ####################################################################
  # Begining of training section
  ####################################################################
  # Get the training data
  (train_x, train_y), (test_x, test_y) = iris_data.load_data()

  # Define the features used for training
  feature_columns = [tf.feature_column.numeric_column(key=key, shape=1, default_value=None, dtype=tf.float32) for key in train_x.keys()]

  # Build 3 layer DNN with 10, 20, 10 units respectively.
  classifier = tf.estimator.DNNClassifier(
    feature_columns=feature_columns,
    hidden_units=[10, 20, 10],
    n_classes=3,
  )
  # Train the model
  classifier.train(
    input_fn = lambda:iris_data.train_input_fn(train_x, train_y, args.batch_size),
    steps=args.steps
  )

  ####################################################################
  # End of training section
  ####################################################################
  # Begining of export section
  ####################################################################
  # Define the input receiver for the raw tensors
  def _serving_input_receiver_fn():
    feature_spec = {
      'PetalLength': tf.placeholder(dtype=tf.float32, shape=[1,1], name='PetalLength'),
      'PetalWidth' : tf.placeholder(dtype=tf.float32, shape=[1,1], name='PetalWidth'),
      'SepalLength': tf.placeholder(dtype=tf.float32, shape=[1,1], name='SepalLength'),
      'SepalWidth' : tf.placeholder(dtype=tf.float32, shape=[1,1], name='SepalWidth'),
    }
    return tf.estimator.export.build_raw_serving_input_receiver_fn(feature_spec)()

  # export the raw model
  path = classifier.export_savedmodel(args.export_path+ "-tmp", serving_input_receiver_fn=_serving_input_receiver_fn)

  # Note: one of the output is stored as an dt_int64 which is not yet supported in HANA, so we will need to cast it to a float
  tf.saved_model.loader.load(session, [tf.saved_model.tag_constants.SERVING], path)

  # build tensors info for the inputs & the outputs into the signature def
  signature_def = (
    tf.saved_model.signature_def_utils.build_signature_def(
      inputs = {
        'PetalLength': tf.saved_model.utils.build_tensor_info(tf.get_default_graph().get_tensor_by_name('PetalLength_1:0')),
        'PetalWidth' : tf.saved_model.utils.build_tensor_info(tf.get_default_graph().get_tensor_by_name('PetalWidth_1:0')),
        'SepalLength': tf.saved_model.utils.build_tensor_info(tf.get_default_graph().get_tensor_by_name('SepalLength_1:0')),
        'SepalWidth' : tf.saved_model.utils.build_tensor_info(tf.get_default_graph().get_tensor_by_name('SepalWidth_1:0')),
      },
      outputs = {
        'predicted_class_id' : tf.saved_model.utils.build_tensor_info(tf.get_default_graph().get_tensor_by_name('dnn/head/predictions/ExpandDims:0')),
        'probabilities'      : tf.saved_model.utils.build_tensor_info(tf.get_default_graph().get_tensor_by_name('dnn/head/predictions/probabilities:0'))
      },
      method_name = tf.saved_model.signature_constants.PREDICT_METHOD_NAME
    )
  )
  # recreate the model directory
  shutil.rmtree(args.export_path)
  os.makedirs(args.export_path)
  shutil.rmtree(args.export_path)

  # save the model with proper format
  builder = tf.saved_model.builder.SavedModelBuilder(args.export_path + "/" + str(int(round(time.time()))))
  builder.add_meta_graph_and_variables (
      session,
      [tf.saved_model.tag_constants.SERVING],
      signature_def_map = {
          tf.saved_model.signature_constants.DEFAULT_SERVING_SIGNATURE_DEF_KEY : signature_def
      },
  )
  builder.save(as_text=False)

  # remove the temporary model
  shutil.rmtree(path)
  ####################################################################
  # End of export section
  ####################################################################

  tf.reset_default_graph()
  session.close()

if __name__ == '__main__':
    tf.app.run()
