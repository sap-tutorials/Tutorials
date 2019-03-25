from __future__ import absolute_import
from __future__ import division
from __future__ import print_function

import sys, os, shutil, time, tempfile, errno

sys.path.append('/home/tmsadm/models/samples/core/get_started')
import iris_data
import tensorflow as tf

tf.app.flags.DEFINE_integer('steps'           , 10000                                    , 'number of training steps.')
tf.app.flags.DEFINE_integer('batch_size'      , 100                                      , 'batch size.')
tf.app.flags.DEFINE_string ('export_path'     , os.path.expanduser("~") + '/export/iris' , 'export path')
args = tf.app.flags.FLAGS

def main(unused_argv):
  tf.logging.set_verbosity(tf.logging.INFO)

  ####################################################################
  # Beginning of training section
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

  ####################################################################
  # Beginning of export section
  ####################################################################
  # Define the input receiver spec
  feature_spec = {
      'PetalLength': tf.placeholder(dtype=tf.float32, shape=[None,1], name='PetalLength'),
      'PetalWidth' : tf.placeholder(dtype=tf.float32, shape=[None,1], name='PetalWidth'),
      'SepalLength': tf.placeholder(dtype=tf.float32, shape=[None,1], name='SepalLength'),
      'SepalWidth' : tf.placeholder(dtype=tf.float32, shape=[None,1], name='SepalWidth'),
  }
  # Define the input receiver for the raw tensors
  def serving_input_receiver_fn():
    return tf.estimator.export.build_raw_serving_input_receiver_fn(feature_spec)()

  # decreate a temp dir for the raw model export
  tmp_model_dir = tempfile.mkdtemp()

  # export the raw model
  raw_model_path = classifier.export_savedmodel(tmp_model_dir, serving_input_receiver_fn=serving_input_receiver_fn)

  with tf.Graph().as_default():
    with tf.Session() as session:
      # reload the model to add the default signature
      tf.saved_model.loader.load(session, [tf.saved_model.tag_constants.SERVING], raw_model_path)

      # build tensors info for the inputs & the outputs into the signature def
      signature_def = (
        tf.saved_model.signature_def_utils.build_signature_def(
          inputs = {
            'PetalLength': tf.saved_model.utils.build_tensor_info(tf.get_default_graph().get_tensor_by_name('PetalLength:0')),
            'PetalWidth' : tf.saved_model.utils.build_tensor_info(tf.get_default_graph().get_tensor_by_name('PetalWidth:0')),
            'SepalLength': tf.saved_model.utils.build_tensor_info(tf.get_default_graph().get_tensor_by_name('SepalLength:0')),
            'SepalWidth' : tf.saved_model.utils.build_tensor_info(tf.get_default_graph().get_tensor_by_name('SepalWidth:0')),
          },
          outputs = {
            'predicted_class_id' : tf.saved_model.utils.build_tensor_info(
              # cast the ExpandDims tensors into a float instead of an int64
              tf.cast (
                tf.get_default_graph().get_tensor_by_name(
                  'dnn/head/predictions/ExpandDims:0'),
                  dtype = tf.float32
                )
              ),
            'probabilities'      : tf.saved_model.utils.build_tensor_info(tf.get_default_graph().get_tensor_by_name('dnn/head/predictions/probabilities:0'))
           },
           method_name = tf.saved_model.signature_constants.PREDICT_METHOD_NAME
         )
       )

       # remove all previous final models
       try:
         shutil.rmtree(args.export_path)
       except OSError as e:
         if e.errno != errno.EEXIST and e.errno != errno.ENOENT:
           raise

       # save the model with proper format
       builder = tf.saved_model.builder.SavedModelBuilder(args.export_path + "/" + str(int(round(time.time()))))
       builder.add_meta_graph_and_variables (
         session,
         [tf.saved_model.tag_constants.SERVING],
         signature_def_map = {
             tf.saved_model.signature_constants.DEFAULT_SERVING_SIGNATURE_DEF_KEY : signature_def
         },
       )
       # save the model with a default signature
       builder.save(as_text=False)

   # remove the intermediate saved model
   try:
     shutil.rmtree(tmp_model_dir)
   except OSError as e:
     if e.errno != errno.EEXIST and e.errno != errno.ENOENT:
       raise
   ####################################################################
   # End of export section
   ####################################################################

 if __name__ == '__main__':
     tf.app.run()
