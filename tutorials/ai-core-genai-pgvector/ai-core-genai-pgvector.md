---
parser: v2
auto_validation: true
time: 20
tags: [ tutorial>Beginner, software-product>sap-ai-core, topic>machine-learning]
primary_tag: software-product>sap-ai-core
author_name: Dhrubajyoti Paul 
author_profile: https://github.com/dhrubpaul
---

# Using pgvector store with LLMs in AI core through Gen AI SDK
<!-- description --> Using pgvector from PostgreSQL to store embeddings and using them in similarity search.

## Prerequisites

- Access to SAP AI core with sap extended plan.
- An already spinned up deployment for text embedding. [Refer here](https://developers.sap.com/tutorials/ai-core-generative-ai.html)

## You will learn

- How to set up PostgreSQL with pgvector in SAP BTP
- How to use pgvector to do similarity search

### Enable PostgreSQL

You need access to PostgreSQL on SAP BTP, hyperscaler option in your SAP global account.

![image](images/pg01.png)

### Assign Entitlement for PostgreSQL in sub-account

Go to Entitlements tab and click on the **Configure Entitlements** button.

![image](images/pg03.png)

Click on **Add Service Plans** button.

![image](images/pg02.png)

Choose the **PostgreSQL, Hyperscaler Option**.

![image](images/pg11.png)

### Create a PostgreSQL instance

To create a PosrgreSQL instance in your BTP subaccount, go to **Services** -> **Instances and Subscriptions**. Click on the **Create** button.

![image](images/pg10.png)

Choose the **PostgreSQL, Hyperscaler Option** service. Give a name for the Instance. Leave everything else as it is. Click on the **Next** button.

![image](images/pg09.png)

Choose PostgreSQL Engine Version greater than 13. Click on the **Next** button.

![image](images/pg08.png)

Click **Create** to create your instance

![image](images/pg07.png)

### Enabling pgvector

Once the postgreSQL instance is spinned up, we can enable the pgvector extnsion in postgreSQL.

![image](images/spin_up.png)

Open your terminal and login to cloud foundry.

```TEXT
CF login -a <endpoint-url>
```

![image](images/cf_login.png)

Run the following command to get your authentication token.

```TEXT
cf oauth-token
```

![image](images/pg12.png)

Once we have the authentication token, run the following command to enable pgvector in your PostgreSQL

```TEXT
curl -X PUT 'https://api-backing-services.<cf_region>.data.services.cloud.sap/v1/postgresql-db/instances/<instance_id>/extensions/vector' --header 'Authorization: bearer <token>' --header 'Content-Type: application/json' --data-raw '{"database": "<db_name>"}' 
```

![image](images/pg19.png)

The instance_id can be found in BTP by clicking on the instance.

![image](images/pg05.png)

### Creating vector embeddings of documents

Install generative-ai-hub-sdk in your system, using the following command.

```TEXT
pip3 install generative-ai-hub-sdk
```

You can check your installation using the command 

```TEXT
pip show generative-ai-hub-sdk
```

![image](images/pg04.png)

Make sure that you have generative-ai-hub-sdk of version >= 0.1.0.

Use the python environment of your choice to run the following code.

```PYTHON
from langchain.document_loaders import PyPDFLoader
from langchain.vectorstores.pgvector import PGVector
from gen_ai_hub.proxy.core.proxy_clients import get_proxy_client

proxy_client = get_proxy_client('gen-ai-hub')

```

Here we create embeddings for the GenAI hub help documentations. Download the following documents and save them in your current working folder.

[GenAI Hub in AI Core documentation](https://help.sap.com/doc/7ca8e589556c4596abafe95545dfc212/CLOUD/en-US/553250b6ec764a05be43a7cd8cba0526.pdf)

[GenAI Hub in AI Launchpad documentation](https://help.sap.com/doc/d2ad92a97b5245279120bff4397f7865/CLOUD/en-US/4705bbd2e82a455aa4cb11082adcf27e.pdf)

Now run the following code snippet to load the documents.

```PYTHON
# Load PDF
loaders = [
    PyPDFLoader("ailaunchpad_genai.pdf"),
    PyPDFLoader("aicore_genai.pdf")]
docs = []
for loader in loaders:
    docs.extend(loader.load())
```

Import and initialize the text embedding model from generative-ai-hub-sdk.

```PYTHON
from gen_ai_hub.proxy.langchain.init_models import init_embedding_model

embeddings = init_embedding_model('text-embedding-ada-002')

response = embeddings.embed_query("Just a test for embeddings..")
print(response)
```

![image](images/pg18.png)

### Creating an app in Cloud Foundry

As a security policy, SAP restricts direct access to hosted instance of PostgreSQL over the internet. To access the hosted instance of PostgreSQL over the internet, you have to use a service that is hosted within the Cloud Foundry, which is binded to the hosted instance of PostgreSQL. To fix this, we can create a cloud foundry app that binds to the PostgreSQL instance. 

To create an app in cloud foundry, create a new folder and add the following files

```PYTHON hello.py
from flask import Flask
import os

app = Flask(__name__)

# Get port from environment variable or choose 9099 as local default 
port = int(os.getenv("PORT", 9099))
@app.route('/')
def hello_world():
    return 'Hello World! I am an instance ' + str(os.getenv("CF_INSTANCE_INDEX", 0))

if __name__ == '__main__':
    # Run the app, listening on all IPs with our chosen port number
    app.run(host='0.0.0.0', port=port)   

```

```TEXT requirements.txt
flask
gunicorn
```

```YAML manifest.yaml
---
applications:
- name: test_app_1101
  memory: 128MB
  disk_quota: 256MB
  random-route: true
  buildpack: python_buildpack
  command: python hello.py
```

After creating the above files, open a terminal in the same folder and login to cloud foundry

```TEXT
cf login -a <api_url> -u <user> -o <org> -s <space>
```

![image](images/pgcf1.png)

Push your app to cloud foundry using the following command

```TEXT
cf push
```

![image](images/pgcf2.png)

![image](images/pgcf3.png)

You can list the apps present in cloud foundry using the following command.

```TEXT
cf apps
```

![image](images/pgcf4.png)

You have to make sure that SSH is enabled on cloud foundry. Use the following command to check whether SSH is enabled.

```TEXT
cf ssh-enabled <app-name>
```

If SSH is disabled, run the following command to enable it

```TEXT
cf enable-ssh <app-name>
```

![image](images/pgcf5.png)

Once you enable SSH in your app, you have to restage the app. Run the following command in your terminal

```TEXT
cf restage <app-name>
```

![image](images/pgcf6.png)

![image](images/pgcf7.png)

Now we have to bind this application to our PostgreSQL instance. 

Go to your BTP subaccount. Go to **Services** -> **Instances and Subscriptions**. 

Go to your instance.

Click on the **create** button

![image](images/bindbtp1.png)

Choose your application and click on the **Create** button.

![image](images/bindbtp2.png)

Now we can use this app as a proxy in PostgreSQL. Run the following command in the terminal

```TEXT
cf ssh -L localhost:5898:<host-name>:<port> <app-name> -N
```

### Connecting to the database

The pgvector needs a connection string to connect to the database. The connection string takes the format 

```TEXT
postgresql+psycopg2://<username>:<password>@<hostname>:<port_no>/<dbname>
```

These values can be found in the service keys of postgreSQL instance in your BTP. Alternatively, you can create the connection string from the environment variables.

```PYTHON
import os

CONNECTION_STRING = PGVector.connection_string_from_db_params(
    driver=os.environ.get("PGVECTOR_DRIVER", "psycopg2"),
    host=os.environ.get("PGVECTOR_HOST", <host-name>),
    port=int(os.environ.get("PGVECTOR_PORT", <port-number>)),
    database=os.environ.get("PGVECTOR_DATABASE", <database-name>),
    user=os.environ.get("PGVECTOR_USER",<user_id>),
    password=os.environ.get("PGVECTOR_PASSWORD",<your-password>),
)
```
![image](images/alter.png)

Set a collection name of your choice

```PYTHON
COLLECTION_NAME = "my-docs"
```

Run the following code to create the PGVector Database

```PYTHON
db = PGVector.from_documents(
    embedding=embeddings,
    documents=docs,
    collection_name=COLLECTION_NAME,
    connection_string=CONNECTION_STRING,
)
```

![image](images/pg17.png)

### Similarity search in Vector Database

The PGVector has built-in methods to do similarity search in the document. Run the following code to find the similar content from the documents for the given query.


```PYTHON
query = "How to deploy a model in AI core GenAI Hub"

results = db.similarity_search(query, k=1)
answer = results[0].page_content
print(answer)

```

![image](images/similarity.png)

Now you can use the gpt-4 model to answer the above query while using the above output as reference. This will give more context for the gpt-4 model to answer the question. Run the following code to call the gpt-4 model and pass the query along with output from previous step

```PYTHON
from gen_ai_hub.proxy.langchain.init_models import init_llm

llm = init_llm('gpt-4', temperature=0., max_tokens=256)

llm.invoke("Refer this "+ answer + " and now answer " + query).content
```

![image](images/grounded.png)

