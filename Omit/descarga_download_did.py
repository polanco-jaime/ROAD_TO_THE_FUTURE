from google.cloud import bigquery
from google.oauth2 import service_account as sa
import os
import pandas as pd

#################################### 

# Download query results.


def from_bq_to_pc (base_name ,
                   query_string  ,  
                   service_account  
                   ):
    import os
    
    try:
        os.chdir("C:/Users/USER/Desktop/DID roads/ROAD_TO_THE_FUTURE/Data")
    except:
        cwd = os.getcwd()
        # Print the current working directory
        print("Current working directory: {0}".format(cwd))
 
     
    credentials = sa.Credentials.from_service_account_info( service_account   )  
    bqclient = bigquery.Client(credentials=credentials, project= credentials.project_id,)
    # create_replace_final_table = """ 
    #                             CREATE OR REPLACE TABLE  `lee-javeriana.01_road_to_the_future.01_roads_to_the_future` 
    #                             OPTIONS ( ) AS

    #                             SELECT *,  'base_10p' fuente FROM `lee-javeriana.01_road_to_the_future.view_base_10p`  
    #                             UNION ALL
    #                             SELECT *,  'base_ai' fuente FROM `lee-javeriana.01_road_to_the_future.view_base_ai`  
    #                             UNION ALL
    #                             SELECT *,  'base_ic' fuente FROM `lee-javeriana.01_road_to_the_future.view_base_ic`  
    #                             UNION ALL
    #                             SELECT *,  'base_ent' fuente FROM `lee-javeriana.01_road_to_the_future.view_base_ent`  
    #                             """
    # job_config = bigquery.QueryJobConfig()
    # job_config.allow_large_results = True
    # query_job = bqclient.query( create_replace_final_table,  location='US',  job_config=job_config)  # API request - starts the query
    # query_job.result() 

     
    dataframe = ( bqclient.query( query_string.format(base_name) ).result().to_dataframe() )

    dataframe.to_parquet(f'{base_name}.parquet')
    print(dataframe.head())
    return print(dataframe.head())

##############################
query =  """SELECT * except(fuente) 
		 FROM  `lee-javeriana.01_road_to_the_future.01_roads_to_the_future`  
		WHERE fuente = '{}' """
cuenta = {
  "type": "service_account",
  "project_id": "lee-javeriana",
  "private_key_id": "ba7e08422b3cf594f5c57b2c1933549a7729d028",
  "private_key": "-----BEGIN PRIVATE KEY-----\nMIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQDoKDeNfvP5eSZw\nXZTTT72IcJ49O5QB3VdF6OYPolNBbffL2PDeDwevBEGZhK9aKUkbT5fguMElzd0z\niklPuH8KMGptTcUSWPYNt0k4NrJoc7Kg8lq3/KaxnIeDirMqlQZbvSGvvvgdOHeN\nJyIIVpNoKuaYM5FkMWgPeivJ/QXLyVWGX7n2AUKmgguLG59k1ePyqd9D7bNf5MD9\nktCRO8we4emyCkNTHbjBWv9s+jKZ2eyEqYxgFZqFEEptNl8c8HPkizUVfk1Xspgj\nbhqjRBu269ncELCZkjtkLPxBUvUDsAWndgNyUp4C2R1sYxeBJFNmOdZQnqAZ8PZM\nZf7M7FKfAgMBAAECggEABKZr+NZ7Plrjkh2cJQcBKPheGwp6wpAAUJyp7jpv1RNf\ngsKqJ6O0B8voh0T5APyeg8Cg4XXuthPWo4XyEpNp+M/5po3Uh2am99hdGMlzry1R\nSlfNL5RzP4LlbfuU7u+EOWMwIQwmSra+VXv14QytScla/0WmwS1O92LL+Yqq/cCi\nK0KlQ3Owa4oI+f8hrcem9vgMcU5Rm4iWEISuffb5aU3UrASpkhPtylqKvKbNymc0\nPNx6YX4mLgcrt7oW9LJyYubh0bXCLR4jBk66Lb2eDK/bpr4zhbnQ0boSPV0hJC86\nQBzgizjQLvQ6ZQQwmraAJtRLdDAQMhKnwTklEpDzTQKBgQD0Jlp7dZIwi/Lzumbq\nOET2xEW2kX9q5fB9ug3XN/mar98sL7UpoqvAp6idJzS/7m40GMOWCCZkOyqeoWsR\nlZAe/oYjsvQ1dluezhge4n1glDhOx6yNTCoIGTjVcQCAmbHskUPxmH7Mr9N9B/lr\nG6bTpqXmBiNk4fGnsFddKJeFBQKBgQDzbNmONLy2epM0cA2t/sUVx6I7HZTG6nET\nF+yGyT6SB72SdYhVb1QtNQgxju8dcgcuQhBZ2OnVvWB8BGMMtRuDV5+2hxIU+qaI\ncsYBH01fjea1YPvjWciCyxUTwPDx3cfgTyKr+UXN+fR+lkMsP+jJB761mA5gJejB\n8oc2OVIKUwKBgDcOqxrqQYRXGvuhjnHFLLAlRL9OAljd76S6n0Joag5bGM3DGGyC\nNY8lf92oqLmbkBiJeRnNTrMNsKfDKPz3KGAE78T/JxkjTrf6K/BTDQkJg8UoPB5F\nDblzzqA7cmyNvFTn0VWbGQUZpWytrq5btvxFuWWs/rBST4eMObdVXw3tAoGAQA6e\n2XsrReHPpvbtwqHOf0K/gErygb3j76BWD8oZexgqXMYtED5xqkvQUipI5jjBhvy/\njMU2E5b4Mup8njVyfgeq8b9tlLO46yM408bG96RB1+0jm9pel3yVmVMmlJ9GH9Lz\nTsoUAnYKTt1/478VpQEywFq5Pbf8Hj2SM0S2QB0CgYEApDcqXiZqqZSbLgZxcWoz\nSH/yWA+1fg/Zr7rFXvl0+kUIYGL3mjXhBOdqs+JyB00MhGkWMKHvaohvJNSPqBYX\nXCTbY5fMvxcfpL8PnInLZ9MovXQuMPkedCNb85y3Klf6Ltq6q8M6dg5juZPyF9G9\nJJfR4hgT70mbyLfOheLwfo8=\n-----END PRIVATE KEY-----\n",
  "client_email": "laptop-connect@lee-javeriana.iam.gserviceaccount.com",
  "client_id": "115261421246460401122",
  "auth_uri": "https://accounts.google.com/o/oauth2/auth",
  "token_uri": "https://oauth2.googleapis.com/token",
  "auth_provider_x509_cert_url": "https://www.googleapis.com/oauth2/v1/certs",
  "client_x509_cert_url": "https://www.googleapis.com/robot/v1/metadata/x509/laptop-connect%40lee-javeriana.iam.gserviceaccount.com"
}
##################################
print("Current working directory  base_ai")
from_bq_to_pc ( base_name ="base_ai",
                   query_string = query ,  
                   service_account = cuenta  )

print("Current working directory base_10p")
from_bq_to_pc ( base_name ="base_10p",
                   query_string = query ,  
                   service_account = cuenta  )

print("Current working directory base_ic")
from_bq_to_pc ( base_name ="base_ic",
                   query_string = query ,  
                   service_account = cuenta  )

print("Current working directory  base_ent")
from_bq_to_pc ( base_name ="base_ent",
                   query_string = query ,  
                   service_account = cuenta  )




