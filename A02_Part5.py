# --------------------------------------------------------
#
# PYTHON PROGRAM DEFINITION
#
# The knowledge a computer has of Python can be specified in 3 levels:
# (1) Prelude knowledge --> The computer has it by default.
# (2) Borrowed knowledge --> The computer gets this knowledge from 3rd party libraries defined by others
#                            (but imported by us in this program).
# (3) Generated knowledge --> The computer gets this knowledge from the new functions defined by us in this program.
#
# When launching in a terminal the command:
# user:~$ python3 this_file.py
# our computer first processes this PYTHON PROGRAM DEFINITION section of the file.
# On it, our computer enhances its Python knowledge from levels (2) and (3) with the imports and new functions
# defined in the program. However, it still does not execute anything.
#
# --------------------------------------------------------

# ------------------------------------------
# IMPORTS
# ------------------------------------------
import pyspark
import pyspark.sql.functions


# ------------------------------------------
# FUNCTION my_main
# ------------------------------------------
def my_main(spark, my_dataset_dir):
    # 1. We define the Schema of our DF.
    my_schema = pyspark.sql.types.StructType(
        [pyspark.sql.types.StructField("start_time", pyspark.sql.types.StringType(), False),
         pyspark.sql.types.StructField("stop_time", pyspark.sql.types.StringType(), False),
         pyspark.sql.types.StructField("trip_duration", pyspark.sql.types.IntegerType(), False),
         pyspark.sql.types.StructField("start_station_id", pyspark.sql.types.IntegerType(), False),
         pyspark.sql.types.StructField("start_station_name", pyspark.sql.types.StringType(), False),
         pyspark.sql.types.StructField("start_station_latitude", pyspark.sql.types.FloatType(), False),
         pyspark.sql.types.StructField("start_station_longitude", pyspark.sql.types.FloatType(), False),
         pyspark.sql.types.StructField("stop_station_id", pyspark.sql.types.IntegerType(), False),
         pyspark.sql.types.StructField("stop_station_name", pyspark.sql.types.StringType(), False),
         pyspark.sql.types.StructField("stop_station_latitude", pyspark.sql.types.FloatType(), False),
         pyspark.sql.types.StructField("stop_station_longitude", pyspark.sql.types.FloatType(), False),
         pyspark.sql.types.StructField("bike_id", pyspark.sql.types.IntegerType(), False),
         pyspark.sql.types.StructField("user_type", pyspark.sql.types.StringType(), False),
         pyspark.sql.types.StructField("birth_year", pyspark.sql.types.IntegerType(), False),
         pyspark.sql.types.StructField("gender", pyspark.sql.types.IntegerType(), False),
         pyspark.sql.types.StructField("trip_id", pyspark.sql.types.IntegerType(), False)
         ])

    # 2. Operation C1: 'read' to create the DataFrame from the dataset and the schema
    inputDF = spark.read.format("csv") \
        .option("delimiter", ",") \
        .option("quote", "") \
        .option("header", "false") \
        .schema(my_schema) \
        .load(my_dataset_dir)

    # ------------------------------------------------
    # START OF YOUR CODE:
    # ------------------------------------------------

    # Remember that the entire work must be done “within Spark”:
    # (1) The function my_main must start with the creation operation 'read' above loading the dataset to Spark SQL.
    # (2) The function my_main must finish with an action operation 'collect', gathering and printing by the screen the result of the Spark SQL job.
    # (3) The function my_main must not contain any other action operation 'collect' other than the one appearing at the very end of the function.
    # (4) The resVAL iterator returned by 'collect' must be printed straight away, you cannot edit it to alter its format for printing.

    inputDF.persist()

    # 3. Operation T1: get all the distinct start station names
    distinctStartStationDF = inputDF.select(pyspark.sql.functions.col("start_station_name").alias("station")).distinct()

    # 4. Operation P1: We persist distinctStartStationDF
    distinctStartStationDF.persist()

    # 5. Operation T2: get all the distinct stop station names
    distinctStopStationDF = inputDF.select(pyspark.sql.functions.col("stop_station_name").alias("station")).distinct()

    # 6. Operation P2: We persist distinctStopStationDF
    distinctStopStationDF.persist()

    # 7. Merge dataframe distinctStartStationDF and distinctStopStationDF
    unionDF = distinctStartStationDF.union(distinctStopStationDF)

    # 8. Operation P3: We persist unionDF
    unionDF.persist()

    # 9. Get the unique station names only
    resultDF = unionDF.distinct()

    # 10. Operation P4: We persist resultDF
    resultDF.persist()

    # 11. Operation A1: Action of displaying the content of resultDF
    resultDF.show()

    # 12. Operation T3: We get the left join based in one column
    resultDF1 = resultDF.join(inputDF,
                              resultDF["station"] == inputDF["start_station_name"],
                              "left_outer"
                              )
    # 13. Operation P5: We persist resultDF1
    resultDF1.persist()

    # 14. Operation T4: Agg to get the count of trip starting from each station
    resultDF2 = resultDF1.groupBy(["station"]).agg({"start_station_name": "count"}).orderBy(
        pyspark.sql.functions.col("station").asc())

    # 15. Operation P5: We persist resultDF2
    resultDF2.persist()

    # 16. Operation T4: We get the left join based in one column
    resultDF3 = resultDF.join(inputDF,
                              resultDF["station"] == inputDF["stop_station_name"],
                              "left_outer"
                              )

    # 17. Operation P6: We persist resultDF3
    resultDF3.persist()

    # 18. Operation T5: Agg to get the count of trip ending at each station
    resultDF4 = resultDF3.groupBy(["station"]).agg({"stop_station_name": "count"}).orderBy(
        pyspark.sql.functions.col("station").asc())

    # 19. Merge two dataframes
    resultDF5 = resultDF2.alias("a").join(resultDF4.alias("b"),
                                          resultDF2["station"] == resultDF4["station"],
                                          "inner").select("a.station", "a.count(start_station_name)",
                                                          "b.count(stop_station_name)")

    # 20. Operation P7: We persist resultDF5
    resultDF5.persist()

    # 21. Operation T6: Change Column name from "count(start_station_name)" to "num_departure_trips"
    resultDF6 = resultDF5.withColumnRenamed("count(start_station_name)", "num_departure_trips")

    # 22. Operation P7: We persist resultDF6
    resultDF6.persist()

    # 23. Operation T7: Change Column name from "count(stop_station_name)" to "num_arrival_trips"
    resultDF7 = resultDF6.withColumnRenamed("count(stop_station_name)", "num_arrival_trips")

    # 24. Operation P8: We persist resultDF7
    resultDF7.persist()

    # 24. Operation S1: Sort station names
    solutionDF = resultDF7.orderBy(pyspark.sql.functions.col("station").asc())

    # ------------------------------------------------
    # END OF YOUR CODE
    # ------------------------------------------------

    # Operation A1: 'collect' to get all results
    resVAL = solutionDF.collect()
    for item in resVAL:
        print(item)


# --------------------------------------------------------
#
# PYTHON PROGRAM EXECUTION
#
# Once our computer has finished processing the PYTHON PROGRAM DEFINITION section its knowledge is set.
# Now its time to apply this knowledge.
#
# When launching in a terminal the command:
# user:~$ python3 this_file.py
# our computer finally processes this PYTHON PROGRAM EXECUTION section, which:
# (i) Specifies the function F to be executed.
# (ii) Define any input parameter such this function F has to be called with.
#
# --------------------------------------------------------
if __name__ == '__main__':
    # 1. We use as many input arguments as needed
    pass

    # 2. Local or Databricks
    local_False_databricks_True = True

    # 3. We set the path to my_dataset and my_result
    my_local_path = "../../../../3_Code_Examples/L20-25_Spark_Environment/"
    my_databricks_path = "/"

    my_dataset_dir = "FileStore/tables/6_Assignments/my_dataset_1/"

    if local_False_databricks_True == False:
        my_dataset_dir = my_local_path + my_dataset_dir
    else:
        my_dataset_dir = my_databricks_path + my_dataset_dir

    # 4. We configure the Spark Session
    spark = pyspark.sql.SparkSession.builder.getOrCreate()
    spark.sparkContext.setLogLevel('WARN')
    print("\n\n\n")

    # 5. We call to our main function
    my_main(spark, my_dataset_dir)
