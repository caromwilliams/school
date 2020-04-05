package edu.gatech.cse6242

import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.SparkConf
import org.apache.spark.sql.SQLContext
import org.apache.spark.sql.functions._

object Q2 {

	def main(args: Array[String]) {
    	val sc = new SparkContext(new SparkConf().setAppName("Q2"))
		val sqlContext = new SQLContext(sc)
		import sqlContext.implicits._

	
    	val file = sc.textFile("hdfs://localhost:8020" + args(0))

        val source = file.map(_.split("\t")).map(p => (p(0).toInt, p(2).toInt)).toDF("source","source_weight")

	val source2 = source.filter(source("source_weight") >4)
			.groupBy("source")
			.agg(-(sum("source_weight")) as "source_weight")

	val target = file.map(_.split("\t")).map(p => (p(1).toInt, p(2).toInt)).toDF("target","target_weight")

	val target2 = target.filter(target("target_weight") >4)
			.groupBy("target")
			.agg(sum("target_weight") as "weight")

	val result = source2.unionAll(target2)

	val final_result = result .groupBy("source")
			.agg(sum("source_weight"))		
		
    	final_result.map(x => x.mkString("\t")).saveAsTextFile("hdfs://localhost:8020" + args(1))
  	}
}
