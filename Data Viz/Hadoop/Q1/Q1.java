package edu.gatech.cse6242;

import java.util.StringTokenizer;

import org.apache.hadoop.fs.Path;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.io.*;
import org.apache.hadoop.mapreduce.*;
import org.apache.hadoop.util.*;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import java.io.IOException;

public class Q1 {

	public static class Map extends Mapper<Object, Text, Text, IntWritable>{
		private Text tgt = new Text();
		private IntWritable weight = new IntWritable();
      
	public void map(Object key, Text value, Context context) throws IOException, InterruptedException{
        	String row = value.toString();
		String tokens[] = row.split("\t");
        	tgt.set((tokens[1]));
        	weight.set(Integer.parseInt(tokens[2]));
        context.write(tgt, weight);		
		}
	    }

	public static class Reduce extends Reducer<Text, IntWritable, Text, IntWritable>{
        	private IntWritable final_result = new IntWritable();

        public void reduce(Text key, Iterable<IntWritable> values, Context context) throws IOException, InterruptedException{
	int min_degree = Integer.MAX_VALUE;
        for(IntWritable val : values) {
		if(val.get() <= min_degree) 
			{min_degree = val.get();}
		}
	final_result.set(min_degree);
	context.write(key, final_result);
        	}
	}

	public static void main(String[] args) throws Exception {
	Configuration conf = new Configuration();
	Job job = Job.getInstance(conf, "Q1");

	job.setJarByClass(Q1.class);
	job.setMapperClass(Map.class);
	job.setCombinerClass(Reduce.class);
	job.setReducerClass(Reduce.class);
	job.setOutputKeyClass(Text.class);
	job.setOutputValueClass(IntWritable.class);

	FileInputFormat.addInputPath(job, new Path(args[0]));
	FileOutputFormat.setOutputPath(job, new Path(args[1]));
	System.exit(job.waitForCompletion(true) ? 0 : 1);
		}
	}
