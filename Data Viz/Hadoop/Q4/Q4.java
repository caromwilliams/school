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


public class Q4 {
    
	public static class DegreeMapper extends Mapper<Object, Text, Text, IntWritable>{
		private Text id = new Text();
        	private IntWritable degree = new IntWritable();

        public void map(Object key, Text value, Context context) throws IOException, InterruptedException{
            StringTokenizer itr = new StringTokenizer(value.toString());
	int a2 = 1;
	int a1 = -1;
	for (int x = 0; itr.hasMoreTokens(); x++) {
		id.set(itr.nextToken());
		if ( (x & 1) == 0 ) {
			degree.set(a2); 
		} else { 
			degree.set(a1); 
		}
	context.write(id, degree);
            	}
	    }
    	}

	public static class FreqMapper extends Mapper<Object, Text, IntWritable, IntWritable>{
		private IntWritable id = new IntWritable();		
		private final static IntWritable one = new IntWritable(1);

	public void map(Object key, Text value, Context context) throws IOException, InterruptedException {
		StringTokenizer itr = new StringTokenizer(value.toString(), "\n");
	while (itr.hasMoreTokens()) {
        	String row = itr.nextToken();
		String tokens[] = row.split("\t");
        	id.set(Integer.parseInt(tokens[1]));
        	context.write(id, one);
		}
	    }
	}
    
	public static class DegreeReducer extends Reducer<Text,IntWritable, Text ,IntWritable> {
    		private IntWritable result = new IntWritable();

    	public void reduce(Text key, Iterable<IntWritable> values, Context context) throws IOException, InterruptedException {
      	int sum = 0;
      	for (IntWritable val : values) {
        	sum += val.get();
      		}
      	result.set(sum);
      	context.write(key, result);
    		}
	}

	public static class FreqReducer extends Reducer<IntWritable,IntWritable,IntWritable,IntWritable> {
		private IntWritable final_result = new IntWritable();

	public void reduce(IntWritable key, Iterable<IntWritable> values, Context context) throws IOException, InterruptedException {
	int sum = 0;
	for (IntWritable val : values) {
		sum += val.get();
		}
	final_result.set(sum);
	context.write(key, final_result);
		}
	}

	public static void main(String[] args) throws Exception {
        	Configuration conf = new Configuration();
        	Job job1 = Job.getInstance(conf, "Q4_job1");

        	job1.setJarByClass(Q4.class);
        	job1.setMapperClass(DegreeMapper.class);

        	job1.setCombinerClass(DegreeReducer.class);
        	job1.setReducerClass(DegreeReducer.class);

        	job1.setOutputKeyClass(Text.class);
        	job1.setOutputValueClass(IntWritable.class);
        	
		FileInputFormat.addInputPath(job1, new Path(args[0]));
        	FileOutputFormat.setOutputPath(job1, new Path("tmp"));

        	job1.waitForCompletion(true);
        	Job job2 = Job.getInstance(conf, "Q4_job2");

        	job2.setJarByClass(Q4.class);
        	job2.setMapperClass(FreqMapper.class);

        	job2.setCombinerClass(FreqReducer.class);
        	job2.setReducerClass(FreqReducer.class);

        	job2.setOutputKeyClass(IntWritable.class);
        	job2.setOutputValueClass(IntWritable.class);

        	FileInputFormat.addInputPath(job2, new Path("tmp"));
        	FileOutputFormat.setOutputPath(job2, new Path(args[1]));

        	System.exit(job2.waitForCompletion(true) ? 0:1);                                  
		}
	}