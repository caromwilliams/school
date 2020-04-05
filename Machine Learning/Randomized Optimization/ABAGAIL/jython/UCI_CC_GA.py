"""
Implementation of randomized hill climbing, simulated annealing, and genetic algorithm to
find optimal weights to a neural network classifying gamma.

Based on AbaloneTest.java by Hannah Lau
"""

from __future__ import with_statement

import os
import csv
import time

import sys
sys.path.append("/Users/carolinewilliams/Desktop/Machine Learning/Assignment 2/ABAGAIL/ABAGAIL.jar")

from func.nn.backprop import BackPropagationNetworkFactory
from shared import SumOfSquaresError, DataSet, Instance
from opt.example import NeuralNetworkOptimizationProblem

import opt.RandomizedHillClimbing as RandomizedHillClimbing
import opt.SimulatedAnnealing as SimulatedAnnealing
import opt.ga.StandardGeneticAlgorithm as StandardGeneticAlgorithm

#from __future__ import with_statement

INPUT_FILE = os.path.join("..", "src", "opt", "test", "UCI_Credit_Card.txt")

INPUT_LAYER = 23
HIDDEN_LAYER = 1
OUTPUT_LAYER = 1
TRAINING_ITERATIONS = 5000


def initialize_instances():
    """Read the gamma.txt CSV data into a list of instances."""
    instances = []

    # Read in the gamma.txt CSV file
    with open(INPUT_FILE, "r") as gamma:
        reader = csv.reader(gamma)

        for row in reader:
            instance = Instance([float(value) for value in row[:-1]])
            instance.setLabel(Instance(0 if float(row[-1]) ==0 else 1))
            instances.append(instance)

    return instances


def train(oa, network, oaName, instances, measure):
    """Train a given network on a set of instances.

    :param OptimizationAlgorithm oa:
    :param BackPropagationNetwork network:
    :param str oaName:
    :param list[Instance] instances:
    :param AbstractErrorMeasure measure:
    """
    print "\nError results for %s\n---------------------------" % (oaName,)

    for iteration in xrange(TRAINING_ITERATIONS):
        oa.train()

        error = 0.00
        for instance in instances:
            network.setInputValues(instance.getData())
            network.run()

            output = instance.getLabel()
            output_values = network.getOutputValues()
            example = Instance(output_values, Instance(output_values.get(0)))
            error += measure.value(output, example)

        print "%0.03f" % error


def main():
    """Run algorithms on the gamma dataset."""
    instances = initialize_instances()
    factory = BackPropagationNetworkFactory()
    measure = SumOfSquaresError()
    data_set = DataSet(instances)

    networks = []  # BackPropagationNetwork
    nnop = []  # NeuralNetworkOptimizationProblem
    oa = []  # OptimizationAlgorithm
    oa_names = ["GA"]
    results = ""
    graph_x = ""
    graph_y = ""

    classification_network = factory.createClassificationNetwork([INPUT_LAYER, HIDDEN_LAYER, OUTPUT_LAYER])
    networks.append(classification_network)
    nnop.append(NeuralNetworkOptimizationProblem(data_set, classification_network, measure))

    population = [100,50,10]
    mate = [50,25,5]
    mutate = [25,10,2]
    for k in range(3):
        oa.append(StandardGeneticAlgorithm(population[k], mate[k], mutate[k], nnop[0]))
        
        for i, name in enumerate(oa_names):
            start = time.time()
            correct = 0
            incorrect = 0
            
            train(oa[i], networks[i], oa_names[i], instances, measure)
            end = time.time()
            training_time = end - start

            print(population[k], mate[k], mutate[k])

            optimal_instance = oa[i].getOptimal()
            networks[i].setWeights(optimal_instance.getData())

            start = time.time()
            for instance in instances:
                networks[i].setInputValues(instance.getData())
                networks[i].run()

                predicted = instance.getLabel().getContinuous()
                actual = networks[i].getOutputValues().get(0)

                if abs(predicted - actual) < 0.5:
                    correct += 1
                else:
                    incorrect += 1

                end = time.time()
                testing_time = end - start

            results += "\nResults for %s, %s, %s, %s: \nCorrectly classified %d instances." % (name, population[k], mate[k], mutate[k], correct)
            results += "\nIncorrectly classified %d instances.\nPercent correctly classified: %0.03f%%" % (incorrect, float(correct)/(correct+incorrect)*100.0)
            results += "\nTraining time: %0.03f seconds" % (training_time,)
            results += "\nTesting time: %0.03f seconds\n" % (testing_time,)

            graph_x += "\npopulation:'%s', mate:'%s', mutate:'%s'" % (population[k],mate[k],mutate[k])
            graph_y += ",%0.2f" % ((float(correct)/(correct+incorrect)*100.0))

    #print graph_x
    print graph_y

if __name__ == "__main__":
    main()

