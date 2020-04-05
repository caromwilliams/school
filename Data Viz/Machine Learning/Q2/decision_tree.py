from util import entropy, information_gain, partition_classes
import numpy as np 
import ast

class DecisionTree(object):
    def __init__(self):
        # Initializing the tree as an empty dictionary or list, as preferred
        #self.tree = []
        #self.tree = {}
        self.tree = {}
        pass

    def learn(self, X, y):
        # TODO: Train the decision tree (self.tree) using the the sample X and labels y
        # You will have to make use of the functions in utils.py to train the tree
        
        # One possible way of implementing the tree:
        #    Each node in self.tree could be in the form of a dictionary:
        #       https://docs.python.org/2/library/stdtypes.html#mapping-types-dict
        #    For example, a non-leaf node with two children can have a 'left' key and  a 
        #    'right' key. You can add more keys which might help in classification
        #    (eg. split attribute and split value)
        self.X = X
        self.y = y

        num_samples = self.X.shape[0]
        num_features = self.X.shape[1]

        m = self.max_features(num_features)
        indices = np.random.choice(num_features, size=m, replace=False)

        self.tree = self.create_tree(X, y, indices)
        pass


    def classify(self, record):
        # TODO: classify the record using self.tree and return the predicted label
        node = self.tree

        while isinstance(node, TreeNode):
            if test_instance[node.fi] > node.thresh:
                node = node.b_2
            else:
                node = node.b_1
        return node
        pass
