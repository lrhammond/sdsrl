# LERN
# Functions for learning schemas from matrices, and also for learning policies from schemas


import util
from scipy import optimize
import numpy as np


# Learns schemas for a particular object attribute given data X and y
def learnSchemas(X, y, schemas, regConst=10, L=10):
    # Initialise variables
    currSchemas = [util.toBinarySchema(schema) for schema in schemas]
    newSchemas = []
    newEvidence = []
    ones = np.ones(1, len(X[0]))
    zeros = np.zeros(1, len(X[0]))
    reg = regConst * ones
    # While there are still schema transitions to explain and the complexity limit L has not been reached
    while (sum(y) > 0) and (len(currSchemas) < L):
        # Intialise LP inputs
        f = np.zeros(1, len(X[0]))
        A = []
        # Check each row in X
        for i in range(len(X)):
            # Solve for as many positive cases as possible
            if y[i] == 1:
                f = f + (ones - np.array(X[i]))
            # Add constraints to prevent false negatives
            elif y[i] == 0:
                row = [X[i] - 1 for i in range(len(X[i]))]
                A.append(row)
            # Check for non-binary values                
            else:
                print("Error, y is non-binary")
                return
        # Set up LP and solve for new schema w
        f = f + reg
        lb = np.negative(np.ones(1, len(A)))
        A_mat - np.array(A)
        w = optimize.linprog(f, A_mat, lb, bounds=(0,1))
        print w
        # Convert w to binary version and add to set of schemas
        w_binary = w > 0
        newSchemas.append(w_binary)
        currSchemas.append(w_binary)
        W = np.array(currSchemas)
        # Remove solved entries from X and y
        for j in range(len(X)):
            if (y[j] == 1) and (sum(W * np.array(X[j])) >= 1):
                newEvidence.append(X[j])
                del X[j]
                del y[j]
    return [newSchemas, newEvidence, X]