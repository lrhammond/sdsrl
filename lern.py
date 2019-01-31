# LERN
# Functions for learning schemas from matrices, and also for learning policies from schemas


import util
from scipy import optimize
import numpy as np


# Checks whether existing schemas predict a datapoint correctly or not
def check(schemas, evidence, [xRow, yRow[i]], index):
    # If no schema is active we output "none"
    output = "none"
    # Check each schema that predicts this attribute of the object
    for i in range(len(schemas[index])):
        if schemas[index][i].isActive(xRow):
            # If an active schema predicts the attribute value correctly and there are no wrong predictions, output "predicted"
            if schemas[index][i].head == yRow[i] and output != "wrong":
                output = "predicted"
                continue
            # If an incorrect prediction is made by a schema we remove it and add the relevant evidence back to the learning data
            else:
                
                output == "wrong"
    return output
                

# Learns schemas for a particular object attribute given data X and y
def learnSchemas(X, y, regConst=10, L=10)

    # Initialise variables
    schemas = []
    ones = np.ones(1, len(X[0]))
    zeros = np.zeros(1, len(X[0]))
    reg = regConst * ones

    # While there are still schema transitions to explain and the complexity limit L has not been reached
    while (sum(y) > 0) and (len(schemas) < L):
        
        # Intialise LP inputs
        f = np.zeros(1, len(X[0]))
        A = []
        
        # Check each row in X
        for i in range(len(X)):
            # Solve for as many positive cases as possible
            if y[i] == 1:
                f = f + (ones - np.array(X[i]))
            # Add constraints to prevent false negatives
            else if y[i] == 0:
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
        schemas.append(w_binary)
        W = np.array(schemas)
        
        # Remove solved entries from X and y
        for j in range(len(X)):
            if (y[j] == 1) and (sum(W * np.array(X[j])) >= 1):
                del X[i]
                del y[i]
        
    return schemas