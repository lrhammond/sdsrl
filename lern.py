# LERN
# Functions for learning schemas from matrices, and also for learning policies from schemas


TOL = 0.001

import util
from scipy import optimize as opt
import numpy as np


# Learns schemas for a particular object attribute given data X and y
def learnSchemas(model, X, y, schemas, regConst=10, L=10):
    
    # If all the cases are positive there are no constraints for learning schemas so we do not try
    if y == [1 for entry in y]:
        return [[], [], X]
    
    # Initialise variables
    currSchemas = [util.toBinarySchema(model, schema) for schema in schemas]
    newSchemas = []
    newEvidence = []
    ones = np.ones((1, len(X[0])))
    zeros = np.zeros((1, len(X[0])))
    reg = regConst * ones
    # While there are still schema transitions to explain and the complexity limit L has not been reached
    while (sum(y) > 0) and (len(currSchemas) < L):
        # Intialise LP inputs
        f = np.zeros((1, len(X[0])))
        A = []
        # Check each row in X
        for i in range(len(X)):
            # Solve for as many positive cases as possible
            if y[i] == 1:
                f = f + (ones - np.array(X[i]))
            # Add constraints to prevent false negatives
            elif y[i] == 0:
                # row = [X[i] - 1 for i in range(len(X[i]))]
                row = X[i] - ones
                A.append(row)
            # Check for non-binary values                
            else:
                print("Error, y is non-binary")
                return
        # Set up LP and solve for new schema w
        f = f + reg
        lb = np.negative(np.ones((1, len(A))))
        A_mat = np.array(util.flatten(A))
        
    
        
        w = opt.linprog(f, A_mat, lb).x
        # Convert w to binary version and add to set of schemas
        w_binary = w > TOL
        w_binary = [int(entry) for entry in w_binary]
        newSchemas.append(w_binary)
        currSchemas.append(w_binary)
        W = np.array(currSchemas)
        # Remove solved entries from X and y
        for row in X:
            index = X.index(row)  
            x = np.array(row)
            x.transpose        
            if (y[index] == 1) and (sum(np.matmul(W,x)) >= 1):
                newEvidence.append(X[index])
                del X[index]
                del y[index]
        
        print("sum(y) = " + str(sum(y)))
        print("num schemas atm = " + str(len(currSchemas)))
                
    #print newSchemas
    
    for row in X:
        index = X.index(row)       
        if y[index] == 0:
            del X[index]
                
    return [newSchemas, newEvidence, X]