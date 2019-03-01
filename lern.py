# LERN
# Functions for learning schemas from matrices, and also for learning policies from schemas

LIMIT = 10
TOL = 0

import util
from scipy import optimize as opt
import numpy as np
import blox


# Learns schemas for a particular object attribute given data X and y
def learnSchemas(model, xYes, xNo, schemas, R=0.0001, L=LIMIT):
    # # If all the cases are positive there are no constraints for learning schemas so we do not try
    # if len(xNo) == 0:
    #     return [[], [], xYes]
    # Initialise variables
    oneVector = np.ones((1, len(xYes[0])))
    evidence = []
    # While there are still schema transitions to explain and the complexity limit L has not been reached
    while (len(xYes) > 0) and (len(schemas) < L):
        # Create LP inputs
        f = np.sum([oneVector - np.array(x) for x in xYes], axis=0)
        f = np.divide(f, len(xYes))
        f = f + (R * oneVector)
        A = np.negative(np.array(util.flatten([oneVector - np.array(x) for x in xNo])))
        b = np.negative(np.ones((1, len(xNo))))
        # Solve LP
        w = opt.linprog(f, A, b, options={'tol':1.0e-6,'maxiter':10000000}).x
        # Convert w to binary version and add to set of schemas
        w_binary = w > TOL
        w_binary = [int(entry) for entry in w_binary]
        schemas.append(w_binary)
        # Remove solved entries from xYes
        for x in xYes:

            # print("X: ")
            # print [i for i in range(len(x)) if x[i] > 0]
            # print("W: ")
            # print [i for i in range(len(w_binary)) if w_binary[i] > 0]


            if np.dot(w_binary, np.array(x)) == sum(w_binary):
                evidence.append(x)
                xYes.remove(x)

                # print("Removed X!")


    return [schemas, evidence, xYes]
