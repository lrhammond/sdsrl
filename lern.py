# LERN
# Functions for learning schemas from matrices, and also for learning policies from schemas

LIMIT = 10
TOL = 0.001

import util
from scipy import optimize as opt
import numpy as np


# Learns schemas for a particular object attribute given data X and y
def learnSchemas(model, xYes, xNo, schemas, R=0.5, L=LIMIT):
    # If all the cases are positive there are no constraints for learning schemas so we do not try
    if len(xNo) == 0:
        return [[], [], xYes]
    # Initialise variables
    oneVector = np.ones((1, len(xYes[0])))
    evidence = []
    # While there are still schema transitions to explain and the complexity limit L has not been reached
    while (len(xYes) > 0) and (len(schemas) < L):
        # Create LP inputs
        f = np.sum([oneVector - np.array(x) for x in xYes], axis=0)
        f = np.divide(f, len(xYes))
        f = f + (R * oneVector)
        A = np.negative(np.array([[oneVector - np.array(x) for x in xNo]]))
        b = np.negative(np.ones((1, len(xNo))))
        # Solve LP
        w = opt.linprog(f, A, b).x
        # Convert w to binary version and add to set of schemas
        w_binary = w > TOL
        w_binary = [int(entry) for entry in w_binary]

        print("new schema:")
        print w_binary

        schemas.append(w_binary)
        # Remove solved entries from xYes
        for x in xYes:
            if sum(np.dot(w,np.array(x))) == sum(w.tolist()):
                evidence.append(x)
                del x
        print("sum(y) = " + str(sum(y)))
        print("num schemas atm = " + str(len(schemas)))
                
    return [schemas, evidence, xYes]