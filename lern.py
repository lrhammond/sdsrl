# LERN
# Functions for learning schemas from matrices, and also for learning policies from schemas

LIMIT = 10
TOL = 0

import util
from scipy import optimize as opt
import numpy as np
import blox


# Learns schemas for a particular object attribute given data X and y
def learnSchemas(model, xYes, xNo, schemas, R=0.1, L=LIMIT):
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
        f = f * len(xYes)
        A = np.negative(np.array(util.flatten([oneVector - np.array(x) for x in xNo])))
        b = np.negative(np.ones((1, len(xNo))))
        C = np.array(util.flatten([oneVector - np.array(x) for x in xYes]))
        d = np.zeros((1, len(xYes)))
        # Solve LP
        w = opt.linprog(f, A, b, options={'tol':1e-06,'maxiter':10000}).x
        # Minimise schema dimensions


        # Convert w to binary version and add to set of schemas
        w_binary = w > TOL
        w_binary = [int(entry) for entry in w_binary]

        # schemas.append(w_binary)


        # Remove solved entries from xYes
        newEvidence = []
        for x in xYes:

            # print("X: ")
            # print [i for i in range(len(x)) if x[i] > 0]
            # print("W: ")
            # print [i for i in range(len(w_binary)) if w_binary[i] > 0]


            if np.dot(w_binary, np.array(x)) == sum(w_binary):
                newEvidence.append(x)
                xYes.remove(x)

                # print("Removed X!")

        # # Shrink schema
        # f = np.ones((1, len(w_binary)))
        # # f = np.divide(f, len(xYes))
        # # f = f + (R * oneVector)
        # A = np.negative(np.array(util.flatten([oneVector - np.array(x) for x in xNo])))
        # b = np.negative(np.ones((1, len(xNo))))
        # C = np.array(util.flatten([oneVector - np.array(x) for x in newEvidence]))
        # d = np.zeros((1, len(newEvidence)))
        # # Solve LP
        # w = opt.linprog(f, A, b, C, d, options={'tol': 1e-06, 'maxiter': 10000}).x
        #
        # w_binary = w > TOL
        # w_binary = [int(entry) for entry in w_binary]

        schemas.append(w_binary)


        evidence = evidence + newEvidence



    return [schemas, evidence, xYes]
