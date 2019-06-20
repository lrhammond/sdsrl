# LERN
# Functions for learning schemas from matrices, and also for learning policies from schemas

LIMIT = 10
TOL = 1e-3

import sys
import util
from scipy import optimize as opt
import numpy as np
import blox
from copy import deepcopy
from random import choice
import inta
import os
import veri
import subprocess as sp

import gc
# import pyscipopt

# Updates Q function using abstracted trajectory samples from M
def hypermax(model, num_samples, rmax_actions, constraints, gamma, horizon, rmax, max=3):

    # Create Prolog file and initialise model
    inta.createPrologFile(model, num_samples, rmax_actions, constraints, gamma, horizon, rmax)

    action = "N/A"
    counter = 0

    while action not in model.obsActions[0] and counter < max:

        counter += 1

        # Run HYPE algorithm using YAP
        # os.system("yap -l models/{0}/hype_model.pl -g run > models/{0}/hype_output.txt".format(model.name))

        # sp.call("yap -l models/{0}/hype_model.pl -g run > models/{0}/hype_output.txt".format(model.name), "halt.", shell=True)

        p = sp.Popen("yap -l models/{0}/hype_model.pl -g run > models/{0}/hype_output.txt".format(model.name), shell=True, stdin=sp.PIPE)
        p.communicate("halt.")
        # p.terminate()

        # Run in case HYPE crashes due to currently undiagnosed memory issues
        # os.system("halt.")
        # os.system("yap -g halt")

        # sp.call("halt.", shell=True)
        # sp.call("yap -g halt", shell=True)


        # Open file containing actions and read the most recent action in
        with open("models/" + model.name + "/hype_output.txt", 'r') as f:
            lines = f.read().splitlines()
            action = lines[-1][7:-1]
            
            # TODO
            expected_value = None

    if counter == max:
        action = "N/A"
        print("HYPE failed to compute an action {0} times, skipping on this step".format(max))

    return action, expected_value


# Learns schemas for a particular object attribute given data X and y using linprog
def learnSchemas(xYes, xNo, schemas, deterministic, R=0, L=LIMIT, max_failures=5):

    failures = 0

    # If there are any duplicate data we remove them
    xYes = util.deDupe(xYes)
    xNo = util.deDupe(xNo)

    # If there are any contradictory data we remove them
    yes = [tuple(item) for item in xYes]
    no = [tuple(item) for item in xNo]
    both = [list(item) for item in list(set(yes) & set(no))]
    if len(both) != 0:
        print("{0} contradictory data points detected and removed.".format(len(both)))
        if deterministic:
            print("Error: This should never happen in deterministic environments, indicating that data recording is functioning improperly")

    # How the data points are removed depends on whether we are in a deterministic environment or not
    for datum in both:
        xNo.remove(datum)
        if deterministic:
            xYes.remove(datum)

    # If there are no more positive cases we do not try learning here
    if len(xYes) == 0:
        print("No more positive cases after removing contradictory and/or duplicate data, schemas not learnt.")
        return [schemas, [], []]

    # If all the cases are positive there are no constraints for learning schemas so we do not try
    if len(xNo) == 0:
        print("No negative cases after removing contradictory and/or duplicate data so no constraints for learning, schemas not learnt.")
        schemas.append([0 for _ in schemas[0]])
        return [schemas, xYes, []]

    # Initialise variables
    oneVector = np.ones((1, len(xYes[0])))
    evidence = []
    badSchemas = []
    REMxYes = []

    # While there are still schema transitions to explain and the complexity limit L has not been reached
    while (len(xYes) + len(REMxYes) > 0) and (len(schemas) < L):

        if failures == max_failures:
            print("{0} failures, skipping learning".format(max_failures))
            break

        # Create LP inputs
        f = np.sum([oneVector - np.array(x) for x in xYes], axis=0)
        f = np.divide(f, len(xYes))

        # Add weight regulariser
        f = f + (R * oneVector)

        # if len(badSchemas) != 0:
        #     b = np.sum(badSchemas, axis=0)
        #     b = np.divide(b, len(badSchemas))
        #     f = f + (R * b)

        # f = f * len(xYes)

        A = np.negative(np.array(util.flatten([oneVector - np.array(x) for x in xNo])))
        b = np.negative(np.ones((1, len(xNo))))

        # C = np.array(util.flatten([oneVector - np.array(x) for x in xYes]))
        # d = np.zeros((1, len(xYes)))

        print("---")

        # print("Learning schema...")
        solved = []

        # Solve LP
        try:
            result = opt.linprog(f, A, b, bounds=(0,1), options={'tol':1e-4,'maxiter':1e6})
        except ValueError:
            print("Failed to find an LP solution on this iteration")
        else:
            # print("Status: " + result.message)

            w = result.x

            # Convert w to binary version and add to set of schemas
            w_binary = w > TOL
            w_binary = [int(entry) for entry in w_binary]

            old = np.array(w_binary)

            # print("w:")
            # print w
            # print("w_binary:")
            # print w_binary

            # old_s = util.fromBinarySchema(model, w_binary, "HEAD")
            # print("Learnt new schema:")
            # old_s.display()

            # schemas.append(w_binary)

            # Remove solved entries from xYes
            for x in xYes:

                # print("X: ")
                # print [i for i in range(len(x)) if x[i] > 0]
                # print("W: ")
                # print [i for i in range(len(w_binary)) if w_binary[i] > 0]

                if np.dot(w_binary, np.array(x)) == sum(w_binary):
                    solved.append(x)
                    xYes.remove(x)

                    # print("Removed X!")

            # print("Schema solves " + str(len(solved)) + " positive cases against " + str(len(xNo)) + " negative cases")
            # print("Still have " + str(len(xYes) + len(REMxYes)) + " positive cases remaining")

        # If we have learned a bad schema we don't add it, and skip learning until we have more data
        if len(solved) == 0:
            print("Schema failed to be learnt on this iteration")
            failures += 1


            # print("w_binary:")
            # print w_binary
            # print("positive cases:")
            # for x in xYes:
            #     print [int(y) for y in x]

            # if w_binary not in badSchemas:
            #     badSchemas.append(w_binary)
            # else:
            #     print("Same bad schema learnt again!")

            xYes.sort(key= lambda x: np.dot(np.array(x),np.array(w_binary)))

            REMxYes += xYes[:len(xYes) // 2]
            xYes = xYes[len(xYes) // 2:]

            continue

            # return [schemas, evidence, xYes]

        else:
            # Reset failure count
            failures = 0
            xYes = xYes + REMxYes
            REMxYes = []

        # print("Shrinking schema...")

        # Shrink schema
        f = np.ones((1, len(w_binary)))
        # f = np.divide(f, len(xYes))
        # f = f + (R * oneVector)
        A = np.negative(np.array(util.flatten([oneVector - np.array(x) for x in xNo])))
        b = np.negative(np.ones((1, len(xNo))))
        C = np.array(util.flatten([oneVector - np.array(x) for x in (solved + [w_binary])]))
        d = np.zeros((1, len(solved) + 1))

        # Solve LP
        try:
            result = opt.linprog(f, A, b, C, d, bounds=(0, 1), options={'tol': 1e-6, 'maxiter': 1e4})
        except ValueError:
            print("Failed to find an LP solution on this iteration.")
            failures += 1
            continue

        # print("Status: " + result.message)

        old_w_binary = w_binary
        w = result.x
        w_binary = w > TOL
        w_binary = [int(entry) for entry in w_binary]

        # if w_binary == old_w_binary:
        #     print("Schema not shrunk")

        # final = [w_binary[i] * old_w_binary[i] for i in range(len(w_binary))]
        # w_binary = final

        # print("w:")
        # print w
        # print("w_binary:")
        # print w_binary

        # old_s = util.fromBinarySchema(model, w_binary, "HEAD")
        # print("Shrunk new schema:")
        # old_s.display()

        schemas.append(w_binary)

        for x in xYes:

            # print("X: ")
            # print [i for i in range(len(x)) if x[i] > 0]
            # print("W: ")
            # print [i for i in range(len(w_binary)) if w_binary[i] > 0]

            if np.dot(w_binary, np.array(x)) == sum(w_binary):
                solved.append(x)
                xYes.remove(x)

                # print("Removed X!")

        print("Schema solves " + str(len(solved)) + " positive cases against " + str(len(xNo)) + " negative cases")
        if (len(xYes) + len(REMxYes)) == 0:
            print("0 positive cases remaining")
        else:
            print("Still have " + str(len(xYes) + len(REMxYes)) + " positive cases remaining")

        evidence += solved

    return [schemas, evidence, xYes]


# # Learns schemas for a particular object attribute given data X and y using SCIP
# def learnSchemas2(model, xYes, xNo, schemas, R=0.1, L=LIMIT):
#     length = len(xYes[0])
#     evidence = []
#     # If there are any contradictory data we remove them
#     yes = [tuple(item) for item in xYes]
#     no = [tuple(item) for item in xNo]
#     both = [list(item) for item in list(set(yes) & set(no))]
#     for datum in both:
#         xYes.remove(datum)
#         xNo.remove(datum)
#     # If all the cases are positive there are no constraints for learning schemas so we do not try
#     if len(xNo) == 0:
#         return [[[0 for item in xYes[0]]], xYes, []]
#     # While there are still schema transitions to explain and the complexity limit L has not been reached
#     while (len(xYes) > 0) and (len(schemas) < L):
#
#
#
#         scip = pyscipopt.Model()
#         w = {}
#         for i in range(length):
#             w[i] = scip.addVar(name="w(%s)"%i)
#         for neg in xNo:
#             scip.addCons(pyscipopt.quicksum((1-neg[j])*w[j] for j in range(length)) >= 1)
#         f = {}
#         for k in range(length):
#             f[k] = sum([(1-pos[k]) for pos in xYes])
#             f[k] /= len(xYes)
#             f[k] += L
#         scip.setObjective(pyscipopt.quicksum(f[l]*w[l] for l in range(length)), "minimize")
#         scip.hideOutput()
#         scip.optimize()
#         w = np.array([scip.getVal(w[m]) for m in range(length)])
#         w_binary = w > TOL
#         w_binary = [int(entry) for entry in w_binary]
#
#
#
#         # Remove solved entries from xYes
#         solved = []
#         for x in xYes:
#             if np.dot(w_binary, np.array(x)) == sum(w_binary):
#                 solved.append(x)
#                 xYes.remove(x)
#         schemas.append(w_binary)
#         evidence = evidence + solved
#     return [schemas, evidence, xYes]
