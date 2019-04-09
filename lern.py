# LERN
# Functions for learning schemas from matrices, and also for learning policies from schemas

LIMIT = 10
TOL = 1e-3

import util
from scipy import optimize as opt
import numpy as np
import blox
from copy import deepcopy
from random import choice
import inta
from pydc import HYPE
# import pyscipopt


# Learning procedure
def hyperMax(name, mode, numEpisodes, numSteps, numSamples, epsilon):

    # Set up game according to mode and return description of initial state
    environment = inta.setup(mode,test=True)
    initState = inta.observeState(mode, environment)
    rewards = [0 for ep in range(numEpisodes)]

    # Intialise model and Q-function
    M = blox.Model(name, mode, initState)
    M.obsActions = [["u", "l", "d", "r", "none"],[[1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1],[0,0,0,0]]]
    M.updateDicts()
    # Q = QFunction(mode)
    Q = 0

    # Learn model and Q-function
    for i in range(numEpisodes):
        print("===========")
        print("Episode " + str(i))
        # Set up model for new episode
        environment = inta.setup(mode, test=True)
        initState = inta.observeState(mode, environment)
        M.initialise(mode, initState)
        ended = False
        current_reward = 0
        for j in range(numSteps):
            # Check if the game has ended
            if ended:
                # Display endgame information
                rewards[i] = current_reward
                print("*********")
                print("Game Over")
                print("Score: " + str(current_reward))
                print("*********")
                # Clean up data, evidence, and learnt schemas
                M.clean()
                # Update Q-function using HYPE
                state = M.curr
                hype(M, numSamples, state, Q)
                break
            else:
                print("-------")
                print("Step " + str(j))
                # Take action in the game and observe reward using RMAX
                action = rmax(M, Q, epsilon)
                print("Action: " + action)
                [reward, ended] = inta.performAction(M, mode, environment, action)
                current_reward += reward
                # If the game has ended we only update the action and reward, as the state doesn't matter
                if not ended:
                    state = [inta.observeState(mode, environment), action, reward]
                    prevState = deepcopy(state[0])
                else:
                    state[0] = prevState
                    state[1] = action
                    state[2] = reward
                # Update model, data, and schemas
                M.prev = M.getModelState()
                M.oldMap = deepcopy(M.objMap)
                M.updateModel(mode, state)
                M.curr = M.getModelState()
                M.updateData()
                M.learn()




        # print("Pre-clean:")
        # for i in M.schemas:
        #     for j in i.keys():
        #         for k in i[j]:
        #             k.display()
        # print("Post-clean:")
        # for i in M.schemas:
        #     for j in i.keys():
        #         for k in i[j]:
        #             k.display()

    print("Rewards:")
    print rewards

    print("Schemas:")
    for i in M.schemas:
        for j in i.keys():
            for k in i[j]:
                print k.display()

    print("Evidence:")
    for i in M.evidence:
        for j in i.keys():
            print("Attribute: " + str(j))
            for k in i[j]:
                print k

    # print("Remaining:")
    # for i in range(len(M.XY)):
    #     for j in M.XY[i].keys():
    #         if j == -1:
    #             continue
    #         print("Attribute: " + str(j))
    #         for k in M.XY[i][j]:
    #             predicted = False
    #             for schema in M.schemas[i][j]:
    #                 if schema.isActive(k):
    #                     predicted = True
    #             if not predicted:
    #                 print k

    return


# Chooses and returns action using RMAX framework with M and Q
def rmax(M, Q, epsilon):

    # TODO
    # return "DOWN"
    return choice(M.obsActions[0][:4])


# Updates Q function using abstracted trajectory samples from M
def hype(model, num_samples, state, Q):
    # Create Prolog file and initialise model
    inta.createPrologFile(model)
    hype = HYPE("models/" + model.name + ".pl", num_samples)
    # Form list of observations describing the current/initial state
    observations = ""
    for key in model.objects.keys():
        object = model.objects[key]
        observations += object.observe()
    observations = "[" + observations[:-2] + "]"


    print observations


    # Run HYPE and print the best action
    result = hype.plan_step(observations, num_samples, max_horizon=10, used_horizon=5, use_abstraction=True)
    best_action = result["best_action"]
    print best_action
    return


# Learns schemas for a particular object attribute given data X and y using linprog
def learnSchemas(model, xYes, xNo, schemas, R=0.1, L=LIMIT):
    # If there are any contradictory data we remove them
    yes = [tuple(item) for item in xYes]
    no = [tuple(item) for item in xNo]
    both = [list(item) for item in list(set(yes) & set(no))]
    for datum in both:
        xYes.remove(datum)
        xNo.remove(datum)
    # If there are no more positive cases we do not try learning here
    if len(xYes) == 0:
        return [schemas, xYes, []]
    # If all the cases are positive there are no constraints for learning schemas so we do not try
    if len(xNo) == 0:
        return [schemas + [[0 for item in xYes[0]]], xYes, []]
    # Initialise variables
    oneVector = np.ones((1, len(xYes[0])))
    evidence = []


    badSchemas = []

    REMxYes = []



    # While there are still schema transitions to explain and the complexity limit L has not been reached
    while (len(xYes) + len(REMxYes) > 0) and (len(schemas) < L):
        # Create LP inputs


        f = np.sum([oneVector - np.array(x) for x in xYes], axis=0)
        f = np.divide(f, len(xYes))
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

        # Solve LP
        result = opt.linprog(f, A, b, bounds=(0,1), options={'tol':1e-06,'maxiter':100000})
        w = result.x



        # print("Status: " + result.message)


        # Minimise schema dimensions


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

        # print("Schema solves " + str(len(newEvidence)) + " positive cases against " + str(len(xNo)) + " negative cases")
        # print("Still have " + str(len(xYes) + len(REMxYes)) + " positive cases remaining")

        # If we have learned a bad schema we don't add it, and skip learning until we have more data
        if len(newEvidence) == 0:
            print("Schema failed to be learnt on this iteration")


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
            xYes = xYes + REMxYes
            REMxYes = []


        # print("Shrinking schema...")

        # Shrink schema
        f = np.ones((1, len(w_binary)))

        # f = np.divide(f, len(xYes))
        # f = f + (R * oneVector)


        A = np.negative(np.array(util.flatten([oneVector - np.array(x) for x in xNo])))
        b = np.negative(np.ones((1, len(xNo))))
        C = np.array(util.flatten([oneVector - np.array(x) for x in (newEvidence + [w_binary])]))
        d = np.zeros((1, len(newEvidence) + 1))


        # Solve LP
        result = opt.linprog(f, A, b, C, d, bounds=(0,1), options={'tol': 1e-6, 'maxiter': 100000})

        w = result.x

        # print("Status: " + result.message)

        old_w_binary = w_binary

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
                newEvidence.append(x)
                xYes.remove(x)

                # print("Removed X!")

        print("Schema solves " + str(len(newEvidence)) + " positive cases against " + str(len(xNo)) + " negative cases")
        if (len(xYes) + len(REMxYes)) == 0:
            print("0 positive cases remaining")
        else:
            print("Still have " + str(len(xYes) + len(REMxYes)) + " positive cases remaining")




        evidence = evidence + newEvidence

    return [schemas, evidence, xYes]





# Learns schemas for a particular object attribute given data X and y using SCIP
def learnSchemas2(model, xYes, xNo, schemas, R=0.1, L=LIMIT):
    length = len(xYes[0])
    evidence = []
    # If there are any contradictory data we remove them
    yes = [tuple(item) for item in xYes]
    no = [tuple(item) for item in xNo]
    both = [list(item) for item in list(set(yes) & set(no))]
    for datum in both:
        xYes.remove(datum)
        xNo.remove(datum)
    # If all the cases are positive there are no constraints for learning schemas so we do not try
    if len(xNo) == 0:
        return [[[0 for item in xYes[0]]], xYes, []]
    # While there are still schema transitions to explain and the complexity limit L has not been reached
    while (len(xYes) > 0) and (len(schemas) < L):



        scip = pyscipopt.Model()
        w = {}
        for i in range(length):
            w[i] = scip.addVar(name="w(%s)"%i)
        for neg in xNo:
            scip.addCons(pyscipopt.quicksum((1-neg[j])*w[j] for j in range(length)) >= 1)
        f = {}
        for k in range(length):
            f[k] = sum([(1-pos[k]) for pos in xYes])
            f[k] /= len(xYes)
            f[k] += L
        scip.setObjective(pyscipopt.quicksum(f[l]*w[l] for l in range(length)), "minimize")
        scip.hideOutput()
        scip.optimize()
        w = np.array([scip.getVal(w[m]) for m in range(length)])
        w_binary = w > TOL
        w_binary = [int(entry) for entry in w_binary]



        # Remove solved entries from xYes
        newEvidence = []
        for x in xYes:
            if np.dot(w_binary, np.array(x)) == sum(w_binary):
                newEvidence.append(x)
                xYes.remove(x)
        schemas.append(w_binary)
        evidence = evidence + newEvidence
    return [schemas, evidence, xYes]
