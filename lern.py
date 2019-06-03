# LERN
# Functions for learning schemas from matrices, and also for learning policies from schemas

LIMIT = 10
TOL = 1e-3
RMAX = 1e6

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

import gc
# import pyscipopt


# Learning procedure
def run(name, mode, numEpisodes, numSteps, numSamples, epsilon, manual_episodes=0):

    # Create directory to store files in
    if not os.path.exists("models/" + name):
        os.makedirs("models/" + name)

    # Set up game according to mode and return description of initial state
    environment, dims = inta.setup(name, mode)
    initState = inta.observeState(mode, environment, dims)
    rewards = [0 for _ in range(numEpisodes)]
    all_constraints = inta.get_file(name, "constraints")
    constraints = ", ".join(all_constraints.splitlines())

    # Intialise model
    M = blox.Model(name, mode, initState)
    M.obsActions = [["u", "l", "d", "r", "none"],[[1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1],[0,0,0,0]]]
    M.updateDicts()
    with open("models/" + M.name + "/episodes.txt", 'w+') as f:
        f.write("Episode action sequences and rewards for " + name + "\n\n")

    # Learn model and policy
    for i in range(numEpisodes):
        with open("models/" + M.name + "/episodes.txt", 'a') as f:
            f.write("Episode " + str(i) + "\n")
        print("===========")
        print("Episode " + str(i))

        # Set up model for new episode
        environment, dims = inta.setup(name, mode)
        initState = inta.observeState(mode, environment, dims)
        M.initialise(mode, initState)
        ended = False
        current_reward = 0
        for j in range(numSteps):

            # Check if the game has ended
            if ended or j == numSteps - 1:

                # Display endgame information
                rewards[i] = current_reward
                print("*********")
                print("Game Over")
                print("Score: " + str(current_reward))
                print("*********")
                with open("models/" + M.name + "/episodes.txt", 'a') as f:
                    f.write("Score: " + str(current_reward) + "\n\n")
                break
            else:
                print("-------")
                print("Step " + str(j))

                # Clean up data, evidence, and learnt schemas
                M.clean()

                # If the current state is new we initialise it to have maximum reward
                state = util.to_tuple(sorted([(key, M.curr[key]) for key in M.curr.keys()]))
                if state not in M.R.keys():
                    M.R[state] = dict(zip(M.obsActions[0], [RMAX for _ in M.obsActions]))
                rmax_actions = [a for a in M.R[state] if M.R[state][a] == RMAX]

                # If using manual control for episode, input action
                if i in range(manual_episodes):
                    action = "N/A"
                    while action not in M.obsActions[0]:
                        action = raw_input("Enter action: ")
                    method = "input"

                # Otherwise find the best action using HYPE
                else:
                    action, expected_value = hypermax(M, numSamples, rmax_actions, constraints)
                    method = "HYPE"

                # If HYPE fails to select an action then we use RMAX, the policy (if available) or make a random choice
                if action == "N/A":
                    if len(rmax_actions) != 0:
                        action = choice(rmax_actions)
                        method = "RMAX"
                    elif state in M.pi.keys():
                        action = M.pi[state]
                        method = "policy"
                    else:
                        action = choice(M.obsActions[0])
                        method = "random"

                # Otherwise we perform the action selected by HYPE and update the policy            
                else:
                    M.pi[state] = action

                # Output action information
                with open("models/" + M.name + "/episodes.txt", 'a') as f:
                    f.write(action + " (from {0})\n".format(method))
                print("Action taken: " + action)

                # If there is an action to be taken, perform it and update the reward
                if action != "none":
                    [reward, ended] = inta.performAction(M, mode, environment, action)
                    current_reward += reward
                    M.R[state][action] = reward

                # If the game has ended we only update the action and reward, as the state doesn't matter
                if not ended:
                    observation = [inta.observeState(mode, environment, dims), action, reward]
                else:
                    observation = [None, action, reward]

                # Update model
                M.prev = M.getModelState()
                M.oldMap = deepcopy(M.objMap)
                M.updateModel(mode, observation)
                M.curr = M.getModelState()
                M.obsChanges.update(set(util.changes(M)))
                
                # Save transition so we don't have to update our data or do learning next time
                new_state = util.to_tuple(sorted([(key, M.curr[key]) for key in M.curr.keys()]))
                transition = util.to_tuple([state, action, new_state, reward])
                state = transition[:2]
                
                # If the transition has not previously been observed then we update the data and learn transitions
                new_trans = False
                if transition not in M.obsTrans:
                    M.obsTrans.add(transition)
                    M.updateData()
                    new_trans = True

                # If the state is also new then record it separately and learn rewards
                new_state = False
                if state not in M.obsState:
                    M.obsState.add(state)
                    new_state = True

                M.learn(new_trans, new_state)

    # Print information from experiment
    print("Rewards:")
    print rewards
    print("Schemas:")
    attributes = ["X_pos", "Y_pos", "X_size", "Y_size", "Colour", "Shape", "Nothing", "Reward"]
    for i in range(len(M.schemas)):
        for j in M.schemas[i].keys():
            for k in M.schemas[i][j]:
                print(attributes[i] + " = " + j + " <- " + k.display(no_head=True))
    # print("Evidence:")
    # for i in M.evidence:
    #     for j in i.keys():
    #         print("Attribute: " + str(j))
    #         for k in i[j]:
    #             print k
    # print("Remaining:")
    # for i in range(len(M.data)):
    #     for j in M.data[i].keys():
    #         if j == -1:
    #             continue
    #         print("Attribute: " + str(j))
    #         for k in M.data[i][j]:
    #             predicted = False
    #             for schema in M.schemas[i][j]:
    #                 if schema.isActive(k):
    #                     predicted = True
    #             if not predicted:
    #                 print k

    return


# Updates Q function using abstracted trajectory samples from M
def hypermax(model, num_samples, rmax_actions, constraints, max=3):

    # Create Prolog file and initialise model
    inta.createPrologFile(model, num_samples, rmax_actions, constraints, gamma=0.95, horizon=10)

    action = "N/A"
    counter = 0

    while action not in model.obsActions[0] and counter < max:

        counter += 1

        # Run HYPE algorithm using YAP
        os.system("yap -l models/{0}/hype_model.pl -g run > models/{0}/hype_output.txt".format(model.name))
        # Run in case HYPE crashes due to currently undiagnosed memory issues
        os.system("halt.")
        os.system("yap -g halt")

        # Open file containing actions and read the most recent action in
        with open("models/" + model.name + "/hype_output.txt", 'r') as f:
            lines = f.read().splitlines()
            action = lines[-1][7:-1]
            
            # TODO
            expected_value = None

    if counter == max:
        action = "N/A"
        print("HYPE failed to compute an action {0} times, skipping on this step".format(max))
    else:
        print("HYPE selected: " + action)

    return action, expected_value




# Learns schemas for a particular object attribute given data X and y using linprog
def learnSchemas(xYes, xNo, schemas, R=0, L=LIMIT, max_failures=5):

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
    for datum in both:
        xYes.remove(datum)
        xNo.remove(datum)

    # If there are no more positive cases we do not try learning here
    if len(xYes) == 0:
        print("No more positive cases after removing contradictory data, schemas not learnt.")
        return [schemas, [], []]

    # If all the cases are positive there are no constraints for learning schemas so we do not try
    if len(xNo) == 0:
        print("No negative cases so no constraints for learning, schemas not learnt.")
        schemas.append([0 for _ in schemas[0]])
        return [schemas, xYes, []]

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

        # Solve LP
        result = opt.linprog(f, A, b, bounds=(0,1), options={'tol':1e-06,'maxiter':100000})
        w = result.x

        # print("Status: " + result.message)

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
            failures += 1
            if failures == max_failures:
                print("{0} failures, skipping learning".format(max_failures))
                break

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

        evidence += newEvidence

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
#         newEvidence = []
#         for x in xYes:
#             if np.dot(w_binary, np.array(x)) == sum(w_binary):
#                 newEvidence.append(x)
#                 xYes.remove(x)
#         schemas.append(w_binary)
#         evidence = evidence + newEvidence
#     return [schemas, evidence, xYes]
