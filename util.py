# UTIL
# Various utility and helper functions, as well as standard constants and indexes


import numpy as np
from sklearn import preprocessing
from operator import add
from copy import deepcopy
from blox import Schema
from collections import OrderedDict
from itertools import groupby


# Define constants for repeated use and indexing
X_POS = 0
Y_POS = 1
X_SIZE = 2
Y_SIZE = 3
COLOUR = 4
SHAPE = 5
NOTHING = 6
REWARD = 7
NEIGHBOURS = 8
ACTION = 9
LIMIT = 10


# One-hot encode a list of values or categories and return the new list
def oneHot(inputList):
    # Create encoder
    label_encoder = preprocessing.LabelEncoder()
    onehot_encoder = preprocessing.OneHotEncoder(sparse=False)
    # Form inputs and apply encoder
    a = np.array(inputList)
    i = label_encoder.fit_transform(a)
    i = i.reshape(len(i), 1)
    b = onehot_encoder.fit_transform(i)
    # Output result as list
    outputList = b.tolist()
    return outputList


# Given a position return a list of the eight surrounding neighbours and their relative position vectors
def neighbourPositions(pos):
    n1 = [tuple(map(add, pos, (-1, -1))), ["left", "above"]]
    n2 = [tuple(map(add, pos, (0, -1))), ["centre", "above"]]
    n3 = [tuple(map(add, pos, (1, -1))), ["right", "above"]]
    n4 = [tuple(map(add, pos, (1, 0))), ["right", "centre"]]
    n5 = [tuple(map(add, pos, (1, 1))), ["right", "below"]]
    n6 = [tuple(map(add, pos, (0, 1))), ["centre", "below"]]
    n7 = [tuple(map(add, pos, (-1, 1))), ["left", "below"]]
    n8 = [tuple(map(add, pos, (-1, 0))), ["left", "centre"]]
    return [n1, n2, n3, n4, n5, n6, n7, n8]


# Form a vector describing an object and its neighbours for use in learning
def formXvector(objId, state, oldMap):
    # Form primary object vector entry
    objPos = (state[objId][0], state[objId][1])
    vector = [deepcopy(state[objId])]
    vector[0][0] = "centre"
    vector[0][1] = "centre"
    # Form vector entries for neighbours of object
    neighbours = neighbourPositions(objPos)
    for nb in neighbours:
        if nb[0] in oldMap.keys() and len(oldMap[nb[0]]) != 0:

            # print nb[0]
            # print oldMap[nb[0]]
            # print oldMap[nb[0]][0]

            nbVector = deepcopy(state[oldMap[nb[0]][0]])
            nbVector[0] = nb[1][0]
            nbVector[1] = nb[1][1]
            vector.append(nbVector)
        # If there is no neighbour, add a vector with all None entries apart from the 'nothing' attribute
        else:
            new = [None for attribute in range(NOTHING+1)]
            new[NOTHING] = "Yes"
            vector.append(new)
    return vector


# Form a vector decribing the object relative to its previous state
def formYvector(objId, oldState, newState):
    # Form object vector
    vector = deepcopy(newState[objId])
    # Update position entries to be categorical variables relative to previous position
    oldPos = (oldState[objId][0], oldState[objId][1])
    newPos = (newState[objId][0], newState[objId][1])
    if newPos == oldPos:
        vector[0] = "centre"
        vector[1] = "centre"
    else:
        neighbours = neighbourPositions(oldPos)
        for nb in neighbours:
            if nb[0] == newPos:
                vector[0] = nb[1][0]
                vector[1] = nb[1][1]
    return vector


# Check against previous state and output any object whose attributes have changed
def changes(model):
    changes = []
    if model.prev != None:
        for objId in model.prev.keys():
            if model.prev[objId] != model.curr[objId]:
                changes.append(objId)
    return changes


# Converts a binary vector x to a human-readable data point
def fromBinary(model, x_original):
    x = deepcopy(x_original)
    output = []
    objLengths = [len(obs[1][0]) for obs in model.observations[:NOTHING+1]]
    actionLength = len(model.obsActions[1][0])
    length = (9*sum(objLengths)) + actionLength
    # Check that x is the right length
    if len(x) != length:
        print("Error: Binary vector is not the right length")
        return
    # Iterate over object descriptions
    for i in range(1+NEIGHBOURS):
        objOutput = []
        objVector = x[:sum(objLengths)]
        x[:sum(objLengths)] = []
        # Iterate over attribute vectors
        for j in range(NOTHING+1):
            attVector = objVector[:objLengths[j]]
            objVector[:objLengths[j]] = []
            if attVector == [0 for item in attVector]:
                objOutput.append(None)
            else:
                attVector = tuple(attVector)
                objOutput.append(model.dictionaries[j][1][attVector])
        output.append(objOutput)
    # What remains of x is the action vector
    if x == [0 for item in x]:
        output.append(None)
    else:
        actVector = tuple(x)
        actOutput = model.dictionaries[ACTION][1][actVector]
        output.append(actOutput)
    return output


# Converts a human-readable data point x to a binary vector
def toBinary(model, x_original):
    x = deepcopy(x_original)
    output = []
    sumObjLengths = sum([len(obs[1][0]) for obs in model.observations[:NOTHING+1]])
    # Iterate over object descriptions
    for i in range(1+NEIGHBOURS):
        # # If there is no object in the neighbour position we add a vector of zeros apart from the 'nothing' attribite
        # blank = [None for item in x[i]]
        # blank[NOTHING] = "yes"
        # if x[i] == blank:
        #     new = [0 for k in range(sumObjLengths)]
        #     new[-1] = 1
        #     output = output + new
        #     continue
        # Add binary description of attributes based on one-hot encoding of observations

        for j in range(NOTHING+1):
            if x[i][j] == None:
                output = output + [0 for item in model.observations[j][1][0]]
            else:
                output = output + model.dictionaries[j][0][x[i][j]]
    # Add binary description of action
    output = output + model.dictionaries[ACTION][0][x[ACTION]]
    return output


# Converts a binary schema x to a human-readable schema
def fromBinarySchema(model, s_original, head):
    s = deepcopy(s_original)
    output = Schema()
    output.head = head
    objLengths = [len(obs[1][0]) for obs in model.observations[:NOTHING+1]]
    actionLength = len(model.obsActions[1][0])
    length = (9*sum(objLengths)) + actionLength
    # Check that s is the right length
    if len(s) != length:
        print("Error: Binary schema is not the right length")
        return
    # Iterate over object descriptions
    for i in range(1+NEIGHBOURS):
        objVector = s[:sum(objLengths)]
        s[:sum(objLengths)] = []
        # Iterate over attribute vectors
        for j in range(NOTHING+1):
            attVector = tuple(objVector[:objLengths[j]])
            objVector[:objLengths[j]] = []
            # If this vector represents an object attribute in the body of the schema
            if list(attVector) != [0 for k in range(len(attVector))]:
                attribute = model.dictionaries[j][1][attVector]
                output.objectBody[(i,j)] = attribute
    # What remains of x is the action vector
    actVector = tuple(s)
    if list(actVector) != [0 for k in range(len(actVector))]:
        action = model.dictionaries[ACTION][1][actVector]
        output.actionBody = action
    return output


# Converts a human-readable schema s to a binary schema
def toBinarySchema(model, s_original):
    s = deepcopy(s_original)
    # Create initial blank schema
    lengths = [len(obs[1][0]) for obs in model.observations[:7]]
    blankObjects = [[[0 for j in range(length)] for length in lengths] for k in range(1+NEIGHBOURS)]
    # Instantiate according to preconditions
    for key in s.objectBody.keys():
        [i,j] = list(key)
        attribute = s.objectBody[key]
        vector = model.dictionaries[j][0][attribute]
        blankObjects[i][j] = vector
    # Form and output final binary schema vector
    objectVector = flatten(flatten(blankObjects))
    action = s.actionBody
    if action == None:
        actionVector = [0 for item in model.obsActions[1][0]]
    else:
        actionVector = model.dictionaries[ACTION][0][action]
    vector = objectVector + actionVector
    return vector


# Flattens list by one level
def flatten(fullList):
    return [item for sublist in fullList for item in sublist]


# Converts list of observations and their one-hot encoded versions to dictionaries
def obsToDicts(obs):
    attributeToBinary = dict(zip(obs[0], obs[1]))
    binaryToAttribute = dict(zip([tuple(vector) for vector in obs[1]], obs[0]))
    return [attributeToBinary, binaryToAttribute]


# Takes a list and removes any duplicate entries, printing how many items were removed
def deDupe(old):
    new = [k for k,v in groupby(sorted(old))]
    numRemoved = len(old) - len(new)
    if numRemoved != 0:
        print("Successfully removed " + str(numRemoved) + " duplicate data")
    return new


# Simplifies a set a of schemas
def simplify(model, old, head):
    new = []
    # Remove pointless attributes
    counter = 0
    for schema in old:
        toRemove = []
        trimmed = False
        for key in schema.objectBody:
            obj = list(key)
            if obj[0] in range(NEIGHBOURS + 1) and obj[1] in range(Y_POS+1):
                toRemove.append(key)
                trimmed = True
        if trimmed:
            counter += 1
        for rKey in toRemove:



            # print("REMOVING:")
            # print rKey
            # print schema.objectBody[rKey]
            #
            #


            del schema.objectBody[rKey]



    if counter != 0:
        print("Successfully reduced " + str(counter) + " schemas")
    # Remove more complex schemas
    oldBinary = [tuple(toBinarySchema(model, s)) for s in old]
    newBinary = list(set(oldBinary))
    newBinary = [list(item) for item in newBinary]
    newBinary.sort(key=sum)
    for s1 in newBinary:
        for s2 in newBinary:
            if s1 != s2 and np.dot(np.array(s1), np.array(s2)) == sum(s1):
                newBinary.remove(s2)


                print("Removed:")
                ds2 = fromBinarySchema(model, s2, head)
                print ds2.display()
                print("Because of:")
                ds1 = fromBinarySchema(model, s1, head)
                print ds1.display()


        new.append(fromBinarySchema(model, s1, head))
        newBinary.remove(s1)
    # Display notification to user if any schemas were removed
    numRemoved = len(old) - len(new)
    if numRemoved != 0:
        print("Successfully cut " + str(numRemoved) + " schemas")
    return new


