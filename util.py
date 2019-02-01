# UTIL
# Various utility and helper functions, as well as standard constants and indexes


from numpy import array
from sklearn.preprocessing import LabelEncoder
from sklearn.preprocessing import OneHotEncoder
from operator import add


# Define constants for repeated use and indexing
X_POS = 0
Y_POS = 1
X_SIZE = 2
Y_SIZE = 3
COLOUR = 4
SHAPE = 5
VISIBLE = 6
REWARD = 7
NEIGHBOURS = 8
ACTION = 9
LIMIT = 10


# One-hot encode a list of values or categories and return the new list
def oneHot(inputList):
    # Create encoder
    label_encoder = LabelEncoder()
    onehot_encoder = OneHotEncoder(sparse=False)
    # Form inputs and apply encoder
    a = array(inputList)
    i = label_encoder.fit_transform(a)
    i = i.reshape(len(i), 1)
    b = onehot_encoder.fit_transform(i)
    # Output result as list
    outputList = b.tolist()
    return outputList
    

# Given a position return a list of the eight surrounding neighbours and their relative position vectors
def neighbourPositions(pos):
    n1 = [tuple(map(add, pos, (-1, 1))), ["left", "above"]
    n2 = [tuple(map(add, pos, (0, 1))), ["centre", "above"]]
    n3 = [tuple(map(add, pos, (1, 1))), ["right", "above"]]
    n4 = [tuple(map(add, pos, (1, 0))), ["right", "centre"]]
    n5 = [tuple(map(add, pos, (1, -1))), ["right", "below"]]
    n6 = [tuple(map(add, pos, (0, -1))), ["centre", "below"]]
    n7 = [tuple(map(add, pos, (-1, -1))), ["left", "below"]]
    n8 = [tuple(map(add, pos, (-1, 0))), ["left", "centre"]]
    return [n1, n2, n3, n4, n5, n6, n7, n8]
    

# Form a vector describing an object and its neighbours for use in learning
def formXvector(objId, state, oldMap):
    # Form primary object vector entry
    objPos = (state[objId][0], state[objId][1])
    vector = [state[objId]]
    vector[0][0] = "centre"
    vector[0][1] = "centre"
    # Form vector entries for neighbours of object
    neighbours = neighbourPositions(objPos)
    for nb in neighbours:
        if nb[0] is in oldMap.keys():
            nbVector = state[oldMap[nb[0]]
            nbVector[0] = nb[1][0]
            nbVector[1] = nb[1][1]
            vector.append(nbVector)
        else:
            vector.append([])  
    return vector


# Form a vector decribing the object relative to its previous state
def formYvector(objId, oldState, newState):
    # Form objkect vector
    vector = newState[objId]
    # Update position entries to be categorical variables relative to previous position
    oldPos = (oldState[objId][0], oldState[objId][1])
    newPos = (newState[objId][0], newState[objId][1])
    neighbours = neighbourPositions(oldPos)
    for nb in neighbours:
        if nb[0] == newPos:
            vector[0] = nb[1][0]
            vector[1] = nb[1][1]
            break
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
def fromBinary(model, x):
    x = x.tolist() 
    output = []
    objLengths = [len(obs[1][0]) for obs in model.observations[:7]]
    actionLength = len(model.obsActions[1][0])
    length = sum(objLengths) + actionLength
    # Check that x is the right length
    if len(x) != length:
        print("Error: Binary vector is not the right length")
        return
    # Iterate over object descriptions
    for i in range(1+NEIGHBOURS):
        objOutput = []
        objVector = x[:length]
        x[:length] = []
        # Iterate over attribute vectors
        for j in range(X_POS,VISIBLE):
            attVector = tuple(objVector[:objLengths[j]])
            objVector[:objLengths[j]] = []
            objOutput.append(model.dictionaries[j][1][attVector])
        output.append(objOutput)
    # What remains of x is the action vector
    actVector = tuple(x)
    actOutput = model.dictionaries[ACTION][1][actVector]
    output.append(actOutput)
    return output
    

# Converts a human-readable data point x to a binary vector
def toBinary(model, x):
    output = []
    # Iterate over object descriptions
    for i in range(1+NEIGHBOURS):
        # Add binary description of attributes based on one-hot encoding of observations
        for j in range(X_POS,VISIBLE):
            output = output + model.dictionaries[j][0][x[i][j]]
    # Add binary description of action
    output = output + model.dictionaries[ACTION][0][x[ACTION]]
    return output


# Converts a binary schema x to a human-readable schema  
def fromBinarySchema(model, s):
    s = s.tolist() 
    output = Schema()
    objLengths = [len(obs[1][0]) for obs in model.observations[:7]]
    actionLength = len(model.obsActions[1][0])
    length = sum(objLengths) + actionLength
    # Check that s is the right length
    if len(s) != length:
        print("Error: Binary schema is not the right length")
        return
    # Iterate over object descriptions
    for i in range(1+NEIGHBOURS):
        objOutput = []
        objVector = x[:length]
        x[:length] = []
        # Iterate over attribute vectors
        for j in range(X_POS,VISIBLE):
            attVector = tuple(objVector[:objLengths[j]])
            objVector[:objLengths[j]] = []
            if attVector != [0 for i in len(attVector)]:
                attribute = model.dictionaries[j][1][attVector])
                output.objectBody[(i,j)] = attribute
    # What remains of x is the action vector
    actVector = tuple(x)
    action = model.dictionaries[ACTION][1][actVector]
    schema.actionBody = action
    return output


# Converts a human-readable schema s to a binary schema
def toBinarySchema(model, s):
    # Create initial blank schema
    lengths = [len(obs[1][0]) for obs in model.observations[:7]]
    blank = [[0 for i in range(length)] for length in lengths]
    blankObjects = [blank for i in range(1+NEIGHBOURS)]
    # Instantiate according to preconditions
    for key in s.objectBody.keys():
        i = list(key)
        attribute = s.objectBody(key)
        vector = model.dictionaries[i[1]][0](attribute)
        blankObjects[i[0]][i[1]] = vector
    # Form and output final binary schema vector
    objectVector = flatten(flatten(blankObjects))
    action = s.actionBody
    actionVector = model.dictionaries[ACTION][0](action)
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
