# UTIL
# Various utility and helper functions, as well as standard constants and indexes


from numpy import array
from sklearn.preprocessing import LabelEncoder
from sklearn.preprocessing import OneHotEncoder
from operator import add


# Define constants for repeated use and indexing
LIMIT = 10
X_POS = 0
Y_POS = 1
X_SIZE = 2
Y_SIZE = 3
COLOUR = 4
SHAPE = 5
VISIBLE = 6


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
    

def fromBinary(model, vector, i=None):
    if i != None:
        