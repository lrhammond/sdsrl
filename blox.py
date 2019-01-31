# BLOX
# Classes for data structures used during learning and verification


import util
from lern import learnSchemas


# Define the model class
class Model:
    
    # Initialise model according to mode
    def __init__(self, mode, initState, xMax=0, yMax=0):
        # Intialise dimensions of map and object/map attributes
        self.xMax = xMax
        self.yMax = yMax
        self.objects = {}
        self.oldMap = {}
        self.objMap = {}
        # Create variables for storing the current action and reward values
        self.action = None
        self.reward = None
        # Create set of objects given vgdl state
        newAttributeValues = self.setObjects(mode, initState)
        # Initialise state descriptions of model
        self.prev = None
        self.curr = self.getModelState()
        # Create lists for storing schemas and learning data
        self.schemas = [[],[],[],[],[],[],[],[]]
        self.evidence = [[],[],[],[],[],[],[],[]]
        self.XY = [[],[],[],[],[],[],[],[]]
        # Create lists for storing observed attributes and one-hot encoded versions
        self.obsActions = [[],[]]
        self.obsRewards = [[],[]]
        self.obsXsizes = [[],[]]
        self.obsYsizes = [[],[]]
        self.obsColours = [[],[]]
        self.obsShapes = [[],[]]
        return
               
    # Outputs dictionary representing the state of the model
    def getModelState(self):
        state = {}
        for objId in Model.objects.keys():
            state[objId] = obj.getObjectState()
        state{"action"} = self.action
        state{"reward"} = self.reward
        return state
    
    # Set up objects and map based on vgdl state
    def setObjects(self, state):
        if mode == "vgdl":
            i = max(list(self.objects.keys()) + [0])
            for objType in state.keys():
                for objPos in state(objectType):
                    position = list(objPos)
                    # Create new object and add to the set of objects and the map
                    newObj = Object(i, position[0], position[1])
                    newObj.colour = objType                   
                    newObj.x_size = 1                  
                    newObj.y_size = 1   
                    newObj.shape = "square"
                    self.objects[i] = newObj                   
                    if objPos is in self.objMap.keys():
                        self.objMap[objPos].append(i)
                    else:
                        self.objMap[objPos] = [i]
                    i = i + 1
                    # Update lists of observed attribute values and change schema learning data representations
                    self.updateObsLists(newObj, None, None)
                    # Update map dimensions if needed
                    if position[0] > self.xMax:
                        self.xMax = position[0]
                    if position[1] > self.yMax:
                        self.yMax = position[1]
            return
        # TODO
        else if mode == "ale":
            return
        else:
            return
            
    # Update lists of observed values and propagate changes to schema learning matrices
    def updateObsLists(self, obj, action, reward):
        # Intialise vector to record any changes
        changed = [False, False, False, False, False, False]
        # Check for object attributes
        if obj != None:
            if obj.x_size is not in self.obsXsizes[0]:
                self.obsXsizes[0].append(obj.x_size)
                self.obsXsizes[1] = util.oneHot(self.obsXsizes[0])
                changed[0] = True
            if obj.y_size is not in self.obsYsizes[0]:
                self.obsYsizes[0].append(obj.y_size)
                self.obsYsizes[1] = util.oneHot(self.obsYsizes[0])
                changed[1] = True
            if obj.colour is not in self.obsColours[0]:
                self.obsColours[0].append(obj.colour)
                self.obsColours[1] = util.oneHot(self.obsColours[0])
                changed[2] = True
            if obj.shape is not in self.obsShapes[0]:
                self.obsShapes[0].append(obj.shape)
                self.obsShapes[1] = util.oneHot(self.obsShapes[0])
                changed[3] = True
        # Check for new actions
        if action != None:
            if action is not in self.obsActions[0]:
                self.obsActions[0].append(action)
                self.obsActions[1] = util.oneHot(self.obsActions[0])
                changed[4] = True
        # Check for new actions
        if reward != None:
            if reward is not in self.obsRewards[0]:
                self.obsRewards[0].append(reward)
                self.obsRewards[1] = util.oneHot(self.obsRewards[0])
                changed[5] = True
        # TEMPORARILY REMOVED, MAY NOT BE EFFICIENT        
        # Update data matrices according to changes
        # self.updateMatrices(changes)
        return
            
    # Update model based on new observation        
    def updateModel(self, state):
        # Update action and reward as well as lists of observed values
        self.action = state[1]
        self.reward = state[2]
        self.updateObsLists(None, self.action, self.reward)
        if mode == "vgdl":
            # Update objects forming state description
            state = state[0]
            # Intialise arrays for storing IDs of objects that have changed or moved or both
            moved = []
            changed = []
            both = []
            # Check each object to see if it has changed or moved
            for objId in self.objects.keys():
                objType = self.objects[objId].colour
                objPos = (self.objects[objId].x_pos, self.objects[objId].x_pos)
                if objType is in state.keys():
                    if objPos is in state[objType]:
                        state[objType].remove(objPos)
                    else:
                        moved.append(objId)
                else:
                    changed.append(objId)
            # Update objects that have moved unless there is some ambiguity after LIMIT attempts
            i = 0
            while (len(moved) != 0) and (i < LIMIT):
                for objId in moved:
                    objType = self.objects[objId].colour
                    oldPos = (self.objects[objId].x_pos, self.objects[objId].x_pos)
                    possibleNewPos = state[objType]
                    neighbours = [n[0] for n in neighbourPositions(oldPos)]
                    intersection = list(set(possibleNewPos) & set(neighbours))
                    # If no neighbour of the same type can be found, the object may have changed
                    if len(intersection) == 0:
                        changed.append(objId)
                        moved.remove(objId)
                    # If a single neighbour of the same type can be found, it is assumed to be the same object
                    else if len(intersection) == 1:
                        newPos = intersection[O]
                        position = list(newPos)
                        self.objects[objId].x_pos = posiion[0]
                        self.objects[objId].y_pos = posiion[1]
                        del self.objMap[oldPos]
                        self.objMap[newPos] = objId
                        state[objType].remove(newPos)
                        moved.remove(objId)
                        # Update map dimensions if needed
                        if position[0] > self.xMax:
                            self.xMax = position[0]
                        if position[1] > self.yMax:
                            self.yMax = position[1]
                    # If not then there is some ambiguity, so we try resolving other cases first
                    else:
                        continue
                i = i + 1
            if i >= LIMIT:
                print("Error: Ambiguity in attempting to identify moved objects")
                return
            # Update objects that have changed unless there is some ambiguity after LIMIT attempts
            j = 0
            while (len(changed) != 0) and (j < LIMIT):
                for objId in changed:
                    oldType = self.objects[objId].colour
                    objPos = (self.objects[objId].x_pos, self.objects[objId].x_pos)
                    possibleNewType = []
                    for objType in state.keys():
                        if objPos is in state[objType]:
                            possibleNewType.append(objType)
                    # If no object is now found in this position the object may have both changed and moved
                    if len(possibleNewType) == 0:
                        both.append(objId)
                        changed.remove(objId)
                    # If an object of a single type is found to exist in this location then we assume it is the same object
                    else if len(possibleNewType) == 1:
                        newType = possibleNewType[O]
                        self.objects[objId].colour = newType
                        state[objType].remove(objPos)
                        changed.remove(objId)
                        # Update list of observed attribute values and data structures for schema learning
                        self.updateObsLists(self.objects[objId], None, None)
                    # If not then there is some ambiguity, so we try resolving other cases first
                    else:
                        continue
                j = j + 1
            if j >= LIMIT:
                print("Error: Ambiguity in attempting to identify changed objects")
                return
            # Update objects that have changed and moved            
            k = 0
            while (len(both) != 0) and (k < LIMIT):
                for objId in both:
                    oldPos = (self.objects[objId].x_pos, self.objects[objId].x_pos)
                    possibleNewPos = state.values()
                    neighbours = [n[0] for n in neighbourPositions(oldPos)]
                    intersection = list(set(possibleNewPos) & set(neighbours))
                    # If there are no neighbours at all we assume the object has disappeared
                    if len(intersection) == 0:
                        self.objects[objId].visible = False
                        del self.objMap[oldPos]
                        both.remove(objId)
                    # If there is a single neighbour (of different type) we assume it is the same object
                    if len(intersection) == 1:
                        newPos = intersection[O]
                        position = list(newPos)
                        self.objects[objId].x_pos = position[0]
                        self.objects[objId].y_pos = position[1]
                        for objType in state.keys():
                            if newPos is in state[objType]:
                                newType = objType
                                break
                        self.objects[objId].colour = newType
                        del self.objMap[oldPos]
                        self.objMap[newPos] = objId
                        state[newType].remove(newPos)
                        moved.remove(objId)
                        # Update list of observed attribute values and data structures for schema learning
                        self.updateObsLists(self.objects[objId], None, None)
                        # Update map dimensions if needed
                        if position[0] > self.xMax:
                            self.xMax = position[0]
                        if position[1] > self.yMax:
                            self.yMax = position[1]
                    # If not then there is some ambiguity, so we try resolving other cases first
                    else:
                        continue
                k = k + 1
            if k >= LIMIT:
                print("Error: Ambiguity in attempting to identify objects that have both changed and moved")
                return
            # Finally, add any new objects that might have appeared
            if len(state.values()) != 0:
                self.setObjects(state)
            return          
         # TODO
         else if mode == "ale":
             return
         else:
             return
    
    # Updates matrices that store data for learning schemas         
    def updateData(self):
        changes = util.changes(self)
        # If there are no changes to any object or the reward or action, then nothing needs updating
        if len(changes) == 0:
            return
        # If there is a new reward or action then this is relevant to all objects
        else if "action" is in changes or "reward" is in changes:
            for objId in model.objects.keys():
                xRow = util.formXvector(objId, self.prev, self.oldMap) + [self.action]
                yRow = util.formYvector(objId, self.prev, self.curr) + [self.reward]
                # Add new data points if they have not already been recorded
                for i in range(len(yRow):
                    checkData = self.checkData([xRow, yRow[i]], i)
                    if checkData = "predicted":
                        continue
                    if [xRow, yRow[i]] is not in self.XY[i]:
                        self.XY[i].append([xRow, yRow[i]])
            return
        # Otherwise we just update the data with those objects that have changed
        else:    
            for objId in changes:
                xRow = util.formXvector(objId, self.prev, self.oldMap) + [self.action]
                yRow = util.formYvector(objId, self.prev, self.curr) + [self.reward]
                # Add new data points if they have not already been recorded
                for i in range(len(yRow):
                    checkData = self.checkData([xRow, yRow[i]], i)
                    if checkData = "predicted":
                        continue
                    if [xRow, yRow[i]] is not in self.XY[i]:
                        self.XY[i].append([xRow, yRow[i]])      
            return
            
    # Checks whether existing schemas predict a datapoint correctly or not
    def check(self, [xRow, yRow[i]], index):
        # If no schema is active we output "none"
        output = "none"
        # Check each schema that predicts this attribute of the object
        for i in range(len(self.schemas[index])):
            if self.schemas[index][i].isActive(xRow):
                # If an active schema predicts the attribute value correctly and there are no wrong predictions, output "predicted"
                if self.schemas[index][i].head == yRow[i] and output != "wrong":
                    output = "predicted"
                # If an incorrect prediction is made by a schema we remove it and add the relevant evidence back to the learning data
                else:
                    self.XY =  self.XY + self.evidence[index][i]
                    del self.evidence[index][i]
                    del self.schemas[index]
                    output == "wrong"
        return output
        
    # TEMPORARILY REMOVED, MAY NOT BE EFFICIENT  
    # Updates one-hot encoding of matrices used to store data for schema learning
    def updateMatrices(self, changes):
        possibleChanges = [self.obsColours, self.obsShapes, self.obsXsizes, self.obsYsizes, self.obsActions, self.obsRewards]
        # For each data point in X
        for i in range(len(self.X)):
            # Change the binary encoding of each object and its neighbours
            for j in range(9):
                for k in range(4):
                    # If there is new attribute value of this type
                    if changes[k]:
                        oldBinaryAttribute = self.X[i][j][k+2]
            # Change the binary encoding of each action
            if changes[4]:
                oldBinaryAction = self.X[i][9] 
            # Change the binary encoding of each reward
            if changes[5]:
                oldBinaryReward = self.X[i][10]
          return      
    
    # Updates and learns new schemas       
    def learn(self):
        # For each object attribute
        for i in range(len(self.XY))
            [X, Y] = util.formBinaryMatrices(self.XY[i])
            # For each binary object attribute to be predicted
            for j in range(len(Y[0])):
                y = [row[j] for row in Y]
                [binarySchemas, binaryEvidence] = learnSchemas(X, y)
                # Convert learnt schemas and evidence from binary output and add
                newSchemas = [util.fromBinary(self, schema) for schema in binarySchemas]
                vector = [0 for element in Y[0]]
                vector[j] = 1
                newEvidence = [[util.fromBinary(self, evidence), util.fromBinary(self, vector, i)] for evidence in binaryEvidence]
                self.schemas[i] = self.schemas[i] + newSchemas
                self.evidence[i] = self.evidence[i] + newEvidence
          
# Define the object class
class Object:

    # Intialise object
    def __init__(self, id, x_pos=None, y_pos=None):
        self.id = id
        self.x_pos = x_pos
        self.y_pos = y_pos
        self.x_size = None
        self.y_size = None
        self.colour = None
        self.shape = None
        self.visible = True
        return
    
    # Outputs list representing the state of the object
    def getObjectState(self):
        state = [self.x_pos, self.y_pos, self.x_size, self.y_size, self.colour, self.shape, self.visible]
        return state
        

# Define the schema class
class Schema:
    
    # Initilaise schema
    def __init__(self, id):
        self.id = id
        self.objectBody = {}
        self.actionBody = None
        self.head = None
    
    # Checks if the schema is active against a vector describing an object
    def isActive(self, x):
        output = True
        # Check if object attribute preconditions are met
        if len(self.objectBody.keys()) != 0:
            for key in self.objectBody.keys():
                i = list(key):
                if self.objectBody(key) != x[i[0]][i[1]]:
                    return False
        # Checks if action preconditions are met
        if self.actionBody != None and self.actionBody != x[9]:
            return False
        return True
        

# Define the Q-function class
class QFunction:
    # TODO