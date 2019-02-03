# BLOX
# Classes for data structures used during learning and verification

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


import util
from lern import learnSchemas
from copy import deepcopy


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
        # Create lists for storing observed attributes and one-hot encoded versions
        self.obsXpos = [["left", "centre", "right"], [[1,0],[0,0],[0,1]]]
        self.obsYpos = [["below", "centre", "above"], [[1,0],[0,0],[0,1]]]
        self.obsXsizes = [[],[]]
        self.obsYsizes = [[],[]]
        self.obsColours = [[],[]]
        self.obsShapes = [[],[]]
        self.obsVisible = [["yes", "no"], [[1],[0]]]
        # Create lists for storing observed actions and rewards
        self.obsActions = [["nothing"],[[0]]]
        self.obsRewards = [[],[]]
        # Create dictionaries for fast conversion between attribute values and binary versions
        self.observations = [self.obsXpos, self.obsYpos, self.obsXsizes, self.obsYsizes, self.obsColours, self.obsShapes, self.obsVisible, self.obsRewards, None, self.obsActions]
        self.dictionaries = {}
        # Create lists for storing schemas and learning data
        self.schemas = [{"left":[], "centre":[], "right":[]},{"below":[], "centre":[], "above":[]},{},{},{},{},{"yes":[],"no":[]},{}]
        self.evidence = [{"left":[], "centre":[], "right":[]},{"below":[], "centre":[], "above":[]},{},{},{},{},{"yes":[],"no":[]},{}]
        self.XY = [{"left":[], "centre":[], "right":[]},{"below":[], "centre":[], "above":[]},{},{},{},{},{"yes":[],"no":[]},{}]
        # Create set of objects given vgdl state
        newAttributeValues = self.setObjects(mode, initState)
        # Initialise state descriptions of model and update dictionaries
        self.prev = None
        self.curr = self.getModelState()
        self.updateDicts()
        return
               
    # Outputs dictionary representing the state of the model
    def getModelState(self):
        state = {}
        for objId in self.objects.keys():
            state[objId] = self.objects[objId].getObjectState()
        state["action"] = self.action
        state["reward"] = self.reward
        return state
    
    # Set up objects and map based on vgdl state
    def setObjects(self, mode, state):
        if mode == "vgdl":
            i = max(list(self.objects.keys()) + [0])
            for objType in state.keys():
                for objPos in state[objType]:
                    position = list(objPos)
                    # Create new object and add to the set of objects and the map
                    newObj = Object(i, position[0], position[1])
                    newObj.colour = objType                   
                    newObj.x_size = 1                  
                    newObj.y_size = 1   
                    newObj.shape = "square"
                    self.objects[i] = newObj                   
                    if objPos in self.objMap.keys():
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
        elif mode == "ale":
            return
        else:
            return
            
    # Update lists of observed values and propagate changes to schema learning matrices
    def updateObsLists(self, obj, action, reward):
        # Check for object attributes
        if obj != None:
            if obj.x_size not in self.obsXsizes[0]:
                self.obsXsizes[0].append(obj.x_size)
                self.obsXsizes[1] = util.oneHot(self.obsXsizes[0])
                self.updateDataKeys(X_SIZE, obj.x_size)
            if obj.y_size not in self.obsYsizes[0]:
                self.obsYsizes[0].append(obj.y_size)
                self.obsYsizes[1] = util.oneHot(self.obsYsizes[0])
                self.updateDataKeys(Y_SIZE, obj.y_size)
            if obj.colour not in self.obsColours[0]:
                self.obsColours[0].append(obj.colour)
                self.obsColours[1] = util.oneHot(self.obsColours[0])
                self.updateDataKeys(COLOUR, obj.colour)
            if obj.shape not in self.obsShapes[0]:
                self.obsShapes[0].append(obj.shape)
                self.obsShapes[1] = util.oneHot(self.obsShapes[0])
                self.updateDataKeys(SHAPE, obj.shape)
        # Check for new actions
        if action != None and action not in self.obsActions[0] and action != "nothing":
            # Remove 'nothing' action for one-hot encoding
            self.obsActions[0].remove("nothing")
            # Add new action and update one-hot encoded values
            self.obsActions[0].append(action)
            self.obsActions[1] = util.oneHot(self.obsActions[0])
            # Add 'nothing' option back in again
            actionLength = len(self.obsActions[1][0])
            self.obsActions[0].append("nothing")
            self.obsActions[1].append([0 for i in range(actionLength)])
        # Check for new rewards
        if reward != None and reward not in self.obsRewards[0]:
            self.obsRewards[0].append(reward)
            self.obsRewards[1] = util.oneHot(self.obsRewards[0])
            self.updateDataKeys(REWARD, reward)
        return
        
    # Update keys for storing data and schemas
    def updateDataKeys(self, index, attribute):
        self.schemas[index][attribute] = []
        self.evidence[index][attribute] = []
        self.XY[index][attribute] = []
        return
        
    # Update dictionaries for fast conversion between attribute values and binary versions
    def updateDicts(self):
        self.observations = [self.obsXpos, self.obsYpos, self.obsXsizes, self.obsYsizes, self.obsColours, self.obsShapes, self.obsVisible, self.obsRewards, None, self.obsActions]
        self.dictionaries = []
        for obs in self.observations:
            if obs == None:
                self.dictionaries.append(None)
            else:
                self.dictionaries.append(util.obsToDicts(obs))
        return
            
    # Update model based on new observation        
    def updateModel(self, mode, state):
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
                objPos = (self.objects[objId].x_pos, self.objects[objId].y_pos)
                
                
                
                
                
                if objType in state.keys():
                    if objPos in state[objType]:
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
                    oldPos = (self.objects[objId].x_pos, self.objects[objId].y_pos)
                    possibleNewPos = state[objType]
                    neighbours = [n[0] for n in util.neighbourPositions(oldPos)]
                    intersection = [item for item in possibleNewPos if item in neighbours]
                    # If no neighbour of the same type can be found, the object may have changed
                    if len(intersection) == 0:
                        changed.append(objId)
                        moved.remove(objId)
                    # If a single neighbour of the same type can be found, it is assumed to be the same object
                    elif len(intersection) == 1:
                        newPos = intersection[0]
                        position = list(newPos)
                        self.objects[objId].x_pos = position[0]
                        self.objects[objId].y_pos = position[1]
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
                    objPos = (self.objects[objId].x_pos, self.objects[objId].y_pos)
                    possibleNewType = []
                    for objType in state.keys():
                        if objPos in state[objType]:
                            possibleNewType.append(objType)
                    # If no object is now found in this position the object may have both changed and moved
                    if len(possibleNewType) == 0:
                        both.append(objId)
                        changed.remove(objId)
                    # If an object of a single type is found to exist in this location then we assume it is the same object
                    elif len(possibleNewType) == 1:
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
                    oldPos = (self.objects[objId].x_pos, self.objects[objId].y_pos)
                    possibleNewPos = state.values()
                    neighbours = [n[0] for n in util.neighbourPositions(oldPos)]
                    intersection = [item for item in possibleNewPos if item in neighbours]
                    # If there are no neighbours at all we assume the object has disappeared
                    if len(intersection) == 0:
                        self.objects[objId].visible = "no"
                        del self.objMap[oldPos]
                        both.remove(objId)
                    # If there is a single neighbour (of different type) we assume it is the same object
                    if len(intersection) == 1:
                        newPos = intersection[O]
                        position = list(newPos)
                        self.objects[objId].x_pos = position[0]
                        self.objects[objId].y_pos = position[1]
                        for objType in state.keys():
                            if newPos in state[objType]:
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
                self.setObjects(mode, state)
            return          
        # TODO
        elif mode == "ale":
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
        elif "action" in changes or "reward" in changes:
            for objId in self.objects.keys():
                xRow = util.formXvector(objId, self.prev, self.oldMap) + [self.action]
                yRow = util.formYvector(objId, self.prev, self.curr) + [self.reward]
                # Add new data points if they have not already been recorded
                
                for i in range(len(yRow)):
                    if self.checkDatum([xRow, yRow[i]], i):
                        continue    
                    if xRow not in self.XY[i][yRow[i]]:
                        self.XY[i][yRow[i]].append(xRow)
            return
            
            
        # Otherwise we just update the data with those objects that have changed
        else:    
            for objId in changes:
                xRow = util.formXvector(objId, self.prev, self.oldMap) + [self.action]
                yRow = util.formYvector(objId, self.prev, self.curr) + [self.reward]
                # Add new data points if they have not already been recorded
                
              
                for i in range(len(yRow)):
                
                    if self.checkDatum([xRow, yRow[i]], i):
                        continue
                    if xRow not in self.XY[i][yRow[i]]:
                        self.XY[i][yRow[i]].append(xRow)     
            return
            
    # Checks whether existing schemas predict a datapoint correctly or not
    def checkDatum(self, datum, index):
        # If no schema is active we output "none"
        predicted = False
        errorMade = False
        # Check each schema that predicts this attribute of the object
        for key in self.schemas[index].keys():
            for schema in self.schemas[index][key]:
                if schema.isActive(datum[0]):
                    # If an active schema predicts the attribute value correctly output "predicted"
                    if key == datum[1]:
                        predicted = True
                    # If an incorrect prediction is made by a schema we remove it and add the relevant evidence back to the learning data
                    else:
                        self.schemas[index][key].remove(schema)
                        self.XY[index][key] = self.XY[index][key] + self.evidence[index][key]
                        errorMade = True
            # If an incorrect prediction was made we remove all evidence for this particular attribute value
            if errorMade:
                self.evidence[index][key] = []
        return predicted
        
        
        
        
        
        
        
        
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
        self.updateDicts()
        # For each object attribute
        for i in range(len(self.XY)):
    
            remainingEvidence = {}
            
            # For each binary object attribute to be predicted
            for key in self.XY[i].keys():
                # Form lists of positive and negative cases
                
                
                xYes = self.XY[i][key]
      
                
                others = deepcopy(self.XY[i].keys())
                others.remove(key)
                
                print others
                # print("right")
#                 print self.XY[i]['right']
#                 print("centre")
#                 print self.XY[i]['centre']
                xNo = [self.XY[i][other] for other in others]
                print xNo
                xNo = util.flatten(xNo)
                print xNo
           
                
                
                # Form vectors for learning
                y = [1 for item in xYes]
                X = [util.toBinary(self, item) for item in xYes]
                y = y + [0 for item in xNo]
                X = X + [util.toBinary(self, item) for item in xNo]
                # Learn schemas and output new schemas, evidence, and remaining cases 
                [binarySchemas, binaryEvidence, binaryRemaining] = learnSchemas(self, X, y, self.schemas[i][key])
                # Convert learnt schemas and evidence from binary output and add to model
                self.schemas[i][key] = self.schemas[i][key] + [util.fromBinarySchema(self, schema) for schema in binarySchemas]
                self.evidence[i][key] = self.evidence[i][key] + [util.fromBinary(self, datum) for datum in binaryEvidence]
                remainingEvidence[key] = [util.fromBinary(self, datum) for datum in binaryRemaining]
            
            self.XY[i] = remainingEvidence
            return
            
        return
                
          
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
        self.visible = "yes"
        return
    
    # Outputs list representing the state of the object
    def getObjectState(self):
        state = [self.x_pos, self.y_pos, self.x_size, self.y_size, self.colour, self.shape, self.visible]
        return state
        

# Define the schema class
class Schema:
    
    # Initilaise schema
    def __init__(self):
        # SCHEMA NAMING REMOVED FOR NOW
        # self.id = id
        self.objectBody = {}
        self.actionBody = None
        self.head = None
        return
    
    # Checks if the schema is active against a vector describing an object
    def isActive(self, x):
        # Check if object attribute preconditions are met
        if len(self.objectBody.keys()) != 0:
            for key in self.objectBody.keys():
                i = list(key)
                if len(x[i[0]]) != 0 and self.objectBody[key] != x[i[0]][i[1]]:
                    return False
        # Checks if action preconditions are met
        if self.actionBody != None and self.actionBody != x[9]:
            return False
        return True
     
    # Prints out schema in human-readble format  
    def display(self):
        objects = ["obj"] + ["nb" + str(i+1) for i in range(8)]
        attributes = ["x_pos", "y_pos", "x_size", "y_size", "colour", "shape", "visible"]
        # SCHEMA NAMING REMOVED FOR NOW
        # schemaName = "Schema " + str(self.id) + ": "
        schemaName = ""
        schemaBody = ""
        for key in self.objectBody.keys():
            i = list(key)
            precondition = attributes[i[1]] + "(" + objects[i[0]] + ")=" + str(self.objectBody(key))
            schemaBody = schemaBody + preconditon + " AND "
        schemaBody = schemaBody + "action=" + str(self.actionBody)
        schemaHead = str(self.head)
        output = schemaName + schemaBody + " ---> " + schemaHead
        print(output)
        return


# Define the Q-function class
class QFunction:
    
    # Intialise Q-function
    def __init__(self, mode):
        # TODO
        return
        
    # Given a model, including current state choose an action to perform
    def chooseAction(self, mode, model):
        if mode == "vgdl":
            # TODO
            return
        elif mode == "ale":
            # TODO
            return
        else:
            return
    
    def update(self, model):
        # TODO
        return