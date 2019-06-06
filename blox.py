# BLOX
# Classes for data structures used during learning and verification

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


import util
import lern
from copy import deepcopy


# Define the model class
class Model:

    # Initialise model according to mode
    def __init__(self, name, mode, initState, xMax=0, yMax=0, deterministic=True):


        self.name = name
        self.deterministic = deterministic

        self.num_objects = 0
        self.num_schemas = 0

        # Create empty policy, Q function and reward function
        self.pi = {}
        self.Q = {}
        self.R = {}

        # Intialise dimensions of map and object/map attributes
        self.xMax = xMax
        self.yMax = yMax

        # Create lists for storing observed attributes and one-hot encoded versions
        self.obsXpos = [["left", "centre", "right"], [[1,0,0],[0,1,0],[0,0,1]]]
        self.obsYpos = [["below", "centre", "above"], [[1,0,0],[0,1,0],[0,0,1]]]
        self.obsXsizes = [[],[]]
        self.obsYsizes = [[],[]]
        self.obsColours = [[],[]]
        self.obsShapes = [[],[]]
        self.obsNothing = [["yes", "no"], [[1,0],[0,1]]]

        # Create lists for storing observed states, actions, rewards, and transitions
        self.obsActions = [["none"],[[0]]]
        self.obsRewards = [[],[]]
        self.obsTrans = set([])
        self.obsState = set([])
        self.obsChanges = set([])

        # Create dictionaries for fast conversion between attribute values and binary versions
        self.observations = [self.obsXpos, self.obsYpos, self.obsXsizes, self.obsYsizes, self.obsColours, self.obsShapes, self.obsNothing, self.obsRewards, None, self.obsActions]
        self.dictionaries = {}

        # Create lists for storing schemas and learning data
        self.schemas = [{"left":[], "centre":[], "right":[]},{"below":[], "centre":[], "above":[]},{},{},{},{},{"yes":[],"no":[]},{}]
        self.evidence = [{"left":[], "centre":[], "right":[]},{"below":[], "centre":[], "above":[]},{},{},{},{},{"yes":[],"no":[]},{}]
        self.data = [{"left":[], "centre":[], "right":[]},{"below":[], "centre":[], "above":[]},{},{},{},{},{"yes":[],"no":[]},{}]

        # Initialise state descriptions of model and update dictionaries
        self.initialise(mode, initState)

        return


    # Initialises model according an initial state
    def initialise(self, mode, initState):

        # Create variables for storing the current action and reward values, plus the RMAX value
        self.action = None
        self.reward = None



        self.RMAX = 100




        # Initialise map and object dictionaries
        self.objects = {}
        self.oldMap = {}
        self.objMap = {}

        # Set up model using intial observation
        self.num_objects = 0
        self.setObjects(mode, initState)
        self.prev = None
        self.curr = self.getModelState()
        self.updateDicts()

        return


    # Resets and renames a model for use in a new environment with the same dynamics
    def reset(self, name):

        # Reset/rename variables
        self.name = name
        self.pi = {}
        self.Q = {}
        self.R = {}
        self.xMax = 0
        self.yMax = 0
        self.obsTrans = set([])
        self.obsState = set([])
        self.obsChanges = set([])




        # PROBLEMATIC? THIS SHOULD BE CONSISTENT
        # We remove non-positive data points for each schema
        for att in range(REWARD + 1):
            for val in self.observations[att][0]:
                self.data[att][val] = []






        # We also remove all data points for each reward schema
        for r in self.obsRewards[0]:
            self.evidence[REWARD][r] = []

        return


    # Outputs dictionary representing the state of the model
    def getModelState(self):

        state = {}
        for objId in self.objects.keys():
            state[objId] = self.objects[objId].getObjectState()

        return state

    # Set up objects and map based on vgdl state
    def setObjects(self, mode, state):

        # for key in state.keys():
        #     # state[key.capitalize()] = state.pop(key)
        #     state[key] = state.pop(key)
        if mode == "vgdl":

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
                        self.objMap[objPos].append(self.num_objects)
                    else:
                        self.objMap[objPos] = [self.num_objects]
                    self.num_objects += 1

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
        if action != None and action not in self.obsActions[0] and action != "none":

            # Remove 'nothing' action for one-hot encoding
            self.obsActions[0].remove("none")

            # Add new action and update one-hot encoded values
            self.obsActions[0].append(action)
            self.obsActions[1] = util.oneHot(self.obsActions[0])

            # Add 'nothing' option back in again
            actionLength = len(self.obsActions[1][0])
            self.obsActions[0].append("none")
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
        self.data[index][attribute] = []

        return

    # Update dictionaries for faster conversion between attribute values and binary versions
    def updateDicts(self):

        self.observations = [self.obsXpos, self.obsYpos, self.obsXsizes, self.obsYsizes, self.obsColours, self.obsShapes, self.obsNothing, self.obsRewards, None, self.obsActions]
        self.dictionaries = []
        for obs in self.observations:
            if obs == None:
                self.dictionaries.append(None)
            else:
                self.dictionaries.append(util.obsToDicts(obs))

        return


    # Update model based on new observation
    def updateModel(self, mode, observation):

        # Update action and reward as well as lists of observed values
        self.action = observation[1]
        self.reward = observation[2]
        self.updateObsLists(None, self.action, self.reward)

        # If the game has ended there is no new state observed
        if observation[0] == None:
            return

        # Update objects forming state description
        state = deepcopy(observation[0])
        # for key in state.keys():
            # state[key.capitalize()] = state.pop(key)
        if mode == "vgdl":

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
                        self.objMap[oldPos].remove(objId)
                        if newPos in self.objMap.keys():
                            self.objMap[newPos].append(objId)
                        else:
                            self.objMap[newPos] = [objId]
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
                        newType = possibleNewType[0]
                        self.objects[objId].colour = newType
                        state[oldType].remove(objPos)
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
                        self.objects[objId].nothing = "yes"


                        print("oops, object " + str(objId) + " disappeared")



                        self.objMap[oldPos].remove(objId)
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
                        if newPos in self.objMap.keys():
                            self.objMap[newPos].append(objId)
                        else:
                            self.objMap[newPos] = [objId]
                        state[newType].remove(newPos)
                        both.remove(objId)

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

        # Initialise variables
        rRow = {}
        a = self.action
        r = self.reward

        # Update transition data
        for objId in self.objects.keys():
            xRow = util.formXvector(objId, self.prev, self.oldMap) + [a]
            yRow = util.formYvector(objId, self.prev, self.curr) + [r]
            rRow[objId] = xRow

            # Add new data points if they have not already been recorded
            for i in range(REWARD):
                if self.checkDatum([xRow, yRow[i]], i) and xRow not in self.evidence[i][yRow[i]]:
                    self.evidence[i][yRow[i]].append(xRow)
                elif xRow not in self.data[i][yRow[i]]:
                    self.data[i][yRow[i]].append(xRow)

        # Update reward data
        if rRow not in self.evidence[REWARD][r]:

            # If the data point is predicted it is added to the evidence set
            predicted = False
            for key in rRow.keys():
                if self.checkDatum([rRow[key], r], REWARD):
                    predicted = True
                    self.evidence[REWARD][r].append(rRow)
                    break

            # Otherwise we add it to the data set
            if not predicted and rRow not in self.data[REWARD][r]:
                self.data[REWARD][r] += [rRow]

        return


    # Checks whether existing schemas predict a datapoint correctly or not
    def checkDatum(self, datum, index):

        # If no schema is active then the attribute value is not predicted
        predicted = False

        # Check each schema that predicts this attribute of the object
        attributes = ["X_pos", "Y_pos", "X_size", "Y_size", "Colour", "Shape", "Nothing", "Reward"]
        for key in self.schemas[index].keys():
            errorMade = False
            for schema in self.schemas[index][key]:
                if schema.isActive(datum[0]):

                    # If an active schema predicts the attribute value correctly output "predicted"
                    if key == datum[1]:
                        predicted = True

                    # If an incorrect prediction is made by a schema we remove it and add the relevant evidence back to the learning data
                    else:
                        # print("------------------------------------")
                        # print("Datum body: ")
                        # for item in datum[0]:
                        #     print item
                        # print("Datum head: ")
                        # print datum[1]
                        # print("------------------------------------")
                        print("Removed schema:")
                        print(attributes[index] + " = " + str(key) + " <- " + schema.display(no_head=True))
                        # print schema.objectBody
                        # print schema.actionBody
                        # print("---")
                        self.schemas[index][key].remove(schema)
                        errorMade = True

            # If an incorrect prediction was made we remove all evidence for this particular attribute value
            if errorMade:
                self.data[index][key] += self.evidence[index][key]
                self.evidence[index][key] = []

        return predicted


    # Function for cleaning model of duplicate information
    def clean(self):

        # For each possible attribute or reward value
        attributes = ["X_pos", "Y_pos", "X_size", "Y_size", "Colour", "Shape", "Nothing", "Reward"]
        for att in range(REWARD + 1):
            for val in self.observations[att][0]:

                # Remove duplicate data and schemas
                self.data[att][val] = util.deDupe(self.data[att][val])
                self.evidence[att][val] = util.deDupe(self.evidence[att][val])

                # Simplify schemas
                self.schemas[att][val] = util.simplify(self, self.schemas[att][val], val, attributes[att])

        return


    # Updates and learns new schemas
    def learn(self, transitions, rewards):

        # Prepare for learning
        self.updateDicts()
        attributes = ["X_pos", "Y_pos", "X_size", "Y_size", "Colour", "Shape", "Nothing", "Reward"]
        if transitions and rewards:
            att_list = range(REWARD + 1)
        elif transitions and not rewards:
            att_list = range(REWARD)
        elif not transitions and rewards:
            att_list = [REWARD]
        else:
            return

        # For each object attribute or reward
        for i in att_list:

            remaining = dict(zip(self.data[i].keys(),[[] for key in self.data[i].keys()]))

            # For each attribute/reward value to be predicted
            for key in self.data[i].keys():

                # If the maximum number of schemas has already been learn we skip this round of learning
                if len(self.schemas[i][key]) >= LIMIT:
                    remaining[key] = self.data[i][key]
                    continue

                # If we are predicting rewards the learning data is constructed from all objects that have changed
                if i == REWARD:

                    # Form positive cases
                    xYes = []
                    xNo = []
                    for datum in self.data[i][key]:
                        predicted = False
                        for o in datum.keys():
                            if self.checkDatum([datum[o], key], i):
                                predicted = True
                                self.evidence[i][key].append(datum)
                                break
                        if not predicted:
                            xYes += [datum[c] for c in self.obsChanges]
                            xNo += [datum[o]for o in datum.keys() if o not in self.obsChanges]

                    # Form negative cases
                    for other in self.data[i].keys():
                        if other != key:
                            xNo += util.flatten([[datum[o] for o in datum.keys()] for datum in self.data[i][other] + self.evidence[i][other]])

                # Otherwise we construct learning data in the standard way
                else:

                    # Form positive cases
                    xYes = []
                    for datum in self.data[i][key]:
                        if datum[0][i] != key:
                            if self.checkDatum([datum,key], i):
                                self.evidence[i][key].append(datum)
                            else:
                                xYes.append(datum)
                    self.data[i][key] = [datum for datum in self.data[i][key] if datum not in self.evidence[i][key]]

                    # Form negative cases
                    xNo = [self.data[i][other] + self.evidence[i][other] for other in self.data[i].keys() if other != key]
                    xNo = util.flatten(xNo)

                # If there are no changes in this attribute of the primary object then we skip this round of learning
                if len(xYes) == 0:
                    remaining[key] = self.data[i][key]
                    # print("no changes for " + str(key))
                    continue

                # Form binary vectors for learning
                xYes = [util.toBinary(self, item) for item in xYes]
                xNo = [util.toBinary(self, item) for item in xNo]
                schemas = [util.toBinarySchema(self, schema) for schema in self.schemas[i][key]]
                oldSchemas = deepcopy(schemas)

                # print("Learning for " + str(key))

                # Learn and output schemas, new evidence, and remaining positive cases
                if i == REWARD:
                    [binarySchemas, _, _] = lern.learnSchemas(xYes, xNo, schemas)
                else:
                    [binarySchemas, binaryEvidence, binaryRemaining] = lern.learnSchemas(xYes, xNo, schemas)

                # print("111111111111111111")
                # print schemas
                # print("222222222222222222")
                # print binarySchemas
                # print("333333333333333333")

                # Name and display new schemas to user
                new_schemas = [util.fromBinarySchema(self, s, key) for s in binarySchemas if s not in oldSchemas]
                if len(new_schemas) != 0:
                    print("New schemas: ")
                    for s in new_schemas:
                        s.id = self.num_schemas
                        self.num_schemas += 1
                        print(attributes[i] + " = " + str(key) + " <- " + s.display(no_head=True))

                # Convert learnt schemas and evidence from binary output and add to model
                self.schemas[i][key] += new_schemas

                # If they are reward schemas then the binary evidence and remaining data are not in the correct form to be stored
                if i == REWARD:
                    for datum in self.data[i][key]:
                        predicted = False
                        for o in datum.keys():
                            if self.checkDatum([datum[o], key], i):
                                predicted = True
                                self.evidence[i][key].append(datum)
                                break
                        if not predicted:
                            remaining[key].append(datum)

                # Otherwise we can convert directly back from the binary data and store the resukt
                else:
                    self.evidence[i][key] += [util.fromBinary(self, datum) for datum in binaryEvidence]
                    remaining[key] = [util.fromBinary(self, datum) for datum in binaryRemaining]

            self.data[i] = remaining

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
        self.nothing = "no"
        return

    # Outputs list representing the state of the object
    def getObjectState(self):
        state = [self.x_pos, self.y_pos, self.x_size, self.y_size, self.colour, self.shape, self.nothing]
        return state

    # Displays object for use in forming Prolog file
    def observe(self, no_obj=False):
        if no_obj:
            name = "no_obj" + str(self.id)
        else:
            name = "obj" + str(self.id)
        output = "observation(x_pos(" + name + ")) ~= " + str(self.x_pos).lower() + ", "
        output = output + "observation(y_pos(" + name + ")) ~= " + str(self.y_pos) + ", "
        if not no_obj:
            output = output + "observation(x_size(" + name + ")) ~= " + str(self.x_size) + ", "
            output = output + "observation(y_size(" + name + ")) ~= " + str(self.y_size) + ", "
            output = output + "observation(colour(" + name + ")) ~= " + self.colour + ", "
            output = output + "observation(shape(" + name + ")) ~= " + self.shape + ", "
        output = output + "observation(nothing(" + name + ")) ~= " + self.nothing
        return output

    def display(self):
        output = ["obj" + str(self.id)] + [str(i) for i in self.getObjectState()]
        return ", ".join(output)

# Define the schema class
class Schema:

    # Initilaise schema
    def __init__(self):
        self.id = id
        self.objectBody = {}
        self.actionBody = None
        self.head = None
        self.positive = 0
        self.negative = 0
        return

    # Checks if the schema is active against a vector describing an object
    def isActive(self, x):

        # Check if object attribute preconditions are met
        if len(self.objectBody.keys()) != 0:
            for key in self.objectBody.keys():
                objAtt = list(key)

                # If there is no object in the position referred to by the attribute
                # print x
                # print objAtt[0]

                if len(x[objAtt[0]]) == 0:
                    return False

                # If the object attribute is not the same as in the datum
                if self.objectBody[key] != x[objAtt[0]][objAtt[1]]:
                    return False

        # Checks if action preconditions are met
        if self.actionBody != None and self.actionBody != x[9]:
            return False

        return True

    # Prints out schema in human-readable format
    def display(self, no_head=False):

        objects = ["obj"] + ["nb" + str(i+1) for i in range(NEIGHBOURS)]
        objNames = [name.capitalize() for name in objects]
        added = [False for item in objects]
        added[0] = True
        attributes = ["x_pos", "y_pos", "x_size", "y_size", "colour", "shape", "nothing"]

        # SCHEMA NAMING REMOVED FOR NOW
        # schemaName = "Schema " + str(self.id) + ": "
        # schemaName = ""

        schemaBody = ""
        for (i,j) in self.objectBody.keys():

            # Assert neighbour relation if needed
            if not added[i]:
                addNeighbour = objects[i] + "(Obj," + objNames[i] + "):t"
                schemaBody = schemaBody + addNeighbour + ", "
                added[i] = True

            # Add schema preconditions
            precondition = attributes[j] + "(" + objNames[i] + "):t ~= " + str(self.objectBody[(i,j)])
            schemaBody = schemaBody + precondition + ", "

        if self.actionBody == None:
            schemaBody = schemaBody[:-2]
        else:
            schemaBody = schemaBody + "action(" + str(self.actionBody) + ")"

        if no_head:
            return schemaBody
        else:
            return schemaBody + ", " + str(self.head)


# # Define the Q-function class
# class QFunction:
#
#     # Intialise Q-function
#     def __init__(self, mode):
#         # TODO
#         return
#
#     # Given a model, including current state choose an action to perform
#     def chooseAction(self, mode, model):
#         if mode == "vgdl":
#             # TODO
#             return
#         elif mode == "ale":
#             # TODO
#             return
#         else:
#             return
#
#     def update(self, model):
#         # TODO
#         return
