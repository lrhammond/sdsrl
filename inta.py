# INTA
# Functions for interfacing between game playing environments and the data needed for learning


import sys
from mdpmap import MDPconverter
from core import VGDLParser
from rlenvironment import RLEnvironment
import pygame
import util
import numpy as np


# Setup game according to mode chosen
def setup(mode):
    
    if mode == "vgdl":
        # Read in level and game descriptions
        levelPath = raw_input("Input path to level file: ")
        gamePath = raw_input("Input path to game file: ")
        with open(levelPath, 'r') as levelFile:
            level = levelFile.read()
        with open(gamePath, 'r') as gameFile:
            game = gameFile.read()
        # Start game
        sys.path.insert(0, 'pyvgdlmaster/vgdl')
        g = VGDLParser().parseGame(game)
        g.buildLevel(level)
        rle = RLEnvironment(game, level, observationType='global', visualize=True)
        # Set up RLE
        rle.actionDelay = 200
        rle.recordingEnabled = True
        rle.reset()
        environment = rle
        
    # TODO
    else if mode == "ale":
        
    else:
        return
    
    # Return environment
    return environment


# Observe state from game environment and output in basic format
def observeState(mode, environment):
    
    if mode == "vgdl":
        state = environment._obstypes.copy()
        agentState = environment.getState()
        state['agent'] = [(agentState[0], agentState[1])]
    
    # TODO
    else if mode == "ale":
        
    else:
        return
    
    return state
    

# Perform action in game enviroment and output reward and whether game has ended
def performAction(mode, environment, actions):
    
    if mode == "vgdl":
        # Take action
        environment._performAction(actions)
        action = rle._allEvents[-1][1]
        # Get reward
        (ended, won) = rle._isDone()
        if ended:
            if won:
                reward = 10
            else:
                reward = -10
        else:
            reward = -1
        # Return reward and whether game has ended
        return [reward, ended]
    
    # TODO
    else if mode == "ale":
        
    else:
        return
    
    return 
        
        
# Encode sequence of states/rewards/actions into one-hot encoded list representations
def encodeSequence(mode, fileName=None, rStates, rRewards, rActions)

    if mode == "vgdl":
        
        # One-hot encode rewards and actions
        bRewards = util.oneHot(rRewards)
        bActions = util.oneHot(rActions)
        bStates = util.oneHot
        # Create map of items based on location
        objectMap = {}
        # One-hot encode objects and their attributes, with absolute location
        objectTypes = list(set([state.keys() for state in rStates]))
        
        bTypes = util.oneHot(objectTypes)
        typeDict = dict(zip(objectTypes, bTypes))
        bStates = []
        for state in rStates:
            bState = []
            i = 0
            for objectType in state.keys():
                for position in state(objectType):
                    bState.append([position, typeDict(objectType)])
                    objectMap(position) = i
                    i = i + 1
            bStates.append(bState)
        # Optionally record information to file    
        if fileName != None:
            # TODO
            # Save state information
            f = open(fileName+"_states.csv", "w")
            f.close()
            # Save reward information
            f = open(fileName+"_rewards.csv", "w")
            f.close()
            # Save action information
            f = open(fileName+"_actions.csv", "w")
            f.close()

    else:
        # TODO
        return
    
    return bStates, bRewards, bActions, objectMap
    

# Form set of matrices X and Y for supervised learning of schemas using relaxed (binary) integer programming
def formMatrices(states, rewards, actions, objectMap):
    
    # Intialise matrix X
    X = []
    
    # Range across all recorded states
    for t in range(len(bStates)):
        state = bStates[t]
        nextState = bStates[t + 1]
        # Range across each object
        numObjects = len(state.keys())
        for obj in state.keys():
            # Form object vector
            pos = obj[0]
            objVector = obj[1] + [0,0,0,0] + [1]
            numAttributes = len(objVector)
            # Extend with information about neighbours
            row = objVector
            neighbours = util.neighbourPositions(pos)
            for nb in neighbours:
                if nb[0] is in objectMap.keys():
                    nbVector = state[objectMap(nb(0))][1] + nb[1] + [1]
                    row = row + nbVector
                else:
                    row = row + [0 for item in objVector]
            # Add actions the append to matrix X
            row = row + bActions[t]
            X.append(row)
            
    # Form list Y of vectors y for each attribute using matrix X without entries for first time step
    Xnotfirst = X[numObjects:]
    Y = [[row[i] for row in X] for i in range(numAttributes)]
                    
    # Remove entries for the last timestep from X
    del X[:numObjects]
    
    return X, Y