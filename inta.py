# INTA
# Functions for interfacing between game playing environments and the data needed for learning

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


import sys
sys.path.insert(0, 'pyvgdlmaster/vgdl')
from mdpmap import MDPconverter
from core import VGDLParser
from rlenvironment import RLEnvironment
import pygame
import util
import numpy as np


# Setup game according to mode chosen
def setup(mode, test=False):
    if mode == "vgdl":
        if test == False:
            # Read in level and game descriptions
            levelPath = raw_input("Input path to level file: ")
            gamePath = raw_input("Input path to game file: ")
        else:
            levelPath = "level2.txt"
            gamePath = "game.txt"
        with open(levelPath, 'r') as levelFile:
            level = levelFile.read()


            # print("\nLEVEL:\n\n" + level)


        with open(gamePath, 'r') as gameFile:
            game = gameFile.read()


            # print("\nGAME:\n\n" + game)


        # Start game
        g = VGDLParser().parseGame(game)
        g.buildLevel(level)
        rle = RLEnvironment(game, level, observationType='global', visualize=True)
        # Set up RLE
        rle.actionDelay = 200
        rle.recordingEnabled = True
        rle.reset()
        rle._game._drawAll()
        environment = rle
    # TODO
    elif mode == "ale":
        return
    else:
        return
    # Return environment
    return environment


# Observe state from game environment and output in basic format
def observeState(mode, environment):
    if mode == "vgdl":
        # Get environment information
        state = environment._obstypes.copy()
        # Get agent information
        agentState = environment.getState()
        state['agent'] = [(agentState[0], agentState[1])]
    # TODO
    elif mode == "ale":
        return
    else:
        return
    return state


# Perform action in game enviroment and output reward and whether game has ended
def performAction(model, mode, environment, action):
    if mode == "vgdl":
        # Take action
        actionVector = np.array(model.dictionaries[ACTION][0][action])
        environment._performAction(actionVector)
        environment._game._drawAll()
        # Get reward
        (ended, won) = environment._isDone()
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
    elif mode == "ale":
        return
    else:
        return
    return
