# INTA
# Functions for interfacing between game playing environments and the data needed for learning

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


import sys
sys.path.insert(0, 'pyvgdlmaster/vgdl')
from mdpmap import MDPconverter
from core import VGDLParser
from rlenvironment import RLEnvironment
import pygame
import util
import numpy as np
import os


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


def createPrologFile(model):
    f = open("models/" + model.name + ".pl", "w+")

    # f = open(os.path.join("models/", model.name + ".pl"), "w")

    # Write title, setup information, and options to file
    f.write("% Prolog model of MDP for " + model.name + "\n\n")
    f.write(
    """% Libraries
    :- use_module(library(planning)).
    :- use_module(library(lists)).

    % Options
    :- set_options(default).
    :- set_query_propagation(true).
    :- set_inference(backward(lazy)).
    :- set_current2nextcopy(false).
    
    % Functions
    builtin(x_pos(_)).
    builtin(y_pos(_)).
    builtin(x_size(_)).
    builtin(y_size(_)).
    builtin(colour(_)).
    builtin(shape(_)).
    builtin(nothing(_)).\n\n""")
    # Write actions to file
    f.write("% Actions\n")
    actions = ",".join(model.obsActions[0])
    f.write("adm(action(A):t <- member(A, ["+actions+"]).\n\n")
    # Write neighbour relations to file
    f.write(
    """% Neighbours
    nb1(X,Y):t <- x_pos(Y):t - x_pos(X):t = -1, y_pos(Y):t - y_pos(X):t = 1.
    nb2(X,Y):t <- x_pos(Y):t - x_pos(X):t = 0, y_pos(Y):t - y_pos(X):t =s 1.
    nb3(X,Y):t <- x_pos(Y):t - x_pos(X):t = 1, y_pos(Y):t - y_pos(X):t = 1.
    nb4(X,Y):t <- x_pos(Y):t - x_pos(X):t = 1, y_pos(Y):t - y_pos(X):t = 0.
    nb5(X,Y):t <- x_pos(Y):t - x_pos(X):t = 1, y_pos(Y):t - y_pos(X):t = -1.
    nb6(X,Y):t <- x_pos(Y):t - x_pos(X):t = 0, y_pos(Y):t - y_pos(X):t = -1.
    nb7(X,Y):t <- x_pos(Y):t - x_pos(X):t = -1, y_pos(Y):t - y_pos(X):t = -1.
    nb8(X,Y):t <- x_pos(Y):t - x_pos(X):t = -1, y_pos(Y):t - y_pos(X):t = 0.\n\n""")
    # Write attribute schemas to file
    attributes = ["x_pos", "y_pos", "x_size", "y_size", "colour", "shape", "nothing"]
    change = {"centre":"", "left":" - 1", "right":" + 1", "below":" - 1", "above":" + 1"}
    f.write("% Attribute Schemas\n")
    for i in range(len(model.schemas) - 1):
        for j in model.schemas[i].keys():
            for k in model.schemas[i][j]:
                if i == X_POS or i == Y_POS:
                    att = attributes[i]
                    f.write(att + "(X):t+1 = " + k.display() + ", " + k.head + " is "  + att + "(X):t" + change[k.head] +".\n")
                else:
                    f.write(attributes[i] + "(X):t+1 = " + k.display() + ".\n")
    f.write("\n")
    # Write reward schemas to file
    f.write("% Reward Schemas\n")
    f.write("reward:0 = 0.\n")
    for r in model.schemas[REWARD - 1].keys():
        for s in model.schemas[REWARD - 1][r]:
            f.write("reward:t+1 = " + s.display() + ".\n")
    f.write("\n")
    # Write initialisation details to file
    f.write("% Initialisation\n")
    for key in model.objects.keys():
        object = model.objects[key]
        f.write(object.display() + "\n")

    f.close()
    return