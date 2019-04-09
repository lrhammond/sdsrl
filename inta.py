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


def createPrologFile(model):
    f = open("models/" + model.name + ".pl", "w+")
    # f = open(os.path.join("models/", model.name + ".pl"), "w")
    # Write title, setup information, and options to file
    f.write("% Prolog model for " + model.name + "\n\n")
    f.write("""% Libraries
:- use_module(library(planning)).
:- use_module(library(lists)).
:- use_module(library(system)).
:- use_module(library(dcpf)).
:- use_module(library(distributionalclause)).
:- use_module(library(sst)).

% Options
:- set_options(default).
:- set_query_propagation(true).
:- set_inference(backward(lazy)).
:- set_current2nextcopy(false).

% Parameters
maxV(D,100):t <- true.
getparam(params) :- bb_put(user:spant,0),
                    setparam(
                        % Enable abstraction
                        true,
                        % Ratio of the samples reserved for the first action
                        1.0,
                        % Use correct formula (leave true)
                        true,
                        % Strategy to store V function
                        max,
                        % Execute action
                        best,
                        % most,
                        % Domain
                        propfalse,
                        % relfalse,
                        % Discount
                        0.95,
                        % Probability to explore in the beginning (first sample)
                        0.25,
                        % Probability to explore in the end (last sample)
                        0.05,
                        % Number of previous samples to use to estimate Q (larger is better but slower)
                        100,
                        % Max horizon span
                        200,
                        % Lambda init
                        0.9,
                        % Lambda final
                        0.9,
                        % UCBV
                        false,
                        % Decay
                        0.015,
                        % Action selection
                        softmax,
                        % egreedy,
                        % Pruning
                        0,
                        % WHeuInit
                        -0.1,
                        % WHeuFinal
                        -0.1),
                    !.

% Functions
builtin(x_pos(_)).
builtin(y_pos(_)).
builtin(x_size(_)).
builtin(y_size(_)).
builtin(colour(_)).
builtin(shape(_)).
builtin(nothing(_)).
Var:t+1 ~ val(Val) <- observation(Var) ~= Val.
observation(Var):t+1 ~ val(Val) <- Var:t+1 ~= Val.
attributes(Obj, X_pos, Y_pos, X_size, Y_size, Colour, Shape, Nothing):t <- x_pos(Obj):t ~= X_pos, 
                                                                           y_pos(Obj):t ~= Y_pos, 
                                                                           x_size(Obj):t ~= X_size, 
                                                                           y_size(Obj):t ~= Y_size, 
                                                                           colour(Obj):t ~= Colour,
                                                                           shape(Obj):t ~= Shape,
                                                                           nothing(Obj):t ~= Nothing.\n\n""")
    # Write constants to file
    f.write("% Constants\n")
    observations = util.flatten([model.obsXsizes[0], model.obsYsizes[0], model.obsColours[0], model.obsShapes[0], model.obsNothing[0]])
    observations = list(set(observations))
    constants = [c + " = " + c.lower() for c in observations]
    constant_list = ", ".join(constants)
    f.write("constants <- " + constant_list + ".\n")
    f.write("constants.\n\n")
    # Write actions to file
    f.write("% Actions\n")
    actions = ",".join(model.obsActions[0])
    f.write("adm(action(A)):t <- member(A, [" + actions + "]).\n\n")
    # Write neighbour relations to file
    f.write("""% Neighbours
nb1(Obj,Nb):t <- attributes(Obj, Xobj, Yobj, _, _, _, _, _):t, attributes(Nb, Xnb, Ynb, _, _, _, _, _):t, Xnb is Xobj - 1, Ynb is Yobj + 1.
nb2(Obj,Nb):t <- attributes(Obj, Xobj, Yobj, _, _, _, _, _):t, attributes(Nb, Xnb, Ynb, _, _, _, _, _):t, Xnb is Xobj    , Ynb is Yobj + 1.
nb3(Obj,Nb):t <- attributes(Obj, Xobj, Yobj, _, _, _, _, _):t, attributes(Nb, Xnb, Ynb, _, _, _, _, _):t, Xnb is Xobj + 1, Ynb is Yobj + 1.
nb4(Obj,Nb):t <- attributes(Obj, Xobj, Yobj, _, _, _, _, _):t, attributes(Nb, Xnb, Ynb, _, _, _, _, _):t, Xnb is Xobj + 1, Ynb is Yobj    .
nb5(Obj,Nb):t <- attributes(Obj, Xobj, Yobj, _, _, _, _, _):t, attributes(Nb, Xnb, Ynb, _, _, _, _, _):t, Xnb is Xobj + 1, Ynb is Yobj - 1.
nb6(Obj,Nb):t <- attributes(Obj, Xobj, Yobj, _, _, _, _, _):t, attributes(Nb, Xnb, Ynb, _, _, _, _, _):t, Xnb is Xobj    , Ynb is Yobj - 1.
nb7(Obj,Nb):t <- attributes(Obj, Xobj, Yobj, _, _, _, _, _):t, attributes(Nb, Xnb, Ynb, _, _, _, _, _):t, Xnb is Xobj - 1, Ynb is Yobj - 1.
nb8(Obj,Nb):t <- attributes(Obj, Xobj, Yobj, _, _, _, _, _):t, attributes(Nb, Xnb, Ynb, _, _, _, _, _):t, Xnb is Xobj - 1, Ynb is Yobj    .\n\n""")
# NEW FORMATTING USED HERE
# nb1(X,Y):t <- x_pos(Y):t ~= X_pos_y, x_pos(X):t ~= X_pos_x, X_pos_y is X_pos_x - 1, y_pos(Y):t ~= Y_pos_y, y_pos(X):t ~= Y_pos_x, Y_pos_y is Y_pos_x + 1.
# nb2(X,Y):t <- x_pos(Y):t ~= X_pos_y, x_pos(X):t ~= X_pos_x, X_pos_y = X_pos_x,      y_pos(Y):t ~= Y_pos_y, y_pos(X):t ~= Y_pos_x, Y_pos_y is Y_pos_x + 1.
# nb3(X,Y):t <- x_pos(Y):t ~= X_pos_y, x_pos(X):t ~= X_pos_x, X_pos_y is X_pos_x + 1, y_pos(Y):t ~= Y_pos_y, y_pos(X):t ~= Y_pos_x, Y_pos_y is Y_pos_x + 1.
# nb4(X,Y):t <- x_pos(Y):t ~= X_pos_y, x_pos(X):t ~= X_pos_x, X_pos_y is X_pos_x + 1, y_pos(Y):t ~= Y_pos_y, y_pos(X):t ~= Y_pos_x, Y_pos_y = Y_pos_x.
# nb5(X,Y):t <- x_pos(Y):t ~= X_pos_y, x_pos(X):t ~= X_pos_x, X_pos_y is X_pos_x + 1, y_pos(Y):t ~= Y_pos_y, y_pos(X):t ~= Y_pos_x, Y_pos_y is Y_pos_x - 1.
# nb6(X,Y):t <- x_pos(Y):t ~= X_pos_y, x_pos(X):t ~= X_pos_x, X_pos_y = X_pos_x,      y_pos(Y):t ~= Y_pos_y, y_pos(X):t ~= Y_pos_x, Y_pos_y is Y_pos_x - 1.
# nb7(X,Y):t <- x_pos(Y):t ~= X_pos_y, x_pos(X):t ~= X_pos_x, X_pos_y is X_pos_x - 1, y_pos(Y):t ~= Y_pos_y, y_pos(X):t ~= Y_pos_x, Y_pos_y is Y_pos_x - 1.
# nb8(X,Y):t <- x_pos(Y):t ~= X_pos_y, x_pos(X):t ~= X_pos_x, X_pos_y is X_pos_x - 1, y_pos(Y):t ~= Y_pos_y, y_pos(X):t ~= Y_pos_x, Y_pos_y = Y_pos_x.\n\n""")
    # Write 'nothing' rules to file
    f.write("% Nothing\n")
    objects = ",".join(["obj" + str(i) for i in model.objects.keys()])
    f.write("nothing(Obj):t ~ val(Nothing) <- member(Obj, [" + objects + "]), Nothing = no.\n")
    f.write("nothing(Obj):t ~ val(Nothing) <- \+member(Obj, [" + objects + "]), Nothing = yes.\n\n")
    # Write attribute schemas to file
    attributes = ["x_pos", "y_pos", "x_size", "y_size", "colour", "shape", "nothing"]
    change = {"centre":"", "left":" - 1", "right":" + 1", "below":" - 1", "above":" + 1"}
    f.write("% Attribute Schemas\n")
    # for i in range(len(model.schemas) - 1):
    #     for j in model.schemas[i].keys():
    #         for k in model.schemas[i][j]:
    #             if i == X_POS or i == Y_POS:
    #                 att = attributes[i]
    #                 f.write(att + "(X):t+1 ~ val(New) <- (" + k.display() + " = New, " + att + "(X):t ~= Curr, " + k.head + " is Curr" + change[k.head] + " ; " + att + "(X):t ~= New).\n")
    #             else:
    #                 f.write(attributes[i] + "(X):t+1 ~ val(New) <- (" + k.display() + " = New ; " + attributes[i] + "(X):t) ~= New).\n")
    for i in range(len(model.schemas) - 1):
        numSchemas = 0
        att = attributes[i]
        for j in model.schemas[i].keys():
            for k in range(len(model.schemas[i][j])):
                s = model.schemas[i][j][k]
                numSchemas += 1
                if i == X_POS or i == Y_POS:
                    f.write("schema_" + att + "(Obj, New):t <- " + s.display(no_head=True) + ", " + att + "(Obj):t ~= Curr, New is Curr" + change[s.head] + ".\n")
                else:
                    f.write("schema_" + att + "(Obj, New):t <- " + s.display() + " = New.\n")
        if numSchemas != 0:
            f.write("no_schema_" + att + "(Obj, New):t <- \+schema_" + att + "(Obj, _):t, " + att + "(Obj):t ~= New.\n")
            f.write(att + "(Obj):t+1 ~ val(New) <- schema_" + att + "(Obj, New):t.\n")
            f.write(att + "(Obj):t+1 ~ val(New) <- no_schema_" + att + "(Obj, New):t.\n")
        else:
            f.write(att + "(Obj):t+1 ~ val(New) <- " + att + "(Obj):t ~= New.\n")
    f.write("\n")
    # Write reward schemas to file
    f.write("% Reward Schemas\n")
    for r in model.schemas[-1].keys():
        for s in model.schemas[-1][r]:
            f.write("reward:t+1 ~ val(Reward) <- (" + s.display() + " = Reward ; Reward = -1).\n")


    f.write("reward(Obj):t ~ val(R) <- attributes(Obj, X_pos, Y_pos, X_size, Y_size, Colour, Shape, Nothing):t, schema_reward(R, X_pos, Y_pos, X_size, Y_size, Colour, Shape, Nothing):t.\n")
    f.write("reward(Obj):t ~ val(-1) <- attributes(Obj, X_pos, Y_pos, X_size, Y_size, Colour, Shape, Nothing):t, \+schema_reward(R, X_pos, Y_pos, X_size, Y_size, Colour, Shape, Nothing):t.\n")
    f.write("reward:t ~ val(R) <- reward(Obj):t ~= R.\n\n")
    # Write initialisation details to file (DOESN'T SEEM TO WORK PROPERLY)
    # f.write("% Initialisation\n")
    # for key in model.objects.keys():
    #     object = model.objects[key]
    #     f.write(object.display())
    # Write constraints to file
    # TODO
    f.close()
    return