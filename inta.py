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
        dims = rle.outdim
        environment = rle
    # TODO
    elif mode == "ale":
        return
    else:
        return
    # Return environment
    return environment, dims


# Observe state from game environment and output in basic format
def observeState(mode, environment, dims):
    if mode == "vgdl":
        # Get environment information
        state = environment._obstypes.copy()
        # Get agent information
        agentState = environment.getState()
        state['agent'] = [(agentState[0], agentState[1])]
        height = dims[0] - 1
        # Flip the y-axis for VGDL environments
        y_flip_state = {}
        for key in state.keys():
            y_flip_state[key] = [(i, height-j) for (i,j) in state[key]]
        state = y_flip_state
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


def createPrologFile(model, numSamples, gamma=0.95, horizon=10):

    # Form list of observations describing the current/initial state
    observations = ""
    for key in model.objects.keys():
        object = model.objects[key]
        observations += object.observe()
    obs_list = [model.objects[key].observe() for key in model.objects.keys()] + ["observation(nothing(no_object))~=yes"]
    observations = ", ".join(obs_list)
    state = observations
    observations = "[" + observations + "]"
    observations = observations.replace(" ", "")
    f = open("models/" + model.name + "/hype_model.pl", "w+")
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
                        {0},
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
Var:t+1 ~ val(Val) <- observation(Var) ~= Val.
observation(Var):t+1 ~ val(Val) <- Var:t+1 ~= Val.
attributes(Obj, X_pos, Y_pos, X_size, Y_size, Colour, Shape, Nothing):t <- x_pos(Obj):t ~= X_pos, 
                                                                           y_pos(Obj):t ~= Y_pos, 
                                                                           x_size(Obj):t ~= X_size, 
                                                                           y_size(Obj):t ~= Y_size, 
                                                                           colour(Obj):t ~= Colour,
                                                                           shape(Obj):t ~= Shape,
                                                                           nothing(Obj):t ~= Nothing.\n\n""".format(gamma))
    # Write actions to file
    f.write("% Actions\n")
    actions = ",".join(model.obsActions[0])
    f.write("adm(action(A)):t <- member(A, [" + actions + "]).\n\n")
    # Write neighbour relations to file
    f.write("""% Neighbours
nb1(Obj,Nb):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, NbX is X - 1, NbY is Y + 1, map(NbX, NbY, Nb):t.
nb2(Obj,Nb):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, NbX is X,     NbY is Y + 1, map(NbX, NbY, Nb):t.
nb3(Obj,Nb):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, NbX is X + 1, NbY is Y + 1, map(NbX, NbY, Nb):t.
nb4(Obj,Nb):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, NbX is X + 1, NbY is Y    , map(NbX, NbY, Nb):t.
nb5(Obj,Nb):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, NbX is X + 1, NbY is Y - 1, map(NbX, NbY, Nb):t.
nb6(Obj,Nb):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, NbX is X,     NbY is Y - 1, map(NbX, NbY, Nb):t.
nb7(Obj,Nb):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, NbX is X - 1, NbY is Y - 1, map(NbX, NbY, Nb):t.
nb8(Obj,Nb):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, NbX is X - 1, NbY is Y    , map(NbX, NbY, Nb):t.\n\n""")
    # Write map rules to file
    f.write("""% Map
map(X, Y, Obj):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y.
map(X, Y, no_object):t <- """)
    places = ["\+((map(X, Y, obj{0}):t))".format(i) for i in model.objects.keys()]
    f.write(", ".join(places) + ".\n\n")
    # Write 'nothing' rule to file
    f.write("% Nothing\n")
    f.write("nothing(no_object):t+1 ~ val(Curr) <- nothing(no_object):t ~= Curr.\n\n")
    # Write attribute schemas to file
    attributes = ["x_pos", "y_pos", "x_size", "y_size", "colour", "shape", "nothing"]
    change = {"centre":"", "left":" - 1", "right":" + 1", "below":" - 1", "above":" + 1"}
    f.write("% Attribute Schemas\n")
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
            f.write(att + "(Obj):t+1 ~ val(New) <- schema_" + att + "(Obj, New):t.\n")
        f.write(att + "(Obj):t+1 ~ val(Curr) <- " + att + "(Obj):t ~= Curr.\n")
    f.write("\n")
    # Write reward schemas to file
    f.write("% Reward Schemas\n")
    f.write("""reward:t ~ val(-10) <-  map(1, 2, obj19):t, action(d).
reward:t ~ val(-10) <- map(2, 1, obj19):t, action(l).
reward:t ~ val(10) <- map(2, 1, obj19):t, action(r).
reward:t ~ val(-1) <- map(1, 2, obj19):t, \+action(d).
reward:t ~ val(-1) <- map(1, 2, obj19):t, \+action(l).
reward:t ~ val(-1) <- map(1, 1, obj19):t, \+action(r).
reward:t ~ val(-1) <- \+(map(1, 2, obj19):t), \+(map(2, 1, obj19):t).\n\n""")
#     f.write("""reward:t ~ val(-10) <-  map(1, 1, obj19):t.
# reward:t ~ val(10) <- map(3, 1, obj19):t.
# reward:t ~ val(0) <- \+((map(1,1,obj19):t)), \+((map(3,1,obj19):t)).""")
    # for r in model.schemas[-1].keys():
    #     for s in model.schemas[-1][r]:
    #         f.write("reward:t+1 ~ val(Reward) <- (" + s.display() + " = Reward ; Reward = -1).\n")

    # f.write("reward(Obj):t ~ val(R) <- attributes(Obj, X_pos, Y_pos, X_size, Y_size, Colour, Shape, Nothing):t, schema_reward(R, X_pos, Y_pos, X_size, Y_size, Colour, Shape, Nothing):t.\n")
    # f.write("reward(Obj):t ~ val(-1) <- attributes(Obj, X_pos, Y_pos, X_size, Y_size, Colour, Shape, Nothing):t, \+schema_reward(R, X_pos, Y_pos, X_size, Y_size, Colour, Shape, Nothing):t.\n")
    # f.write("reward:t ~ val(R) <- reward(Obj):t ~= R.\n\n")
    # Write initialisation details to file (DOESN'T SEEM TO WORK PROPERLY)
    # f.write("% Initialisation\n")
    # for key in model.objects.keys():
    #     object = model.objects[key]
    #     f.write(object.display())


    # Write constraints to file
    # TODO

    # Write run command to file
    f.write("% Run command\n")
    f.write("run :- executedplan_start,executedplan_step(BA,true," + observations + ",{0},{1},TotalR,T,{1},STOP),print(BA),halt.".format(numSamples, horizon))


    f.close()
    return