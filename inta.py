# INTA
# Functions for interfacing between game playing environments and the data needed for learning and forming constraints

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
import shutil
import blox
import pickle


# Setup game according to mode chosen
def setup(name, mode):

    if mode == "vgdl":

        # Read in level and game file information
        level = get_file(name, "level")
        game = get_file(name, "game")

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


# Reads in model if it already exists, or uses a different model for transfer learning, or creates a new model
def create_model(name, mode, initState, deterministic):

    # Create name for pickled model
    file_name = "models/{0}/model.pickle".format(name)

    # If the model already exists we load it back in
    if os.path.isfile(file_name):
        with open(file_name, 'rb') as f:
            model = pickle.load(f)
        print("Using existing model from the same environment")

    # Optionally use the same dynamics as in another environment
    elif raw_input("Use dynamics from another model (y/n)? ") == "y":
        path = ""
        while not os.path.isfile(path):
            path = raw_input("Input path to pickled model: ")
        with open(path, 'rb') as f:
            model = pickle.load(f)
        model.reset(name)
        print("Using existing model from a different environment")

    # Otherwise, create a new model and get actions
    else:
        model = blox.Model(name, mode, initState, deterministic)
        print("Creating new model for environment with unknown dynamics")
        model.obsActions = get_actions()
        model.updateDicts()

    return model


# Gets actions from user so they are specified in advance
def get_actions():

    # Get actions from user
    print("Please enter any and all actions that may be taken in the environment (excluding the \'none\' action)")
    print("When finished entering actions press return")
    action_list = []
    a = raw_input("Enter new action: ")
    while a != "":
        action_list.append(a)
        a = raw_input("Enter new action: ")

    # Add 'none' action and form list of actions
    obs_actions = [action_list, util.oneHot(action_list)]
    action_length = len(obs_actions[1][0])
    obs_actions[0].append("none")
    obs_actions[1].append([0 for i in range(action_length)])

    return obs_actions


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
        action_dict = dict(zip(["u", "l", "d", "r", "none"], [[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1], [0, 0, 0, 0]]))
        action_vector = np.array(action_dict[action])
        if action != "none":
            environment._performAction(action_vector)
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


def createPrologFile(model, num_samples, rmax_actions, constraints, discount, horizon):

    # Form list of observations describing the current/initial state
    obs_list = [model.objects[key].observe() for key in model.objects.keys()] + ["observation(nothing(no_object))~=yes"]
    observations = "[" + ",\n".join(obs_list) + "]"
    # observations = observations.replace(" ", "")
    f = open("models/" + model.name + "/hype_model.pl", "w+")

    # Write title, setup information, and options to file
    f.write("% Prolog model for " + model.name + "\n")
    f.write("""\n% Libraries
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
maxV(D,{0}):t <- true.
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
                        {1},
                        % Probability to explore in the beginning (first sample)
                        0.25,
                        % Probability to explore in the end (last sample)
                        0.15,
                        % Number of previous samples to use to estimate Q (larger is better but slower)
                        {0},
                        % Max horizon span
                        {0},
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
                    !.\n""".format(num_samples, discount))

    # Write core functions to file
    f.write("""\n% Core Functions
Var:t+1 ~ val(Val) <- observation(Var) ~= Val.
observation(Var):t+1 ~ val(Val) <- Var:t+1 ~= Val.\n""")

    # Write helper functions to file
    f.write("""\n% Helper Functions
dist(Obj1, Obj2):t ~ val(D) <- x_pos(Obj1):t ~= X1, y_pos(Obj1):t ~= Y1, x_pos(Obj2):t ~= X2, y_pos(Obj2):t ~= Y2, D is sqrt((X1 - X2)^2 + (Y1 - Y2)^2).
dist_gt(Obj1, Obj2, V):t <- dist(Obj1, Obj2):t ~= D, D > V.
dist_lt(Obj1, Obj2, V):t <- dist(Obj1, Obj2):t ~= D, D < V.
dist_eq(Obj1, Obj2, V):t <- dist(Obj1, Obj2):t ~= D, D = V.
right_of(Obj1, Obj2):t <- x_pos(Obj1):t ~= X1, x_pos(Obj2):t ~= X2, X1 > X2.
left_of(Obj1, Obj2):t <- x_pos(Obj1):t ~= X1, x_pos(Obj2):t ~= X2, X1 < X2.
above(Obj1, Obj2):t <- y_pos(Obj1):t ~= Y1, y_pos(Obj2):t ~= Y2, Y1 > Y2.
below(Obj1, Obj2):t <- y_pos(Obj1):t ~= Y1, y_pos(Obj2):t ~= Y2, Y1 < Y2.
bigger_x(Obj1, Obj2):t <- x_size(Obj1):t ~= XS1, x_size(Obj2):t ~= XS2, XS1 > XS2.
bigger_y(Obj1, Obj2):t <- y_size(Obj1):t ~= YS1, y_size(Obj2):t ~= YS2, YS1 > YS2.
bigger(Obj1, Obj2):t <- x_size(Obj1):t ~= XS1, x_size(Obj2):t ~= XS2, y_size(Obj1):t ~= YS1, y_size(Obj2):t ~= YS2, (XS1 * YS1) > (XS2 * YS2).
occupied_pos(X, Y):t <- is_object(Obj), map(X, Y, Obj):t.
unoccupied_pos(X, Y):t <- map(X, Y, no_object):t.
same_x_pos(Obj1, Obj2):t <- x_pos(Obj1):t ~= X1, x_pos(Obj2):t ~= X2, X1 = X2.
same_y_pos(Obj1, Obj2):t <- y_pos(Obj1):t ~= Y1, y_pos(Obj2):t ~= Y2, Y1 = Y2.
same_x_size(Obj1, Obj2):t <- x_size(Obj1):t ~= XS1, x_size(Obj2):t ~= XS2, XS1 = XS2.
same_y_size(Obj1, Obj2):t <- y_size(Obj1):t ~= YS1, y_size(Obj2):t ~= YS2, YS1 = YS2.
same_colour(Obj1, Obj2):t <- colour(Obj1):t ~= C1, colour(Obj2):t ~= C2, C1 = C2.
same_shape(Obj1, Obj2):t <- shape(Obj1):t ~= S1, shape(Obj2):t ~= S2, S1 = S2.
attributes(Obj, X_pos, Y_pos, X_size, Y_size, Colour, Shape, Nothing):t <- x_pos(Obj):t ~= X_pos, 
                                                                           y_pos(Obj):t ~= Y_pos, 
                                                                           x_size(Obj):t ~= X_size, 
                                                                           y_size(Obj):t ~= Y_size, 
                                                                           colour(Obj):t ~= Colour,
                                                                           shape(Obj):t ~= Shape,
                                                                           nothing(Obj):t ~= Nothing.\n""")

    # Write action rules to file
    f.write("\n% Actions\n")
    actions = ",".join(model.obsActions[0][:-1])
    f.write("""adm(action(A)):t <- member(A, [{0}]).
\+(action_performed:0) <- true.
action_performed:t+1 <- action(A), member(A, [{0}]).\n""".format(actions))

    # Write neighbour relations to file
    f.write("""\n% Neighbours
nb1(Obj,Nb):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, NbX is X - 1, NbY is Y + 1, map(NbX, NbY, Nb):t.
nb2(Obj,Nb):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, NbX is X,     NbY is Y + 1, map(NbX, NbY, Nb):t.
nb3(Obj,Nb):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, NbX is X + 1, NbY is Y + 1, map(NbX, NbY, Nb):t.
nb4(Obj,Nb):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, NbX is X + 1, NbY is Y    , map(NbX, NbY, Nb):t.
nb5(Obj,Nb):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, NbX is X + 1, NbY is Y - 1, map(NbX, NbY, Nb):t.
nb6(Obj,Nb):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, NbX is X,     NbY is Y - 1, map(NbX, NbY, Nb):t.
nb7(Obj,Nb):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, NbX is X - 1, NbY is Y - 1, map(NbX, NbY, Nb):t.
nb8(Obj,Nb):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, NbX is X - 1, NbY is Y    , map(NbX, NbY, Nb):t.\n""")

    # Write map rules to file
    f.write("""\n% Map
map(X, Y, Obj):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y.
map(X, Y, no_object):t <- """)
    places = ["\+((map(X, Y, obj{0}):t))".format(i) for i in model.objects.keys()]
    f.write(", ".join(places) + ".\n")

    # Write 'nothing' rule to file
    f.write("\n% Nothing\n")
    f.write("nothing(no_object):t+1 ~ val(Curr) <- nothing(no_object):t ~= Curr.\n")

    # Write 'is_object' rule to file
    f.write("\n% Objects\n")
    objects = ["obj{0}".format(i) for i in model.objects.keys()]
    f.write("is_object(Obj) <- member(Obj, [" + ",".join(objects) + "]).\n")

    # Write constraints to file
    f.write("\n% Constraints\n")
    f.write("constraints:t <- " + constraints + ".\n")

    # Write attribute schemas to file
    attributes = ["x_pos", "y_pos", "x_size", "y_size", "colour", "shape", "nothing"]
    change = {"centre":"", "left":" - 1", "right":" + 1", "below":" - 1", "above":" + 1"}
    f.write("\n% Attribute Schemas\n")
    for i in range(len(model.schemas) - 1):

        n_s = 0
        att = attributes[i]

        if not deterministic:
            string_info = {}

        for j in model.schemas[i].keys():
            for k in range(len(model.schemas[i][j])):

                s = model.schemas[i][j][k]
                n_s += 1

                # For deterministic models
                if model.deterministic:
                    if i == X_POS or i == Y_POS:
                        f.write("schema_" + att + "(Obj, New):t <- " + s.display(no_head=True) + ", " + att + "(Obj):t ~= Curr, New is Curr" + change[s.head] + ".\n")
                    else:
                        f.write("schema_" + att + "(Obj, New):t <- " + s.display() + " = New.\n")

                # For non-deterministic models
                else:

                    prob = float(s.positive) / (s.positive + s.negative)
                    pred = att + "_" + str(j) + "_" + str(k)

                    if j not in string_info.keys():
                        string_info[j] = []
                    string_info[j].append(pred)

                    f.write("{0}(Obj):t ~ val({2}) <- {3}.\n".format(pred, prob, s.display(no_head=True)))
                    f.write("{0}(Obj):t ~ val(0) <- true.\n".format(pred))


        if model.deterministic:
            if n_s != 0:
                f.write(att + "(Obj):t+1 ~ val(New) <- schema_" + att + "(Obj, New):t.\n")
            f.write(att + "(Obj):t+1 ~ val(Curr) <- " + att + "(Obj):t ~= Curr.\n")

        else:

            if n_s != 0:

                pred_list = util.flatten([string_info(val) for val in string_info.keys()])

                zeros_list = [pred + "(Obj):t ~= 0" for pred in pred_list]
                zeros = ", ".join(zeros_list)

                f.write(att + "(Obj):t+1 ~ val(Curr) <- " + att + "(Obj):t ~= Curr, " + zeros + ".\n")

                vars_list =  [pred + "(Obj):t ~= " + pred.capitalize() for pred in pred_list]
                vars = ", ".join(vars_list)

                p_curr_calc_list = ["(1 - " + pred.capitalize() + ")" for pred in pred_list]
                p_curr_calc = " * ".join(p_curr_calc_list)

                probs_list = []
                denominator = "(" + " + ".join([pred.capitalize() for pred in pred_list]) + ")"
                for j in string_info.keys():
                    numerator = "(" + " + ".join([pred.capitalize() for pred in string_info[j]]) + ")"
                    probs_list.append("P_" + str(j) + " is " + numerator + " * (1 - P_curr) / " + denominator)
                probs =  ", ".join(probs_list)

                if i == X_POS or i == Y_POS:

                    dist_list = ["P_{0} : {1}".format(val, val.capitalize()) for val in string_info.keys()] + ["P_curr : Curr"]
                    dist = "[" + ", ".join(dist_list) + "]"

                    pos_list = [val.capitalize() + " is Curr" + change[val] for val in string_info.keys()]
                    pos = ", ".join(pos_list)

                    f.write("{0}(Obj):t+1 ~ finite({1}) <- {0}(Obj):t ~= Curr, {2}, {3}, P_curr is {4}, {5}.".format(att, dist, pos, vars, p_curr_calc, probs)

                else:

                    dist_list = ["P_{0} : {0}".format(val) for val in string_info.keys()] + ["P_curr : Curr"]
                    dist = "[" + ", ".join(dist_list) + "]"

                    f.write("{0}(Obj):t+1 ~ finite({1}) <- {0}(Obj):t ~= Curr, {2}, P_curr is {3}, {4}.".format(att, dist, vars, p_curr_calc, probs)

            else:
                f.write(att + "(Obj):t+1 ~ val(Curr) <- " + att + "(Obj):t ~= Curr.\n")

    # Write reward schemas to file
    f.write("\n% Reward Schemas\n")
    if len(rmax_actions) != 0:
        f.write("reward:t ~ val({0}) <- constraints:t, action(A), member(A, [{1}]), \+action_performed:t.\n".format(model.RMAX, ",".join(rmax_actions)))
    num_r_schemas = 0
    for r in model.schemas[REWARD].keys():
        for s in model.schemas[REWARD][r]:
            f.write("reward:t ~ val({0}) <- constraints:t, is_object(Obj), ".format(r) + s.display(no_head=True) + ".\n")
            num_r_schemas += 1
    if num_r_schemas == 0:
        f.write("reward:t ~ val(0) <- true.\n")

    # Write run command to file
    f.write("\n% Run command\n")
    f.write("run :- executedplan_start,executedplan_step(BA,true," + observations + ",{0},{1},TotalR,T,{1},STOP),print(BA).".format(num_samples, horizon))
    f.close()

    return


# Gets a file if it doesn't already exists, copies it to the model folder, and returns the relevant data
def get_file(name, type):

    # Check if the file already exists, get the new path and copy to the model folder if it doesn't
    file_name = "models/{0}/{1}.txt".format(name, type)
    if not os.path.isfile(file_name):
        path = ""
        while not os.path.isfile(path):
            path = raw_input("Input path to {0} file: ".format(type))
        shutil.copy(path, file_name)

    # Read in the file
    with open(file_name, 'r') as f:
        output = f.read()

    return output
