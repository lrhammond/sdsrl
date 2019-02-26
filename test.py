# TEST
# Script for running tests of other functions

from copy import deepcopy
from random import choice
import inta
import blox
import lern
import util
import main

# Runs learning and verification procedures
def run(mode, numEpisodes, numSteps):

    # Set up game according to mode and return description of initial state
    environment = inta.setup(mode,test=True)
    initState = inta.observeState(mode, environment)

    # Intialise model and Q-function
    M = blox.Model(mode, initState)
    M.obsActions = [["UP", "LEFT", "DOWN", "RIGHT", "nothing"],[[1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1],[0,0,0,0]]]
    M.updateDicts()
    # Q = QFunction(mode)

    # Learn model and Q-function
    for i in range(numEpisodes):
        ended = False
        for j in range(numSteps):
            # Check if the game has ended
            if ended == True:
                break
            else:
                # Take action in the game
                action = choice(M.obsActions[0])
                action = "RIGHT"

                print action
                [reward, ended] = inta.performAction(M, mode, environment, action)
                state = [inta.observeState(mode, environment), action, reward]
                # Update model, data, and schemas
                M.prev = M.getModelState()
                M.oldMap = deepcopy(M.objMap)
                M.updateModel(mode, state)
                M.curr = M.getModelState()
                M.updateData()
                M.learn()
                # Update Q-function using model
                # Q.update(M)
    # Verify properties of model, Q-function, or resulting policies
    # TODO

run("vgdl",1,3)
