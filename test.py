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
    rewards = [0 for ep in range(numEpisodes)]

    # Intialise model and Q-function
    M = blox.Model(mode, initState)
    M.obsActions = [["UP", "LEFT", "DOWN", "RIGHT", "nothing"],[[1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1],[0,0,0,0]]]
    M.updateDicts()
    # Q = QFunction(mode)

    # Learn model and Q-function
    for i in range(numEpisodes):
        print("===========")
        print("Episode " + str(i))
        # Set up model for new episode
        environment = inta.setup(mode, test=True)
        initState = inta.observeState(mode, environment)
        M.initialise(mode, initState)
        ended = False
        current_reward = 0
        for j in range(numSteps):
            # Check if the game has ended
            if ended:
                rewards[i] = current_reward
                print("*********")
                print("Game Over")
                print("Score: " + str(current_reward))
                print("*********")
                break
            else:
                print("-------")
                print("Step " + str(j))
                # Take action in the game
                action = choice(M.obsActions[0][:4])
                print("Action: " + action)
                # action = "DOWN"
                [reward, ended] = inta.performAction(M, mode, environment, action)
                current_reward += reward
                # If the game has ended we only update the action and reward, as the state doesn't matter
                if not ended:
                    state = [inta.observeState(mode, environment), action, reward]
                else:
                    state[1] = action
                    state[2] = reward
                # Update model, data, and schemas
                M.prev = M.getModelState()

                M.oldMap = deepcopy(M.objMap)

                M.updateModel(mode, state)

                M.curr = M.getModelState()

                M.updateData()

                M.learn()

                # Update Q-function using model
                # Q.update(M)

    print("Rewards:")
    print rewards

    print("Schemas:")
    for i in M.schemas:
        for j in i.keys():
            for k in i[j]:
                k.display()

    print("Evidence:")
    for i in M.evidence:
        for j in i.keys():
            for k in i[j]:
                print k

    print("Remaining:")
    for i in M.XY:
        for j in i.keys():
            for k in i[j]:
                print k

    # Verify properties of model, Q-function, or resulting policies
    # TODO

run("vgdl",5,50)