# MAIN
# Primary script for running learning and verification procedures


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

from copy import deepcopy
import inta
import lern
import util
import blox
import pickle
import os
from random import choice, seed, random



# Run the main algorithm
def run(name, mode, numEpisodes, numSteps, numSamples, discount, horizon, deterministic=True, manual_episodes=0):

    seed()
    RMAX = 100

    print("+++++++++++++++++++++++")
    print("New experiment: " + name)
    print("+++++++++++++++++++++++")

    # Create directory to store files in
    if not os.path.exists("models/" + name):
        os.makedirs("models/" + name)

    # Set up game according to mode and return description of initial state
    environment, dims = inta.setup(name, mode)
    initState = inta.observeState(mode, environment, dims)
    rewards = [0 for _ in range(numEpisodes)]
    all_constraints = inta.get_file(name, "constraints")
    constraints = ", ".join(all_constraints.splitlines())

    # Create and save model
    model = inta.create_model(name, mode, initState, deterministic)
    with open("models/{0}/model.pickle".format(name), 'wb') as f:
        pickle.dump(model, f)


    model.RMAX = RMAX


    # Learn model and policy
    with open("models/" + model.name + "/episodes.txt", 'w+') as f:
        f.write("Episode action sequences and rewards for " + name + "\n\n")
    for i in range(numEpisodes):
        with open("models/" + model.name + "/episodes.txt", 'a') as f:
            f.write("Episode " + str(i) + "\n")
        print("===========")
        print("Episode " + str(i))

        # Set up model for new episode
        environment, dims = inta.setup(name, mode)
        initState = inta.observeState(mode, environment, dims)
        model.initialise(mode, initState)
        ended = False
        current_reward = 0
        for j in range(numSteps):

            # Check if the game has ended
            if ended or j == numSteps - 1:

                # Display endgame information
                rewards[i] = current_reward
                print("*********")
                print("Game Over")
                print("Score: " + str(current_reward))
                print("*********")
                with open("models/" + model.name + "/episodes.txt", 'a') as f:
                    f.write("Score: " + str(current_reward) + "\n\n")

                # Save model to be used again if needed
                model.clean()
                with open("models/{0}/model.pickle".format(name), 'wb') as f:
                    pickle.dump(model, f)

                break
            else:
                print("-------")
                print("Step " + str(j))

                # Clean up data, evidence, and learnt schemas
                model.clean()

                # If the current state is new we initialise it to have maximum reward
                state = util.to_tuple(sorted([(key, model.curr[key]) for key in model.curr.keys()]))
                if state not in model.R.keys():
                    model.R[state] = dict(zip(model.obsActions[0], [RMAX for _ in model.obsActions]))
                rmax_actions = [a for a in model.R[state] if model.R[state][a] == RMAX]

                # If using manual control for episode, input action
                if i in range(manual_episodes):
                    action = "N/A"
                    while action not in model.obsActions[0]:
                        action = raw_input("Enter action: ")
                    method = "input"


                    # if random() < 0.5:
                    #     action2 = choice(model.obsActions[0])
                    #     method = "noise"
                    # else:
                    #     action2 = action


                # Otherwise find the best action using HYPE
                # else:
                #     action, expected_value = lern.hypermax(model, numSamples, rmax_actions, constraints, discount, horizon)
                #     method = "HYPE"

                # action = "N/A"

                # If HYPE fails to select an action then we use RMAX, the policy (if available) or make a random choice
                if action == "N/A":
                    if len(rmax_actions) != 0:
                        action = choice(rmax_actions)
                        method = "RMAX"
                    elif state in model.pi.keys():
                        action = model.pi[state]
                        method = "policy"
                    else:
                        action = choice(model.obsActions[0])
                        method = "random"

                # Otherwise we perform the action selected by HYPE and update the policy
                else:
                    model.pi[state] = action


                # Output action information
                with open("models/" + model.name + "/episodes.txt", 'a') as f:
                    f.write(action + " (from {0})\n".format(method))
                print("Action taken: " + action)

                # If there is an action to be taken, perform it and update the reward
                if action != "none":
                    [reward, ended] = inta.performAction(model, mode, environment, action)
                    current_reward += reward
                    model.R[state][action] = reward

                # If the game has ended we only update the action and reward, as the state doesn't matter
                if not ended:
                    observation = [inta.observeState(mode, environment, dims), action, reward]
                else:
                    observation = [None, action, reward]

                # Update model
                model.prev = model.getModelState()
                model.oldMap = deepcopy(model.objMap)
                model.updateModel(mode, observation)
                model.curr = model.getModelState()
                model.obsChanges.update(set(util.changes(model)))

                # Save transition so we don't have to update our data or do learning next time
                new_state = util.to_tuple(sorted([(key, model.curr[key]) for key in model.curr.keys()]))
                transition = util.to_tuple([state, action, new_state, reward])
                state = transition[:2]

                # If the transition has not previously been observed then we update the data and learn transitions
                update_schema_counts = False
                new_trans = False
                if transition not in model.obsTrans.keys():
                    model.obsTrans[transition] = 1
                    model.schema_updates[transition], model.transition_data[transition] = model.updateData(ended)
                    if not ended:
                        new_trans = True
                elif model.error_made and not ended:
                    new_trans = True

                # Otherwise we update schema probability counts from our recorded observations
                elif not model.deterministic:
                    model.obsTrans[transition] += 1
                    model.update_schemas(transition, ended)
                else:
                    model.obsTrans[transition] += 1

                # If the state is also new then record it separately and learn rewards
                new_state = False
                if state not in model.obsState:
                    model.obsState.add(state)
                    new_state = True
                elif model.error_made:
                    new_state = True

                # Learn new schemas and update their success probabilities if required
                model.learn(new_trans, new_state)
                model.error_made = False
                if not model.deterministic:
                    model.update_probs()




                attributes = ["X_pos", "Y_pos", "X_size", "Y_size", "Colour", "Shape", "Nothing", "Reward"]
                print("======================================")
                print("Schemas at the end of step " + str(j) + ":")
                for att in range(REWARD + 1):
                    for val in model.schemas[att].keys():
                        for s in model.schemas[att][val]:
                            print(str(s.name) + " : " + str(s.positive) + "/" + str(s.negative) + " : " + attributes[
                                att] + " = " + str(val) + " <- " + s.display(no_head=True))
                print("======================================")




    # Print information from experiment
    print("Rewards:")
    print rewards
    print("Schemas:")
    attributes = ["X_pos", "Y_pos", "X_size", "Y_size", "Colour", "Shape", "Nothing", "Reward"]
    for i in range(len(model.schemas)):
        for j in model.schemas[i].keys():
            for k in model.schemas[i][j]:
                print(attributes[i] + " = " + str(j) + " <- " + k.display(no_head=True))
    # print("Evidence:")
    # for i in model.evidence:
    #     for j in i.keys():
    #         print("Attribute: " + str(j))
    #         for k in i[j]:
    #             print k
    # print("Remaining:")
    # for i in range(len(model.data)):
    #     for j in model.data[i].keys():
    #         if j == -1:
    #             continue
    #         print("Attribute: " + str(j))
    #         for k in model.data[i][j]:
    #             predicted = False
    #             for schema in model.schemas[i][j]:
    #                 if schema.isActive(k):
    #                     predicted = True
    #             if not predicted:
    #                 print k

    return



# # Runs learning and verification procedures
# def run(mode, numEpisodes, numSteps):
#
#     # Set up game according to mode and return description of intial state
#     environment = inta.setup(mode)
#     initState = [inta.observeState(mode, environment), None, None]
#
#     # Intialise model and Q-function
#     M = Model(mode, initState)
#     Q = QFunction(mode)
#
#     # Learn model and Q-function
#     for i in range(numEpisodes):
#         ended = False
#         for j in range(numSteps):
#             # Check if the game has ended
#             if ended == True:
#                 break
#             else:
#                 # Take action in the game
#                 action = Q.chooseAction(mode, M)
#                 [reward, ended] = inta.performAction(M, mode, environment, action)
#                 state = [inta.observeState(mode, environment), action, reward]
#                 # Update model, data, and schemas
#                 model.prev = model.getModelState()
#                 model.oldMap = deepcopy(model.objMap)
#                 model.updateModel(state)
#                 model.curr = model.getModelState()
#                 model.updateData()
#                 model.learn()
#                 # Update Q-function using model
#                 Q.update(M)
#     # Verify properties of model, Q-function, or resulting policies
#     # TODO
