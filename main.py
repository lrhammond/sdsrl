# MAIN
# Primary script for running learning and verification procedures

from copy import deepcopy
import inta


# Main function
def main(mode, numEpisodes, numSteps):
    
    # Set up game according to mode and return description of intial state
    environment = setup(mode)
    initState = [inta.observeState(mode, environment), None, None]
    
    # Intialise model and Q function
    M = Model(mode, initState)
    Q = QFunction(mode)
    
    # Learn model and Q function
    for i in range(numEpisodes):
        ended = False
        for j in range(numSteps):
            # Check if the game has ended
            if ended == True:
                break
            else:
                # Take action in the game
                action = Q.chooseAction(mode, M)
                [reward, ended] = inta.performAction(M, mode, environment, action)
                state = [inta.observeState(mode, environment), action, reward]
                # Update model, data, and schemas
                M.prev = M.getModelState()
                M.oldMap = deepcopy(M.objMap)
                M.updateModel(state)
                M.curr = M.getModelState()
                M.updateData()
                M.learn()
                # Update Q function
    
    # Verify properties of model, Q function, or resulting policies
    
    
    # Currently only the VGDL and ALE environments are supported    
    else:
        print("Please enter either 'vgdl' or 'ale' as input mode to main()")
        return