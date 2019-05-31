# TEST
# Script for running tests of other functions

import lern
import veri
import main

# Runs learning and verification procedures
def main(name, mode, numEpisodes, numSteps, numSamples, epsilon, manual_episodes):

    # Learn model, Q-function, and policies
    lern.run(name, mode, numEpisodes, numSteps, numSamples, epsilon, manual_episodes)

    # Verify properties of model, Q-function, or resulting policies
    # TODO

    return

main("def","vgdl",4, 20, 100, 0, 1)