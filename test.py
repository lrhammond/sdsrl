# TEST
# Script for running tests of other functions

import lern
import veri
import main

# Runs learning and verification procedures
def run(name, mode, numEpisodes, numSteps, numSamples, epsilon):

    # Learn model, Q-function, and policies
    lern.hyperMax(name, mode, numEpisodes, numSteps, numSamples, epsilon)

    # Verify properties of model, Q-function, or resulting policies
    # TODO

    return

run("test","vgdl",10, 50, 100, 0)