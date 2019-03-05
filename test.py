# TEST
# Script for running tests of other functions

import lern
import veri
import main

# Runs learning and verification procedures
def run(mode, numEpisodes, numSteps, numSamples, epsilon):

    # Learn model, Q-function, and policies
    lern.hyperMax(mode, numEpisodes, numSteps, numSamples, epsilon)

    # Verify properties of model, Q-function, or resulting policies
    # TODO

    return

run("vgdl",5, 50, 10, 0)