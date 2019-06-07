# TEST
# Script for running tests of other functions

import lern
import veri
import main

# main.run(name="one", mode="vgdl", numEpisodes=2, numSteps=50, numSamples=50, discount=0.95, horizon=5, manual_episodes=1)
# main.run(name="two", mode="vgdl", numEpisodes=1, numSteps=250, numSamples=50, discount=0.95, horizon=5, manual_episodes=0)
main.run(name="det", mode="vgdl", numEpisodes=2, numSteps=50, numSamples=50, discount=0.95, horizon=5, manual_episodes=1)
