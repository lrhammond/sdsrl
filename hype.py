# HYPE
# Runs the Hype algorithm via the PyDC wrapper using command line arguments


from pydc import HYPE
import sys

# Set variables to respective command line arguments
name = sys.argv[1]
num_samples = int(sys.argv[2])
observations = sys.argv[3]

print observations

# Create model
hype = HYPE("models/" + name + ".pl", num_samples)



# Run HYPE and output the best action
result = hype.plan_step(observations, num_samples, max_horizon=10, used_horizon=6, use_abstraction=True)
# Check if action was successfully selected
if result["best_action"] == "" or result["best_action"] == None:
    print("HYPE failed to select an action on this iteration")
    best_action = "N/A"
else:
    best_action = result["best_action"][7:-1]
# Save most recent best action to file
f = open("models/" + name + ".txt", "a+")
f.write("\n" + best_action)
f.close()



# from pydc import DDC
#
# model = DDC("models/" + name + ".pl", num_samples)
# model.query("c.")