# VERI
# Functions for entering, storing, and managing constraints, as well as verifying policies and plans

from copy import deepcopy
import util

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

def create_dtpl_file(model, num_steps, gamma):

    # Open file and write header
    f = open("models/" + model.name + ".pl", "w+")
    f.write("% DTProblog model for " + model.name + "\n")
    # Add possible decisions for each timestep
    f.write("\n% Decisions\n")
    for i in range(num_steps):
        f.write("?::l({0}); ?::r({0}); ?::u({0}); ?::d({0}); ?::none({0}).\n".format(i))
    # Add description of initial object attributes
    f.write("\n% Objects\n")
    for o in model.objects.keys():
        f.write("att(" + model.objects[o].display() + ", 0).\n")
    # Add attributes for 'no_object' placeholder
    f.write("\n% No object\n")
    f.write("att(no_object, X_pos, Y_pos, X_size, Y_size, Colour, Shape, yes, T) :- not(att(Obj, X_pos, Y_pos, X_size, Y_size, Colour, Shape, no, T)).\n")
    # Add attribute schemas, including one that says nothing changes if no action is taken
    f.write("\n% Attribute schemas\n")
    f.write("att(Obj, X_pos, Y_pos, X_size, Y_size, Colour, Shape, Nothing, T1) :- att(Obj, X_pos, Y_pos, X_size, Y_size, Colour, Shape, Nothing, T0), T1 is T0 + 1, none(T0).\n")
    for a in range(REWARD - 1):
        for k in model.schemas[a].keys():
            for s in model.schemas[a][k]:
                f.write(util.to_problog_rule(a, k, s) + "\n")
    # Add reward schemas, including one that describes a negative reward present if no other reward is achieved
    f.write("\n% Reward schemas\n")
    rewards = []
    r_index = 0
    for r in model.schemas[REWARD].keys():
        for s in model.schemas[REWARD][r]:
            rewards.append(s.head)
            f.write("r{0}(T0) :-".format(r_index) + util.to_problog_rule(a, r, s) + ".\n")
            r_index += 1
    no_reward_list = ["not(r{}(T))".format(r) for r in range(r_index)]
    f.write("no_r(T0) :- " + ", ".join(no_reward_list) + ".\n")
    # Add resultant utilities gained from the different rewards over all timesteps
    f.write("\n% Utility scores\n")
    for i in range(len(rewards)):
        for j in range(num_steps):
            print("utility(r{0}({1}), {2} * {3} ** {1}).\n".format(i, j, rewards[i], gamma))
    for n in range(num_steps):
        f.write("utility(no_r({0}), {1} * {2} ** {0}).\n".format(n, -1, gamma))
    f.close()




