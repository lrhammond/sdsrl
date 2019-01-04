import numpy as np
import csv


def recordState(mode, fileName, state):

    f = open(fileName+".csv", "a")

    if mode == "vgdl":
        # Record objects and their attributes
        i = 1
        for key in state[0].keys():
            for pos in state[0][key]:
                line = [i, pos[0], pos[1], key]
                string = ",".join([str(x) for x in line])
                f.write(string+"\n")
                i = i + 1
        # Record reward and action
        f.write("r,"+str(state[1])+"\n")
        f.write("a,"+str(state[2])+"\n")
        f.write("-\n")

    else:
        # TODO
        return

    f.close()
    

def encodeState(mode, fileName, state)

    f = open(fileName+".csv", "a")

    if mode == "vgdl":
        # Initialise set of actions and rewards
        actions = []
        rewards = []
        # Record objects and their attributes
        i = 1
        types = list(state[0].keys())
        for T in types:
            tPos =  types.index(T)
            tVector = [0 for t in types]
            tVector[tPos] = 1
            for pos in state[0][T]:
                line = [i, pos[0], pos[1]] + tVector
                string = ",".join([str(x) for x in line])
                f.write(string+"\n")
                i = i + 1
        # Record reward
        R = state[1]
        if R is not in rewards:
            rewards.append(R)
        rPos = rewards.index[R]
        rVector = [0 for reward in rewards]
        rVector[rPos] = 1
        f.write("r,"+str(rvector)+"\n")
        # Record action
        A = state[2]
        if A is not in actions:
            actions.append(A)
        aPos = actions.index[A]
        aVector = [0 for action in actions]
        aVector[aPos] = 1
        f.write("a,"+str(A)+"\n")
        # Record end of state
        f.write("-\n")

    else:
        # TODO
        return

    f.close()


def parseState(mode, fileName, states):

    f = open(fileName+".csv", "r")
    
    if mode == "vgdl":
        reader = csv.reader(f)
        for row in reader:
            
        
        
        
        
        
        