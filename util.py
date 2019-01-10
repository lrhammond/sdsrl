# UTIL
# Various utility and helper functions, as well as 


from numpy import array
frfrom numpy import array
from sklearn.preprocessing import LabelEncoder
from sklearn.preprocessing import OneHotEncoder
from operator import add


# One-hot encode a list of values or categories and return the new list
def oneHot(inputList):
    
    label_encoder = LabelEncoder()
    onehot_encoder = OneHotEncoder(sparse=False)
    
    a = array(inputList)
    i = label_encoder.fit_transform(a)
    i = i.reshape(len(i), 1)
    b = onehot_encoder.fit_transform(i)
    
    outputList = b.tolist()
    
    return outputListom sklearn.preprocessing import LabelEncoder
    

# Given a position return a list of the eight surrounding neighbours and their relative position vectors
def neighbourPositions(pos):

    n1 = [tuple(map(add, pos, (-1, 1))), [1, 0, 0, 1]]
    n2 = [tuple(map(add, pos, (0, 1))), [1, 0, 0, 0]]
    n3 = [tuple(map(add, pos, (1, 1))), [1, 1, 0, 0]]
    n4 = [tuple(map(add, pos, (1, 0))), [0, 1, 0, 0]]
    n5 = [tuple(map(add, pos, (1, -1))), [0, 1, 1, 0]]
    n6 = [tuple(map(add, pos, (0, -1))), [0, 0, 1, 0]]
    n7 = [tuple(map(add, pos, (-1, -1))), [0, 0, 1, 1]]
    n8 = [tuple(map(add, pos, (-1, 0))), [0, 0, 0, 1]]
    
    return [n1, n2, n3, n4, n5, n6, n7, n8]
    