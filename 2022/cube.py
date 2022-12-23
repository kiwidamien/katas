"""Thinking about cube nets

For example, the cube net

  1
234
  56

Starting at face 4:
    Left is face 3
    Up is face 1
    Down is face 5
    Right is face 6*  (rotated)

Specifically
    Left: (row, 0) step left --> Face 3 (row, C)
    Up:   (0, col) step up   --> Face 1 (R, col)
    Down: (R, col) step down --> Face 5 (0, col)
    Right: (row, C) step right --> Face 6 (0, R-row) turn 90 deg right

What our cube net needs: a mapping from the edges of one cube to another in location and orientation

My input:
---------
 12
 3
45
6
"""
"""
 12
 3
45
6
"""

"""
X12X
X3XX
45XX
6XXX

1's neighbors:6,2,3,4
2's neighbors:4,5,3,1
3's neighbors:1,2,5,4
4's neighbors:3,5,6,2
5's neighbors:3,2,6,4
6's neighbors:4,5,2,1
"""


face_1 = {
    (1, 0): (3, 0),
    (0, 1): (2, 0),
    (-1, 0): (6, 1),
    (0, -1): (4, 2)
}

face_2 = {
    (1, 0): (3,1),
    (0, 1):  (5, 0),
    (0, -1): (1, 0),
    (-1, 0): (6, 0)
}

face_3 = {
    (1,0): (5,0),
    (0,1): (2,1),
    (-1,0): (1,0),
    (0,-1): (4,-1)
}


def _preprocess(rows_and_cols):
    d = {}
    for index, row in enumerate(rows_and_cols):
        for col_index, face_num in enumerate(row):
            if face_num != ' ':
                d[(index+1, col_index+1)] = int(face_num)
    return d


def expand_net(face, dict_face):
    location = [loc for loc, f in dict_face.items() if face==f][0]
    face = {}
    up = (location[0] - 1, location[1])
    down = (location[0] + 1, location[1])
    right = (location[0], location[1] + 1)
    left = (location[0], location[1] - 1)
    if up in dict_face:
        face[(-1, 0)]  = (dict_face[up], 0)
    if down in dict_face:
        face[(1, 0)] =  (dict_face[down], 0)
    else:
        new_loc = (location[0] + 1, location[0] + 1)
        if new_loc in dict_face:
            face[(1, 0)] = (dict_face[new_loc], 1)  
    if left in dict_face:
        face[(0, -1)] = (dict_face[left], 0)
    if right in dict_face:
        face[(0, 1)] = (dict_face[right], 0)
    return face


